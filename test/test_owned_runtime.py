"""Ownership guard: real process groups, exact lock fences, EOF and PTY replay bytes."""
import base64
import json
import os
from pathlib import Path
import signal
import subprocess
import sys
import tempfile
import threading
import time

HELPER=Path(sys.argv[1]).resolve()

def wait(fn):
    for _ in range(200):
        if fn():return
        time.sleep(.02)
    raise AssertionError('condition did not settle')

def alive(pid):
    try:os.kill(pid,0);return True
    except ProcessLookupError:return False

with tempfile.TemporaryDirectory(prefix='monty-owned-') as root:
    root=Path(root).resolve();pidfile=root/'child'
    command=[sys.executable,'-u','-c',
             "import os,subprocess,time;from pathlib import Path;"
             "p=subprocess.Popen(['sleep','60']);"
             "Path("+repr(str(pidfile))+").write_text(str(os.getpid())+' '+str(p.pid));"
             "print('ready',flush=True);time.sleep(60)"]
    lock=str(root/'lock')
    owner=subprocess.Popen([sys.executable,'-u',str(HELPER),'pipe',str(root),lock,*command],stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    try:
        assert owner.stdout.readline()==b'ready\n'
        parent,child=map(int,pidfile.read_text().split())
        conflict=subprocess.run([sys.executable,str(HELPER),'pipe',str(root),lock,*command],capture_output=True,timeout=5)
        assert conflict.returncode!=0
        owner.stdin.close();owner.wait(timeout=8)
        wait(lambda:not alive(parent))
        # Reaped/orphan zombies are inert; ps stat distinguishes them from writers.
        stat=subprocess.run(['ps','-o','stat=','-p',str(child)],capture_output=True,text=True).stdout.strip()
        assert not stat or stat.startswith('Z'),stat
    finally:
        if owner.poll() is None:owner.kill();owner.wait()
    # A guest/escaped process needs explicit cleanup beyond the host group.
    guest=root/'guest';cleaned=root/'cleaned'
    cleanup=[sys.executable,'-c',
      "import fcntl,os,signal;from pathlib import Path;"
      "lock=open("+repr(lock)+",'r+');"
      "\ntry:fcntl.flock(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)"
      "\nexcept BlockingIOError:pass"
      "\nelse:raise AssertionError('cleanup lost its ownership fence')"
      "\nos.kill(int(Path("+repr(str(guest))+").read_text()),signal.SIGKILL);"
      "Path("+repr(str(cleaned))+").touch()"]
    native=[sys.executable,'-u','-c',
      "import subprocess,time;from pathlib import Path;"
      "p=subprocess.Popen(['sleep','60'],start_new_session=True);"
      "Path("+repr(str(guest))+").write_text(str(p.pid));print('guest ready',flush=True);time.sleep(60)"]
    mode=json.dumps({'mode':'supervisor','cleanup':cleanup})
    guarded=subprocess.Popen([sys.executable,'-u',str(HELPER),mode,str(root),lock,*native],
                             stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    try:
        assert guarded.stdout.readline()==b'guest ready\n'
        guarded.stdin.close();guarded.wait(timeout=12)
        assert cleaned.exists(),guarded.stderr.read()
        stat=subprocess.run(['ps','-o','stat=','-p',guest.read_text()],capture_output=True,text=True).stdout.strip()
        assert not stat or stat.startswith('Z'),stat
        cleaned.unlink()
        normal=subprocess.Popen([sys.executable,'-u',str(HELPER),mode,str(root),lock,
                                 sys.executable,'-c','pass'],stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
        assert normal.wait(timeout=12)==0,normal.stderr.read()
        normal.stdin.close()
        assert not cleaned.exists(), 'normal completion ran cancellation cleanup'
    finally:
        if guarded.poll() is None:guarded.kill();guarded.wait()
    target=subprocess.Popen([sys.executable,'-u',str(HELPER),'pipe',str(root),'',
      sys.executable,'-u','-c',"import time;print('runtime ready',flush=True);time.sleep(60)"],
      stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    assert target.stdout.readline()==b'runtime ready\n'
    lease_mode=json.dumps({'mode':'lease','target_pid':target.pid})
    lease=subprocess.Popen([sys.executable,'-u',str(HELPER),lease_mode,str(root),lock],
                           stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    try:
        assert json.loads(lease.stdout.readline())=={'type':'ready'}
        lease.stdin.close()
        time.sleep(.1)
        assert lease.poll() is None, 'server loss released the worker before native cleanup'
        conflict=subprocess.run([sys.executable,str(HELPER),'pipe',str(root),lock,sys.executable,'-c','pass'],capture_output=True,timeout=5)
        assert conflict.returncode!=0
        target.stdin.close();target.wait(timeout=8);assert lease.wait(timeout=5)==0
    finally:
        if target.poll() is None:target.stdin.close();target.wait(timeout=8)
        if lease.poll() is None:lease.kill();lease.wait()
    terminal=subprocess.Popen([sys.executable,'-u',str(HELPER),'pty',str(root),lock,sys.executable,'-u','-c',
      "import os,sys;print('native tty',os.isatty(0),flush=True);print(input(),flush=True)"],stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    try:
        assert json.loads(terminal.stdout.readline())=={'type':'ready'}
        data=json.loads(terminal.stdout.readline());assert b'native tty True' in base64.b64decode(data['data'])
        terminal.stdin.write((json.dumps({'type':'resize','rows':37,'cols':101})+'\n').encode())
        terminal.stdin.write((json.dumps({'type':'input','data':base64.b64encode(b'exact input\n').decode()})+'\n').encode());terminal.stdin.flush()
        output=b'';code=None
        for line in terminal.stdout:
            record=json.loads(line)
            if record['type']=='output':output+=base64.b64decode(record['data'])
            elif record['type']=='exit':code=record['code']
        assert b'exact input' in output and code==0,(output,code)
        terminal.wait(timeout=5)
    finally:
        if terminal.poll() is None:terminal.stdin.close();terminal.wait(timeout=8)
    # A child may fill stdout before consuming its prompt. Backpressure on its
    # stdin must leave the guard able to read stdout and notice owner death.
    size=256*1024
    duplex=subprocess.Popen([sys.executable,'-u',str(HELPER),'pipe',str(root),'',sys.executable,'-u','-c',
        "import sys;sys.stdout.buffer.write(b'o'*"+str(size)+");sys.stdout.buffer.flush();"
        "data=sys.stdin.buffer.read("+str(size)+");assert data==b'i'*"+str(size)+";print('complete',flush=True)"],
        stdin=subprocess.PIPE,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    try:
        sender=threading.Thread(target=lambda:(duplex.stdin.write(b'i'*size),duplex.stdin.flush()))
        sender.start()
        output=bytearray()
        reader=threading.Thread(target=lambda:output.extend(duplex.stdout.read()))
        reader.start()
        duplex.wait(timeout=8);sender.join(timeout=1);reader.join(timeout=1)
        assert not sender.is_alive() and not reader.is_alive()
        assert duplex.returncode==0 and output==b'o'*size+b'complete\n',duplex.stderr.read()
    finally:
        if duplex.poll() is None:duplex.terminate();duplex.wait(timeout=8)
print('owned runtimes: session lock, native PTY, resize/input, output drain and process-group cleanup passed')
