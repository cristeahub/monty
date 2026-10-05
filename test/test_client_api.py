#!/usr/bin/env python3
"""Checkout-binary E2E; no personal native homes, transcripts or model calls."""
import base64
import concurrent.futures
import hashlib
import http.server
import json
import os
import pty
import signal
import termios
import fcntl
from pathlib import Path
import socket
import socketserver
import struct
import subprocess
import sys
import tempfile
import threading
import time
import urllib.error
import urllib.request

BIN = str(Path(sys.argv[1]).resolve())

class Client:
    def __init__(self, home):
        self.home = Path(home)
        state = self.home / '.monty'
        self.url = json.loads((state / 'client-service.json').read_text())['url']
        self.token = (state / 'client-service-token').read_text()
    def req(self, path, data=None, headers=None):
        headers = {'Authorization': 'Bearer ' + self.token, **(headers or {})}
        if data is not None:
            headers['Content-Type'] = 'application/json'
        req = urllib.request.Request(self.url + '/v1/' + path, headers=headers,
                                     data=None if data is None else json.dumps(data).encode())
        with urllib.request.urlopen(req, timeout=40) as response:
            return json.load(response)
    def history(self, cid):
        offset = 0
        result = None
        while True:
            page = self.req('conversations/' + cid + '/history?offset=' + str(offset))
            if result is None:
                result = page
            else:
                for session, more in zip(result['sessions'], page['sessions']):
                    session['records'].extend(more['records'])
                    session['next_offset'] = more['next_offset']
            if all(session['next_offset'] is None for session in page['sessions']):
                return result
            offset += 8

    def cmd(self, key, data, status='finished'):
        self.req('commands/' + key, data)
        receipt = until(lambda: self.req('commands/' + key),
                        lambda r: r['status'] not in ('accepted', 'running'))
        assert receipt['status'] == status, receipt
        return receipt['result']
    def revision(self, cid):
        return self.req('conversations/' + cid)['revision']

def until(fn, predicate=bool, seconds=12):
    end = time.monotonic() + seconds
    while time.monotonic() < end:
        value = fn()
        if predicate(value):
            return value
        time.sleep(.05)
    raise AssertionError('condition did not settle: ' + repr(value))

def exact(stream, n):
    data = bytearray()
    while len(data) < n:
        chunk = stream.read(n-len(data))
        if not chunk:
            raise EOFError()
        data.extend(chunk)
    return bytes(data)

def receive(stream):
    a, b = exact(stream, 2)
    length = b & 127
    if length == 126:
        length = struct.unpack('!H', exact(stream, 2))[0]
    elif length == 127:
        length = struct.unpack('!Q', exact(stream, 8))[0]
    assert length < 17_000_000
    mask = exact(stream, 4) if b & 128 else None
    data = exact(stream, length)
    if mask:
        data = bytes(c ^ mask[n % 4] for n, c in enumerate(data))
    if a & 15 == 8:
        raise EOFError()
    return json.loads(data)

def frame(data):
    data = json.dumps(data).encode()
    length = len(data)
    return bytes([129, length]) + data if length < 126 else (
        b'\x81\x7e' + struct.pack('!H', length) + data if length < 65536 else
        b'\x81\x7f' + struct.pack('!Q', length) + data)

class Handler(socketserver.StreamRequestHandler):
    def send(self, value):
        with self.write_lock:
            self.wfile.write(frame(value)); self.wfile.flush()
    def handle(self):
        self.write_lock = threading.Lock()
        self.rfile.readline()
        headers = {}
        while True:
            line = self.rfile.readline().decode().strip()
            if not line:
                break
            k, v = line.split(':', 1); headers[k.lower()] = v.strip()
        accept = base64.b64encode(hashlib.sha1((headers['sec-websocket-key'] +
                     '258EAFA5-E914-47DA-95CA-C5AB0DC85B11').encode()).digest()).decode()
        self.wfile.write(('HTTP/1.1 101 Switching Protocols\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Accept: ' + accept + '\r\n\r\n').encode());self.wfile.flush()
        server = self.server
        server.clients.add(self)
        try:
            while True:
                req = receive(self.rfile)
                if 'method' not in req:
                    server.answers.append(req);continue
                method = req['method']; params = req.get('params', {})
                server.calls.append((method, params))
                if method == 'initialized':
                    continue
                if method == 'initialize':
                    result = {'userAgent': 'fake-0.160.0'}
                elif method in ('thread/start', 'open'):
                    native = 'thread-' + str(len(server.threads) + 1)
                    server.threads[native] = {'id': native, 'turns': []}
                    server.loaded.add(native)
                    result = {'thread': server.threads[native]} if method == 'thread/start' else {'session': native, 'entries': []}
                elif method == 'thread/name/set':
                    result = {}
                elif method == 'thread/loaded/list':
                    result = {'data': sorted(server.loaded), 'nextCursor': None}
                elif method in ('thread/read', 'thread/resume', 'history', 'join'):
                    native = params.get('threadId', params.get('session'))
                    if native not in server.threads:
                        self.send({'id': req['id'], 'error': {'code': -32602, 'message': 'unknown thread'}});continue
                    if method in ('thread/resume', 'join'):
                        server.loaded.add(native)
                    if method in ('history', 'join'):
                        result = {'session': native, 'leaf': 'entry-1', 'entries': [
                            {'uuid': 'entry-1', 'parent': None, 'exchange': 'x', 'kind': 'message', 'payload': {'text': 'different shape'}}]}
                    else:
                        result = {'thread': server.threads[native]}
                elif method in ('turn/start', 'send'):
                    server.sends += 1
                    native = params.get('threadId', params.get('session'))
                    turn = {'id': 'turn-' + str(server.sends), 'status': 'completed', 'items': [
                        {'id': 'u-' + str(server.sends), 'type': 'userMessage', 'content': params.get('input', [])},
                        {'id': 'a-' + str(server.sends), 'type': 'agentMessage', 'text': 'hello'}]}
                    server.threads[native]['turns'].append(turn)
                    for item in turn['items']:
                        server.emit({'method': 'item/completed', 'params': {'threadId': native, 'turnId': turn['id'], 'item': item}})
                    result = {'turn': turn}
                    if server.drop_next:
                        server.drop_next = False
                        self.request.shutdown(socket.SHUT_RDWR)
                        return
                elif method in ('turn/interrupt', 'abort'):
                    result = {}
                else:
                    raise AssertionError(method)
                if method in ('thread/start','open') and server.drop_open:
                    server.drop_open=False;self.request.shutdown(socket.SHUT_RDWR);return
                self.send({'id': req['id'], 'result': result})
        except (EOFError, ConnectionError, OSError):
            pass
        finally:
            server.clients.discard(self)

class Native(socketserver.ThreadingUnixStreamServer):
    daemon_threads = True
    def __init__(self, path):
        self.clients = set();self.calls = [];self.threads = {};self.loaded = set()
        self.sends = 0;self.answers = [];self.drop_next = False;self.drop_open = False
        super().__init__(str(path), Handler)
        self.thread = threading.Thread(target=self.serve_forever, daemon=True);self.thread.start()
    def emit(self, event):
        for client in list(self.clients):
            try: client.send(event)
            except OSError: pass
    def close(self):
        for client in list(self.clients):
            try: client.request.shutdown(socket.SHUT_RDWR)
            except OSError: pass
        self.shutdown();self.server_close()

class Service:
    def __init__(self, home, config, extra=None):
        self.home=Path(home);self.config=config
        self.env={**{k:v for k,v in os.environ.items() if not k.startswith('MONTY_')}, 'MONTY_HOME':str(home), 'CODEX_HOME':str(home/'codex'),
                  'MANTLE_CONTEXT':'', **(extra or {})}
        self.log=open(home/'service.log','a+')
        self.process=None;self.start()
    def start(self):
        state=self.home/'.monty/client-service.json'
        if state.exists():state.unlink()
        self.process=subprocess.Popen([BIN,'service','serve','--home',str(self.home),'--config',str(self.config)],env=self.env,stdout=self.log,stderr=self.log)
        for _ in range(100):
            if state.exists():break
            if self.process.poll() is not None:
                self.log.seek(0);raise AssertionError(self.log.read())
            time.sleep(.05)
        self.client=Client(self.home)
    def stop(self):
        if self.process and self.process.poll() is None:
            self.process.terminate();self.process.wait(timeout=5)
    def close(self):
        self.stop();self.log.close()

def http_error(code, fn):
    try:fn()
    except urllib.error.HTTPError as e:assert e.code == code, (e.code,e.read())
    else:raise AssertionError('expected HTTP '+str(code))

def autostart_suite(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    native=home/'codex';native.mkdir()
    server=Native(home/'codex.sock')
    codex=home/'fake-codex'
    codex.write_text('#!/usr/bin/env python3\nimport json,sys\n'
      + 'if sys.argv[1:4]==["app-server","daemon","start"]:print(json.dumps('+repr({'socketPath':str(home/'codex.sock'),'appServerVersion':'0.160.0'})+'))\n'
      + 'else:\n assert sys.argv[1:3]==["resume","--remote"]\n assert "--dangerously-bypass-approvals-and-sandbox" not in sys.argv\n')
    codex.chmod(0o700)
    config=home/'profiles.json';config.write_text(json.dumps({'profiles':[
        {'id':'default','harness':'codex','command':str(codex),'codex_home':str(native),'roots':[str(home)]}]}))
    service=Service(home,config)
    metadata=home/'.monty/client-service.json'
    log=home/'.monty/client-service.log'
    def unlocked():
        with (home/'.monty/client-service.lock').open('r+') as lock:
            try:fcntl.lockf(lock,fcntl.LOCK_EX|fcntl.LOCK_NB)
            except BlockingIOError:return False
            return True
    def stop_background():
        def stopped():
            if unlocked():return True
            try:os.kill(json.loads(metadata.read_text())['pid'],signal.SIGTERM)
            except ProcessLookupError:pass
            return False
        until(stopped)
    def launch():
        return subprocess.run([BIN],cwd=repo,env={**service.env,'MONTY_CODEX_COMMAND':str(codex)},
                              capture_output=True,text=True,timeout=25)
    def lifecycle(*args, target=home):
        return subprocess.run([BIN,'service',*args,'--home',str(target)],cwd=repo,env=service.env,
                              capture_output=True,text=True,timeout=25)
    fresh=home/'fresh';fresh.mkdir()
    try:
        # Explicit commands also work before any foreground service was configured.
        run=lifecycle('stop',target=fresh)
        assert run.returncode==0 and 'already stopped' in run.stdout,run.stderr
        assert not (fresh/'.monty').exists()
        run=lifecycle('start',target=fresh)
        assert run.returncode==1 and 'service start --config FILE' in run.stderr,run.stderr
        for port in ['-1','65536']:
            run=lifecycle('start','--config',str(config),'--port='+port,target=fresh)
            assert run.returncode==1 and 'port must be' in run.stderr,run.stderr
        run=lifecycle('start','--config',str(config),target=fresh)
        assert run.returncode==0,run.stderr
        assert Client(fresh).req('health')['profiles'][0]['id']=='default'
        assert not server.threads and not (fresh/'.monty/tasks.local.json').exists()
        client=Client(fresh);epoch=client.req('health')['epoch']
        assert client.req('shutdown',{'epoch':epoch})=={'epoch':epoch}
        run=lifecycle('stop',target=fresh);assert run.returncode==0,run.stderr
        for key,value in [('harness','codex'),('codex-yolo','true')]:
            subprocess.run([BIN,'settings','set',key,value,'--home',str(home)],env=service.env,check=True,capture_output=True)
        configured=json.loads(metadata.read_text())
        assert configured['config_path']==str(config) and configured['port']==0
        run=lifecycle('stop');assert run.returncode==0,run.stderr
        service.process.wait(timeout=5)
        # Dead service metadata is expected after shutdown. Concurrent plain
        # launches must reach one detached service and one thread per caller.
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            runs=list(pool.map(lambda _:launch(),range(4)))
        assert all(run.returncode==0 for run in runs),[run.stderr for run in runs]
        started=json.loads(metadata.read_text())
        assert started['epoch']!=configured['epoch']
        assert len(server.threads)==4 and log.read_text().count('Monty client API ')==1
        assert all(p['approvalPolicy']=='never' and p['sandbox']=='danger-full-access'
                   for method,p in server.calls if method=='thread/start')
        assert log.stat().st_mode&0o777==0o600
        assert not (home/'.monty/tasks.local.json').exists()
        run=launch();assert run.returncode==0,run.stderr
        assert json.loads(metadata.read_text())==started
        count=len(server.threads)
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            runs=list(pool.map(lambda _:lifecycle('start'),range(4)))
        assert all(run.returncode==0 for run in runs),[run.stderr for run in runs]
        assert len(server.threads)==count and json.loads(metadata.read_text())==started
        alternate=home/'alternate.json';alternate.write_text(config.read_text())
        for args in [('start','--config',str(alternate)),('start','--port','8765')]:
            run=lifecycle(*args)
            assert run.returncode==1 and 'service stop first' in run.stderr,run.stderr
        client=Client(home)
        http_error(409,lambda:client.req('shutdown',{'epoch':configured['epoch']}))
        assert client.req('health')['epoch']==started['epoch']
        # Authentication failure must not replace a live service or dispatch a thread.
        token=home/'.monty/client-service-token';original=token.read_text()
        try:
            token.write_text('invalid-token')
            count=len(server.threads)
            run=launch();assert run.returncode==1 and 'bearer authentication required' in run.stderr,run.stderr
            for operation in ['start','stop']:
                run=lifecycle(operation)
                assert run.returncode==1 and 'bearer authentication required' in run.stderr,run.stderr
            assert len(server.threads)==count and json.loads(metadata.read_text())==started
        finally:token.write_text(original)
        # A stale/reused PID must never be signalled; stop targets the authenticated epoch.
        metadata.write_text(json.dumps({**started,'pid':os.getpid()}))
        try:
            with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
                runs=list(pool.map(lambda _:lifecycle('stop'),range(4)))
            assert all(run.returncode==0 for run in runs),[run.stderr for run in runs]
        finally:metadata.write_text(json.dumps(started))
        assert unlocked() and len(server.threads)==count
        run=lifecycle('stop');assert run.returncode==0 and 'already stopped' in run.stdout,run.stderr
        # Invalid saved configuration fails before starting another process.
        original=config.read_text();config.write_text('{"profiles":[]}')
        try:
            before=log.read_text()
            run=launch();assert run.returncode==1 and 'configure one to 32 profiles' in run.stderr,run.stderr
            assert unlocked() and log.read_text()==before
        finally:config.write_text(original)
        run=launch();assert run.returncode==0,run.stderr
        assert json.loads(metadata.read_text())['epoch']!=started['epoch']
        assert log.read_text().count('Monty client API ')==2
        print('client service start/stop, automatic startup, reuse, concurrency and recovery passed')
    finally:
        lifecycle('stop',target=fresh)
        service.close();stop_background();server.close()

def fake_suite(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    native_home=home/'codex';native_home.mkdir()
    server=Native(home/'codex.sock');second=Native(home/'second.sock')
    codex=home/'fake-codex'
    codex.write_text('#!/usr/bin/env python3\nimport json,sys\nfrom pathlib import Path\n'
      + 'if sys.argv[1:4]==["app-server","daemon","start"]:print(json.dumps('+repr({'socketPath':str(home/'codex.sock'),'appServerVersion':'0.160.0'})+'))\n'
      + 'else:Path('+repr(str(home/'terminal.json'))+').write_text(json.dumps(sys.argv[1:]))\n')
    codex.chmod(0o700)
    config=home/'profiles.json';config.write_text(json.dumps({'allow_fixture':True,'profiles':[
        {'id':'default','harness':'codex','command':str(codex),'codex_home':str(native_home),'roots':[str(repo),str(home)]},
        {'id':'second','harness':'fixture','socket':str(home/'second.sock'),'roots':[str(repo)]}]}))
    service=Service(home,config);c=service.client
    try:
        http_error(401,lambda:c.req('health',headers={'Authorization':'no'}))
        http_error(403,lambda:c.req('health',headers={'Origin':'https://evil.example'}))
        http_error(403,lambda:c.req('health',headers={'Host':'evil.example'}))
        assert c.req('inventory')['tasks']==[]
        assert not (home/'.monty/tasks.json').exists()
        request={'op':'start','profile':'default','context':{'workspaces':[str(repo)],'instructions':'trusted current instructions','environment':{'MONTY_JOB_TITLE':'isolated'}}}
        with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
            receipts=list(pool.map(lambda _:c.cmd('start-once',request),range(8)))
        cid=receipts[0]['id'];native=receipts[0]['sessions'][0]['native_id']
        assert len({r['id'] for r in receipts})==1 and len(server.threads)==1
        http_error(409,lambda:c.req('commands/start-once',{**request,'title':'different'}))
        params=next(params for method,params in server.calls if method=='thread/start')
        assert params['config']['shell_environment_policy.set']['MONTY_JOB_TITLE']=='isolated'
        assert (params['approvalPolicy'],params['sandbox'])==('on-request','workspace-write')
        assert c.history(cid)['sessions'][0]['records']
        send={'op':'send','conversation':cid,'revision':c.revision(cid),'text':'one turn'}
        with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
            list(pool.map(lambda _:c.cmd('send-once',send),range(8)))
        assert server.sends==1
        # Replay is bounded independently of connected observers; expired cursors resync.
        cursor=c.req('health')['cursor']
        delta={'method':'item/agentMessage/delta','params':{'threadId':native,'delta':'SSE fixture'}}
        server.emit(delta)
        until(lambda:c.req('health')['cursor'],lambda value:value!=cursor)
        stream_request=urllib.request.Request(c.url+'/v1/events',headers={
          'Authorization':'Bearer '+c.token,'Last-Event-ID':cursor})
        with urllib.request.urlopen(stream_request,timeout=10) as stream:
            while True:
                line=stream.readline()
                assert line, 'SSE closed before replaying a native event'
                if line.startswith(b'data:') and b'SSE fixture' in line:break
        for _ in range(300):server.emit(delta)
        sequence=int(cursor.rsplit(':',1)[1])
        until(lambda:int(c.req('health')['cursor'].rsplit(':',1)[1]),lambda value:value>sequence+256)
        http_error(409,lambda:c.req('events',headers={'Last-Event-ID':cursor}))
        # An identical snapshot still reconciles newer provisional/live records.
        c.cmd('initial-sync',{'op':'sync','conversation':cid})
        snapshot=json.loads(json.dumps({'thread':server.threads[native]}))
        raw_count=sum(b['raw']==snapshot for b in c.history(cid)['sessions'][0]['records'])
        assert raw_count
        for item in [{'id':'a-1','type':'agentMessage','text':'provisional'},
                     {'id':'abandoned-live','type':'agentMessage','text':'not in snapshot'}]:
            server.emit({'method':'item/completed','params':{'threadId':native,'turnId':'turn-1','item':item}})
        until(lambda:c.history(cid),lambda h:any(i['source_id']=='abandoned-live'
          for b in h['sessions'][0]['records'] for i in b['items']))
        c.cmd('identical-sync',{'op':'sync','conversation':cid})
        reconciled=c.history(cid)['sessions'][0]
        latest={i['source_id']:i for b in reconciled['records'] for i in b['items']}
        assert latest['a-1']['content']['text']=='hello' and not latest['a-1'].get('provisional_order')
        assert all('abandoned-live' not in key for key in reconciled['effective'])
        assert sum(b['raw']==snapshot for b in reconciled['records'])==raw_count
        # Activity originating outside the API is captured by the permanent observer.
        image=repo/'temporary.png';image.write_bytes(b'owned attachment bytes')
        server.threads[native]['turns'].append({'id':'terminal-turn','status':'completed','items':[
            {'id':'terminal','type':'userMessage','content':[{'type':'localImage','path':str(image)}]},
            {'id':'compact','type':'contextCompaction'}, {'id':'opaque','type':'futureType','data':{
                'access_token':'do-not-store','accessToken':'do-not-store','refreshToken':'do-not-store','idToken':'do-not-store'}}]})
        server.emit({'method':'item/completed','params':{'threadId':native,'item':{'id':'terminal'}}})
        def items():
            return [i for b in c.history(cid)['sessions'][0]['records'] for i in b['items']]
        captured=until(items,lambda xs:any(i['source_id']=='terminal' for i in xs))
        attachment=next(i for i in captured if i['source_id']=='terminal')['attachments'][0]
        image.unlink();assert (home/'.monty/conversation-blobs'/attachment['sha256']).read_bytes()==b'owned attachment bytes'
        assert 'do-not-store' not in json.dumps(c.history(cid))
        # Revisions, rollback, and abandoned items remain retained.
        server.threads[native]['turns'][-1]['items'][0]['content']=[{'type':'text','text':'edited'}]
        until(items,lambda xs:len([i for i in xs if i['source_id']=='terminal'])==2)
        server.threads[native]['turns']=server.threads[native]['turns'][:1]
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['effective'],lambda xs:all('terminal' not in x for x in xs))
        assert len([i for i in items() if i['source_id']=='terminal'])==2
        # Two clients race for the same exact approval revision.
        server.emit({'id':900,'method':'item/commandExecution/requestApproval','params':{'threadId':native,'turnId':'turn-1','itemId':'a-1'}})
        current=until(lambda:c.req('conversations/'+cid),lambda x:bool(x['sessions'][0].get('pending')))
        pending=current['sessions'][0]['pending'][0]
        answer={'op':'respond','conversation':cid,'revision':current['revision'],'pending_id':pending['id'],'pending_revision':1,'response':{'decision':'decline'}}
        c.req('commands/answer-a',answer);c.req('commands/answer-b',answer)
        until(lambda:len(server.answers),lambda n:n==1)
        until(lambda:c.req('commands/answer-b'),lambda r:r['status']=='rejected')
        server.emit({'method':'serverRequest/resolved','params':{'threadId':native,'requestId':900}})
        until(lambda:c.req('conversations/'+cid)['sessions'][0].get('pending',[]),lambda x:not x)
        # Unknown dispatch is durable across restart; retry does not resend.
        server.emit({'id':901,'method':'item/commandExecution/requestApproval','params':{'threadId':native,'turnId':'turn-1','itemId':'a-1'}})
        disconnected_pending=until(lambda:c.req('conversations/'+cid)['sessions'][0].get('pending'))[0]
        server.drop_next=True
        uncertain={'op':'send','conversation':cid,'revision':c.revision(cid),'text':'lost reply'}
        c.cmd('uncertain',uncertain,status='outcome_unknown');assert server.sends==2
        assert not c.req('conversations/'+cid)['sessions'][0].get('pending')
        c.cmd('stale-disconnected-response',{**answer,'revision':c.revision(cid),
          'pending_id':disconnected_pending['id']},status='rejected')
        service.stop();service.start();c=service.client
        c.cmd('uncertain',uncertain,status='outcome_unknown');assert server.sends==2
        http_error(409,lambda:c.req('events',headers={'Last-Event-ID':'old:1'}))
        until(lambda:len(server.clients))
        c.cmd('interrupt',{'op':'interrupt','conversation':cid,'revision':c.revision(cid),'turn_id':'turn-2'})
        c.cmd('handoff',{'op':'handoff','conversation':cid},status='rejected')
        # A second, materially different history shape receives exact context.
        switched=c.cmd('switch',{'op':'switch','mode':'branch','conversation':cid,'revision':c.revision(cid),
          'profile':'second','context':{'workspaces':[str(repo)],'instructions':'current trusted instructions','objective':'continue'}})
        assert switched['id']==cid and len(switched['sessions'])==2
        supplied=next(params['package'] for method,params in second.calls if method=='open')
        assert supplied==switched['transfers'][0]['package']
        assert supplied['historical_data'][0]['trust']=='historical_data'
        assert second.sends==0 and server.sends==2
        until(lambda:c.history(cid)['sessions'][1]['records'])
        picker=subprocess.run([BIN,'terminal','--home',str(home),'--profile','default','--mode','last'],
          cwd=repo,env=service.env,capture_output=True,text=True)
        assert picker.returncode==1 and 'No registered conversation' in picker.stderr,picker.stderr
        # Live and backfilled context packages share the same boundary kind.
        returned=c.cmd('switch-back',{'op':'switch','mode':'branch','conversation':cid,'revision':c.revision(cid),
          'profile':'default','context':{'workspaces':[str(repo)]}})
        transferred=until(lambda:c.history(cid)['sessions'][2],lambda sess:any(
          i['kind']=='contextTransfer' and not i.get('provisional_order')
          for b in sess['records'] for i in b['items']))
        boundaries=[i for b in transferred['records'] for i in b['items'] if i['kind']=='contextTransfer']
        assert any(i.get('provisional_order') for i in boundaries)
        assert all(i['content']=={'transfer':returned['transfers'][1]['id']} for i in boundaries)
        # Terminal uses the precise endpoint and native thread, never standalone.
        worker_dir=home/'ambient-worker';worker_dir.mkdir()
        sidecar=worker_dir/'codex-session-id';sidecar.write_text('existing-worker-thread\n')
        env={**service.env,'MONTY_API_PROFILE':'default','MONTY_TASK_KEY':'local:must-not-link','MONTY_JOB_TITLE':'worker title',
             'MONTY_WORKER_DIR':str(worker_dir),'MONTY_WORKSPACE_1':str(home/'missing-workspace')}
        run=subprocess.run([BIN,'terminal','--home',str(home),'--codex-command',str(codex),'--request-id','terminal-start'],cwd=repo,env=env,capture_output=True,text=True)
        assert run.returncode==0,run.stderr
        args=json.loads((home/'terminal.json').read_text());assert args[:3]==['resume','--remote','unix://'+str(home/'codex.sock')],args
        assert args[-1] in server.threads
        task_free=c.req('commands/terminal-start')['result']
        assert task_free['title']=='Monty conversation'
        assert task_free['task_keys']==[] and task_free['sessions'][0]['context']['workspaces']==[str(repo)]
        assert task_free['sessions'][0]['context']['environment']=={}
        assert sidecar.read_text()=='existing-worker-thread\n'
        assert '--dangerously-bypass-approvals-and-sandbox' not in args
        # Regression: plain monty forwards the existing setting into the service
        # and still connects its native terminal to the exact registered thread.
        for key,value in [('harness','codex'),('codex-yolo','true')]:
            subprocess.run([BIN,'settings','set',key,value,'--home',str(home)],env=service.env,check=True,capture_output=True)
        launch_env={**service.env,'MONTY_CODEX_COMMAND':str(codex)}
        for command,extra,yolo in [([],{},True),(['start'],{'MONTY_CODEX_YOLO':'false'},False),
                                  (['start','--codex-yolo'],{'MONTY_CODEX_YOLO':'false'},True),
                                  (['terminal','--codex-command',str(codex)],{},True)]:
            run=subprocess.run([BIN,*command,'--home',str(home)],cwd=repo,env={**launch_env,**extra},capture_output=True,text=True)
            assert run.returncode==0,run.stderr
            params=next(p for method,p in reversed(server.calls) if method=='thread/start')
            assert (params['approvalPolicy'],params['sandbox'])==(('never','danger-full-access') if yolo else ('on-request','workspace-write'))
            args=json.loads((home/'terminal.json').read_text())
            assert args[:3]==['resume','--remote','unix://'+str(home/'codex.sock')],args
            assert '--dangerously-bypass-approvals-and-sandbox' not in args,args
        resumed=args[-1];count=len(server.threads)
        run=subprocess.run([BIN,'continue',resumed,'--home',str(home)],env=launch_env,capture_output=True,text=True)
        assert run.returncode==0,run.stderr
        assert len(server.threads)==count
        args=json.loads((home/'terminal.json').read_text())
        assert args[-1]==resumed and '--dangerously-bypass-approvals-and-sandbox' not in args
        c.cmd('invalid-yolo',{**request,'codex_yolo':'true'},status='rejected')
        assert len(server.threads)==count
        subprocess.run([BIN,'settings','set','codex-yolo','false','--home',str(home)],env=service.env,check=True,capture_output=True)
        # Source failure is visible while previously owned history stays readable.
        second.close();until(lambda:c.req('conversations/'+cid)['sessions'][1]['coverage']['state'],lambda x:x=='unavailable')
        assert c.history(cid)['sessions'][0]['records']
        assert c.req('inventory')['tasks']==[]
        c.cmd('archive',{'op':'archive','conversation':cid});assert c.req('conversations/'+cid)['archived']
        c.cmd('purge',{'op':'purge','conversation':cid,'confirm':True})
        http_error(410,lambda:c.history(cid))
        c.cmd('reimport',{'op':'import','profile':'default','native_id':native,'context':{'workspaces':[str(repo)]}},status='rejected')
        assert not (home/'.monty/conversation-blobs'/attachment['sha256']).exists()
        # Native CLI remains authoritative; API reads preserve its order and workspace metadata.
        def cli(*args):
            run=subprocess.run([BIN,*args,'--home',str(home)],env=service.env,capture_output=True,text=True)
            assert run.returncode==0,run.stderr
            return run.stdout
        other=home/'other';other.mkdir();subprocess.run(['git','init','-q',str(other)],check=True)
        cli('projects','add','--repo',str(repo));cli('projects','add','--repo',str(other))
        cli('task','add','--project','repo','--title','API inventory first')
        cli('task','workspace','add','local:local-001','--repo',str(repo),'--branch','cto/api-one')
        cli('task','workspace','add','local:local-001','--repo',str(other),'--branch','cto/api-two')
        cli('task','add','--project','repo','--title','API inventory second')
        tasks_file=home/'.monty/tasks.local.json';before=tasks_file.read_bytes()
        view=c.req('inventory?all_groups=true')
        native_table=cli('list','--no-sync','--all-groups')
        assert view['banner']=='Groups: all (--all-groups)\n'
        assert [row['key'] for row in view['tasks']]==['local:local-001','local:local-002']
        assert view['tasks'][0]['workspaces']==json.loads(before)['tasks'][0]['workspaces']
        assert len(view['tasks'][0]['workspaces'])==2
        assert all(row['title'] in native_table and row['status'] in native_table for row in view['tasks'])
        assert before==tasks_file.read_bytes()
        # Malformed native frames cause visible source failure, never task completion.
        server.emit(['not-a-json-rpc-object'])
        until(lambda:next(x for x in c.req('conversations') if not x.get('purged'))['sessions'][0]['coverage']['state'],lambda x:x=='unavailable')
        assert tasks_file.read_bytes()==before
        print('client API E2E passed')
    finally:
        service.close();server.close()
        if second.socket.fileno()!=-1:second.close()

def recovery_suite(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    source=Native(home/'source.sock');dest=Native(home/'dest.sock')
    config=home/'profiles.json';config.write_text(json.dumps({'allow_fixture':True,'profiles':[
      {'id':'source','harness':'fixture','socket':str(home/'source.sock'),'roots':[str(repo)]},
      {'id':'dest','harness':'fixture','socket':str(home/'dest.sock'),'roots':[str(repo)]}]}))
    service=Service(home,config,{'MONTY_FAULT_INJECT':'conversation-after-batch'});c=service.client
    try:
        req={'op':'start','profile':'source','context':{'workspaces':[str(repo)]}}
        c.cmd('batch-fault',req,status='outcome_unknown')
        cid=c.req('conversations')[0]['id']
        before=c.req('conversations/'+cid)
        assert before['sessions'][0]['checkpoint']==0
        orphans=list((home/'.monty/conversations'/cid/'sessions').glob('*/batches/*.json'))
        assert orphans
        service.stop();service.env.pop('MONTY_FAULT_INJECT');service.start();c=service.client
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['checkpoint'],lambda n:n>0)
        assert all(not path.exists() for path in orphans)
        c.cmd('batch-fault',req,status='outcome_unknown');assert len(source.threads)==1
        service.stop();service.env['MONTY_FAULT_INJECT']='conversation-after-destination';service.start();c=service.client
        switch={'op':'switch','conversation':cid,'revision':c.revision(cid),'mode':'branch','profile':'dest','context':{'workspaces':[str(repo)]}}
        c.cmd('destination-fault',switch,status='outcome_unknown')
        intermediate=c.req('conversations/'+cid)
        assert len(intermediate['sessions'])==2 and intermediate['selected_session']==intermediate['sessions'][0]['id']
        assert intermediate['transfers'][0]['status']=='accepted'
        service.stop();service.env.pop('MONTY_FAULT_INJECT');service.start();c=service.client
        c.cmd('destination-fault',switch,status='outcome_unknown');assert len(dest.threads)==1
        recovered=c.cmd('recover-existing',{'op':'recover_switch','conversation':cid,'revision':c.revision(cid),
          'transfer':intermediate['transfers'][0]['id'],'confirm_branch':True})
        assert recovered['selected_session']==recovered['sessions'][1]['id'] and len(dest.threads)==1
        # Acceptance with a lost reply remains unknown; no guessed registration.
        dest.drop_open=True
        req={'op':'switch','conversation':cid,'revision':c.revision(cid),'mode':'branch','profile':'dest','context':{'workspaces':[str(repo)]}}
        c.cmd('unknown-destination',req,status='outcome_unknown');assert len(dest.threads)==2
        c.cmd('unknown-destination',req,status='outcome_unknown');assert len(dest.threads)==2
        # A pending process-crash receipt recovers without any external dispatch.
        c.cmd('restart-receipt',{'op':'sync','conversation':cid})
        dest.emit({'id':902,'method':'item/commandExecution/requestApproval','params':{'threadId':intermediate['sessions'][1]['native_id']}})
        until(lambda:c.req('conversations/'+cid)['sessions'][1].get('pending'))
        service.stop()
        receipt=home/'.monty/conversation-commands/restart-receipt.json'
        stored=json.loads(receipt.read_text());stored['status']='running';receipt.write_text(json.dumps(stored))
        count=len(source.calls)+len(dest.calls)
        service.start();c=service.client
        assert c.req('commands/restart-receipt')['status']=='outcome_unknown'
        until(lambda:c.req('conversations/'+cid)['sessions'][1].get('pending',[]),lambda requests:not requests)
        assert count<=len(source.calls)+len(dest.calls)
        print('client API crash recovery passed')
    finally:
        service.close();source.close();dest.close()

def terminal_profiles_suite(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    personal=home/'codex';personal.mkdir()
    work=home/'work';work.mkdir()
    servers=[Native(home/'personal.sock'),Native(home/'work.sock')]
    sockets={str(personal):str(home/'personal.sock'),str(work):str(home/'work.sock')}
    codex=home/'fake-codex'
    codex.write_text('#!/usr/bin/env python3\nimport json,os,sys\nfrom pathlib import Path\n'
      +'if sys.argv[1:4]==["app-server","daemon","start"]:\n'
      +' print(json.dumps({"socketPath":'+repr(sockets)+'[os.environ["CODEX_HOME"]],"appServerVersion":"0.160.0"}))\n'
      +'else:Path('+repr(str(home/'terminal.json'))+').write_text(json.dumps(sys.argv[1:]))\n')
    codex.chmod(0o700)
    profiles={'profiles':[{'id':name,'harness':'codex','command':str(codex),'codex_home':str(account),'roots':[str(home)]}
                          for name,account in [('default',personal),('company',work)]]}
    config=home/'profiles.json';config.write_text(json.dumps(profiles))
    service=Service(home,config)
    env={**service.env,'MONTY_CODEX_COMMAND':str(codex)}
    work_env={**env,'CODEX_HOME':str(work),'MANTLE_CONTEXT':'work',
              '_MANTLE_SELECTED_HOME':str(work),'_MANTLE_PREV_STATE':'export','_MANTLE_PREV_HOME':str(personal)}
    def launch(args=(),environment=env):
        return subprocess.run([BIN,*args,'--home',str(home)],cwd=repo,env=environment,
                              capture_output=True,text=True,timeout=25)
    def attached(run,account):
        assert run.returncode==0,run.stderr
        args=json.loads((home/'terminal.json').read_text())
        assert args[:3]==['resume','--remote','unix://'+sockets[str(account)]],args
        return args[-1]
    def rejected(args,environment,message):
        before=[len(server.threads) for server in servers]
        run=launch(args,environment)
        assert run.returncode==1 and message in run.stderr,run.stderr
        assert [len(server.threads) for server in servers]==before
    terminal=['terminal','--codex-command',str(codex)]
    try:
        subprocess.run([BIN,'settings','set','harness','codex','--home',str(home)],env=env,check=True,capture_output=True)
        personal_id=attached(launch(),personal)
        work_id=attached(launch(environment=work_env),work)
        # Different accounts may have identical native thread IDs. Continue
        # must resolve them within the active account, including after switching off.
        assert personal_id==work_id
        attached(launch(['continue',work_id],work_env),work)
        attached(launch(['continue',personal_id]),personal)
        assert [len(server.threads) for server in servers]==[1,1]
        shell_home=home/'shell';shell_home.mkdir();(shell_home/'.codex').symlink_to(personal,target_is_directory=True)
        unset_env={**env,'HOME':str(shell_home)};unset_env.pop('CODEX_HOME')
        attached(launch(environment=unset_env),personal)
        alias=home/'work-alias';alias.symlink_to(work,target_is_directory=True)
        attached(launch(terminal,{**work_env,'CODEX_HOME':str(alias)}),work)
        rejected(terminal+['--profile','default'],work_env,'CODEX_HOME does not match')
        rejected((),{**work_env,'MONTY_API_PROFILE':'default'},'CODEX_HOME does not match')
        rejected(terminal+['--profile','missing'],work_env,'profile not configured')
        attached(launch(terminal+['--profile','company'],{**work_env,'MONTY_API_PROFILE':'default'}),work)
        unknown=home/'unknown';unknown.mkdir()
        rejected((),{**work_env,'CODEX_HOME':str(unknown)},'No Codex service profile matches CODEX_HOME')
        assert not (home/'.monty/tasks.local.json').exists()
        # Ambiguous profiles need explicit selection, never list-order fallback.
        service.stop();profiles['profiles'].append({**profiles['profiles'][1],'id':'company-alias'})
        config.write_text(json.dumps(profiles));service.start()
        rejected((),work_env,'Multiple Codex service profiles match CODEX_HOME')
        attached(launch(terminal,{**work_env,'MONTY_API_PROFILE':'company'}),work)
        print('terminal account selection, Mantle switching, overrides and isolation passed')
    finally:
        service.close()
        for server in servers:server.close()

def scope_suite(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    source=Native(home/'source.sock');other=Native(home/'other.sock')
    config=home/'profiles.json'
    profiles={'allow_fixture':True,'profiles':[
      {'id':name,'harness':'fixture','socket':str(home/'source.sock'),'roots':[str(repo)]}
      for name in ('source','alias')]}
    config.write_text(json.dumps(profiles))
    service=Service(home,config);c=service.client
    try:
        request={'op':'start','profile':'source','context':{'workspaces':[str(repo)]}}
        started=c.cmd('start-scoped',request);cid=started['id'];sess=started['sessions'][0]
        assert sess['scope']==hashlib.sha256(str(home/'source.sock').encode()).hexdigest()
        service.stop();profiles['profiles'][0]['socket']=str(home/'other.sock')
        config.write_text(json.dumps(profiles));service.start();c=service.client
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['coverage']['state'],lambda x:x=='unavailable')
        c.cmd('wrong-scope',{'op':'send','conversation':cid,'revision':c.revision(cid),'text':'must not send'},status='rejected')
        assert other.calls==[],other.calls
        assert c.history(cid)['sessions'][0]['records']
        # Explicit attachment may rename a profile without duplicating its source.
        attached=c.cmd('attach-alias',{**request,'op':'attach','profile':'alias','native_id':sess['native_id']})
        assert attached['id']==cid and len(attached['sessions'])==1
        assert attached['sessions'][0]['profile']=='alias'
        c.cmd('purge-scope',{'op':'purge','conversation':cid,'confirm':True})
        service.stop();profiles['profiles'][0]['socket']=str(home/'source.sock')
        config.write_text(json.dumps(profiles));service.start();c=service.client
        before=len(source.calls)
        c.cmd('reimport-renamed',{**request,'op':'import','native_id':sess['native_id']},status='rejected')
        assert len(source.calls)==before
        print('client API account scope passed')
    finally:
        service.close();source.close();other.close()

class Peer:
    def __init__(self, path):
        self.sock=socket.socket(socket.AF_UNIX);self.sock.settimeout(10);self.sock.connect(path)
        self.stream=self.sock.makefile('rb')
        self.sock.sendall(b'GET / HTTP/1.1\r\nHost: localhost\r\nUpgrade: websocket\r\nConnection: Upgrade\r\nSec-WebSocket-Version: 13\r\nSec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n\r\n')
        assert b'101' in self.stream.readline()
        while self.stream.readline()!=b'\r\n':pass
        self.counter=0;self.events=[]
        self.call('initialize',{'clientInfo':{'name':'monty_fixture_peer','version':'1'},'capabilities':{'experimentalApi':True}})
        self.send({'method':'initialized'})
    def send(self,value):
        data=json.dumps(value).encode();mask=os.urandom(4);n=len(data)
        head=bytes([129,n|128]) if n<126 else b'\x81\xfe'+struct.pack('!H',n)
        self.sock.sendall(head+mask+bytes(c^mask[i%4] for i,c in enumerate(data)))
    def call(self,method,params):
        self.counter+=1;self.send({'id':self.counter,'method':method,'params':params})
        while True:
            value=receive(self.stream)
            if value.get('id')==self.counter:
                assert 'error' not in value,value
                return value['result']
            self.events.append(value)
    def close(self):
        self.stream.close();self.sock.close()

class Model(http.server.BaseHTTPRequestHandler):
    def log_message(self,*args):pass
    def do_POST(self):
        self.rfile.read(int(self.headers.get('Content-Length',0)))
        self.send_response(200);self.send_header('Content-Type','text/event-stream');self.end_headers()
        def emit(event):
            self.wfile.write(('event: '+event['type']+'\ndata: '+json.dumps(event)+'\n\n').encode());self.wfile.flush()
        try:
            emit({'type':'response.created','response':{'id':'resp-fixture','status':'in_progress','output':[]}})
            self.server.started.set();self.server.release.wait(15)
            item={'id':'msg-fixture','type':'message','role':'assistant','status':'completed','content':[{'type':'output_text','text':'isolated fixture reply','annotations':[]}]}
            emit({'type':'response.output_item.added','output_index':0,'item':{**item,'status':'in_progress','content':[]}})
            emit({'type':'response.output_text.delta','item_id':'msg-fixture','output_index':0,'content_index':0,'delta':'isolated fixture reply'})
            emit({'type':'response.output_item.done','output_index':0,'item':item})
            emit({'type':'response.completed','response':{'id':'resp-fixture','status':'completed','output':[item],
                'usage':{'input_tokens':1,'output_tokens':1,'total_tokens':2}}})
        except (BrokenPipeError,ConnectionResetError):pass

def real_spike(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    native=home/'codex';native.mkdir()
    model=http.server.ThreadingHTTPServer(('127.0.0.1',0),Model)
    model.started=threading.Event();model.release=threading.Event()
    threading.Thread(target=model.serve_forever,daemon=True).start()
    (native/'config.toml').write_text('model = "monty-fixture"\nmodel_provider = "fixture"\n[model_providers.fixture]\nname = "Fixture"\nbase_url = "http://127.0.0.1:'+str(model.server_port)+'/v1"\nwire_api = "responses"\nrequires_openai_auth = false\n')
    config=home/'profiles.json';config.write_text(json.dumps({'profiles':[{'id':'default','harness':'codex',
        'command':subprocess.check_output(['which','codex'],text=True).strip(),'codex_home':str(native),'roots':[str(repo)]}]}))
    service=Service(home,config);c=service.client;peers=[];tui=None;master=None
    try:
        result=c.cmd('real-start',{'op':'start','profile':'default','codex_yolo':True,'context':{'workspaces':[str(repo)]}})
        cid=result['id'];sess=result['sessions'][0];nid=sess['native_id'];endpoint=sess['endpoint'][7:]
        first=Peer(endpoint);peers.append(first)
        joined=first.call('thread/resume',{'threadId':nid})
        assert joined['thread']['id']==nid
        assert joined['approvalPolicy']=='never' and joined['sandbox']['type']=='dangerFullAccess',joined
        master,slave=pty.openpty()
        fcntl.ioctl(slave,termios.TIOCSWINSZ,struct.pack('HHHH',40,120,0,0))
        tui=subprocess.Popen([BIN,'terminal','--home',str(home),'--mode','exact','--session',nid,'--codex-yolo','--prompt',
          'Reply with the fixture text. Do not run tools.'],cwd=repo,env={**service.env,'TERM':'xterm-256color'},
          stdin=slave,stdout=slave,stderr=slave,start_new_session=True)
        os.close(slave);terminal_output=[]
        def drain():
            try:
                while True:
                    data=os.read(master,8192)
                    if not data:break
                    if sum(map(len,terminal_output))<1000000:terminal_output.append(data)
            except OSError:pass
        threading.Thread(target=drain,daemon=True).start()
        assert model.started.wait(15),b''.join(terminal_output).decode(errors='replace')

        second=Peer(endpoint);peers.append(second)
        joined=second.call('thread/resume',{'threadId':nid})
        assert joined['thread']['id']==nid,joined
        assert joined['approvalPolicy']=='never' and joined['sandbox']['type']=='dangerFullAccess',joined
        assert any(turn['status']=='inProgress' for turn in joined['thread']['turns']),joined
        first.close();peers.remove(first)
        tui.send_signal(signal.SIGHUP);tui.wait(timeout=10)
        still_active=second.call('thread/read',{'threadId':nid,'includeTurns':True})
        assert any(t['status']=='inProgress' for t in still_active['thread']['turns']),still_active
        model.release.set()
        history=until(lambda:c.history(cid),
            lambda h:any('isolated fixture reply' in json.dumps(b) for b in h['sessions'][0]['records']))
        assert history['sessions'][0]['native_id']==nid
        # The old native client retains the ability to write. A Monty switch
        # therefore cannot truthfully promise exclusive input ownership.
        second.call('turn/start',{'threadId':nid,'input':[{'type':'text','text':'Second fixture reply.'}]})
        until(lambda:c.history(cid),lambda h:len(h['sessions'][0]['effective'])>=4)
        print('real Codex 0.160.0: same active thread rejoined; terminal-originated activity mirrored; native TUI SIGHUP preserved execution; old client can still send. All inference served by local fixture.')
    finally:
        model.release.set()
        if tui and tui.poll() is None:tui.kill();tui.wait(timeout=5)
        if master is not None:os.close(master)
        for peer in peers:peer.close()
        service.close()
        subprocess.run(['codex','app-server','daemon','stop'],env=service.env,capture_output=True,timeout=30)
        model.shutdown();model.server_close()

if __name__=='__main__':
    with tempfile.TemporaryDirectory(prefix='mca-',dir='/tmp') as directory:
        home=Path(directory).resolve()
        if '--real' in sys.argv:real_spike(home)
        else:
            fake_suite(home)
            with tempfile.TemporaryDirectory(prefix='mcar-',dir='/tmp') as recovery:
                recovery_suite(Path(recovery).resolve())
            with tempfile.TemporaryDirectory(prefix='mcas-',dir='/tmp') as scoped:
                scope_suite(Path(scoped).resolve())
            with tempfile.TemporaryDirectory(prefix='mcab-',dir='/tmp') as startup:
                autostart_suite(Path(startup).resolve())
            with tempfile.TemporaryDirectory(prefix='mcap-',dir='/tmp') as profiles:
                terminal_profiles_suite(Path(profiles).resolve())
