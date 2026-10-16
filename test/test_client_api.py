#!/usr/bin/env python3
"""Checkout-binary E2E; no personal native homes, transcripts or model calls."""
import base64
import concurrent.futures
import hashlib
import http.server
import json
import os
import pty
import select
import signal
import shutil
import termios
import tty
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
                    server.answers.append(req)
                    continue
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
                elif method == 'skills/list':
                    assert params['forceReload'] is True and params['cwds']
                    result = {'data': []}
                elif method == 'thread/name/set':
                    result = {}
                elif method == 'config/value/write':
                    assert params == {'keyPath': 'tui.vim_mode_default', 'value': True, 'mergeStrategy': 'replace'}, params
                    result = {'status': 'ok'}
                elif method == 'thread/loaded/list':
                    result = {'data': sorted(server.loaded), 'nextCursor': None}
                elif method == 'thread/list':
                    assert params['modelProviders'] == [] and 'exec' in params['sourceKinds']
                    threads = [t for t in server.threads.values() if bool(t.get('archived')) == params['archived']]
                    offset = int(params.get('cursor') or 0)
                    limit = params['limit']
                    result = {'data': threads[offset:offset + limit],
                              'nextCursor': str(offset + limit) if offset + limit < len(threads) else None}
                elif method in ('thread/read', 'thread/resume', 'history', 'join'):
                    native = params.get('threadId', params.get('session'))
                    if native not in server.threads:
                        self.send({'id': req['id'], 'error': {'code': -32602, 'message': 'unknown thread'}});continue
                    if method in ('thread/read', 'history') and server.history_gate is not None:
                        started, release = server.history_gate
                        server.history_gate = None
                        started.set()
                        assert release.wait(10), 'history gate was not released'
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
        self.history_gate = None
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
        from owned_fake_appserver import install
        for profile in json.loads(Path(config).read_text()).get('profiles',[]):
            command=Path(profile.get('command','/nonexistent'))
            if profile['harness']=='codex' and command.is_file() and command.is_relative_to(self.home):
                install(command)
        self.env={**{k:v for k,v in os.environ.items() if not k.startswith('MONTY_')}, 'MONTY_HOME':str(home), 'CODEX_HOME':str(home/'codex'),
                  'MANTLE_CONTEXT':'', **(extra or {})}
        self.log=open(home/'service.log','a+')
        self.process=None;self.start()
    def start(self, port=None):
        state=self.home/'.monty/client-service.json'
        if state.exists():state.unlink()
        self.process=subprocess.Popen([BIN,'service','serve','--home',str(self.home),'--config',str(self.config)]+([] if port is None else ['--port',str(port)]),env=self.env,stdout=self.log,stderr=self.log)
        for _ in range(100):
            if state.exists():break
            if self.process.poll() is not None:
                self.log.seek(0);raise AssertionError(self.log.read())
            time.sleep(.05)
        self.client=Client(self.home)
    def stop(self):
        if self.process and self.process.poll() is None:
            self.process.terminate();self.process.wait(timeout=25)
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
        assert configured['config_path']==str(config) and configured['port']>0
        run=lifecycle('stop');assert run.returncode==0,run.stderr
        service.process.wait(timeout=5)
        # Dead service metadata is expected after shutdown. Concurrent plain
        # launches must reach one detached service and one thread per caller.
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            runs=list(pool.map(lambda _:launch(),range(4)))
        assert all(run.returncode==0 for run in runs),[run.stderr for run in runs]
        started=json.loads(metadata.read_text())
        assert started['epoch']!=configured['epoch']
        assert started['url']==configured['url'] and started['port']==configured['port']
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
        lifecycle('stop')
        # Cohttp wraps an ECONNRESET while checking stale service metadata.
        with socket.socket() as reset:
            reset.bind(('127.0.0.1',0));reset.listen()
            saved=json.loads(metadata.read_text());saved['port']=0
            saved['url']='http://127.0.0.1:'+str(reset.getsockname()[1]);metadata.write_text(json.dumps(saved))
            def reset_health():
                connection,_=reset.accept()
                connection.setsockopt(socket.SOL_SOCKET,socket.SO_LINGER,struct.pack('ii',1,0));connection.close()
            threading.Thread(target=reset_health,daemon=True).start()
            run=launch();assert run.returncode==0,run.stderr
        print('client service start/stop, automatic startup, reuse, concurrency and recovery passed')
    finally:
        lifecycle('stop',target=fresh)
        service.close();stop_background();server.close()

def fake_suite(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    native_home=home/'codex';native_home.mkdir()
    server=Native(home/'codex.sock');second=Native(home/'second.sock')
    codex=home/'fake-codex'
    # An untested version must still connect; compatibility is established by RPC.
    codex.write_text('#!/usr/bin/env python3\nimport json,sys\nfrom pathlib import Path\n'
      + 'if sys.argv[1:4]==["app-server","daemon","start"]:print(json.dumps('+repr({'socketPath':str(home/'codex.sock'),'appServerVersion':'0.999.0'})+'))\n'
      + 'else:Path('+repr(str(home/'terminal.json'))+').write_text(json.dumps(sys.argv[1:]))\n')
    codex.chmod(0o700)
    config=home/'profiles.json';config.write_text(json.dumps({'allow_fixture':True,'profiles':[
        {'id':'default','harness':'codex','command':str(codex),'codex_home':str(native_home),'roots':[str(repo),str(home)]},
        {'id':'second','harness':'fixture','socket':str(home/'second.sock'),'roots':[str(repo)]},
        {'id':'filesystem-root','harness':'fixture','socket':str(home/'codex.sock'),'roots':['/']}]}))
    service=Service(home,config);c=service.client
    try:
        http_error(401,lambda:c.req('health',headers={'Authorization':'no'}))
        http_error(403,lambda:c.req('health',headers={'Origin':'https://evil.example'}))
        http_error(403,lambda:c.req('health',headers={'Host':'evil.example'}))
        # The service accepts only its bearer token; no dashboard, pairing or cookie auth remains.
        health = c.req('health')
        assert 'storage_scope' not in health
        port = c.url.rsplit(':', 1)[1]
        http_error(401, lambda: c.req('health', headers={
            'Authorization': '', 'Cookie': 'monty_session_' + port + '=' + c.token, 'Origin': c.url}))
        for route in ('browser/pair', 'browser/session', 'browser/logout'):
            http_error(404, lambda route=route: c.req(route, {}))
        for path in ('/', '/main.bc.js'):
            http_error(401, lambda path=path: urllib.request.urlopen(c.url + path))
            request = urllib.request.Request(c.url + path, headers={'Authorization': 'Bearer ' + c.token})
            http_error(404, lambda: urllib.request.urlopen(request))
        for args in [('web', '--help=plain'), ('guide', 'web-client')]:
            removed_cli = subprocess.run([BIN, *args, '--home', str(home)] if args[0] == 'web' else [BIN, *args],
                                         env=service.env, capture_output=True, text=True)
            assert removed_cli.returncode != 0, (args, removed_cli.stdout, removed_cli.stderr)
        assert c.req('inventory')['tasks']==[]
        assert not (home/'.monty/tasks.json').exists()
        removed=c.cmd('removed-classify',{'op':'classify','profile':'default',
            'text':'This feature has been removed.','seconds':30},status='rejected')
        assert removed['code']==400 and removed['message']=='unknown command',removed
        assert not server.threads and not (home/'.monty/conversation-executions').exists()
        request={'op':'start','profile':'default','context':{'workspaces':[str(repo)],'instructions':'trusted current instructions','environment':{'MONTY_JOB_TITLE':'isolated'}}}
        assert c.req('health')['codex_tested_versions']==['0.160.0','0.160.1']
        with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
            receipts=list(pool.map(lambda _:c.cmd('start-once',request),range(8)))
        cid=receipts[0]['id'];native=receipts[0]['sessions'][0]['native_id']
        assert len({r['id'] for r in receipts})==1 and len(server.threads)==1
        http_error(409,lambda:c.req('commands/start-once',{**request,'title':'different'}))
        params=next(params for method,params in server.calls if method=='thread/start')
        assert params['historyMode']=='legacy'
        assert (native_home/'skills/monty/SKILL.md').is_file()
        assert any(method=='skills/list' for method,_ in server.calls)
        assert not any(key.startswith('mcp_servers.monty') for key in params['config'])
        assert params['config']['shell_environment_policy.set']['MONTY_JOB_TITLE']=='isolated'
        assert (params['approvalPolicy'],params['sandbox'])==('on-request','workspace-write')
        assert c.history(cid)['sessions'][0]['records']
        send={'op':'send','conversation':cid,'revision':0,'selected_session':receipts[0]['selected_session'],'text':'one turn'}
        with concurrent.futures.ThreadPoolExecutor(max_workers=8) as pool:
            list(pool.map(lambda _:c.cmd('send-once',send),range(8)))
        assert server.sends==1
        c.cmd('wrong-selected-session',{**send,'selected_session':'another-session'},status='rejected')
        c.cmd('legacy-stale-send',{k:v for k,v in send.items() if k!='selected_session'},status='rejected')
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
        c.cmd('invalid-approval', {**answer, 'response': {'decision': 'allow-everything'}}, status='rejected')
        assert c.req('conversations/' + cid)['sessions'][0]['pending'][0]['state'] == 'pending'
        assert server.answers == [], 'invalid response must not reserve or answer the native request'
        # A transcript refresh changes the conversation revision, not the exact
        # pending prompt that the user has already reviewed.
        server.emit({'method':'item/completed','params':{'threadId':native,'turnId':'turn-1',
            'item':{'id':'while-pending','type':'agentMessage','text':'waiting for approval'}}})
        until(lambda:c.revision(cid),lambda revision:revision>current['revision'])
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
        assert not server.clients, 'restart must not silently recreate a runtime'
        c.cmd('interrupt',{'op':'interrupt','conversation':cid,'revision':c.revision(cid),'turn_id':'turn-2'})
        c.cmd('resume-after-unknown',{'op':'resume','conversation':cid,'revision':c.revision(cid),'detached':True})
        until(lambda:len(server.clients))
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
        assert picker.returncode==1 and 'no registered session' in picker.stderr,picker.stderr
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
        args=until(lambda:json.loads((home/'terminal.json').read_text()) if (home/'terminal.json').exists() else None);assert args[:2]==['resume','--remote'] and args[2].startswith('unix://') and args[2]!='unix://'+str(home/'codex.sock'),args
        assert ('config/value/write', {'keyPath':'tui.vim_mode_default','value':True,'mergeStrategy':'replace'}) in server.calls
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
            assert args[:2]==['resume','--remote'] and args[2].startswith('unix://') and args[2]!='unix://'+str(home/'codex.sock'),args
            assert '--dangerously-bypass-approvals-and-sandbox' not in args,args
        # Codex runs tools in the app-server: a terminal start passes the launching
        # pane's Rex context to that thread only, never into the session context.
        rex_run=subprocess.run([BIN,'terminal','--codex-command',str(codex),'--home',str(home)],cwd=repo,
                               env={**launch_env,'REX_SESSION':'session:01a1-caller','REX_BLOCK':'block:01a1-caller'},capture_output=True,text=True)
        assert rex_run.returncode==0,rex_run.stderr
        params=next(p for method,p in reversed(server.calls) if method=='thread/start')
        assert params['config']['shell_environment_policy.set']['REX_BLOCK']=='block:01a1-caller',params
        rex_thread=json.loads((home/'terminal.json').read_text())[-1]
        rex_chat=next(x for x in c.req('conversations') if x['sessions'][0]['native_id']==rex_thread)
        assert 'REX_BLOCK' not in rex_chat['sessions'][0]['context'].get('environment',{}),rex_chat
        resumed=args[-1];count=len(server.threads)
        (native_home/'skills/monty/SKILL.md').unlink()
        run=subprocess.run([BIN,'continue',resumed,'--home',str(home)],env=launch_env,capture_output=True,text=True)
        assert run.returncode==0,run.stderr
        assert len(server.threads)==count
        assert (native_home/'skills/monty/SKILL.md').is_file()
        assert any(method=='skills/list' for method,_ in server.calls)
        args=json.loads((home/'terminal.json').read_text())
        assert args[-1]==resumed and '--dangerously-bypass-approvals-and-sandbox' not in args
        assert args[args.index('-c')+1]=='tui.vim_mode_default=true'
        # Structured worker chats hold the same fence as headless/TTY execution.
        worker_bin=home/'worker-bin';worker_bin.mkdir()
        shutil.copytree(Path(__file__).resolve().parent.parent/'agent-profiles',home/'agent-profiles')
        for name in ('ghostty','osascript'):
            tool=worker_bin/name;tool.write_text('#!/bin/sh\nexit 0\n');tool.chmod(0o700)
        worker_env={**launch_env,'PATH':str(worker_bin)+os.pathsep+launch_env['PATH']}
        task_context=home/'context.md';task_context.write_text('Isolated structured worker.\n')
        for args in (['projects','add','--repo',str(repo)],
                     ['launch','--repo',str(repo),'--title','Structured worker','--branch','cto/structured-worker',
                      '--context',str(task_context),'--worktree','never','--terminal','ghostty','--harness','codex',
                      '--codex-command',str(codex)]):
            run=subprocess.run([BIN,*args,'--home',str(home)],cwd=repo,env=worker_env,capture_output=True,text=True)
            assert run.returncode==0,run.stderr
        job_file=next((home/'.monty/runs').glob('*/workers/structured-worker/job.json'))
        context={'workspaces':[str(repo)],'environment':{'MONTY_JOB_ID':'structured-worker',
                 'MONTY_WORKER_DIR':str(job_file.parent),'MONTY_JOB_FILE':str(job_file)}}
        owned=c.cmd('worker-chat',{'op':'start','profile':'default','context':context})
        owned_id=owned['id'];sidecar=job_file.parent/'codex-session-id';saved=sidecar.read_text()
        rejected=c.cmd('purge-owned-worker',{'op':'purge','conversation':owned_id,'confirm':True},status='rejected')
        assert rejected['code']==409 and 'owned runtimes' in rejected['message'],rejected
        assert c.req('conversations/'+owned_id)['sessions'][0]['native_id']==saved
        before=len(server.threads)
        duplicate=c.cmd('duplicate-worker-chat',{'op':'start','profile':'default','context':context},status='rejected')
        assert duplicate['code']==409 and len(server.threads)==before and sidecar.read_text()==saved,duplicate
        c.cmd('worker-release',{'op':'release_runtime','conversation':owned_id,'revision':c.revision(owned_id)})
        c.cmd('worker-resume',{'op':'resume','conversation':owned_id,'revision':c.revision(owned_id),'detached':True})
        assert sidecar.read_text()==saved
        c.cmd('worker-release-again',{'op':'release_runtime','conversation':owned_id,'revision':c.revision(owned_id)})
        c.cmd('purge-released-worker',{'op':'purge','conversation':owned_id,'confirm':True})
        http_error(410,lambda:c.history(owned_id))
        assert sidecar.read_text()==saved and json.loads(job_file.read_text())['status']=='launch-requested'
        count=len(server.threads)
        # Exercise the real PTY bridge with a fake TUI: one startup Escape,
        # later Insert left alone, resize forwarding, and native exit status.
        tui_fixture=codex.with_name(codex.name+'.original')
        tui_fixture.write_text(r'''#!/usr/bin/env python3
import os, select, sys, time, tty
from pathlib import Path
tty.setraw(0)
assert 'MONTY_CODEX_TUI_COMMAND' not in os.environ
assert os.get_terminal_size(0)==(120,40)
mode=(Path(__file__).parent/'fixture-vim-mode').read_text()
if mode=='exit':
    print('startup failed',flush=True)
    sys.exit(7)
print('Vim: '+mode,flush=True)
if mode=='Insert':
    assert select.select([0],[],[],5)[0]
    assert os.read(0,1)==b'\x1b'
print('Vim: Insert ready '+mode,flush=True)
assert select.select([0],[],[],5)[0]
assert os.read(0,1)==b'i'
deadline=time.monotonic()+5
while os.get_terminal_size(0)!=(132,50) and time.monotonic()<deadline:time.sleep(.01)
assert os.get_terminal_size(0)==(132,50)
sys.exit(7)
''')
        tui_fixture.chmod(0o700)
        for initial in ['Insert','Normal','exit']:
            (home/'fixture-vim-mode').write_text(initial)
            master,slave=pty.openpty();tui=None;output=b''
            try:
                fcntl.ioctl(slave,termios.TIOCSWINSZ,struct.pack('HHHH',40,120,0,0))
                tui=subprocess.Popen([BIN,'continue',resumed,'--home',str(home)],cwd=repo,
                    env=launch_env,
                    stdin=slave,stdout=slave,stderr=slave,start_new_session=True)
                os.close(slave);slave=None
                deadline=time.monotonic()+10
                marker=b'startup failed' if initial=='exit' else ('ready '+initial).encode()
                while marker not in output and time.monotonic()<deadline:
                    if select.select([master],[],[],.1)[0]:
                        try:output+=os.read(master,8192)
                        except OSError:break
                assert marker in output,output
                if initial!='exit':
                    fcntl.ioctl(master,termios.TIOCSWINSZ,struct.pack('HHHH',50,132,0,0))
                    tui.send_signal(signal.SIGWINCH);os.write(master,b'i')
                code=tui.wait(timeout=10)
                assert code==7,(initial,code,output)
                recorded=next(sess for conversation in c.req('conversations') for sess in conversation['sessions']
                              if sess['native_id']==resumed and sess['profile']=='default')
                assert not recorded['terminal'] and recorded['terminal_code']==7,recorded
            finally:
                if tui and tui.poll() is None:tui.kill();tui.wait(timeout=5)
                if slave is not None:os.close(slave)
                os.close(master)
        c.cmd('invalid-yolo',{**request,'codex_yolo':'true'},status='rejected')
        assert len(server.threads)==count
        subprocess.run([BIN,'settings','set','codex-yolo','false','--home',str(home)],env=service.env,check=True,capture_output=True)
        # Source failure is visible while previously owned history stays readable.
        second.close();until(lambda:c.req('conversations/'+cid)['sessions'][1]['coverage']['state'],lambda x:x=='unavailable')
        assert c.history(cid)['sessions'][0]['records']
        assert len(c.req('inventory')['tasks'])==1 and c.req('inventory')['tasks'][0]['status']=='open'
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
        cli('projects','add','--repo',str(other))
        cli('task','add','--project','repo','--title','API inventory first')
        cli('task','workspace','add','local:local-002','--repo',str(repo),'--branch','cto/api-one')
        cli('task','workspace','add','local:local-002','--repo',str(other),'--branch','cto/api-two')
        cli('task','add','--project','repo','--title','API inventory second')
        tasks_file=home/'.monty/tasks.local.json';before=tasks_file.read_bytes()
        view=c.req('inventory?all_groups=true')
        native_table=cli('list','--no-sync','--all-groups')
        assert view['banner']=='Groups: all (--all-groups)\n'
        assert [row['key'] for row in view['tasks']]==['local:local-001','local:local-002','local:local-003']
        assert view['tasks'][1]['workspaces']==json.loads(before)['tasks'][1]['workspaces']
        assert len(view['tasks'][1]['workspaces'])==2
        assert all(row['title'] in native_table and row['status'] in native_table for row in view['tasks'])
        assert before==tasks_file.read_bytes()
        # Malformed native frames cause visible source failure, never task completion.
        server.emit(['not-a-json-rpc-object'])
        until(lambda:next(x for x in c.req('conversations') if not x.get('purged'))['sessions'][0]['coverage']['state'],lambda x:x=='unavailable')
        assert tasks_file.read_bytes()==before
        # Root is a valid explicit workspace boundary; child paths must not be compared with '//'.
        root_chat = c.cmd('root-workspace', {'op': 'start', 'profile': 'filesystem-root',
                                           'context': {'workspaces': [str(repo)]}})
        assert root_chat['sessions'][0]['context']['workspaces'] == [str(repo)]
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
        assert not source.clients, 'restart must not silently create a native connection'
        c.cmd('recover-batch',{'op':'sync','conversation':cid})
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

def warm_archive_suite(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    native=home/'codex';native.mkdir();server=Native(home/'native.sock')
    codex=home/'fake-codex'
    codex.write_text('#!/usr/bin/env python3\nimport json,sys\n'
      +'if sys.argv[1:4]==["app-server","daemon","start"]:print('
      +repr(json.dumps({'socketPath':str(home/'native.sock')}))+')\n'
      +'else:assert sys.argv[1:3]==["resume","--remote"]\n')
    codex.chmod(0o700)
    config=home/'profiles.json';config.write_text(json.dumps({'profiles':[
        {'id':'default','harness':'codex','command':str(codex),'codex_home':str(native),'roots':[str(repo)]}]}))
    service=Service(home,config)
    try:
        result=service.client.cmd('start',{'op':'start','profile':'default','context':{'workspaces':[str(repo)]}})
        cid=result['id'];sess=result['sessions'][0];service.stop()
        path=home/'.monty/conversations'/cid/'conversation.json';manifest=json.loads(path.read_text())
        sess=manifest['sessions'][0];directory=path.parent/'sessions'/sess['id']/'batches'
        # Old raw payloads must not be reparsed on every poll or notification.
        batch=json.dumps({'schema':1,'normalizer':1,'raw':{'padding':'x'*1024*1024},'items':[]})
        for index in range(192):
            bid='archive-'+str(index);(directory/(bid+'.json')).write_text(batch);sess['batches'].append(bid)
        sess['checkpoint']=len(sess['batches']);path.write_text(json.dumps(manifest))
        item={'id':'message','type':'agentMessage','text':'first'}
        server.threads[sess['native_id']]['turns']=[{'id':'turn-1','status':'completed','items':[item]}]
        start=time.monotonic();service.start();c=service.client;c.cmd('cold',{'op':'sync','conversation':cid})
        cold=time.monotonic()-start;warm=[]
        for index in range(3):
            start=time.monotonic();c.cmd('warm-'+str(index),{'op':'sync','conversation':cid})
            run=subprocess.run([BIN,'terminal','--home',str(home),'--codex-command',str(codex)],
                               cwd=repo,env=service.env,capture_output=True,text=True,timeout=15)
            assert run.returncode==0,run.stderr
            warm.append(time.monotonic()-start)
        assert max(warm)<max(.7,cold/3), (cold,warm)
        def latest_revision():
            current=json.loads(path.read_text())['sessions'][0]
            return json.loads((directory/(current['batches'][-1]+'.json')).read_text())['items'][0]['revision']
        assert latest_revision()==1
        item['text']='second';c.cmd('changed',{'op':'sync','conversation':cid});assert latest_revision()==2
        service.stop();item['text']='third';service.start();c=service.client
        c.cmd('restarted',{'op':'sync','conversation':cid});assert latest_revision()==3
        print('large archive warm launches and revision recovery passed: cold %.3fs, warm %s' %
              (cold,', '.join('%.3fs' % duration for duration in warm)))
    finally:
        service.close();server.close()

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
      +'else:Path('+repr(str(home/'terminal.json'))+').write_text(json.dumps({"args":sys.argv[1:],"account":os.environ["CODEX_HOME"]}))\n')
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
        recorded=json.loads((home/'terminal.json').read_text());args=recorded['args']
        assert recorded['account']==str(account),recorded
        assert args[:2]==['resume','--remote'] and args[2].startswith('unix://') and args[2] not in ['unix://'+path for path in sockets.values()],args
        assert (account/'skills/monty/SKILL.md').is_file()
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
        rejected(terminal+['--profile','default'],work_env,'terminal account scope does not match')
        rejected((),{**work_env,'MONTY_API_PROFILE':'default'},'terminal account scope does not match')
        rejected(terminal+['--profile','missing'],work_env,'terminal account scope does not match')
        attached(launch(terminal+['--profile','company'],{**work_env,'MONTY_API_PROFILE':'default'}),work)
        unknown=home/'unknown';unknown.mkdir()
        rejected((),{**work_env,'CODEX_HOME':str(unknown)},'no service profile matches')
        assert not (home/'.monty/tasks.local.json').exists()
        # Ambiguous profiles need explicit selection, never list-order fallback.
        service.stop();profiles['profiles'].append({**profiles['profiles'][1],'id':'company-alias'})
        config.write_text(json.dumps(profiles));service.start()
        rejected((),work_env,'multiple service profiles match')
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
        c.cmd('check-changed-scope',{'op':'sync','conversation':cid})
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
            text=getattr(self.server,'reply','isolated fixture reply')
            item={'id':'msg-fixture','type':'message','role':'assistant','status':'completed','content':[{'type':'output_text','text':text,'annotations':[]}]}
            emit({'type':'response.output_item.added','output_index':0,'item':{**item,'status':'in_progress','content':[]}})
            emit({'type':'response.output_text.delta','item_id':'msg-fixture','output_index':0,'content_index':0,'delta':text})
            emit({'type':'response.output_item.done','output_index':0,'item':item})
            emit({'type':'response.completed','response':{'id':'resp-fixture','status':'completed','output':[item],
                'usage':{'input_tokens':1,'output_tokens':1,'total_tokens':2}}})
        except (BrokenPipeError,ConnectionResetError):pass

def real_spike(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    skill=repo/'.agents/skills/visible-fixture';skill.mkdir(parents=True)
    (skill/'SKILL.md').write_text('---\nname: visible-fixture\ndescription: Isolated skill visibility check.\n---\nOnly act if requested.\n')
    native=home/'codex';native.mkdir()
    model=http.server.ThreadingHTTPServer(('127.0.0.1',0),Model)
    model.started=threading.Event();model.release=threading.Event()
    threading.Thread(target=model.serve_forever,daemon=True).start()
    (native/'config.toml').write_text('model = "monty-fixture"\nmodel_provider = "fixture"\n[model_providers.fixture]\nname = "Fixture"\nbase_url = "http://127.0.0.1:'+str(model.server_port)+'/v1"\nwire_api = "responses"\nrequires_openai_auth = false\n')
    config=home/'profiles.json';config.write_text(json.dumps({'profiles':[{'id':'default','harness':'codex',
        'command':subprocess.check_output(['which','codex'],text=True).strip(),'codex_home':str(native),'roots':[str(repo)]}]}))
    shell_home=home/'shell';shell_home.mkdir()
    service=Service(home,config,{'HOME':str(shell_home)});c=service.client;tui=None;master=None
    try:
        result=c.cmd('real-start',{'op':'start','profile':'default','codex_yolo':True,'context':{'workspaces':[str(repo)]}})
        version=subprocess.check_output(['codex','--version'],env=service.env,text=True).strip()
        cid=result['id'];sess=result['sessions'][0];nid=sess['native_id']
        assert 'endpoint' not in sess and 'socketPath' not in sess
        started_thread=c.history(cid)['sessions'][0]['records'][0]['raw']['thread']
        assert started_thread['historyMode']=='legacy',started_thread['historyMode']
        # Bootstrap the fresh thread without a test client warming it first.
        master,slave=pty.openpty()
        tty.setraw(slave)
        fcntl.ioctl(slave,termios.TIOCSWINSZ,struct.pack('HHHH',40,120,0,0))
        tui=subprocess.Popen([BIN,'terminal','--home',str(home),'--mode','exact','--session',nid,'--codex-yolo'],
          cwd=repo,env={**service.env,'TERM':'xterm-256color'},
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
        # Bare startup must reach Normal mode before any user input or model turn.
        until(lambda:b''.join(terminal_output),lambda output:b'Vim: Normal' in output)
        assert not model.started.is_set()
        offset=sum(map(len,terminal_output))
        os.write(master,b'i')
        until(lambda:b''.join(terminal_output)[offset:],lambda output:b'Vim: Insert' in output)
        os.write(master,b'/skills')
        until(lambda:b''.join(terminal_output)[offset:],lambda output:b'/skills' in output)
        time.sleep(.2);os.write(master,b'\r')
        until(lambda:b''.join(terminal_output)[offset:],lambda output:b'Choose an action' in output)
        os.write(master,b'\r')
        until(lambda:b''.join(terminal_output)[offset:],lambda output:b'visible-fixture' in output)
        os.write(master,b'\x1b');time.sleep(.2);os.write(master,b'\x15')
        os.write(master,b'Hi')
        until(lambda:b''.join(terminal_output)[offset:],lambda output:b'Hi' in output)
        os.write(master,b'\r')
        assert model.started.wait(15),b''.join(terminal_output).decode(errors='replace')

        observer=Client(home)
        def active_turn():
            return any(turn['status']=='inProgress' for batch in observer.history(cid)['sessions'][0]['records']
                       for turn in batch.get('raw',{}).get('thread',{}).get('turns',[]))
        until(active_turn)
        tui.send_signal(signal.SIGHUP);tui.wait(timeout=10)
        assert active_turn()
        terminal=observer.req('terminals/'+sess['id'])
        assert terminal['running'],terminal
        model.release.set()
        history=until(lambda:c.history(cid),
            lambda h:any('isolated fixture reply' in json.dumps(b) for b in h['sessions'][0]['records']))
        assert history['sessions'][0]['native_id']==nid
        observer.cmd('real-second-send',{'op':'send','conversation':cid,'selected_session':sess['id'],'text':'Second fixture reply.'})
        until(lambda:c.history(cid),lambda h:len(h['sessions'][0]['effective'])>=4)
        print(f'real {version}: private server runtime, exact native TUI, initial Vim mode, skill visibility, client SIGHUP survival and shared API history passed; all inference local')
    finally:
        model.release.set()
        if tui and tui.poll() is None:tui.kill();tui.wait(timeout=5)
        if master is not None:os.close(master)
        service.close()
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
            with tempfile.TemporaryDirectory(prefix='mcaw-',dir='/tmp') as warm:
                warm_archive_suite(Path(warm).resolve())
