#!/usr/bin/env python3
"""Pi JSONL mirror and RPC checkout-binary E2E with isolated native homes."""
import base64
import concurrent.futures
import http.server
import json
import os
from pathlib import Path
import subprocess
import shutil
import sys
import tempfile
import threading
import time

from test_client_api import BIN, Client, Service, Native, until, http_error

FIXTURE=Path(__file__).with_name('fake_pi.py').resolve()

def entries(client,cid):
    return [item for session in client.history(cid)['sessions'] for batch in session['records'] for item in batch['items']]

def suite(home):
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    pi=home/'pi';pi.mkdir();(pi/'sessions').mkdir()
    second=home/'pi-second';second.mkdir()
    native=Native(home/'codex.sock')
    codex_home=home/'codex';codex_home.mkdir()
    codex=home/'fake-codex'
    codex.write_text('#!/usr/bin/env python3\nimport json\nprint(json.dumps('+repr({
        'socketPath':str(home/'codex.sock'),'appServerVersion':'0.160.0'})+'))\n');codex.chmod(0o700)
    fake_pi=home/'fake-pi'
    fake_pi.write_text('#!/bin/sh\nexec '+str(Path(sys.executable).resolve())+' '+str(FIXTURE)+' "$@"\n');fake_pi.chmod(0o700)
    config=home/'profiles.json';config.write_text(json.dumps({'profiles':[
      {'id':'pi','harness':'pi','command':str(fake_pi),'pi_home':str(pi),'roots':[str(repo)]},
      {'id':'second','harness':'pi','command':str(fake_pi),'pi_home':str(second),'roots':[str(repo)]},
      {'id':'codex','harness':'codex','command':str(codex),'codex_home':str(codex_home),'roots':[str(repo)]}]}))
    service=Service(home,config,{'MONTY_UNRELATED_WORKER':'must-not-inherit'});c=service.client
    stop_gate=home/'stop-gate'
    context={'workspaces':[str(repo)],'instructions':'Trusted Monty instructions','environment':{
        'MONTY_JOB_TITLE':'explicit','MONTY_FIXTURE_STOP_GATE':str(stop_gate)}}
    try:
        assert 'pi' in c.req('health')['real_adapters']
        path=pi/'sessions/native.jsonl'
        image=base64.b64encode(b'owned inline image').decode()
        records=[{'type':'session','version':3,'id':'native-file','cwd':str(repo)},
          {'type':'message','id':'u1','parentId':None,'message':{'role':'user','content':[{'type':'image','data':image,'mimeType':'image/png'}]}},
          {'type':'message','id':'a1','parentId':'u1','message':{'role':'assistant','content':'abandoned answer','provider':'old','model':'old-model'}},
          {'type':'message','id':'u2','parentId':'u1','message':{'role':'user','content':'new branch'}},
          {'type':'compaction','id':'c1','parentId':'u2','summary':'source summary','firstKeptEntryId':'u2'},
          {'type':'custom','id':'x1','parentId':'c1','data':{'accessToken':'secret-marker'}}]
        original=''.join(json.dumps(x)+'\n' for x in records);path.write_text(original)
        request={'op':'import','profile':'pi','session_file':str(path),'context':context}
        imported=c.cmd('import-file',request);cid=imported['id'];sid=imported['sessions'][0]['id']
        assert not (pi/'calls.jsonl').exists(), 'history import spawned Pi'
        assert path.read_text()==original
        assert not imported['sessions'][0]['observed']
        assert len(entries(c,cid))==5
        selected=c.req('conversations/'+cid)['sessions'][0]['effective']
        assert not any('a1' in key for key in selected) and any('x1' in key for key in selected)
        attachment=next(item for item in entries(c,cid) if item['source_id']=='u1')['attachments'][0]
        assert (home/'.monty/conversation-blobs'/attachment['sha256']).read_bytes()==b'owned inline image'
        assert 'secret-marker' not in json.dumps(c.history(cid))
        c.cmd('same-file',{'op':'sync','conversation':cid})
        assert len(entries(c,cid))==5
        c.cmd('unowned-send',{'op':'send','conversation':cid,'revision':c.revision(cid),'text':'no'},status='rejected')
        extra={'type':'message','id':'a2','parentId':'x1','message':{'role':'assistant','content':'appended from native TUI'}}
        with path.open('a') as out:out.write(json.dumps(extra))
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['coverage']['state'],lambda state:state=='partial')
        assert len(entries(c,cid))==5
        with path.open('a') as out:out.write('\n')
        until(lambda:entries(c,cid),lambda xs:any(x['source_id']=='a2' for x in xs))
        assert len(entries(c,cid))==6
        # Source replacement/truncation keeps every prior revision and branch.
        replacement=path.with_suffix('.tmp');replacement.write_text(original)
        replacement.replace(path)
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['effective'],lambda keys:not any('a2' in key for key in keys))
        assert any(x['source_id']=='a2' for x in entries(c,cid))
        path.write_text(original.replace('native-file','different-identity'))
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['coverage']['state'],lambda state:state=='unavailable')
        path.write_text(original)
        outside=home/'outside.jsonl';outside.write_text(original)
        c.cmd('outside',dict(request,session_file=str(outside)),status='rejected')
        alias=pi/'sessions/alias.jsonl';alias.symlink_to(outside)
        c.cmd('symlink',dict(request,session_file=str(alias)),status='rejected')
        path.unlink()
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['coverage']['state'],lambda state:state=='unavailable')
        assert len(entries(c,cid))==6
        path.write_text(original)
        c.cmd('purge-file',{'op':'purge','conversation':cid,'confirm':True})
        c.cmd('purged-reimport',request,status='rejected')
        assert not (home/'.monty/conversation-blobs'/attachment['sha256']).exists()

        # Real process boundary, continuously drained RPC, exact session ownership.
        start={'op':'start','profile':'pi','context':context}
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            chats=list(pool.map(lambda _:c.cmd('rpc-start',start),range(4)))
        chat=chats[0];cid=chat['id'];sess=chat['sessions'][0]
        assert len({chat['id'] for chat in chats})==1
        def calls():return [json.loads(line) for line in (pi/'calls.jsonl').read_text().splitlines()]
        assert len([x for x in calls() if 'start' in x])==1
        started=next(x for x in calls() if 'start' in x)
        assert started['environment']['MONTY_JOB_TITLE']=='explicit' and 'MONTY_UNRELATED_WORKER' not in started['environment']
        assert '--no-approve' in started['argv'] and '--append-system-prompt' in started['argv']
        send={'op':'send','conversation':cid,'revision':c.revision(cid),'text':'hello'}
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            list(pool.map(lambda _:c.cmd('rpc-send',send),range(4)))
        until(lambda:entries(c,cid),lambda xs:any(x['kind']=='agentMessage' for x in xs))
        assert len([x for x in calls() if x.get('command',{}).get('type')=='prompt'])==1
        assert Path(c.req('conversations/'+cid)['sessions'][0]['source_file']).exists()
        # A dialog response bypasses a prompt awaiting acknowledgement, once only.
        c.req('commands/dialog-send',{'op':'send','conversation':cid,'revision':c.revision(cid),'text':'dialog'})
        current=until(lambda:c.req('conversations/'+cid),lambda chat:bool(chat['sessions'][0].get('pending')))
        pending=current['sessions'][0]['pending'][0]
        answer={'op':'respond','conversation':cid,'revision':current['revision'],'pending_id':pending['id'],
                'pending_revision':1,'response':{'confirmed':False}}
        c.cmd('dialog-invalid',dict(answer,response={'value':'wrong dialog shape'}),status='rejected')
        assert c.req('conversations/'+cid)['sessions'][0]['pending'][0]['state']=='pending'
        c.req('commands/dialog-a',answer);c.req('commands/dialog-b',answer)
        until(lambda:c.req('commands/dialog-send')['status'],lambda status:status=='finished')
        until(lambda:[c.req('commands/'+key)['status'] for key in ('dialog-a','dialog-b')],
              lambda statuses:sorted(statuses)==['finished','rejected'])
        assert len([x for x in calls() if x.get('command',{}).get('type')=='extension_ui_response'])==1
        c.req('commands/expiring-send',{'op':'send','conversation':cid,'revision':c.revision(cid),'text':'expiring dialog'})
        current=until(lambda:c.req('conversations/'+cid),lambda chat:bool(chat['sessions'][0].get('pending')))
        pending=current['sessions'][0]['pending'][0]
        until(time.time,lambda now:now>pending['expires_at'])
        c.cmd('expired-response',{'op':'respond','conversation':cid,'revision':c.revision(cid),
              'pending_id':pending['id'],'pending_revision':1,'response':{'confirmed':True}},status='rejected')
        c.cmd('abort-dialog',{'op':'interrupt','conversation':cid,'revision':c.revision(cid)})
        until(lambda:c.req('commands/expiring-send')['status'],lambda status:status=='finished')
        c.cmd('model',{'op':'set_model','conversation':cid,'revision':c.revision(cid),'provider':'fixture','model':'second'})
        c.cmd('hold',{'op':'send','conversation':cid,'revision':c.revision(cid),'text':'hold'})
        c.cmd('steer',{'op':'send','conversation':cid,'revision':c.revision(cid),'text':'queued steering','delivery':'steer'})
        c.cmd('busy-detach',{'op':'detach','conversation':cid,'revision':c.revision(cid)},status='rejected')
        c.cmd('busy-switch',{'op':'switch','conversation':cid,'revision':c.revision(cid),'mode':'branch','profile':'second','context':context},status='rejected')
        stopped=c.cmd('interrupt',{'op':'interrupt','conversation':cid,'revision':c.revision(cid)})
        assert stopped['cleared_queue']['steering']==['queued steering']
        # Bidirectional switching uses existing trusted packages and native boundaries.
        changed=c.cmd('to-codex',{'op':'switch','conversation':cid,'revision':c.revision(cid),'mode':'branch','profile':'codex','context':context})
        assert len(changed['sessions'])==2
        changed=c.cmd('to-pi',{'op':'switch','conversation':cid,'revision':c.revision(cid),'mode':'branch','profile':'second','context':context})
        assert len(changed['sessions'])==3
        until(lambda:entries(c,cid),lambda xs:any(x['kind']=='contextTransfer' for x in xs))
        second_calls=[json.loads(line) for line in (second/'calls.jsonl').read_text().splitlines()]
        transferred=next(x['command']['message'] for x in second_calls if x.get('command',{}).get('type')=='prompt')
        assert json.loads(transferred.split('\n',1)[1])==changed['transfers'][-1]['package']
        assert not any(x.get('command',{}).get('type')=='bash' for x in second_calls)
        # Restart closes service-owned children; history stays available without respawn.
        counts=len([x for x in calls() if 'start' in x])
        # Concurrent stop also waits while the listener is closed and Pi cleanup
        # still owns the service lock.
        stop_gate.touch()
        def stop():
            return subprocess.run([BIN,'service','stop','--home',str(home)],env=service.env,
                                  capture_output=True,text=True,timeout=25)
        with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
            first=pool.submit(stop)
            try:
                until(lambda:any('stopping' in call for call in calls()))
                second_stop=pool.submit(stop)
                time.sleep(1.5)
                assert not second_stop.done(),second_stop.result().stderr
            finally:stop_gate.unlink()
            for result in [first.result(),second_stop.result()]:
                assert result.returncode==0,result.stderr
        service.process.wait(timeout=5)
        service.start();c=service.client
        assert len([x for x in calls() if 'start' in x])==counts
        assert entries(c,cid)
        c.cmd('missing-runtime',{'op':'send','conversation':cid,'revision':c.revision(cid),'text':'no'},status='rejected')
        c.cmd('no-detach-confirmation',{'op':'resume','conversation':cid,'revision':c.revision(cid)},status='rejected')
        c.cmd('resume',{'op':'resume','conversation':cid,'revision':c.revision(cid),'detached':True})
        lost={'op':'send','conversation':cid,'revision':c.revision(cid),'text':'lost response'}
        c.cmd('lost',lost,status='outcome_unknown')
        c.cmd('lost',lost,status='outcome_unknown')
        calls_after=[json.loads(line) for line in (second/'calls.jsonl').read_text().splitlines()]
        assert len([x for x in calls_after if x.get('command',{}).get('message')=='lost response'])==1
        until(lambda:c.req('conversations/'+cid)['sessions'][-1]['runtime'],lambda state:state=='disconnected')
        # A flushed but uncommitted file batch is discarded and mirrored again on restart.
        crash_path=pi/'sessions/crash.jsonl'
        crash_records=[dict(records[0],id='crash-file'),dict(records[1],id='crash-user')]
        crash_path.write_text(''.join(json.dumps(row)+'\n' for row in crash_records))
        starts_before=len([x for x in calls() if 'start' in x])
        service.stop();service.env['MONTY_FAULT_INJECT']='conversation-after-batch';service.start();c=service.client
        crash_request=dict(request,session_file=str(crash_path))
        c.cmd('pi-batch-fault',crash_request,status='outcome_unknown')
        imported=next(chat for chat in c.req('conversations') if any(s['native_id']=='crash-file' for s in chat['sessions']))
        assert imported['sessions'][0]['checkpoint']==0
        orphans=list((home/'.monty/conversations'/imported['id']/'sessions').glob('*/batches/*.json'))
        assert orphans
        service.stop();service.env.pop('MONTY_FAULT_INJECT');service.start();c=service.client
        until(lambda:entries(c,imported['id']),lambda xs:len(xs)==1)
        assert all(not path.exists() for path in orphans)
        assert len([x for x in calls() if 'start' in x])==starts_before
        # An explicit same-account import can rebind a renamed profile without a runtime.
        service.stop()
        profiles=json.loads(config.read_text());profiles['profiles'][0]['id']='pi-renamed'
        config.write_text(json.dumps(profiles));service.start();c=service.client
        rebound=c.cmd('rename-import',dict(crash_request,profile='pi-renamed'))
        assert rebound['id']==imported['id'] and rebound['sessions'][0]['profile']=='pi-renamed'
        assert len(entries(c,imported['id']))==1
        # Detach owns its final RPC snapshot even when Pi has no file to mirror.
        volatile=c.cmd('volatile-start',{'op':'start','profile':'pi-renamed',
          'context':dict(context,environment={'MONTY_FIXTURE_UNFLUSHED':'1'})})
        volatile_id=volatile['id'];volatile_path=Path(volatile['sessions'][0]['source_file'])
        c.cmd('volatile-detach',{'op':'detach','conversation':volatile_id,'revision':c.revision(volatile_id)})
        final_batches=[batch for batch in c.history(volatile_id)['sessions'][0]['records']
                       if batch['provenance']=='Pi detach boundary']
        assert final_batches and any(item['content']['data']=='unflushed setup' for item in final_batches[-1]['items'])
        assert not volatile_path.exists()
        assert c.req('conversations/'+volatile_id)['sessions'][0]['runtime']=='disconnected'
        # A matching native ID cannot authorize a different resume file.
        rebound=c.cmd('rebind-start',{'op':'start','profile':'pi-renamed',
          'context':dict(context,environment={'MONTY_FIXTURE_REBIND':'1'})})
        rebound_id=rebound['id'];source_file=rebound['sessions'][0]['source_file']
        c.cmd('rebind-send',{'op':'send','conversation':rebound_id,'revision':c.revision(rebound_id),'text':'persist fixture history'})
        c.cmd('rebind-detach',{'op':'detach','conversation':rebound_id,'revision':c.revision(rebound_id)})
        before_stops=len([call for call in calls() if 'stopped' in call])
        result=c.cmd('rebind-resume',{'op':'resume','conversation':rebound_id,
          'revision':c.revision(rebound_id),'detached':True},status='rejected')
        assert result['code']==409 and 'different session file' in result['message']
        until(lambda:len([call for call in calls() if 'stopped' in call]),lambda count:count==before_stops+1)
        assert c.req('conversations/'+rebound_id)['sessions'][0]['source_file']==source_file
        assert c.req('inventory')['tasks']==[]
        assert not (home/'.monty/tasks.local.json').exists()
        print('Pi file mirror, RPC control, dialog races, switching and restart E2E passed')
    finally:
        service.close();native.close()

class Model(http.server.BaseHTTPRequestHandler):
    def log_message(self,*args):pass
    def do_POST(self):
        body=json.loads(self.rfile.read(int(self.headers['Content-Length'])))
        self.server.requests.append(body)
        self.send_response(200);self.send_header('Content-Type','text/event-stream');self.end_headers()
        try:
            def chunk(delta, finish=None):
                value={'id':'chatcmpl-fixture','object':'chat.completion.chunk','created':1,'model':'fixture',
                       'choices':[{'index':0,'delta':delta,'finish_reason':finish}]}
                self.wfile.write(('data: '+json.dumps(value)+'\n\n').encode());self.wfile.flush()
            chunk({'role':'assistant','content':''})
            if 'hold for interrupt' in json.dumps(body['messages'][-1]):
                self.server.started.set();self.server.release.wait(20)
            chunk({'content':'Real Pi with a local model fixture.'})
            chunk({},'stop')
            self.wfile.write(b'data: [DONE]\n\n');self.wfile.flush()
        except (BrokenPipeError,ConnectionResetError):pass

def real_suite(home):
    command=shutil.which('pi');assert command, 'installed Pi 1.0.2 is required'
    repo=home/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    pi=home/'pi';pi.mkdir();shell_home=home/'shell';shell_home.mkdir()
    model=http.server.ThreadingHTTPServer(('127.0.0.1',0),Model)
    model.requests=[];model.started=threading.Event();model.release=threading.Event()
    thread=threading.Thread(target=model.serve_forever,daemon=True);thread.start()
    (pi/'models.json').write_text(json.dumps({'providers':{'fixture':{
      'baseUrl':'http://127.0.0.1:'+str(model.server_port)+'/v1','api':'openai-completions','apiKey':'local-fixture',
      'models':[{'id':'fixture','name':'Local fixture','reasoning':False,'input':['text'],
                 'cost':{'input':0,'output':0,'cacheRead':0,'cacheWrite':0},'contextWindow':32768,'maxTokens':512}]}}}))
    config=home/'profiles.json';config.write_text(json.dumps({'profiles':[
      {'id':'pi','harness':'pi','command':command,'pi_home':str(pi),'roots':[str(repo)],'tools':[]}]}))
    service=Service(home,config,{'HOME':str(shell_home)});c=service.client
    try:
        chat=c.cmd('real-pi-start',{'op':'start','profile':'pi','modelProvider':'fixture','model':'fixture',
          'context':{'workspaces':[str(repo)],'instructions':'This is a local integration fixture.'}})
        cid=chat['id'];native=chat['sessions'][0]['native_id'];source=Path(chat['sessions'][0]['source_file'])
        assert chat['sessions'][0]['observed']
        c.cmd('real-pi-send',{'op':'send','conversation':cid,'revision':c.revision(cid),'text':'Reply with the fixture text.'})
        def real_answer():
            return [item for item in entries(c,cid) if item['kind']=='agentMessage' and
                    'Real Pi with a local model fixture.' in json.dumps(item)]
        until(real_answer,seconds=20)
        assert source.exists()
        assert len(model.requests)==1
        assert c.req('conversations/'+cid)['sessions'][0]['native_id']==native
        # A second HTTP client observes the same process and can interrupt it.
        observer=Client(home)
        c.cmd('real-pi-hold',{'op':'send','conversation':cid,'revision':c.revision(cid),'text':'hold for interrupt'})
        assert model.started.wait(10)
        observer.cmd('real-pi-abort',{'op':'interrupt','conversation':cid,'revision':observer.revision(cid)})
        model.release.set()
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['runtime'],lambda state:state=='idle')
        c.cmd('real-pi-detach',{'op':'detach','conversation':cid,'revision':c.revision(cid)})
        # Native Pi can continue the detached file; the read-only mirror catches it.
        run=subprocess.run([command,'--offline','--no-approve','--no-tools','--no-extensions','--no-skills',
          '--session',str(source),'--provider','fixture','--model','fixture','-p','native terminal continuation'],
          cwd=repo,env={**service.env,'PI_CODING_AGENT_DIR':str(pi),'PI_TELEMETRY':'0'},capture_output=True,text=True,timeout=25)
        assert run.returncode==0,run.stderr
        until(lambda:entries(c,cid),lambda xs:any('native terminal continuation' in json.dumps(x) for x in xs))
        assert json.loads(source.read_text().splitlines()[0])['id']==native
        c.cmd('real-pi-resume',{'op':'resume','conversation':cid,'revision':c.revision(cid),'detached':True})
        assert c.req('conversations/'+cid)['sessions'][0]['native_id']==native
        assert c.req('inventory')['tasks']==[]
        print('real Pi 1.0.2: RPC send/interrupt, shared API observation, native file continuation/mirroring and exact detached resume passed; all inference local')
    finally:
        model.release.set();service.close();model.shutdown();model.server_close()

if __name__=='__main__':
    with tempfile.TemporaryDirectory(prefix='monty-pi-',dir='/tmp') as root:
        (real_suite if '--real' in sys.argv else suite)(Path(root).resolve())
