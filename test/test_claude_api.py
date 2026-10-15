"""Claude scoped adapter and server-owned native terminal checkout-binary E2E."""
import base64
import concurrent.futures
import json
import os
from pathlib import Path
import signal
import shutil
import subprocess
import sys
import tempfile
import time
from test_client_api import Client, Service, until, http_error

BIN=str(Path(sys.argv[1]).resolve())
SDK=Path(__file__).with_name('fake_claude_sdk').resolve()

def items(c,cid):return [x for s in c.history(cid)['sessions'] for b in s['records'] for x in b['items']]

with tempfile.TemporaryDirectory(prefix='monty-claude-') as root:
    root=Path(root).resolve();repo=root/'repo';repo.mkdir();second_repo=root/'other';second_repo.mkdir()
    shutil.copytree(Path(__file__).resolve().parent.parent/'agent-profiles',root/'agent-profiles')
    for directory in [repo,second_repo]:subprocess.run(['git','init','-q',str(directory)],check=True)
    account=root/'claude';account.mkdir();other=root/'other-account';other.mkdir()
    python=root/'sdk-python';python.write_text('#!/bin/sh\nPYTHONPATH='+str(SDK)+' exec '+sys.executable+' "$@"\n');python.chmod(0o700)
    cli=root/'claude-cli'
    cli.write_text('#!'+sys.executable+'\n'+'''import json,os,sys
from pathlib import Path
args=sys.argv[1:]
if '--version' in args:print('2.1.296');sys.exit()
if '--print' in args:
 text=sys.stdin.read();print(json.dumps({'type':'result','subtype':'success','is_error':False,'session_id':'12345678-1234-4234-8234-123456789abc','permission_denials':[],'result':'complete Claude phase'}));sys.exit()
native=args[args.index('--session-id')+1] if '--session-id' in args else args[args.index('--resume')+1]
log=Path(os.environ['CLAUDE_CONFIG_DIR'])/'terminal.json'
log.write_text(json.dumps({'pid':os.getpid(),'ppid':os.getppid(),'native':native,'tty':os.isatty(0),'args':args}))
print('NATIVE CLAUDE '+native,flush=True)
for line in sys.stdin:
 print('RECEIVED '+line.strip(),flush=True)
 if line.strip()=='/exit':break
''');cli.chmod(0o700)
    config=root/'profiles.json';config.write_text(json.dumps({'profiles':[
      {'id':name,'harness':'claude','command':str(cli),'python':str(python),'claude_home':str(directory),'roots':[str(repo),str(second_repo)]}
      for name,directory in [('claude',account),('other',other)]]}))
    service=Service(root,config);c=service.client
    context={'workspaces':[str(repo),str(second_repo)],'instructions':'Monty trusted instructions and standalone context'}
    try:
        assert 'claude' in c.req('health')['real_adapters']
        http_error(403,lambda:c.req('supervisors/authorize', {'execution':'guessed','nonce':'guessed','parent_pid':os.getpid()}))
        forbidden={'op':'start','profile':'claude','context':dict(context,environment={'MONTY_CLAUDE_PRIVATE_KEY':'must-not-be-persisted'})}
        http_error(400,lambda:c.req('commands/credential-environment',forbidden))
        http_error(404,lambda:c.req('commands/credential-environment'))
        http_error(400,lambda:c.req('commands/foreign-home',{'op':'start','profile':'claude',
            'context':dict(context,environment={'MONTY_HOME':str(repo)})}))
        http_error(404,lambda:c.req('commands/foreign-home'))
        for key in ('MONTY_WT_COMMAND', 'MONTY_CONTAINER_COMMAND', 'MONTY_CODEX_YOLO', 'MONTY_PI_COMMAND', 'MONTY_CODEX_COMMAND', 'MONTY_CLAUDE_COMMAND', 'MONTY_API_PROFILE'):
            http_error(400,lambda:c.req('commands/forbidden-command-'+key, {
                'op':'start','profile':'claude','context':dict(context,environment={key:'/bin/sh -c false'})}))
            http_error(404,lambda:c.req('commands/forbidden-command-'+key))
        overridden=c.cmd('worktree-override',{'op':'headless_start','profile':'claude','worker':'absent',
                                             'wt_command':'/bin/sh -c false'},status='rejected')
        assert overridden['code']==409 and 'trusted server command' in overridden['message'], overridden
        invalid_policy=c.cmd('invalid-headless-policy',{'op':'headless_start','profile':'claude','worker':'absent',
                                                       'codex_yolo':'true'},status='rejected')
        assert invalid_policy['code']==400 and 'boolean' in invalid_policy['message'],invalid_policy
        assert not any('must-not-be-persisted' in path.read_text() for path in (root/'.monty').glob('**/*.json'))
        request={'op':'start','profile':'claude','context':context}
        with concurrent.futures.ThreadPoolExecutor(max_workers=4) as pool:
            chats=list(pool.map(lambda _:c.cmd('start-once',request),range(4)))
        chat=chats[0];cid=chat['id'];sess=chat['sessions'][0];native=sess['native_id']
        calls=lambda:[json.loads(x) for x in (account/'calls.jsonl').read_text().splitlines()]
        starts=[x for x in calls() if 'start' in x];assert len(starts)==1
        assert starts[0]['pid']!=service.process.pid and starts[0]['ppid']!=os.getpid()
        assert starts[0]['add_dirs']==[str(second_repo)]
        assert not (other/'calls.jsonl').exists()
        assert (account/'skills/monty/SKILL.md').exists()
        c.cmd('send-once',{'op':'send','conversation':cid,'selected_session':sess['id'],'text':'approval'})
        pending=until(lambda:c.req('conversations/'+cid)['sessions'][0].get('pending'))[0]
        c.cmd('detach-busy',{'op':'detach','conversation':cid,'revision':c.revision(cid)})
        assert c.req('conversations/'+cid)['sessions'][0]['runtime']=='running'
        answer={'op':'respond','conversation':cid,'pending_id':pending['id'],'pending_revision':1,'response':{'decision':'deny'}}
        c.cmd('invalid-answer',dict(answer,response={'decision':'bypass'}),status='rejected')
        with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
            list(pool.map(lambda n:c.req('commands/answer-'+str(n),answer),range(2)))
        until(lambda:sorted(c.req('commands/answer-'+str(n))['status'] for n in range(2)),lambda x:x==['finished','rejected'])
        until(lambda:items(c,cid),lambda xs:any(x['kind']=='agentMessage' for x in xs))
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['runtime'],lambda x:x=='idle')
        c.cmd('question',{'op':'send','conversation':cid,'selected_session':sess['id'],'text':'question'})
        pending=until(lambda:c.req('conversations/'+cid)['sessions'][0].get('pending'))[0]
        c.cmd('answer-question',{'op':'respond','conversation':cid,'pending_id':pending['id'],'pending_revision':1,'response':{'decision':'allow','answers':{'Choose a color':'Blue'}}})
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['runtime'],lambda x:x=='idle')
        discovery=c.req('discovery?profile=claude');assert discovery['data'][0]['native_id']==native
        assert c.req('discovery?profile=other')['data']==[]
        c.cmd('hold',{'op':'send','conversation':cid,'selected_session':sess['id'],'text':'hold'})
        c.cmd('interrupt',{'op':'interrupt','conversation':cid,'selected_session':sess['id']})
        until(lambda:c.req('conversations/'+cid)['sessions'][0]['runtime'],lambda x:x=='idle')
        count=len([x for x in calls() if 'start' in x]);service.stop();service.start();c=service.client
        time.sleep(.2);assert len([x for x in calls() if 'start' in x])==count
        c.cmd('stale-answer',answer,status='rejected')
        c.cmd('resume-exact',{'op':'resume','conversation':cid,'revision':c.revision(cid),'detached':True})
        assert [x for x in calls() if 'start' in x][-1]['start']==native
        # A renamed profile may bind only the same unique account scope. Keep
        # the historical alias and exact session identity in durable records.
        original_config=config.read_text();renamed=json.loads(original_config)
        renamed['profiles'][0]['id']='renamed'
        service.stop();config.write_text(json.dumps(renamed));service.start();c=service.client
        c.cmd('resume-renamed',{'op':'resume','conversation':cid,'revision':c.revision(cid),'detached':True})
        assert [x for x in calls() if 'start' in x][-1]['start']==native
        assert c.req('conversations/'+cid)['sessions'][0]['profile']=='claude'
        service.stop();config.write_text(original_config);service.start();c=service.client
        c.cmd('bad-root',{'op':'start','profile':'claude','context':{'workspaces':[str(root)]}},status='rejected')
        c.cmd('absent',{'op':'import','profile':'claude','native_id':'12345678-1234-4234-8234-123456789abc','context':context},status='rejected')
        invalid=c.cmd('invalid-native',{'op':'import','profile':'claude','native_id':'../other-account','context':context},status='rejected')
        assert invalid['code']==400 and 'UUID' in invalid['message']
        # Native terminal is a server child, and client EOF cannot terminate it.
        env={**service.env,'CLAUDE_CONFIG_DIR':str(account)}
        terminal=subprocess.run([BIN,'terminal','--harness','claude','--home',str(root)],cwd=repo,env=env,capture_output=True,text=True,timeout=15)
        assert terminal.returncode==0,terminal.stderr
        record=until(lambda:json.loads((account/'terminal.json').read_text()) if (account/'terminal.json').exists() else None)
        assert record['tty'] and record['pid']!=service.process.pid
        chat=next(x for x in c.req('conversations') if x['sessions'][0]['native_id']==record['native'])
        sid=chat['sessions'][0]['id'];cid=chat['id'];os.kill(record['pid'],0)
        c.cmd('resize',{'op':'terminal_resize','conversation':cid,'selected_session':sid,'rows':37,'cols':99})
        c.cmd('input',{'op':'terminal_input','conversation':cid,'selected_session':sid,'data':base64.b64encode(b'hello\n').decode()})
        assert c.req('commands/input')['request']['data']=='[terminal input omitted]'
        out=until(lambda:c.req('terminals/'+sid+'?offset=0'),lambda x:b'RECEIVED hello' in base64.b64decode(x['data']))
        assert out['running']
        c.cmd('exit',{'op':'terminal_input','conversation':cid,'selected_session':sid,'data':base64.b64encode(b'/exit\n').decode()})
        until(lambda:c.req('terminals/'+sid)['running'],lambda x:not x)
        # A detached client cannot release the worker fence, and an inherited
        # workspace map cannot redirect a worker away from its durable record.
        task_context=root/'context.md';task_context.write_text('Bounded native worker fixture.\n')
        fake_bin=root/'fake-bin';fake_bin.mkdir()
        osascript=fake_bin/'osascript';osascript.write_text('#!/bin/sh\nexit 0\n');osascript.chmod(0o700)
        worker_env=dict(env,PATH=str(fake_bin)+os.pathsep+env['PATH'])
        for args in (['projects','add','--repo',str(repo)],
                     ['launch','--repo',str(repo),'--title','Native worker','--branch','cto/native-worker',
                      '--context',str(task_context),'--worktree','never','--terminal','ghostty',
                      '--harness','claude','--claude-command',str(cli)]):
            run=subprocess.run([BIN,*args,'--home',str(root)],cwd=repo,env=worker_env,capture_output=True,text=True)
            assert run.returncode==0,run.stderr
        job_file=next((root/'.monty/runs').glob('*/workers/native-worker/job.json'))
        worker_dir=job_file.parent
        worker_env=dict(worker_env,MONTY_JOB_ID='native-worker',MONTY_WORKER_DIR=str(worker_dir),
                        MONTY_JOB_FILE=str(job_file),MONTY_WORKSPACE_1='/stale/other-task',
                        MONTY_WORKSPACE_2='/stale/extra-task',MONTY_JOB_WORKTREE='/stale/other-task')
        run=subprocess.run([BIN,'terminal','--worker','--harness','claude','--home',str(root),
                            '--instructions',str(worker_dir/'MONTY.md'),'--context',str(task_context)],
                           cwd=repo,env=worker_env,capture_output=True,text=True,timeout=15)
        assert run.returncode==0,run.stderr
        record=until(lambda:json.loads((account/'terminal.json').read_text()),lambda r:r['native']!=record['native'])
        native=record['native'];assert (worker_dir/'claude-session-id').read_text().strip()==native
        chat=next(x for x in c.req('conversations') if x['sessions'][0]['native_id']==native)
        worker_context=chat['sessions'][0]['context']
        assert worker_context['workspaces']==[str(repo)]
        assert worker_context['environment']['MONTY_WORKSPACE_1']==str(repo)
        assert 'MONTY_WORKSPACE_2' not in worker_context['environment']
        duplicate=c.cmd('duplicate-worker',{'op':'start','profile':'claude','terminal':True,'context':worker_context})
        rejected=c.cmd('duplicate-worker-open',{'op':'terminal_open','conversation':duplicate['id'],
                         'selected_session':duplicate['sessions'][0]['id'],'revision':duplicate['revision'],'fresh':True},status='rejected')
        assert rejected['code']==409 and 'busy' in rejected['message'],rejected
        assert (worker_dir/'claude-session-id').read_text().strip()==native
        assert json.loads((account/'terminal.json').read_text())['native']==native
        sid=chat['sessions'][0]['id'];cid=chat['id']
        c.cmd('worker-exit',{'op':'terminal_input','conversation':cid,'selected_session':sid,'data':base64.b64encode(b'/exit\n').decode()})
        until(lambda:c.req('terminals/'+sid)['running'],lambda x:not x)
        service.stop();service.start();c=service.client
        retained=c.req('terminals/'+sid)
        assert not retained['running'] and retained['code']==0
        assert ('NATIVE CLAUDE '+native).encode() in base64.b64decode(retained['data'])
        # Abrupt owner death cannot create a second native writer. Byte history
        # survives, while the unfinished terminal has an explicitly unknown result.
        run=subprocess.run([BIN,'terminal','--harness','claude','--home',str(root)],cwd=repo,env=env,capture_output=True,text=True,timeout=15)
        assert run.returncode==0,run.stderr
        crashed=until(lambda:json.loads((account/'terminal.json').read_text()),lambda r:r['native']!=native)
        chat=next(x for x in c.req('conversations') if x['sessions'][0]['native_id']==crashed['native'])
        sid=chat['sessions'][0]['id']
        service.process.kill();service.process.wait(timeout=5)
        service.start();c=service.client
        def native_stopped():
            try:os.kill(crashed['pid'],0);return False
            except ProcessLookupError:return True
        until(native_stopped,seconds=10)
        retained=c.req('terminals/'+sid)
        assert not retained['running'] and retained['code']=='outcome_unknown'
        assert ('NATIVE CLAUDE '+crashed['native']).encode() in base64.b64decode(retained['data'])
        assert c.req('conversations/'+chat['id'])['sessions'][0]['terminal'] is False
        print('Claude API: scoped SDK, ordered workspaces, receipts, approvals/questions, detach, exact resume, restart and server-owned PTY passed')
    finally:service.close()
