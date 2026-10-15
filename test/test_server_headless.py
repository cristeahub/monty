"""Server-owned headless phases, policy pinning and concurrent --wait batches."""
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile

from test_client_api import BIN, Service, until
from test_tasks_api import fixture

with tempfile.TemporaryDirectory(prefix='monty-server-headless-') as directory:
    root=Path(directory).resolve()
    bootstrap,repo=fixture(root)
    bootstrap.close()
    account=root/'claude';account.mkdir()
    actor=root/'native-agent'
    actor.write_text('#!'+sys.executable+'\nROOT='+repr(str(root))+'\n'+r'''
import json,os,sys,time,uuid,subprocess
from pathlib import Path
args=sys.argv[1:]
if '--version' in args:print('2.1.296');sys.exit()
prompt=sys.stdin.read()
worker=os.environ['MONTY_JOB_ID']
parents=[];parent=os.getppid()
while parent>1:
 parents.append(parent)
 value=subprocess.run(['ps','-o','ppid=','-p',str(parent)],capture_output=True,text=True).stdout.strip()
 if not value:break
 parent=int(value)
row={'worker':worker,'pid':os.getpid(),'parents':parents,'args':args,
     'yolo':os.environ.get('MONTY_CODEX_YOLO'),'private_nonce_cleared':not os.environ.get('MONTY_SERVER_NONCE'),
     'scoped_auth':os.environ.get('OPENAI_API_KEY')=='scoped-test-key'}
with open(Path(ROOT)/'phases.jsonl','a') as log:log.write(json.dumps(row)+'\n')
if worker in ('wait-a','wait-b'):
 marker=Path(ROOT)/('ready-'+worker)
 if not marker.exists():
  marker.touch();deadline=time.monotonic()+10
  while not all((Path(ROOT)/('ready-'+name)).exists() for name in ('wait-a','wait-b')):
   if time.monotonic()>deadline:raise RuntimeError('wait batch dispatched sequentially')
   time.sleep(.02)
if '--print' in args:
 print(json.dumps({'type':'result','subtype':'success','is_error':False,'session_id':str(uuid.uuid4()),
                   'permission_denials':[],'result':'Validated scoped server phase.'}))
else:
 assert 'exec' in args
 Path(args[args.index('--output-last-message')+1]).write_text('Validated scoped server phase.\n')
 print(json.dumps({'type':'turn.completed','usage':{'input_tokens':1,'output_tokens':1}}))
''')
    actor.chmod(0o700)
    config=root/'profiles.json'
    config.write_text(json.dumps({'profiles':[
        {'id':'claude','harness':'claude','command':str(actor),'python':sys.executable,'claude_home':str(account),'roots':[str(root)]},
        {'id':'codex','harness':'codex','command':str(actor),'codex_home':str(root/'codex'),'roots':[str(root)],
         'auth_env':{'OPENAI_API_KEY':'SERVER_NATIVE_KEY'}}]}))
    env=dict(bootstrap.env,CLAUDE_CONFIG_DIR=str(account),MONTY_CLAUDE_COMMAND=str(actor),
             MONTY_CODEX_COMMAND=str(actor),MONTY_WT_COMMAND=str(root/'fake-bin/wt'),SERVER_NATIVE_KEY='scoped-test-key')
    service=Service(root,config,env)
    context=root/'context.md';context.write_text('Implement the isolated fixture. No remote actions.\n')
    manifest=root/'.monty/runs/headless-check/jobs.json';manifest.parent.mkdir(parents=True)
    jobs=[{'id':name,'title':name,'branch':'cto/'+name,'repo':str(repo),'context':str(context),'agent_profile':profile}
          for name,profile in [('wait-a','reviewed'),('wait-b','reviewed'),('policy','solo'),('begin','solo')]]
    manifest.write_text(json.dumps({'jobs':jobs}))
    def cli(*args,env_override=None):
        result=subprocess.run([BIN,*args,'--home',str(root)],cwd=repo,env=dict(env,**(env_override or {})),
                              capture_output=True,text=True,timeout=60)
        assert result.returncode==0,(result.stdout,result.stderr)
        return json.loads(result.stdout)
    def phases():return [json.loads(line) for line in (root/'phases.jsonl').read_text().splitlines()]
    try:
        unsafe=manifest.parent/'unsafe.json';unsafe.write_text(json.dumps({'jobs':[{**jobs[0],'id':'unsafe','branch':'cto/unsafe'}]}))
        denied=subprocess.run([BIN,'headless','prepare-many','--manifest',str(unsafe),'--harness','claude',
          '--claude-command',str(actor)+" --mcp-config '{}'",'--home',str(root)],env=env,capture_output=True,text=True)
        assert denied.returncode and 'reviewer command cannot include' in denied.stderr,(denied.stdout,denied.stderr)
        assert not (manifest.parent/'workers/unsafe').exists(), 'unsafe reviewer command reserved a worker'
        cli('headless','prepare-many','--manifest',str(manifest),'--harness','claude','--claude-command',str(actor))
        # The accepted API policy must override both the server's ambient true
        # value and the persisted setting; no client environment grants bypass.
        subprocess.run([BIN,'settings','set','codex-yolo','true','--home',str(root)],env=env,check=True,capture_output=True)
        service.client.cmd('policy',{'op':'headless_start','profile':'codex','worker':'policy','codex_yolo':False})
        policy=next(row for row in phases() if row['worker']=='policy')
        assert policy['yolo']=='false' and policy['scoped_auth'] and '--dangerously-bypass-approvals-and-sandbox' not in policy['args'],policy
        subset=manifest.parent/'batch.json';subset.write_text(json.dumps({'jobs':jobs[:2]}))
        result=cli('headless','run-many','--wait','--manifest',str(subset),'--harness','claude',
                   '--claude-command',str(actor),env_override={'MONTY_CODEX_YOLO':'false'})
        assert result['success'],result
        # Public begin submits a server-owned process; historical dispatch
        # serialization is exercised separately by the legacy fixture.
        started=cli('headless','begin','begin','--harness','claude','--claude-command',str(actor))
        assert started['run_status']=='started' and 'harness_call' not in started,started
        finished=until(lambda:service.client.req('commands/'+started['command']),
                       lambda receipt:receipt['status'] in ('finished','failed'))
        assert finished['status']=='finished' and finished['result']['exit_code']==0,finished
        resumed=cli('headless','begin','--resume','begin','--harness','claude','--claude-command',str(actor))
        assert resumed['run_status']=='started' and resumed['attempt_id']!=started['attempt_id'],resumed
        until(lambda:service.client.req('commands/'+resumed['command']),lambda receipt:receipt['status']=='finished')
        rows=phases()
        assert len({row['pid'] for row in rows})==len(rows),rows
        assert all(service.process.pid in row['parents'] and row['private_nonce_cleared'] for row in rows),rows
        for job in jobs:
            snapshot=json.loads((manifest.parent/'workers'/job['id']/'agent-profile.json').read_text())
            profile=snapshot.get('headless',snapshot)
            expected=(1+len(profile.get('reviews',[]))+(profile.get('fix') is not None))*(2 if job['id']=='begin' else 1)
            assert len([row for row in rows if row['worker']==job['id']])==expected,(job,rows,snapshot)
        reviewers=[row for row in rows if '--tools' in row['args']]
        assert all(not row['scoped_auth'] for row in rows if row['worker']!='policy')
        assert reviewers and all(row['args'][row['args'].index('--tools')+1]=='Read,Grep,Glob' and
          '--restricted' in row['args'] and '--safe-mode' in row['args'] and
          '--strict-mcp-config' in row['args'] and '--setting-sources' in row['args'] for row in reviewers)
        inventory=service.client.req('inventory')['tasks']
        assert inventory and all(task['status']=='open' for task in inventory),inventory
        print('Headless: server ancestry, fresh phases, permission pinning, reviewer capabilities, concurrent wait batch and still-open tasks passed')
    finally:service.close()
