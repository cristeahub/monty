#!/usr/bin/env python3
"""Checkout-binary E2E: no user services, accounts, credentials or model calls."""
import json
import os
import fcntl
from pathlib import Path
import shutil
import shlex
import subprocess
import sys
import tempfile
import time

BINARY = str(Path(sys.argv[1]).resolve())
HELPER = Path(sys.argv[2]).resolve()

SLACK = r'''#!/usr/bin/env python3
import os,json,sys
from pathlib import Path
case=json.loads(Path(os.environ['CASE']).read_text())
args=sys.argv[1:]
assert '--token' not in args and '--app' not in args
assert 'fake-secret' not in ' '.join(args)
assert os.environ.get('SLACK_BOT_TOKEN') == 'fake-secret'
assert '--skip-update' in args and '--no-color' in args
if '--help' in args:
 print('--include --json');sys.exit()
method=args[1]; params=json.loads(args[args.index('--json')+1])
with open(os.environ['CALLS'],'a') as f:f.write(method+' '+json.dumps(params)+'\n')
if case.get('sleep') and method=='conversations.history':
 Path(os.environ['STARTED']).touch(); import time;time.sleep(60)
if method=='auth.test': data=dict(ok=True,team_id='T1',user_id='U1',url='https://test.slack.com/')
elif case.get('hidden_rate'):
 config=Path(args[args.index('--config-dir')+1]);(config/'logs').mkdir()
 (config/'logs/debug.log').write_text('/api/conversations.history responded with status 429. Retrying request in 10m0s...')
 import time;time.sleep(60)
elif case.get('rate'):
 print('HTTP 429\nRetry-After: 600\n\n'+json.dumps(dict(ok=False,error='ratelimited')));sys.exit(1)
elif case.get('api_error'): data=dict(ok=False,error='missing_scope')
elif method=='conversations.info': data=dict(ok=True,channel=dict(id=params['channel']))
elif method=='conversations.list':
 data=dict(ok=True,channels=[dict(id='D1')],response_metadata=dict(next_cursor=''))
elif method in ('conversations.history','conversations.replies'):
 root=case['root']; reply=case['reply']; content=case.get('text','<@U1> please review release')
 if params['channel']=='D1': messages=[]
 elif method=='conversations.history': messages=[] if case.get('deleted') else [dict(ts=root,user='U2',text=content)]
 else:
  messages=[] if case.get('deleted') else [dict(ts=root,user='U2',text=content)]
  if case.get('new_reply'): messages.append(dict(ts=reply,thread_ts=root,user='U3',text='Please approve the old thread'))
 data=dict(ok=True,messages=messages,response_metadata=dict(next_cursor=''))
else: raise AssertionError(method)
print('HTTP 200\nContent-Type: application/json\n\n'+json.dumps(data))
'''

ZULIP = r'''
import json,os
from pathlib import Path
class Client:
 def __init__(self,config_file,retry_on_errors=False,insecure=False):
  assert retry_on_errors is False and insecure is False
  self.base_url='https://zulip.test/api/'
 def get_profile(self): return dict(result='success',user_id=7)
 def get_subscriptions(self): return dict(result='success',subscriptions=[dict(stream_id=10)])
 def get_messages(self,request):
  case=json.loads(Path(os.environ['CASE']).read_text())
  if case.get('zulip_error'): return dict(result='error',code='UNAUTHORIZED')
  if request['narrow'][0]['operator']=='is': messages=[]
  else: messages=[dict(id=42,type='stream',stream_id=10,subject='release',timestamp=float(case['root']),sender_id=8,content=case.get('zulip_text','@user please review'))]
  return dict(result='success',messages=messages,found_oldest=True)
'''

MODEL = r'''#!/usr/bin/env python3
import json,os,sys
from pathlib import Path
args=sys.argv[1:]
assert '--dangerously-bypass-approvals-and-sandbox' not in args
if 'exec' in args:
 assert '--ignore-user-config' in args and '--ignore-rules' in args
 assert args[args.index('--sandbox')+1]=='read-only'
 assert 'features.shell_tool=false' in args and 'features.apps=false' in args and 'mcp_servers={}' in args
else:
 assert all(a in args for a in ['--no-tools','--no-extensions','--no-skills','--no-context-files','--no-session'])
prompt=sys.stdin.read(); data=json.loads(prompt.split('\n',1)[1])
with open(os.environ['MODEL_CALLS'],'a') as f: f.write(json.dumps(data)+'\n')
case=json.loads(Path(os.environ['CASE']).read_text())
if case.get('model_error'): sys.exit(3)
if case.get('model_bad'): print('{"findings":[{"evidence":["invented"]}]}');sys.exit()
seen=set();findings=[]
for o in data['observations']:
 if o['conversation'] in seen: continue
 seen.add(o['conversation'])
 findings.append(dict(conversation=o['conversation'],title='Review requested',reason='Direct request for your review',next_step='Read the source and decide',uncertainty='Only retained messages were considered',evidence=[o['id']]))
print(json.dumps(dict(findings=findings)))
'''

LAUNCHCTL = r'''#!/usr/bin/env python3
import os,sys
from pathlib import Path
state=Path(os.environ['SERVICE'])
args=sys.argv[1:]
if os.environ.get('LAUNCH_FAIL')==args[0]: sys.exit(1)
if args[0]=='print':
 if state.exists(): print('state = waiting');sys.exit()
 print('Could not find service',file=sys.stderr);sys.exit(113)
if args[0]=='bootstrap': state.touch()
if args[0]=='bootout': state.unlink(missing_ok=True)
'''


def write(path, value, executable=False):
    path.write_text(value)
    path.chmod(0o700 if executable else 0o600)


def run_tests(root):
    home = root / 'home'; home.mkdir()
    (home / 'helpers').mkdir(); shutil.copy(HELPER, home / 'helpers/context_source.py')
    tools = root / 'tools'; tools.mkdir()
    write(tools / 'slack', SLACK, True); write(tools / 'model', MODEL, True)
    write(tools / 'launchctl', LAUNCHCTL, True); write(tools / 'zulip.py', ZULIP)
    write(root / 'token', 'fake-secret'); write(root / 'zuliprc', '[api]\nkey=fake\n')
    case = dict(root=f'{time.time()-100:.6f}', reply=f'{time.time()-20:.6f}')
    env = dict(os.environ, HOME=str(home), MONTY_HOME=str(home), MONTY_HARNESS='pi',
               PI_CODING_AGENT_DIR=str(home/'.pi/agent'), CODEX_HOME=str(home/'.codex'), MONTY_PI_COMMAND=str(tools / 'model'), MONTY_CODEX_COMMAND=str(tools / 'model'),
               MONTY_LAUNCHCTL_COMMAND=str(tools / 'launchctl'),
               MONTY_CONTEXT_LAUNCH_AGENTS=str(root / 'agents'),
               PYTHONPATH=str(tools), CASE=str(root / 'case.json'), CALLS=str(root / 'calls'),
               MODEL_CALLS=str(root / 'models'), SERVICE=str(root / 'service'), STARTED=str(root / 'started'))
    for key in ['MONTY_FAULT_INJECT','SLACK_BOT_TOKEN','SLACK_USER_TOKEN']:
        env.pop(key,None)
    def scenario(**updates):
        case.update(updates); write(root / 'case.json',json.dumps(case))
    scenario()
    def cli(*args, ok=True, extra=None):
        p = subprocess.run([BINARY,*args],env=dict(env,**(extra or {})),capture_output=True,text=True,timeout=25)
        assert (p.returncode==0)==ok, (args,p.returncode,p.stdout,p.stderr)
        return p.stdout if ok else p.stderr
    def j(*args,**kwargs): return json.loads(cli(*args,**kwargs))
    def state(): return json.loads((home / '.monty/context/state.json').read_text())
    def due():
        # Public reconfiguration requeues observations but preserves rate-limit
        # deadlines. Advance only this fixture's due time instead of sleeping.
        for file in config_files: cli('context','configure',str(file))
        with open(home/'.monty/state.lock','r+') as lock:
            fcntl.lockf(lock,fcntl.LOCK_EX)
            fixture=state()
            for c in fixture['connections']:c['next_due']=0
            temporary=home/'.monty/context/test-state.tmp'
            write(temporary,json.dumps(fixture))
            os.replace(temporary,home/'.monty/context/state.json')
    def source(id): return next(c for c in state()['connections'] if c['id']==id)
    def tick(**kwargs): return cli('context','tick',**kwargs)
    # Reads neither create state nor contact providers or the model.
    assert j('context','status')['enabled'] is False
    assert j('findings','--json')['findings']==[]
    cli('context','tick')
    assert not (home / '.monty').exists()
    config_files=[]
    for id,source_name,account,channels,credential in [
        ('slack-work','slack','T1',['C1'],'token'),
        ('zulip-work','zulip','https://zulip.test',['10'],'zuliprc')]:
        config=dict(id=id,source=source_name,account=account,channels=channels,include_dms=True,
                    credential_file=str(root/credential),python=sys.executable,slack_cli=str(tools/'slack'),
                    max_pages=5,timeout_seconds=5)
        path=root/(id+'.json');write(path,json.dumps(config));config_files.append(path)
        j('context','configure',str(path));j('context','check',id)
    assert not (root/'models').exists()
    cli('context','on');cli('context','on')
    assert j('context','status')['service']=='loaded'
    plist=next((root/'agents').glob('*.plist')).read_text()
    assert 'StartCalendarInterval' in plist and 'StartInterval' not in plist and 'fake-secret' not in plist
    for directory in [home/'.pi/agent',home/'.codex']:directory.mkdir(parents=True,exist_ok=True)
    wrapper=tools/'server-model'
    wrapper.write_text('#!/bin/sh\n'+ ' '.join([key+'='+shlex.quote(env[key]) for key in ['CASE','MODEL_CALLS']])+ ' exec '+shlex.quote(str(tools/'model'))+' "$@"\n')
    wrapper.chmod(0o700)
    profiles=root/'server-profiles.json'
    profiles.write_text(json.dumps({'profiles':[{'id':name,'harness':name,'command':str(wrapper),key:str(directory),'roots':[str(root)]}
        for name,key,directory in [('pi','pi_home',home/'.pi/agent'),('codex','codex_home',home/'.codex')]]}))
    cli('service','start','--config',str(profiles))
    tick()
    snapshot=j('findings','--json');assert len(snapshot['findings'])==2,snapshot
    assert all(c['backlog']==0 for c in j('context','status')['connections'])
    # Repeated enable preserves generations and scheduling, including backoff.
    before=(home/'.monty/context/state.json').read_bytes()
    cli('context','on')
    assert before==(home/'.monty/context/state.json').read_bytes()
    slack=next(f for f in snapshot['findings'] if f['source']=='slack')
    reference=slack['reference']
    before=(home/'.monty/context/state.json').read_bytes()
    assert len(j('findings','--new','--json')['findings'])==2
    assert before==(home/'.monty/context/state.json').read_bytes()
    cli('findings','acknowledge',reference)
    assert j('findings','--source','slack','--new','--json')['findings']==[]
    assert len(j('findings','--source','zulip','--new','--json')['findings'])==1
    assert j('findings','show',reference)['finding']['resolved'] is False
    cli('findings','resolve',reference)
    page=j('findings','--limit','1','--json');assert page['next_cursor']
    assert len(j('findings','--before',page['next_cursor'],'--json')['findings'])==1
    model_count=len((root/'models').read_text().splitlines());tick()
    assert len((root/'models').read_text().splitlines())==model_count
    # New reply to an existing old root -> stable finding ID, new revision/reopened.
    scenario(new_reply=True);due();tick()
    updated=j('findings','--source','slack','--new','--json')['findings'][0]
    assert updated['id']==slack['id'] and updated['revision']>slack['revision'] and not updated['resolved']
    cli('findings','acknowledge',reference,ok=False)
    assert len(j('findings','--source','slack','--new','--json')['findings'])==1
    # Model errors preserve pending observations independently of collection.
    scenario(model_error=True,text='changed message');due();tick()
    assert source('slack-work')['analysis_error']
    assert any(o['analyzed']!=o['revision'] for o in state()['observations'])
    pending_before=[o for o in state()['observations'] if o['connection']=='slack-work' and o['analyzed']!=o['revision']]
    scenario(text='another edit while model is unavailable');due();tick()
    assert len([o for o in state()['observations'] if o['connection']=='slack-work' and o['analyzed']!=o['revision']])>len(pending_before)
    scenario(model_error=False,model_bad=True);due();tick()
    assert source('slack-work')['analysis_error']
    scenario(model_bad=False);due();tick(extra={'MONTY_HARNESS':'codex'})
    assert source('slack-work')['analysis_error'] is None
    # Rate-limit delay and collection freshness; healthy Zulip still progresses.
    previous=source('slack-work')['last_collection']
    scenario(rate=True,zulip_text='a healthy source update');due();tick()
    assert source('slack-work')['next_due']>time.time()+500
    assert source('slack-work')['last_collection']==previous
    assert source('slack-work')['collection_error']=='Slack rate limited'
    assert source('zulip-work')['collection_error'] is None
    before=(home/'.monty/context/state.json').read_bytes()
    cli('context','on')
    assert before==(home/'.monty/context/state.json').read_bytes()
    # Stop/start and reconfiguration must also preserve a provider deadline.
    deadline=source('slack-work')['next_due']
    cli('context','off');cli('context','on')
    cli('context','configure',str(config_files[0]))
    cli('context','retriage','slack-work')
    assert source('slack-work')['next_due']==deadline
    scenario(rate=False,hidden_rate=True);due();tick()
    assert source('slack-work')['collection_error']=='Slack rate limited'
    assert source('slack-work')['next_due']>time.time()+590
    scenario(hidden_rate=False,api_error=True);due();tick()
    assert source('slack-work')['collection_error']=='Slack API: missing_scope'
    # Atomic-write failure does not move a checkpoint or acknowledge findings.
    before=(home/'.monty/context/state.json').read_bytes()
    cli('findings','acknowledge',updated['reference'],ok=False,extra={'MONTY_FAULT_INJECT':'state-store-before-rename'})
    assert before==(home/'.monty/context/state.json').read_bytes()
    # Crash boundary after network reads: no checkpoint or observation advances.
    scenario(api_error=False,text='retry this ingestion');due()
    checkpoint=source('slack-work')['checkpoint'];observations=state()['observations']
    tick(ok=False,extra={'MONTY_FAULT_INJECT':'context-before-ingest'})
    assert source('slack-work')['checkpoint']==checkpoint
    assert len(state()['observations'])==len(observations)
    due();tick()
    assert any(o['content']=='retry this ingestion' for o in state()['observations'])
    # Cancellation stops an active manual tick and prevents stale writes.
    scenario(api_error=False,sleep=True);due()
    p=subprocess.Popen([BINARY,'context','tick'],env=env,stdout=subprocess.PIPE,stderr=subprocess.PIPE)
    try:
        deadline=time.time()+10
        while not (root/'started').exists() and time.time()<deadline:time.sleep(.05)
        assert (root/'started').exists()
        # An overlapping tick must not start a second collection for this source.
        history_calls=(root/'calls').read_text().count('conversations.history')
        tick(ok=False)
        assert (root/'calls').read_text().count('conversations.history')==history_calls
        assert p.poll() is None
        cli('context','off');p.communicate(timeout=10)
    finally:
        if p.poll() is None:p.kill();p.wait()
    assert j('context','status')['enabled'] is False
    cli('context','off')
    # Failed enable/disable leaves intent visible, and exact retry reconciles.
    cli('context','on',ok=False,extra={'LAUNCH_FAIL':'bootstrap'})
    assert j('context','status')['enabled'] is True
    cli('context','on')
    old_plist=next((root/'agents').glob('*.plist')).read_bytes()
    cli('context','on',ok=False,extra={'LAUNCH_FAIL':'bootout','MONTY_HARNESS':'codex'})
    assert next((root/'agents').glob('*.plist')).read_bytes()==old_plist
    cli('context','on',extra={'MONTY_HARNESS':'codex'})
    assert next((root/'agents').glob('*.plist')).read_bytes()!=old_plist
    cli('context','off',ok=False,extra={'LAUNCH_FAIL':'bootout'})
    status=j('context','status');assert status['enabled'] is False and status['service']=='loaded'
    cli('context','off')
    # Setup validation and path safety fail closed.
    bad=dict(config);bad.pop('channels');write(root/'bad.json',json.dumps(bad))
    cli('context','configure',str(root/'bad.json'),ok=False)
    state_path=home/'.monty/context/state.json'; saved=state_path.read_bytes()
    state_path.unlink();state_path.symlink_to(root/'case.json')
    cli('findings','--json',ok=False)
    state_path.unlink();state_path.write_bytes(saved)
    assert 'fake-secret' not in state_path.read_text()
    assert 'fake-secret' not in (root/'models').read_text()
    # Task conversion is explicit, using a real isolated repo/project registry.
    repo=root/'repo';repo.mkdir();subprocess.run(['git','init','-q',str(repo)],check=True)
    cli('projects','add','--repo',str(repo))
    current=j('findings','--json')['findings'][0]
    cli('findings','to-task',current['reference'],'--project','repo')
    assert 'context' in cli('tasks','list','--no-sync')
    assert any('evidence snapshot' in path.read_text() for path in (home/'.monty').rglob('*.json') if 'context' not in str(path.relative_to(home/'.monty')))
    cli('service','stop')
    print('context E2E passed: disabled default, both providers, triage, revisions, pagination, failure recovery, cancellation, lifecycle and task conversion')

with tempfile.TemporaryDirectory(prefix='monty-context-e2e-') as directory:
    run_tests(Path(directory).resolve())
