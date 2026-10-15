"""Native Claude SDK protocol verification against an isolated local fake model.
Usage: python3 test_claude_native.py BIN CLAUDE SDK_PYTHON
"""
import http.server
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
from test_client_api import BIN, Service, until
from test_tasks_api import fixture

class Model(http.server.BaseHTTPRequestHandler):
    calls=0
    review_tools=[]
    def log_message(self,*_):pass
    def do_GET(self):
        self.send_response(200);self.end_headers();self.wfile.write(b'{}')
    def do_POST(self):
        body=json.loads(self.rfile.read(int(self.headers['Content-Length'])))
        if 'count_tokens' in self.path:
            self.send_response(200);self.send_header('Content-Type','application/json');self.end_headers();self.wfile.write(b'{"input_tokens":10}');return
        type(self).calls+=1
        if 'Independently review Monty worker' in json.dumps(body):
            names={tool['name'] for tool in body.get('tools',[])}
            assert names and names <= {'Read','Grep','Glob'},names
            type(self).review_tools.append(names)
        assert self.headers.get('x-api-key')=='synthetic-test-key'
        self.send_response(200);self.send_header('Content-Type','text/event-stream');self.end_headers()
        events=[('message_start',{'type':'message_start','message':{'id':'msg_fixture','type':'message','role':'assistant','content':[],'model':body['model'],'stop_reason':None,'stop_sequence':None,'usage':{'input_tokens':10,'output_tokens':1}}}),
          ('content_block_start',{'type':'content_block_start','index':0,'content_block':{'type':'text','text':''}}),
          ('content_block_delta',{'type':'content_block_delta','index':0,'delta':{'type':'text_delta','text':'Local native protocol verified.'}}),
          ('content_block_stop',{'type':'content_block_stop','index':0}),
          ('message_delta',{'type':'message_delta','delta':{'stop_reason':'end_turn','stop_sequence':None},'usage':{'output_tokens':5}}),
          ('message_stop',{'type':'message_stop'})]
        for name,event in events:self.wfile.write(('event: '+name+'\ndata: '+json.dumps(event)+'\n\n').encode());self.wfile.flush()

with tempfile.TemporaryDirectory(prefix='monty-claude-native-') as directory:
    root=Path(directory).resolve();bootstrap,repo=fixture(root);bootstrap.close()
    account=root/'account';account.mkdir()
    model=http.server.ThreadingHTTPServer(('127.0.0.1',0),Model);threading.Thread(target=model.serve_forever,daemon=True).start()
    native=str(Path(sys.argv[2]).resolve())
    config=root/'profiles.json';config.write_text(json.dumps({'profiles':[{'id':'claude','harness':'claude','command':native,'python':str(Path(sys.argv[3]).absolute()),'claude_home':str(account),'roots':[str(root)],
      'auth_env':{'ANTHROPIC_API_KEY':'TEST_CLAUDE_KEY','ANTHROPIC_BASE_URL':'TEST_CLAUDE_BASE','CLAUDE_CODE_DISABLE_NONESSENTIAL_TRAFFIC':'TEST_CLAUDE_TRAFFIC'}}]}))
    service=None
    try:
        service=Service(root,config,{**bootstrap.env,'HOME':str(root),'CLAUDE_CONFIG_DIR':str(account),
          'MONTY_WT_COMMAND':str(root/'fake-bin/wt'),'TEST_CLAUDE_KEY':'synthetic-test-key',
          'TEST_CLAUDE_BASE':'http://127.0.0.1:'+str(model.server_port),'TEST_CLAUDE_TRAFFIC':'1'})
        c=service.client;chat=c.cmd('start',{'op':'start','profile':'claude','context':{'workspaces':[str(repo)],'instructions':'Reply with the fake endpoint text. Do not use tools.'}})
        cid=chat['id'];sess=chat['sessions'][0]
        c.cmd('send',{'op':'send','conversation':cid,'selected_session':sess['id'],'text':'Reply briefly.'})
        observed=until(lambda:c.req('conversations/'+cid)['sessions'][0],lambda s:s.get('runtime') in ['idle','outcome_unknown','disconnected'])
        assert observed['runtime']=='idle' and observed.get('runtime_error') is None,observed
        assert Model.calls>0
        def messages():return [x for s in c.history(cid)['sessions'] for b in s['records'] for x in b['items']]
        until(messages,lambda xs:any('Local native protocol verified.' in json.dumps(x) for x in xs))
        context=root/'context.md';context.write_text('Reply with the local fixture text. No tools or edits needed.\n')
        manifest=root/'.monty/runs/native-review/jobs.json';manifest.parent.mkdir(parents=True)
        manifest.write_text(json.dumps({'jobs':[{'id':'native-review','title':'Native review','repo':str(repo),
          'branch':'cto/native-review','context':str(context),'agent_profile':'reviewed'}]}))
        for args in [('prepare-many','--manifest',str(manifest)),('run','--wait','native-review')]:
            result=subprocess.run([BIN,'headless',*args,'--harness','claude','--claude-command',native,
              '--home',str(root)],cwd=repo,env=service.env,capture_output=True,text=True,timeout=90)
            progress={str(path.relative_to(root)):path.read_text()[-6000:] for path in
              root.glob('.monty/runs/native-review/workers/native-review/artifacts/headless/*/logs/*') if path.is_file()}
            assert result.returncode==0,(result.stdout,result.stderr,progress)
        assert len(Model.review_tools)>=2,Model.review_tools
        print('Native Claude SDK and CLI: private server connection, local SSE, exact UUID, retained history and restricted reviewer tool schemas passed')
    finally:
        if service:service.close()
        model.shutdown();model.server_close()
