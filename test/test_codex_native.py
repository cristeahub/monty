"""Native Codex headless account/model policy against a local Responses fixture.
Usage: python3 test_codex_native.py BIN CODEX
"""
import http.server
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import threading

from test_client_api import BIN, Model, Service
from test_tasks_api import fixture


class ScopedModel(Model):
    requests=[]
    def do_POST(self):
        body=self.rfile.read(int(self.headers['Content-Length']))
        type(self).requests.append(json.loads(body))
        if getattr(self.server,'question',False):
            self.server.question=False
            args=json.dumps({'questions':[{'id':'q','header':'Input','question':'Choose one.',
              'options':[{'label':'A','description':'First'},{'label':'B','description':'Second'}]}]})
            item={'type':'function_call','id':'fc-1','call_id':'call-1','name':'request_user_input','arguments':args}
            self.send_response(200);self.send_header('Content-Type','text/event-stream');self.end_headers()
            events=[{'type':'response.created','response':{'id':'resp-question','status':'in_progress','output':[]}},
              {'type':'response.output_item.added','output_index':0,'item':{**item,'arguments':''}},
              {'type':'response.function_call_arguments.delta','output_index':0,'item_id':'fc-1','delta':args},
              {'type':'response.output_item.done','output_index':0,'item':item},
              {'type':'response.completed','response':{'id':'resp-question','status':'completed','output':[item],
               'usage':{'input_tokens':1,'output_tokens':1,'total_tokens':2}}}]
            for event in events:
                self.wfile.write(('event: '+event['type']+'\ndata: '+json.dumps(event)+'\n\n').encode());self.wfile.flush()
            return
        # Reuse the existing native Responses protocol fixture.
        import io
        self.rfile=io.BytesIO(body)
        super().do_POST()


with tempfile.TemporaryDirectory(prefix='monty-codex-native-') as directory:
    root=Path(directory).resolve();bootstrap,repo=fixture(root);bootstrap.close()
    model=http.server.ThreadingHTTPServer(('127.0.0.1',0),ScopedModel)
    model.started=threading.Event();model.release=threading.Event();model.release.set()
    threading.Thread(target=model.serve_forever,daemon=True).start()
    account=root/'codex'
    (account/'config.toml').write_text('model="monty-fixture"\nmodel_provider="fixture"\n'
      '[model_providers.fixture]\nname="Fixture"\nbase_url="http://127.0.0.1:'+str(model.server_port)+
      '/v1"\nwire_api="responses"\nrequires_openai_auth=false\n')
    native=str(Path(sys.argv[2]).absolute())
    config=root/'profiles.json';config.write_text(json.dumps({'profiles':[{'id':'codex','harness':'codex',
      'command':native,'codex_home':str(account),'roots':[str(root)]}]}))
    service=Service(root,config,{**bootstrap.env,'HOME':str(root),'MONTY_WT_COMMAND':str(root/'fake-bin/wt'),
      'MONTY_CODEX_YOLO':'false'})
    try:
        context=root/'context.md';context.write_text('Reply briefly with the local fixture text. No edits or tools needed.\n')
        manifest=root/'.monty/runs/native-review/jobs.json';manifest.parent.mkdir(parents=True)
        manifest.write_text(json.dumps({'jobs':[{'id':'native-review','title':'Native review','repo':str(repo),
          'branch':'cto/native-review','context':str(context),'agent_profile':'reviewed'}]}))
        for args in [('prepare-many','--manifest',str(manifest)),('run','--wait','native-review')]:
            result=subprocess.run([BIN,'headless',*args,'--harness','codex','--codex-command',native,
              '--home',str(root)],cwd=repo,env={**service.env,'MONTY_CODEX_YOLO':'false'},capture_output=True,text=True,timeout=90)
            progress={str(path.relative_to(root)):path.read_text()[-6000:] for path in
              root.glob('.monty/runs/native-review/workers/native-review/artifacts/headless/*/logs/*') if path.is_file()}
            assert result.returncode==0,(result.stdout,result.stderr,progress)
        assert ScopedModel.requests
        assert all(request['model']=='monty-fixture' for request in ScopedModel.requests), [request['model'] for request in ScopedModel.requests]
        model.reply='[]';model.question=True
        classified=service.client.cmd('classify',{'op':'classify','profile':'codex','text':'Return only an empty JSON array.','seconds':30})
        assert classified==[],classified
        assert ScopedModel.requests[-1]['model']=='monty-fixture'
        # Codex advertises this Plan-only tool even in Default mode. Verify
        # the runtime denies it instead of soliciting input or hanging.
        assert {tool.get('name') for tool in ScopedModel.requests[-1].get('tools',[])} <= {'request_user_input'}
        outputs=[item.get('output','') for request in ScopedModel.requests for item in request.get('input',[])
                 if item.get('type')=='function_call_output']
        assert any('unavailable in Default mode' in output for output in outputs),outputs
        print('Native Codex: server-owned headless writers/reviewers and tools-disabled classifier preserve scoped provider/model')
    finally:
        service.close();model.shutdown();model.server_close()
