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
        print('Native Codex: server-owned headless writers/reviewers preserve scoped provider/model')
    finally:
        service.close();model.shutdown();model.server_close()
