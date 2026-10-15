"""Private native app-server fixture relaying to the test's fake model endpoint."""
import json
import os
from pathlib import Path
import select
import socket
import subprocess
import sys
import threading


def proxy(source, destination):
    listener=socket.socket(socket.AF_UNIX)
    listener.bind(destination)
    os.chmod(destination,0o600)
    listener.listen(16)
    def connection(client):
        native=socket.socket(socket.AF_UNIX)
        try:
            native.connect(source)
            while True:
                ready,_,_=select.select([client,native],[],[],1)
                for sock in ready:
                    data=sock.recv(65536)
                    if not data:return
                    (native if sock is client else client).sendall(data)
        finally:
            client.close();native.close()
    while True:
        client,_=listener.accept()
        threading.Thread(target=connection,args=(client,),daemon=True).start()


def install(path):
    path=Path(path)
    text=path.read_text()
    if '# owned-native-fixture' in text:return
    original=path.with_name(path.name+'.original')
    original.write_text(text);original.chmod(0o700)
    helper=Path(__file__).resolve()
    path.write_text('#!/usr/bin/env python3\n# owned-native-fixture\n'
      +'import json,os,subprocess,sys\n'
      +'sys.path.insert(0,'+repr(str(helper.parent))+')\n'
      +'from owned_fake_appserver import proxy\n'
      +'if sys.argv[1:3]==["app-server","--listen"]:\n'
      +' source=json.loads(subprocess.check_output(['+repr(str(original))+',"app-server","daemon","start"],text=True))["socketPath"]\n'
      +' proxy(source,sys.argv[3].removeprefix("unix://") if hasattr(str,"removeprefix") else sys.argv[3][7:])\n'
      +'else:os.execv('+repr(str(original))+',['+repr(str(original))+']+sys.argv[1:])\n')
    path.chmod(0o700)
