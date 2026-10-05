#!/usr/bin/env python3
"""Isolated native Pi protocol fixture; never reads a real account."""
import json
import os
from pathlib import Path
import sys

if '--version' in sys.argv:
    print('1.0.2')
    sys.exit(0)

def option(name, default=None):
    return sys.argv[sys.argv.index(name)+1] if name in sys.argv else default

home=Path(os.environ['PI_CODING_AGENT_DIR'])
if option('--session'):
    path=Path(option('--session'))
    records=[json.loads(line) for line in path.read_text().splitlines()]
    header,entries=records[0],records[1:]
else:
    native=option('--session-id')
    path=Path(option('--session-dir'))/(native+'.jsonl')
    header={'type':'session','version':3,'id':native,'cwd':os.getcwd()}
    entries=[]
model={'provider':option('--provider','fixture'),'id':option('--model','first')}
reported_path=path.with_name('redirected.jsonl') if option('--session') and os.environ.get('MONTY_FIXTURE_REBIND') else path
busy=False
queued=[]
dialog=None

def log(value):
    with (home/'calls.jsonl').open('a') as out:
        out.write(json.dumps(value)+'\n')

log({'start':header['id'],'pid':os.getpid(),'argv':sys.argv[1:],'cwd':os.getcwd(),
     'environment':{k:v for k,v in os.environ.items() if k.startswith('MONTY_')}})

def emit(value):
    print(json.dumps(value),flush=True)

def respond(command, data=None):
    emit({'id':command['id'],'type':'response','command':command['type'],'success':True,'data':data or {}})

def append(kind, **data):
    entry={'type':kind,'id':'entry-'+str(len(entries)+1),'parentId':entries[-1]['id'] if entries else None,**data}
    entries.append(entry)
    path.parent.mkdir(parents=True,exist_ok=True)
    if not path.exists():path.write_text(json.dumps(header)+'\n')
    with path.open('a') as out:out.write(json.dumps(entry)+'\n')
    return entry

def finish():
    global busy
    entry=append('message',message={'role':'assistant','content':[{'type':'text','text':'Pi fixture answer'}],
                                 'provider':model['provider'],'model':model['id']})
    emit({'type':'message_end','message':entry['message']})
    busy=False
    emit({'type':'agent_end','messages':[]})
    emit({'type':'agent_settled'})

for line in sys.stdin:
    command=json.loads(line)
    log({'session':header['id'],'command':command})
    kind=command['type']
    if kind=='get_state':
        respond(command,{'sessionId':header['id'],'sessionFile':str(reported_path),'model':model,
                         'isStreaming':busy,'isCompacting':False,'pendingMessageCount':len(queued)})
    elif kind=='get_entries':
        if os.environ.get('MONTY_FIXTURE_UNFLUSHED'):
            # A fresh in-memory setup record makes the final snapshot observable.
            entries.append({'type':'custom','id':'setup-'+str(len(entries)+1),
                            'parentId':entries[-1]['id'] if entries else None,'data':'unflushed setup'})
        respond(command,{'entries':entries,'leafId':entries[-1]['id'] if entries else None})
    elif kind in ('steer','follow_up'):
        queued.append(command['message'])
        respond(command,{'disposition':'queued'})
    elif kind=='prompt':
        busy=True
        emit({'type':'agent_start'})
        append('message',message={'role':'user','content':[{'type':'text','text':command['message']}]})
        if command['message']=='lost response':sys.exit(7)
        if command['message'] in ('dialog','expiring dialog'):
            dialog=command
            emit({'type':'extension_ui_request','id':'native-dialog','method':'confirm',
                  'title':'Confirm fixture action','timeout':100 if command['message']=='expiring dialog' else 10000})
        else:
            respond(command,{'disposition':'started'})
            if command['message']!='hold':finish()
    elif kind=='extension_ui_response':
        assert command['id']=='native-dialog'
        respond(dialog,{'disposition':'started'})
        dialog=None
        finish()
    elif kind=='clear_queue':
        respond(command,{'steering':queued,'followUp':[]})
        queued=[]
    elif kind=='abort':
        busy=False
        if dialog:
            respond(dialog,{'disposition':'handled'})
            dialog=None
        emit({'type':'agent_settled'})
        respond(command)
    elif kind=='set_model':
        model={'provider':command['provider'],'id':command['modelId']}
        append('model_change',provider=model['provider'],modelId=model['id'])
        respond(command,model)
    else:
        emit({'id':command['id'],'type':'response','command':kind,'success':False,'error':'unsupported fixture command'})
log({'stopped':header['id'],'pid':os.getpid()})
