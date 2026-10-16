"""SDK contract fixture. Its log exposes OS ownership, never reads real accounts."""
import asyncio
from dataclasses import dataclass
import json
import os
from pathlib import Path
import time
import uuid
from types import SimpleNamespace
from .types import PermissionResultAllow

@dataclass
class ResultMessage:
    subtype: str
    is_error: bool
    session_id: str
    result: str
@dataclass
class SystemMessage:
    subtype: str
    data: dict
@dataclass
class AssistantMessage:
    content: list
    model: str
    uuid: str

class ClaudeAgentOptions:
    def __init__(self, **options):self.__dict__.update(options)

def home():return Path(os.environ['CLAUDE_CONFIG_DIR'])
def log(value):
    with (home()/'calls.jsonl').open('a') as output:output.write(json.dumps(value)+'\n')
def path(native):return home()/'sessions'/(native+'.json')
def get_session_info(native):
    p=path(native)
    return SimpleNamespace(cwd=json.loads(p.read_text())['cwd']) if p.exists() else None
@dataclass
class SDKSessionInfo:
    session_id: str
    summary: str
    cwd: str
    last_modified: int
    first_prompt: str
@dataclass
class SessionMessage:
    type: str
    uuid: str
    message: dict
    parent_tool_use_id: object=None

def get_session_messages(native):
    pause=home()/'pause-history'
    if pause.exists():
        pause.unlink()
        (home()/'history-started').touch()
        deadline=time.monotonic()+8
        while not (home()/'release-history').exists():
            if time.monotonic()>deadline:raise TimeoutError('history gate was not released')
            time.sleep(.01)
    log({'history':native})
    p=path(native)
    return [SessionMessage(**x) for x in json.loads(p.read_text())['entries']] if p.exists() else []
def list_sessions(limit=25,offset=0):
    sessions=[]
    for p in sorted((home()/'sessions').glob('*.json')):
        raw=json.loads(p.read_text());sessions.append(SDKSessionInfo(p.stem,'Claude fixture',raw['cwd'],1,'fixture'))
    return sessions[offset:offset+limit]

class ClaudeSDKClient:
    def __init__(self,options):self.options=options;self.text='';self.stopped=asyncio.Event()
    async def connect(self):
        self.native=self.options.resume or self.options.session_id
        p=path(self.native)
        if self.options.resume and not p.exists():raise ValueError('absent session')
        p.parent.mkdir(exist_ok=True)
        if not p.exists():p.write_text(json.dumps({'cwd':self.options.cwd,'entries':[]}))
        log({'start':self.native,'pid':os.getpid(),'ppid':os.getppid(),'cwd':self.options.cwd,'add_dirs':self.options.add_dirs,'instructions':self.options.system_prompt['append']})
    def append(self,role,content):
        p=path(self.native);raw=json.loads(p.read_text());entry={'type':role,'uuid':str(uuid.uuid4()),'message':{'role':role,'content':content}}
        raw['entries'].append(entry);p.write_text(json.dumps(raw));return entry
    async def query(self,text,session_id=None):
        assert session_id==self.native
        self.text=text;self.stopped.clear();self.append('user',[{'type':'text','text':text}]);log({'prompt':text,'session':self.native})
    async def receive_response(self):
        yield SystemMessage('init',{'session_id':self.native})
        if self.text in ('approval','question'):
            tool='Bash' if self.text=='approval' else 'AskUserQuestion'
            value={'command':'fixture command'} if tool=='Bash' else {'questions':[{'question':'Choose a color','header':'Color','options':[{'label':'Blue','description':'blue'}]}]}
            result=await self.options.can_use_tool(tool,value,None);log({'response':result.__dict__})
        if self.text=='hold':await self.stopped.wait()
        if self.text=='disconnect':os._exit(7)
        entry=self.append('assistant',[{'type':'text','text':'Claude fixture answer'}])
        yield AssistantMessage(entry['message']['content'],'fixture',entry['uuid'])
        if self.text!='incomplete':yield ResultMessage('success',False,self.native,'Claude fixture answer')
    async def interrupt(self):self.stopped.set();log({'interrupt':self.native})
    async def disconnect(self):log({'close':self.native})
