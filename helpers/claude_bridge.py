"""Monty-owned SDK adapter. Private JSON lines, no listener or durable storage."""
import asyncio
import dataclasses
import importlib.metadata
import json
import sys
import uuid

from claude_agent_sdk import (ClaudeAgentOptions, ClaudeSDKClient, ResultMessage,
                              get_session_messages, get_session_info, list_sessions)
from claude_agent_sdk.types import PermissionResultAllow, PermissionResultDeny

VERSION = '0.2.166'


def emit(value):
    print(json.dumps(value, separators=(',', ':')), flush=True)


def serial(value):
    if dataclasses.is_dataclass(value):
        return dataclasses.asdict(value)
    return value


async def main():
    if importlib.metadata.version('claude-agent-sdk') != VERSION:
        raise RuntimeError('Monty requires claude-agent-sdk==' + VERSION)
    client = None
    running = None
    session_id = None
    pending = {}

    async def permission(tool, input_data, _context):
        request_id = str(uuid.uuid4())
        result = asyncio.get_running_loop().create_future()
        pending[request_id] = result
        emit({'type': 'permission_request', 'id': request_id, 'tool': tool, 'input': input_data})
        try:
            response = await result
            if response['decision'] == 'allow':
                updated = dict(input_data)
                if tool == 'AskUserQuestion':
                    updated['answers'] = response['answers']
                return PermissionResultAllow(updated_input=updated)
            return PermissionResultDeny(message='User declined')
        finally:
            pending.pop(request_id, None)
            emit({'type': 'request_resolved', 'id': request_id})

    async def receive():
        terminal = False
        emit({'type': 'agent_start'})
        try:
            async for message in client.receive_response():
                value = serial(message)
                kind = type(message).__name__
                if kind == 'SystemMessage' and message.subtype == 'init':
                    if message.data['session_id'] != session_id:
                        raise RuntimeError('Claude changed native session identity')
                emit({'type': 'sdk_message', 'kind': kind, 'message': value})
                if isinstance(message, ResultMessage):
                    terminal = True
                    if message.session_id != session_id or message.is_error or message.subtype != 'success':
                        raise RuntimeError('Claude did not produce a successful terminal result')
            if not terminal:
                raise RuntimeError('Claude stream ended without a terminal result')
        except Exception as error:
            emit({'type': 'runtime_error', 'message': str(error)})
        finally:
            emit({'type': 'agent_settled'})

    try:
        while True:
            line = await asyncio.to_thread(sys.stdin.buffer.readline, 262145)
            if not line:
                break
            if len(line) > 262144 or not line.endswith(b'\n'):
                raise ValueError('invalid bridge frame')
            request = json.loads(line)
            command = request['type']
            try:
                if command == 'init':
                    if client is not None:
                        raise ValueError('already initialized')
                    session_id = request['session_id']
                    uuid.UUID(session_id)
                    options = ClaudeAgentOptions(
                        cli_path=request['command'], cwd=request['workspaces'][0],
                        add_dirs=request['workspaces'][1:],
                        system_prompt={'type': 'preset', 'preset': 'claude_code', 'append': request['instructions']},
                        resume=session_id if request.get('resume') else None,
                        session_id=None if request.get('resume') else session_id,
                        permission_mode='default', can_use_tool=permission, model=request.get('model'),
                        include_partial_messages=True, setting_sources=['user', 'project', 'local'])
                    client = ClaudeSDKClient(options=options)
                    await client.connect()
                    data = {'sessionId': session_id}
                elif command == 'get_state':
                    data = {'sessionId': session_id, 'running': running is not None and not running.done()}
                elif command == 'prompt':
                    if running is not None and not running.done():
                        raise ValueError('Claude is already running')
                    await client.query(request['message'], session_id=session_id)
                    running = asyncio.create_task(receive())
                    data = {'accepted': True}
                elif command == 'abort':
                    await client.interrupt()
                    data = {}
                elif command == 'respond':
                    pending[request['request_id']].set_result(request['response'])
                    data = {}
                elif command == 'history':
                    info = get_session_info(request['session_id'])
                    if info is None:
                        emit({'type': 'response', 'id': request['id'], 'success': True, 'data': {'found': False}})
                        continue
                    data = {'found': True, 'entries': [serial(x) for x in get_session_messages(request['session_id'])], 'cwd': info.cwd}
                elif command == 'list':
                    data = {'data': [serial(x) for x in list_sessions(limit=25, offset=request.get('offset', 0))]}
                else:
                    raise ValueError('unknown bridge command')
                emit({'type': 'response', 'id': request['id'], 'success': True, 'data': data})
            except Exception as error:
                emit({'type': 'response', 'id': request['id'], 'success': False, 'error': str(error)})
    finally:
        for future in pending.values():
            if not future.done():
                future.cancel()
        if running is not None and not running.done():
            running.cancel()
            await asyncio.gather(running, return_exceptions=True)
        if client is not None:
            await client.disconnect()


if __name__ == '__main__':
    asyncio.run(main())
