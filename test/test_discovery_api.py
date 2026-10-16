#!/usr/bin/env python3
"""Read-only native discovery against an isolated checkout service."""
import json
from pathlib import Path
import subprocess
import tempfile
from urllib.parse import urlencode

from test_client_api import Native, Service, http_error


def check(home):
    repo = home / 'repo'
    repo.mkdir()
    subprocess.run(['git', 'init', '-q', str(repo)], check=True)
    native = Native(home / 'native.sock')
    pi = home / 'pi'
    sessions = pi / 'sessions'
    sessions.mkdir(parents=True)
    config = home / 'profiles.json'
    config.write_text(json.dumps({'allow_fixture': True, 'profiles': [
        {'id': 'local', 'harness': 'fixture', 'socket': str(home / 'native.sock'),
         'roots': [str(repo)]},
        {'id': 'pi', 'harness': 'pi', 'command': '/must-not-run', 'pi_home': str(pi),
         'roots': [str(repo)]}]}))
    service = Service(home, config)
    try:
        c = service.client
        for i in range(26):
            native.threads[str(i)] = {'id': str(i), 'name': 'Native chat ' + str(i),
                'preview': 'Saved message', 'cwd': str(repo), 'updatedAt': i, 'turns': []}
            records = [{'type': 'session', 'version': 3, 'id': str(i), 'cwd': str(repo)},
                {'type': 'message', 'id': 'u', 'message': {'role': 'user', 'content': 'Saved message'}},
                {'type': 'session_info', 'id': 'title', 'name': 'Pi chat ' + str(i)}]
            (sessions / (str(i).zfill(2) + '.jsonl')).write_text(''.join(json.dumps(r) + '\n' for r in records))
        native.threads['archived'] = {'id': 'archived', 'name': 'Old chat', 'archived': True, 'turns': []}
        native.threads['0']['name'] = None
        native.threads['0']['preview'] = 'a' * 499 + '🦊' * 5
        # Native logs can contain only metadata, without a user message.
        (sessions / '25.jsonl').write_text(json.dumps({'type': 'session', 'version': 3, 'id': '25', 'cwd': str(repo)}) + '\n' +
            json.dumps({'type': 'session_info', 'id': 'info', 'name': 'Metadata only'}) + '\n')
        originals = {p: p.read_bytes() for p in sessions.iterdir()}
        for profile in ('local', 'pi'):
            page = c.req('discovery?' + urlencode({'profile': profile}))
            if profile == 'local':
                assert page['data'][0]['title'] == 'a' * 499 + '…'
            assert len(page['data']) == 25 and page['next_cursor']
            assert all(r['conversation'] is None for r in page['data'])
            more = c.req('discovery?' + urlencode({'profile': profile, 'cursor': page['next_cursor']}))
            assert len(more['data']) == 1 and more['next_cursor'] is None
            assert not page['errors'] and not more['errors']
            params = {'profile': profile, 'native_id': '0', 'session_file': str(sessions / '00.jsonl')}
            history = c.req('discovery/history?' + urlencode(params))
            assert history['read_only'] is True
        assert c.req('conversations') == [], 'viewing native history must not import it'
        assert c.req('inventory')['tasks'] == []
        assert originals == {p: p.read_bytes() for p in sessions.iterdir()}
        assert all(method in ('initialize', 'initialized', 'thread/list', 'thread/read') for method, _ in native.calls)
        assert c.req('discovery?profile=local&archived=true')['data'][0]['native_id'] == 'archived'
        (sessions / '00.jsonl').write_text('broken\n')
        page = c.req('discovery?profile=pi')
        assert page['coverage'] == 'partial' and page['errors']
        http_error(400, lambda: c.req('discovery?profile=unknown'))
        http_error(403, lambda: c.req('discovery/history?' + urlencode({'profile': 'pi', 'session_file': str(home / 'secret.jsonl')})))
        print('discovery: both sources, all pages, native history, archive and partial coverage passed')
    finally:
        service.close()
        native.close()


if __name__ == '__main__':
    with tempfile.TemporaryDirectory(prefix='monty-discovery-', dir='/tmp') as directory:
        check(Path(directory).resolve())
