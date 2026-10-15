#!/usr/bin/env python3
"""Run the compiled Bonsai client in an isolated browser and Monty home.

Usage: python3 test/test_web_ui.py BIN ASSETS PLAYWRIGHT_MODULE SCREENSHOTS
Install Playwright in a temporary directory; no JavaScript is used by the app.
"""
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
from urllib.parse import urlparse

from test_client_api import BIN, Native
from test_tasks_api import fixture


def check(home):
    native = Native(home / 'native.sock')
    native.ui_scenarios = True
    service, repo = fixture(home, native_socket=home / 'native.sock', profile='Personal')
    try:
        def cli(*args):
            subprocess.run([BIN, *args, '--home', str(home)], env=service.env,
                           check=True, capture_output=True)
        cli('task', 'add', '--project', 'repo', '--title', 'Build the Monty web client')
        cli('task', 'workspace', 'add', 'local:local-001', '--repo', str(repo), '--branch', 'monty/web-client')
        second = home / 'second-workspace'
        second.mkdir()
        subprocess.run(['git', 'init', '-q', str(second)], check=True)
        cli('projects', 'add', '--repo', str(second))
        cli('task', 'workspace', 'add', 'local:local-001', '--repo', str(second), '--branch', 'monty/second-workspace')
        c = service.client
        chat = c.cmd('setup-chat', {'op': 'start', 'profile': 'Personal',
            'title': 'Designing the workspace', 'context': {'workspaces': [str(repo)]},
            'task_keys': ['local:local-001']})
        c.cmd('setup-message', {'op': 'send', 'conversation': chat['id'],
            'revision': c.revision(chat['id']), 'text': 'Let’s make a calm place for tasks and conversations.'})
        attachment = repo / 'fixture-attachment.txt'
        attachment.write_text('Retained attachment bytes')
        native.threads['native-only'] = {'id': 'native-only', 'name': 'A native design discussion',
            'preview': 'A conversation from the terminal', 'cwd': str(repo), 'updatedAt': 1,
            'turns': [{'id': 'old-turn', 'status': 'completed', 'items': [
                {'id': 'old-user', 'type': 'userMessage', 'content': [
                    {'type': 'localImage', 'path': str(attachment)},
                    {'type': 'localImage', 'path': str(repo / 'missing.png')}]},
                {'id': 'old-message', 'type': 'agentMessage', 'text': '<script>window.transcriptExecuted=true</script>'}]}]}
        native.threads['old-native'] = {'id': 'old-native', 'name': 'An archived & native conversation', 'archived': True,
            'cwd': str(repo), 'turns': []}
        for n in range(26):
            native.threads['extra-' + str(n)] = {'id': 'extra-' + str(n), 'name': 'Extra native ' + str(n), 'cwd': str(repo), 'turns': []}
        paired = c.req('browser/pair', {'assets': str(Path(sys.argv[2]).resolve())})
        stopping = threading.Event()
        def restart():
            while not stopping.wait(.05):
                if (home / 'restart-request').exists():
                    service.stop()
                    service.start(port=urlparse(c.url).port)
                    result = service.client.req('browser/pair', {'assets': str(Path(sys.argv[2]).resolve())})
                    (home / 'restart-code').write_text(result['code'])
                    return
        helper = threading.Thread(target=restart, daemon=True)
        helper.start()
        output = Path(sys.argv[4]).resolve()
        output.mkdir(parents=True, exist_ok=True)
        try:
            subprocess.run(['node', str(Path(__file__).with_suffix('.cjs')), str(Path(sys.argv[3]).resolve()),
                            c.url, paired['code'], str(output), str(home)], check=True, env=os.environ, timeout=180)
        finally:
            stopping.set()
            helper.join(timeout=10)
        assert any(method == 'thread/list' and params.get('searchTerm') == 'archived & native' for method, params in native.calls)
        assert native.sends == 9, 'lost responses and reconnects must never repeat a message'
        assert [a['result'] for a in native.answers] == [
            {'decision': 'accept'}, {'decision': 'accept'}, {'answers': {'style': {'answers': ['Calm']}, 'phrase': {'answers': ['secret-fixture-phrase']}}},
            {'permissions': {'network': {'enabled': True}}, 'scope': 'turn'}]
        assert all('secret-fixture-phrase' not in p.read_text() for p in (home / '.monty').glob('**/*.json'))
        imported = [chat for chat in c.req('conversations') if any(s['native_id'] == 'native-only' for s in chat['sessions'])]
        assert len(imported) == 1 and not imported[0]['sessions'][0]['observed'], 'restart must retain the import without automatically recreating its writer'
        assert len(c.req('conversations')) == 4
        assert c.req('tasks/local:local-002')['status'] == 'done'
        assert (home / 'tools.log').read_text().count('osascript') == 2
        print('compiled Bonsai browser: task lifecycle, filters/deep links, native paging/archive/import, both streams, approvals, dialogs, interrupts, attachments, lost-response/offline/reload/restart recovery, storage failure, two-tab drafts, keyboard, secret redaction and mobile passed')
    finally:
        service.close()
        native.close()


if __name__ == '__main__':
    with tempfile.TemporaryDirectory(prefix='monty-ui-', dir='/tmp') as directory:
        check(Path(directory).resolve())
