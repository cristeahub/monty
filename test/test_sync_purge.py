"""Purging an observed archive while a background snapshot is in flight."""
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import threading

from test_client_api import Native, Service, http_error, until


with tempfile.TemporaryDirectory(prefix='monty-sync-purge-', dir='/tmp') as temporary:
    home = Path(temporary).resolve()
    repo = home / 'repo'
    repo.mkdir()
    subprocess.run(['git', 'init', '-q', str(repo)], check=True)
    source = Native(home / 'native.sock')
    config = home / 'profiles.json'
    config.write_text(json.dumps({'allow_fixture': True, 'profiles': [{
        'id': 'default', 'harness': 'fixture', 'socket': str(home / 'native.sock'),
        'roots': [str(repo)],
    }]}))
    service = Service(home, config)
    started, release = threading.Event(), threading.Event()
    try:
        client = service.client
        request = {'op': 'start', 'profile': 'default', 'context': {'workspaces': [str(repo)]}}
        chat = client.cmd('observed', request)
        cid = chat['id']
        source.history_gate = started, release
        assert started.wait(5), 'background poll did not read the observed session'
        client.cmd('purge-in-flight', {'op': 'purge', 'conversation': cid, 'confirm': True})
        http_error(410, lambda: client.history(cid))
        release.set()

        # A fresh poll proves the original watcher survived its stale snapshot.
        survivor = client.cmd('survivor', request)
        native = survivor['sessions'][0]['native_id']
        def reads():
            return sum(method == 'history' and params.get('session') == native
                       for method, params in source.calls)
        previous = reads()
        until(reads, lambda count: count > previous, seconds=5)
        assert service.process.poll() is None and client.req('health')
        http_error(410, lambda: client.history(cid))
        archive = home / '.monty' / 'conversations' / cid
        assert json.loads((archive / 'conversation.json').read_text())['sessions'] == []
        assert not (archive / 'sessions').exists(), 'late snapshot recreated purged history'
        print('History sync: in-flight purge preserves tombstones and background polling')
    finally:
        release.set()
        service.close()
        source.close()


with tempfile.TemporaryDirectory(prefix='monty-mirror-purge-', dir='/tmp') as temporary:
    home = Path(temporary).resolve()
    repo = home / 'repo'
    repo.mkdir()
    subprocess.run(['git', 'init', '-q', str(repo)], check=True)
    account = home / 'claude'
    (account / 'sessions').mkdir(parents=True)
    sdk = Path(__file__).with_name('fake_claude_sdk').resolve()
    python = home / 'sdk-python'
    python.write_text('#!' + sys.executable + '\nimport os,sys\n'
                      + 'os.environ["PYTHONPATH"]=' + repr(str(sdk)) + '\n'
                      + 'os.environ["PYTHONDONTWRITEBYTECODE"]="1"\n'
                      + 'os.execv(sys.executable,[sys.executable,*sys.argv[1:]])\n')
    python.chmod(0o700)
    config = home / 'profiles.json'
    config.write_text(json.dumps({'profiles': [{
        'id': 'claude', 'harness': 'claude', 'command': '/usr/bin/true',
        'python': str(python), 'claude_home': str(account), 'roots': [str(repo)],
    }]}))
    service = Service(home, config)
    try:
        client = service.client
        def imported(key, native):
            (account / 'sessions' / (native + '.json')).write_text(json.dumps({'cwd': str(repo), 'entries': []}))
            return client.cmd(key, {'op': 'import', 'profile': 'claude', 'native_id': native,
                                    'context': {'workspaces': [str(repo)]}})
        chat = imported('mirror', '12345678-1234-4234-8234-123456789abc')
        cid = chat['id']
        (account / 'pause-history').touch()
        until(lambda: (account / 'history-started').exists(), seconds=5)
        client.cmd('purge-mirror', {'op': 'purge', 'conversation': cid, 'confirm': True})
        (account / 'release-history').touch()
        native = '12345678-1234-4234-8234-123456789abd'
        imported('mirror-survivor', native)
        def reads():
            return sum(json.loads(line).get('history') == native
                       for line in (account / 'calls.jsonl').read_text().splitlines())
        previous = reads()
        until(reads, lambda count: count > previous, seconds=5)
        assert service.process.poll() is None and client.req('health')
        http_error(410, lambda: client.history(cid))
        assert not (home / '.monty' / 'conversations' / cid / 'sessions').exists()
        print('Claude mirror: in-flight purge preserves service and archive deletion')
    finally:
        (account / 'release-history').touch()
        service.close()
