#!/usr/bin/env python3
"""Task lifecycle through the client service, with real repos and fake runners."""
import json
import os
from pathlib import Path
import shutil
import shlex
import subprocess
import sys
import tempfile

from test_client_api import BIN, Service, http_error


def fixture(home, native_socket=None, profile='local'):
    repo = home / 'repo'
    repo.mkdir()
    subprocess.run(['git', 'init', '-q', str(repo)], check=True)
    subprocess.run(['git', '-C', str(repo), '-c', 'user.name=Test', '-c',
                    'user.email=test@example.invalid', 'commit', '-qm', 'Initial', '--allow-empty'], check=True)
    shutil.copytree(Path(__file__).resolve().parent.parent / 'agent-profiles', home / 'agent-profiles')
    fake = home / 'fake-bin'
    fake.mkdir()
    for name in ('codex', 'pi', 'ghostty', 'osascript', 'sdef', 'gh'):
        path = fake / name
        path.write_text('#!/usr/bin/env python3\nfrom pathlib import Path\n'
            'import sys, json\n'
            + (f'if sys.argv[1:4] == ["app-server", "daemon", "start"]:\n print(json.dumps({{"socketPath": {str(native_socket)!r}, "appServerVersion": "0.160.0"}})); sys.exit(0)\n'
               if name == 'codex' and native_socket else '')
            +
            f'with open({str(home / "tools.log")!r}, "a") as log: log.write({name!r} + " " + repr(sys.argv[1:]) + "\\n")\n'
            f'if {name!r} == "osascript" and Path({str(home / "fail-terminal")!r}).exists(): sys.exit(7)\n')
        path.chmod(0o700)
    (fake / 'wt').write_text(r'''#!/usr/bin/env python3
import subprocess, sys
from pathlib import Path
repo = Path.cwd()
def git(*args):
    return subprocess.check_output(['git', *args], text=True).strip()
rows = git('worktree', 'list', '--porcelain').split('\n\n')
entries = {}
for row in rows:
    fields = dict(line.split(' ', 1) for line in row.splitlines() if ' ' in line)
    if fields.get('branch', '').startswith('refs/heads/'):
        entries[fields['branch'][11:]] = fields['worktree']
if sys.argv[1] == 'list':
    print(repo.name + ':')
    for branch, path in entries.items(): print('  ' + branch + ' -> ' + path)
elif sys.argv[1] == 'b':
    branch = sys.argv[2]
    path = repo.parent / ('worktree-' + repo.name + '-' + branch.replace('/', '-'))
    if branch not in entries:
        git('worktree', 'add', '-b', branch, str(path))
    print(entries.get(branch, str(path)))
elif sys.argv[1] == 'db':
    branch = sys.argv[2]
    if branch in entries: git('worktree', 'remove', entries[branch])
    git('branch', '-D', branch)
else: raise AssertionError(sys.argv)
''')
    (fake / 'wt').chmod(0o700)
    (home / 'codex').mkdir()
    (home / 'other-codex').mkdir()
    config = home / 'profiles.json'
    config.write_text(json.dumps({'profiles': [{'id': profile, 'harness': 'codex',
        'command': str(fake / 'codex'), 'codex_home': str(home / 'codex'), 'roots': [str(repo)]},
        {'id': 'other', 'harness': 'codex', 'command': str(fake / 'codex'),
         'codex_home': str(home / 'other-codex'), 'roots': [str(repo)]}]}))
    if native_socket:
        pi_home = home / 'pi'
        pi_home.mkdir()
        (fake / 'pi').write_text('#!/bin/sh\nexec ' + shlex.quote(sys.executable) + ' ' +
            shlex.quote(str(Path(__file__).with_name('fake_pi.py').resolve())) + ' "$@"\n')
        data = json.loads(config.read_text())
        data['profiles'].append({'id': 'Pi', 'harness': 'pi', 'command': str(fake / 'pi'),
                                 'pi_home': str(pi_home), 'roots': [str(repo)]})
        config.write_text(json.dumps(data))
    service = Service(home, config, {'PATH': str(fake) + ':' + os.environ['PATH'],
        'MONTY_CODEX_YOLO': 'true', 'MONTY_BRANCH_PREFIX': 'testprefix'})
    cli(service, 'projects', 'add', '--repo', str(repo))
    # Rex is the built-in default; these fixtures fake Ghostty unless a check opts in.
    cli(service, 'settings', 'set', 'terminal', 'ghostty')
    return service, repo


def cli(service, *args, code=0):
    result = subprocess.run([BIN, *args, '--home', str(service.home)], env=service.env,
                            capture_output=True, text=True)
    assert result.returncode == code, (args, result.stdout, result.stderr)
    return result.stdout + result.stderr


def check(home):
    service, repo = fixture(home)
    try:
        c = service.client
        cli(service, 'settings', 'set', 'codex-yolo', 'true')
        assert c.req('projects')[0]['repo'] == str(repo)
        created = c.cmd('create', {'op': 'task_create', 'project': 'repo', 'title': 'Safe web launch', 'notes': 'Test notes'})
        key = created['key']
        c.req('commands/create', {'op': 'task_create', 'project': 'repo', 'title': 'Safe web launch', 'notes': 'Test notes'})
        assert len(c.req('inventory')['tasks']) == 1
        assert created['notes'] == 'Test notes'
        def request(op, **args):
            return {'op': op, 'task': key, 'revision': c.req('tasks/' + key)['revision'], **args}
        def preflight(name, action='launch'):
            return c.cmd(name, request('task_preflight', action=action, profile='local',
                context='Implement the task. Acceptance: all tests pass. No remote actions are authorized.'))
        c.cmd('invalid-task', {'op': 'task_complete', 'task': '1', 'confirm': True}, status='rejected')
        c.cmd('blank-title', {'op': 'task_create', 'project': 'repo', 'title': '   '}, status='rejected')
        c.cmd('long-title', {'op': 'task_create', 'project': 'repo', 'title': 'x' * 501}, status='rejected')
        c.cmd('stale', {'op': 'task_complete', 'task': key, 'revision': 'old', 'confirm': True}, status='rejected')
        http_error(404, lambda: c.req('tasks/local:missing'))
        preview = preflight('preview')
        assert preview['ready'], preview
        assert preview['workspaces'][0]['branch'].startswith('testprefix/')
        assert '[dry-run]' in preview['preview']['stdout']
        assert not list((home / '.monty/runs').glob('**/job.json'))
        assert not (home / 'tools.log').exists(), 'preflight must not invoke terminal or harness'
        # A stale context must never launch the worker.
        context = home / '.monty/runs/web-preview/context.md'
        context.write_text('changed')
        c.cmd('stale-context', {'op': 'task_apply', 'preflight': 'preview', 'confirm': True}, status='rejected')
        preview = preflight('reviewed')
        assert preview['ready'], preview
        assert preview['agent_profile'] == 'simplified'
        c.cmd('unconfirmed', {'op': 'task_apply', 'preflight': 'reviewed'}, status='rejected')
        applied = c.cmd('launch', {'op': 'task_apply', 'preflight': 'reviewed', 'confirm': True})
        assert applied['process']['exit_code'] == 0, applied
        assert applied['requested']
        c.req('commands/launch', {'op': 'task_apply', 'preflight': 'reviewed', 'confirm': True})
        c.cmd('duplicate-launch', {'op': 'task_apply', 'preflight': 'reviewed', 'confirm': True}, status='rejected')
        assert (home / 'tools.log').read_text().count('osascript') == 1
        service.stop()
        service.start()
        c = service.client
        c.req('commands/launch', {'op': 'task_apply', 'preflight': 'reviewed', 'confirm': True})
        assert (home / 'tools.log').read_text().count('osascript') == 1, 'restart must preserve receipts'
        task = c.req('tasks/' + key)
        assert task['worker']['status'] == 'launch-requested'
        assert task['status'] == 'open', 'startup receipt must not complete the task'
        scripts = list((home / '.monty').glob('**/*launch.sh'))
        assert scripts
        for script in scripts:
            assert '--codex-yolo' not in script.read_text(), 'service must not inherit bypass'
            assert 'exec env MONTY_CODEX_YOLO=false ' in script.read_text(), 'terminal must not reread unsafe defaults'
        assert 'monty done' in cli(service, 'task', 'done', key, code=1)
        c.cmd('relaunch', request('task_preflight', action='launch', profile='local', context='Again'), status='rejected')
        c.cmd('wrong-account', request('task_preflight', action='resume', profile='other'), status='rejected')
        resumed = preflight('resume-preview', 'resume')
        assert resumed['ready'], resumed
        result = c.cmd('resume', {'op': 'task_apply', 'preflight': 'resume-preview', 'confirm': True})
        assert result['process']['exit_code'] == 0, result
        assert c.req('tasks/' + key)['worker']['directory'] == task['worker']['directory']
        assert (home / 'tools.log').read_text().count('osascript') == 2
        # Materialize its checkout through the existing wt path, then prove safe completion.
        worktree = Path(cli(service, 'task', 'workspace', 'ensure', key, '--repo', str(repo)).strip())
        (worktree / 'valuable.txt').write_text('keep me')
        failed = c.cmd('dirty-complete', request('task_complete', confirm=True))
        assert failed['process']['exit_code'] != 0, failed
        assert (worktree / 'valuable.txt').read_text() == 'keep me'
        assert c.req('tasks/' + key)['status'] == 'open'
        (worktree / 'valuable.txt').unlink()
        finished = c.cmd('complete', request('task_complete', confirm=True))
        assert finished['process']['exit_code'] == 0, finished
        assert not worktree.exists()
        assert c.req('tasks/' + key)['status'] == 'done'
        assert c.req('inventory')['tasks'] == []
        assert c.req('inventory?scope=all')['tasks'][0]['key'] == key
        reopened = preflight('reopen-preview', 'resume')
        assert reopened['ready'], reopened
        result = c.cmd('reopen', {'op': 'task_apply', 'preflight': 'reopen-preview', 'confirm': True})
        assert result['process']['exit_code'] == 0, result
        assert c.req('tasks/' + key)['status'] == 'open'
        # A failed terminal request still carries the complete lifecycle recovery output.
        created = c.cmd('create-failed', {'op': 'task_create', 'project': 'repo', 'title': 'Terminal failure'})
        key = created['key']
        assert preflight('failed-preview')['ready']
        (home / 'fail-terminal').touch()
        failed = c.cmd('failed-launch', {'op': 'task_apply', 'preflight': 'failed-preview', 'confirm': True})
        assert failed['process']['exit_code'] != 0
        assert 'recover' in (failed['process']['stdout'] + failed['process']['stderr']).lower()
        assert c.req('tasks/' + key)['status'] == 'open'
        # Rex is one shared terminal selection: preflight records it, apply re-checks
        # it, and the launch goes through the rex CLI rather than AppleScript.
        rex_state = home / 'rex-state.json'
        rex_log = home / 'rex.log'
        fake = home / 'fake-bin'
        (fake / 'rex').write_text('#!/bin/sh\nREX_FAKE_STATE=%s REX_FAKE_LOG=%s exec %s -I %s "$@"\n' % (
            shlex.quote(str(rex_state)), shlex.quote(str(rex_log)), shlex.quote(sys.executable),
            shlex.quote(str(Path(__file__).with_name('fake_rex.py').resolve()))))
        (fake / 'rex').chmod(0o700)
        applescript_calls = (home / 'tools.log').read_text().count('osascript')
        cli(service, 'settings', 'set', 'terminal', 'rex')
        created = c.cmd('create-rex', {'op': 'task_create', 'project': 'repo', 'title': 'Rex web launch'})
        key = created['key']
        preview = preflight('rex-preview')
        assert preview['ready'] and preview['terminal'] == 'rex', preview
        assert not rex_log.exists() or not any('run' in row for row in rex_log.read_text().splitlines()), 'preflight must not create terminals'
        cli(service, 'settings', 'set', 'terminal', 'ghostty')
        c.cmd('rex-stale', {'op': 'task_apply', 'preflight': 'rex-preview', 'confirm': True}, status='rejected')
        cli(service, 'settings', 'set', 'terminal', 'rex')
        preview = preflight('rex-reviewed')
        assert preview['terminal'] == 'rex', preview
        applied = c.cmd('rex-launch', {'op': 'task_apply', 'preflight': 'rex-reviewed', 'confirm': True})
        assert applied['process']['exit_code'] == 0 and applied['terminal'] == 'rex', applied
        rows = [json.loads(line) for line in rex_log.read_text().splitlines()]
        assert sum('run' in row for row in rows) == 1 and sum('new' in row for row in rows) == 1, rows
        assert (home / 'tools.log').read_text().count('osascript') == applescript_calls, 'Rex launches never call AppleScript'
        worker_dir = Path(c.req('tasks/' + key)['worker']['directory'])
        ref = json.loads((worker_dir / 'rex-terminal.json').read_text())
        assert ref['session_id'] == json.loads((home / '.monty/rex-session.json').read_text())['session_id']
        assert c.req('tasks/' + key)['status'] == 'open', 'a Rex launch receipt never completes the task'
        cli(service, 'settings', 'set', 'terminal', 'ghostty')
        print('task API: create, preflight, duplicate/stale protection, launch, resume, dirty completion, archive/reopen, recovery and Rex selection passed')
    finally:
        service.close()


if __name__ == '__main__':
    with tempfile.TemporaryDirectory(prefix='monty-tasks-api-', dir='/tmp') as directory:
        check(Path(directory).resolve())
