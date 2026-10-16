#!/usr/bin/env python3
"""Checkout-binary Rex launch, placement, reconnect and recovery checks with an
isolated MONTY_HOME, a fake rex CLI and real temporary Git repositories."""
import json
import os
from pathlib import Path
import re
import shlex
import shutil
import subprocess
import sys
import tempfile

binary = str(Path(sys.argv[1]).resolve())
fake_rex = str(Path(sys.argv[2]).resolve())

with tempfile.TemporaryDirectory(prefix='monty-rex-') as temporary:
    home = Path(temporary).resolve()
    repo = home / "repo with ' quotes æ"
    repo.mkdir()
    env = {**os.environ, 'HOME': str(home), 'MONTY_HOME': str(home),
           'MONTY_CODEX_YOLO': 'false', 'GIT_CONFIG_GLOBAL': '/dev/null',
           'GIT_CONFIG_SYSTEM': '/dev/null', 'GIT_CONFIG_NOSYSTEM': '1',
           'GIT_CONFIG_COUNT': '0', 'GIT_TEMPLATE_DIR': '/dev/null'}
    for key in ('MANTLE_CONTEXT', '_MANTLE_SELECTED_HOME', 'MONTY_AGENT_PROFILE',
                'MONTY_TERMINAL', 'MONTY_TARGET',
                'REX_SESSION', 'REX_BLOCK', 'REX_SERVER', 'REX_CLIENT'):
        env.pop(key, None)
    subprocess.run(['git', 'init', '-q', str(repo)], env=env, check=True)
    subprocess.run(['git', '-C', str(repo), '-c', 'user.name=Test', '-c',
                    'user.email=test@example.invalid', 'commit', '-qm', 'Initial',
                    '--allow-empty'], env=env, check=True)
    shutil.copytree(Path(__file__).resolve().parent.parent / 'agent-profiles',
                    home / 'agent-profiles')
    fake = home / 'fake-bin'
    fake.mkdir()
    for name in ('pi', 'gh', 'sdef'):
        path = fake / name
        path.write_text('#!/bin/sh\nexit 0\n')
        path.chmod(0o700)
    (fake / 'rex').write_text('#!/bin/sh\nexec ' + shlex.quote(sys.executable) + ' -I ' + shlex.quote(fake_rex) + ' "$@"\n')
    (fake / 'rex').chmod(0o700)
    log = home / 'rex.log'
    state_file = home / 'rex-state.json'
    env.update(PATH=str(fake) + ':' + env['PATH'], REX_FAKE_LOG=str(log), REX_FAKE_STATE=str(state_file))

    def state():
        return json.loads(state_file.read_text())

    def calls():
        return [json.loads(line) for line in log.read_text().splitlines()] if log.exists() else []

    def mutations():
        return list(state()['blocks']) if state_file.exists() else []

    def cli(*args, code=0, extra=None):
        result = subprocess.run([binary, *args, '--home', str(home)],
                                env={**env, **(extra or {})}, text=True, capture_output=True)
        assert result.returncode == code, (args, result.stdout, result.stderr)
        return result.stdout + result.stderr

    cli('projects', 'add', '--repo', str(repo))
    project = json.loads((home / '.monty' / 'projects.json').read_text())['projects'][0]['id']
    context = home / 'context.md'
    context.write_text('# Implement Rex support\n')
    title = "Rex ' tab $(touch BAD) \"quoted\" æøå; `touch BAD`"

    def manifest(worker):
        task = re.search(r'local-\d+', cli('task', 'add', '--project', project, '--title', worker)).group()
        directory = home / '.monty' / 'runs' / 'rex-test'
        directory.mkdir(parents=True, exist_ok=True)
        worker_dir = directory / 'workers' / worker
        path = directory / (worker + '.json')
        path.write_text(json.dumps({'jobs': [{'id': worker, 'title': title, 'repo': str(repo),
            'branch': 'test/' + worker, 'context': str(context),
            'worker_dir': str(worker_dir), 'task_key': 'local:' + task}]}))
        return path, worker_dir

    def launch(path, terminal='rex', code=0, extra=None, target='tab'):
        return cli('launch-many', '--manifest', str(path), '--terminal', terminal, '--target', target,
                   '--harness', 'pi', '--worktree', 'never', code=code, extra=extra)

    def resume(worker, code=0, extra=None, target='tab', *flags):
        return cli('resume', worker, '--terminal', 'rex', '--target', target, '--harness', 'pi',
                   '--worktree', 'never', *flags, code=code, extra=extra)

    def reference(worker_dir):
        return json.loads((worker_dir / 'rex-terminal.json').read_text())

    def job(worker_dir):
        return json.loads((worker_dir / 'job.json').read_text())

    inside = {'REX_SESSION': '', 'REX_BLOCK': '', 'REX_SERVER': 'unix:///fake/rex/server.sock'}

    # ---- doctor ---------------------------------------------------------------
    doctor = cli('doctor', '--terminal', 'rex', '--harness', 'pi', '--worktree', 'never')
    assert re.search(r'PASS +rex ', doctor) and re.search(r'PASS +rex-server +unix:///fake/rex/server.sock', doctor), doctor
    down = cli('doctor', '--terminal', 'rex', '--harness', 'pi', '--worktree', 'never', code=1, extra={'REX_FAKE_DOWN': '1'})
    assert re.search(r'FAIL +rex-server', down) and 'Open the Rex app' in down, down
    missing = cli('doctor', '--terminal', 'rex', '--harness', 'pi', '--worktree', 'never', code=1,
                  extra={'PATH': '/usr/bin:/bin'})
    assert re.search(r'FAIL +rex ', missing) and 'rex-server' not in missing, missing
    assert not log.exists() or all('whoami' in row for row in calls()), 'doctor must stay read-only'

    # ---- dry-run never touches Rex -----------------------------------------------
    path, worker = manifest('success')
    if log.exists():
        log.unlink()
    launch(path, terminal='dry-run')
    assert not log.exists() and not worker.exists(), 'dry-run mutated launch state'

    # ---- tab inside Rex: caller session, explicit server, exact command ----------
    # The caller's session must exist on the fake server.
    seed = subprocess.run([str(fake / 'rex'), 'new', 'caller', '--json'], env=env, capture_output=True, text=True, check=True)
    seeded = json.loads(seed.stdout)
    caller_session = seeded['session_id']
    caller_block = seeded['initial_windows'][0]['block_ids'][0]
    inside.update(REX_SESSION=caller_session, REX_BLOCK=caller_block)
    log.unlink()
    launch(path, extra=inside)
    runs = [row for row in calls() if 'run' in row]
    assert len(runs) == 1 and len(mutations()) == 2, calls()  # the seeded caller block plus one tab
    args = runs[0]
    assert args[args.index('-S') + 1] == 'unix:///fake/rex/server.sock' and '--autostart=false' in args
    assert args[args.index('-s') + 1] == caller_session
    assert args[args.index('-c') + 1] == str(repo)
    assert args[args.index('--label') + 1] == title
    assert args[args.index('--') + 1:args.index('--') + 3] == ['/bin/zsh', '-l']
    assert args[args.index('--') + 3].endswith('monty-success-launch.sh')
    assert '--keep-open' in args and args[args.index('--shell') + 1] == 'none'
    assert not any('do' in row for row in calls()), 'in-context tabs must not move the app view'
    assert not any('client' in row for row in calls()), 'caller-relative tabs must not need REX_CLIENT'
    assert list(state()['sessions']) == [caller_session], 'a task must open a tab in the caller session'
    assert not (home / '.monty' / 'rex-session.json').exists()
    assert job(worker)['status'] == 'launch-requested'
    ref = reference(worker)
    assert ref['schema'] == 'monty:rex-terminal:v1' and ref['session_id'] == caller_session
    assert ref['block_id'].startswith('block:') and ref['window_id'].startswith('window:')
    assert ref['server'] == 'unix:///fake/rex/server.sock' and ref['target'] == 'tab'
    assert ref['launch_script'] == job(worker)['launch_script'] and ref['client_id'] is None
    assert not any(k.startswith('rex') or k.endswith('_id') for k in job(worker)) and 'block:' not in json.dumps(job(worker))
    assert not any((d / 'BAD').exists() for d in (repo, home, Path.cwd())), 'title was executed by a shell'
    assert state()['blocks'][ref['block_id']]['label'] == title
    # Repeat batch launch never re-requests a launch-requested worker.
    launch(path, extra=inside)
    assert len(mutations()) == 2, 'batch retry duplicated a requested worker'

    # ---- split inside Rex anchors at the caller's own pane ---------------------------
    path, worker = manifest('split')
    launch(path, target='split', extra=inside)
    args = [row for row in calls() if 'split' in row and 'inspect' not in row][-1]
    assert args[args.index('-b') + 1] == caller_block and args[args.index('--split') + 1] == 'right'
    assert reference(worker)['window_id'] == state()['blocks'][caller_block]['window']
    assert reference(worker)['target'] == 'split'

    # ---- split outside Rex fails in preflight before any reservation --------------------
    path, worker = manifest('split-outside')
    before = len(mutations())
    out = launch(path, target='split', code=1)
    assert 'REX_SESSION and REX_BLOCK' in out, out
    assert not worker.exists() and len(mutations()) == before, 'preflight failure reserved or created state'

    # ---- window: one Rex session per worker, shown through the app client ---------------
    # Client discovery must ignore bare peers and null/non-object info without REX_CLIENT.
    path, worker = manifest('window')
    launch(path, target='window', extra=inside)
    args = [row for row in calls() if 'new' in row][-1]
    assert args[args.index('new') + 1] == 'monty: ' + title and args[args.index('--window') + 1] == title
    ref = reference(worker)
    assert ref['client_id'] == 'client:01a1-app' and ref['target'] == 'window'
    selected = state()['selected'][-1]
    assert selected == {'client': 'client:01a1-app', 'session_id': ref['session_id'], 'window_id': ref['window_id']}
    assert state()['sessions'][ref['session_id']]['label'] == 'monty: ' + title

    # Window needs a displayable app; explain exactly what is missing.
    for name, extra, needle in [('no-app', {'REX_FAKE_NO_APP': '1'}, 'no Rex app is connected'),
                                ('two-apps', {'REX_FAKE_TWO_APPS': '1'}, 'set REX_CLIENT'),
                                ('no-select', {'REX_FAKE_NO_SELECT': '1'}, 'session.select'),
                                ('down', {'REX_FAKE_DOWN': '1'}, 'not reachable')]:
        path, worker = manifest(name)
        before = len(mutations())
        out = launch(path, target='window', code=1, extra={**inside, **extra})
        assert needle in out, (name, out)
        assert not worker.exists() and len(mutations()) == before, name
    path, worker = manifest('chosen-app')
    launch(path, target='window', extra={**inside, 'REX_FAKE_TWO_APPS': '1', 'REX_CLIENT': 'client:01a1-other-app'})
    assert reference(worker)['client_id'] == 'client:01a1-other-app'

    # ---- tab outside Rex lands in one Monty-owned session that is reused -----------------
    path, worker = manifest('outside-a')
    launch(path)
    home_session = json.loads((home / '.monty' / 'rex-session.json').read_text())
    assert home_session['schema'] == 'monty:rex-session:v1' and home_session['server'] == 'unix:///fake/rex/server.sock'
    assert state()['sessions'][home_session['session_id']]['label'] == 'monty'
    ref = reference(worker)
    assert ref['session_id'] == home_session['session_id'] and ref['client_id'] == 'client:01a1-app'
    assert state()['selected'][-1]['window_id'] == ref['window_id'], 'context-less tabs must be shown in the app'
    news = len([row for row in calls() if 'new' in row])
    path, worker_b = manifest('outside-b')
    launch(path)
    assert len([row for row in calls() if 'new' in row]) == news, 'the Monty session must be reused'
    assert reference(worker_b)['session_id'] == home_session['session_id']
    # A vanished Monty session is recreated, never guessed from labels.
    current = state()
    del current['sessions'][home_session['session_id']]
    state_file.write_text(json.dumps(current))
    path, worker_c = manifest('outside-c')
    launch(path)
    replaced = json.loads((home / '.monty' / 'rex-session.json').read_text())
    assert replaced['session_id'] != home_session['session_id'] and reference(worker_c)['session_id'] == replaced['session_id']

    # ---- tab outside Rex honours a persisted rex-session (label or ID) -------------------
    assert cli('settings', 'get', 'rex-session').strip() == '' and 'rex-session   (monty-owned session)' in cli('settings')
    assert 'without spaces' in cli('settings', 'set', 'rex-session', 'two words', code=1)
    assert cli('settings', 'set', 'rex-session', 'caller').strip() == 'rex-session = caller outside any Mantle profile'
    assert cli('settings', 'get', 'rex-session').strip() == 'caller' and 'rex-session   caller' in cli('settings')
    doctor = cli('doctor', '--terminal', 'rex', '--harness', 'pi', '--worktree', 'never')
    assert re.search(r'PASS +rex-session +caller \(' + re.escape(caller_session) + r'\)', doctor), doctor
    news = len([row for row in calls() if 'new' in row])
    path, worker = manifest('setting-session')
    launch(path)
    assert len([row for row in calls() if 'new' in row]) == news, 'a configured session must never be created'
    ref = reference(worker)
    assert ref['session_id'] == caller_session, 'the tab must land in the configured session, recorded by full ID'
    assert ref['client_id'] == 'client:01a1-app' and state()['selected'][-1]['window_id'] == ref['window_id']
    assert state()['blocks'][ref['block_id']]['session'] == caller_session
    # A full ID works the same way; a vanished or mistyped session fails before reservation.
    cli('settings', 'set', 'rex-session', caller_session)
    path, worker = manifest('setting-session-id')
    launch(path)
    assert reference(worker)['session_id'] == caller_session
    cli('settings', 'set', 'rex-session', 'no-such-session')
    assert re.search(r'FAIL +rex-session', cli('doctor', '--terminal', 'rex', '--harness', 'pi', '--worktree', 'never', code=1))
    path, worker = manifest('setting-missing')
    before = len(mutations())
    out = launch(path, code=1)
    assert 'rex-session setting no-such-session is not a live session' in out and "set rex-session ''" in out, out
    assert not worker.exists() and len(mutations()) == before, 'a dead configured session reserved or created state'
    # Inside Rex the caller's own session still wins over the setting.
    path, worker = manifest('setting-inside')
    launch(path, extra=inside)
    assert reference(worker)['session_id'] == caller_session and len(mutations()) == before + 1
    # Clearing the setting returns to the Monty-owned session.
    assert 'cleared' in cli('settings', 'set', 'rex-session', '')
    assert cli('settings', 'get', 'rex-session').strip() == ''
    path, worker = manifest('setting-cleared')
    launch(path)
    assert reference(worker)['session_id'] == json.loads((home / '.monty' / 'rex-session.json').read_text())['session_id']

    # ---- the setting and the Monty-owned session follow the Mantle profile -----------------
    work = {'MANTLE_CONTEXT': 'work', '_MANTLE_PREV_STATE': 'unset',
            '_MANTLE_SELECTED_HOME': str(home / 'codex-home'), 'CODEX_HOME': str(home / 'codex-home')}
    assert 'rex-session   (session labelled work, profile work)' in cli('settings', extra=work)
    assert cli('settings', 'get', 'rex-session', extra=work).strip() == ''
    news = len([row for row in calls() if 'new' in row])
    path, worker = manifest('profile-a')
    launch(path, extra=work)
    assert len([row for row in calls() if 'new' in row]) == news + 1
    profile_session = json.loads((home / '.monty' / 'rex-session.work.json').read_text())
    assert state()['sessions'][profile_session['session_id']]['label'] == 'work', 'the owned session carries the profile name'
    assert reference(worker)['session_id'] == profile_session['session_id']
    assert json.loads((home / '.monty' / 'rex-session.json').read_text())['session_id'] != profile_session['session_id']
    path, worker = manifest('profile-b')
    launch(path, extra=work)
    assert len([row for row in calls() if 'new' in row]) == news + 1 and reference(worker)['session_id'] == profile_session['session_id']
    # A session the user already labelled after the profile is used instead of creating one.
    current = state()
    del current['sessions'][profile_session['session_id']]
    state_file.write_text(json.dumps(current))
    seed = subprocess.run([str(fake / 'rex'), 'new', 'work', '--json'], env=env, capture_output=True, text=True, check=True)
    users_work = json.loads(seed.stdout)['session_id']
    news = len([row for row in calls() if 'new' in row])
    path, worker = manifest('profile-c')
    launch(path, extra=work)
    assert len([row for row in calls() if 'new' in row]) == news and reference(worker)['session_id'] == users_work
    # Outside any profile the plain Monty session is still used.
    path, worker = manifest('profile-off')
    launch(path)
    assert reference(worker)['session_id'] == json.loads((home / '.monty' / 'rex-session.json').read_text())['session_id']
    # An explicit mapping is stored per profile and addressed as rex-session or rex-session.<profile>.
    assert 'rex-session = caller for Mantle profile work' in cli('settings', 'set', 'rex-session', 'caller', extra=work)
    persisted = json.loads((home / '.monty' / 'settings.json').read_text())
    assert persisted['rex_sessions'] == {'work': 'caller'} and persisted['rex_session'] is None
    assert cli('settings', 'get', 'rex-session', extra=work).strip() == 'caller'
    assert cli('settings', 'get', 'rex-session').strip() == ''
    assert cli('settings', 'get', 'rex-session.work').strip() == 'caller'
    shown = cli('settings', extra=work)
    assert 'rex-session   caller (profile work)' in shown and 'rex-session.work caller' in shown, shown
    assert 'not a Mantle profile name' in cli('settings', 'get', 'rex-session.bad.name', code=1)
    path, worker = manifest('profile-mapped')
    launch(path, extra=work)
    assert reference(worker)['session_id'] == caller_session
    doctor = cli('doctor', '--terminal', 'rex', '--harness', 'pi', '--worktree', 'never', extra=work)
    assert re.search(r'PASS +rex-session +caller \(' + re.escape(caller_session) + r'\) for profile work', doctor), doctor
    assert 'rex-session' not in cli('doctor', '--terminal', 'rex', '--harness', 'pi', '--worktree', 'never')
    assert 'cleared for Mantle profile work' in cli('settings', 'set', 'rex-session.work', '')
    assert cli('settings', 'get', 'rex-session', extra=work).strip() == ''
    assert json.loads((home / '.monty' / 'settings.json').read_text())['rex_sessions'] == {}

    # ---- lost or garbled responses keep launch-requested without replay ------------------
    for name, extra in [('invalid', {'REX_FAKE_INVALID': 'run'}), ('failed', {'REX_FAKE_FAIL': 'run'})]:
        path, worker = manifest(name)
        before = len(mutations())
        out = launch(path, code=1, extra={**inside, **extra})
        assert job(worker)['status'] == 'launch-requested' and 'launch_error' not in job(worker), out
        assert not (worker / 'rex-terminal.json').exists()
        assert " resume '" + name + "'" in out and 'Recovery:' in out, out
        launch(path, extra=inside)
        assert len(mutations()) == before + (1 if name == 'invalid' else 0), 'batch retry must not replay a requested worker'

    # ---- resume: a live terminal is focused, never duplicated ---------------------------
    path, worker = manifest('reconnect')
    launch(path, extra=inside)
    ref = reference(worker)
    before = len(mutations())
    out = resume('reconnect', extra=inside)
    assert 'still live' in out and ref['block_id'] in out, out
    assert len(mutations()) == before and state()['focused'][-1] == ref['block_id']
    assert reference(worker) == ref and job(worker)['status'] == 'launch-requested'
    out = resume('reconnect', 0, inside, 'tab', '--fresh')
    assert 'still live' in out, out
    assert len(mutations()) == before, 'a fresh conversation must not add a second writer to a live terminal'
    # An exited child means retained output only: resume requests a new tab.
    current = state()
    current['blocks'][ref['block_id']]['child'] = None
    current['blocks'][ref['block_id']]['last_exit'] = {'exit_code': 0}
    state_file.write_text(json.dumps(current))
    resume('reconnect', extra=inside)
    assert len(mutations()) == before + 1
    newer = reference(worker)
    assert newer['block_id'] != ref['block_id'] and newer['requested_at'] >= ref['requested_at']
    assert newer['session_id'] == caller_session, 'resuming must open a tab in the caller session'
    # Stale IDs (block gone) also fall through to a new tab; an unreachable server never does.
    current = state()
    del current['blocks'][newer['block_id']]
    state_file.write_text(json.dumps(current))
    before = len(mutations())
    out = resume('reconnect', code=1, extra={**inside, 'REX_FAKE_DOWN': '1'})
    assert 'cannot verify' in out and len(mutations()) == before, out
    assert reference(worker) == newer
    resume('reconnect', extra=inside)
    assert len(mutations()) == before + 1 and reference(worker)['block_id'] != newer['block_id']
    # A dry-run resume never consults or creates Rex terminals.
    before_calls = len(calls())
    cli('resume', 'reconnect', '--terminal', 'dry-run', '--harness', 'pi', '--worktree', 'never')
    assert len(calls()) == before_calls

    # ---- native status query is read-only ------------------------------------------------
    before_calls = len(mutations())
    out = cli('rex', 'status', 'reconnect')
    assert 'terminal: live (zsh running)' in out and 'status: working app=claude-code' in out, out
    assert reference(worker)['block_id'] in out and len(mutations()) == before_calls
    current = state()
    current['blocks'][reference(worker)['block_id']]['child'] = None
    current['blocks'][reference(worker)['block_id']]['last_exit'] = {'exit_code': 3}
    state_file.write_text(json.dumps(current))
    assert 'terminal: exited (3)' in cli('rex', 'status', 'reconnect')
    assert 'no recorded Rex terminal' in cli('rex', 'status', 'failed', code=1)

    # ---- terminal selection precedence: flag, MONTY_TERMINAL, persisted setting ----------
    assert cli('settings', 'get', 'terminal').strip() == 'rex', 'Rex is the built-in default'
    assert re.search(r'terminal +rex', cli('settings'))
    assert 'must be ghostty or rex' in cli('settings', 'set', 'terminal', 'dry-run', code=1)
    cli('settings', 'set', 'terminal', 'rex')
    assert cli('settings', 'get', 'terminal').strip() == 'rex' and re.search(r'terminal +rex', cli('settings'))
    path, worker = manifest('setting-default')
    before = len(mutations())
    cli('launch-many', '--manifest', str(path), '--harness', 'pi', '--worktree', 'never', extra=inside)
    assert len(mutations()) == before + 1 and (worker / 'rex-terminal.json').exists()
    path, worker = manifest('env-override')
    cli('launch-many', '--manifest', str(path), '--harness', 'pi', '--worktree', 'never',
        extra={**inside, 'MONTY_TERMINAL': 'dry-run'})
    assert not worker.exists() and len(mutations()) == before + 1
    cli('settings', 'set', 'terminal', 'ghostty')
    print('Rex launch, placement, reconnect and recovery checks passed')
