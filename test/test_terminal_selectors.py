"""Native terminal selectors stay within their worker or task-free workspace."""
import json
import os
from pathlib import Path
import subprocess
import tempfile

from test_client_api import BIN, Native, Service, until


def check(home):
    repos = [home / 'repo', home / 'other-repo']
    for repo in repos:
        repo.mkdir()
        subprocess.run(['git', 'init', '-q', str(repo)], check=True)
    account = home / 'codex'
    account.mkdir()
    native = Native(home / 'native.sock')
    log = home / 'terminals.jsonl'
    codex = home / 'fake-codex'
    codex.write_text('#!/usr/bin/env python3\nimport json,os,sys\n'
        'if sys.argv[1:4]==["app-server","daemon","start"]:\n'
        ' print(json.dumps(' + repr({'socketPath': str(home / 'native.sock'), 'appServerVersion': '0.160.0'}) + '))\n'
        'else:\n'
        ' with open(' + repr(str(log)) + ',"a") as out:\n'
        '  out.write(json.dumps({"native":sys.argv[-1],"cwd":os.getcwd(),"worker":os.environ.get("MONTY_JOB_ID")})+"\\n")\n')
    codex.chmod(0o700)
    config = home / 'profiles.json'
    config.write_text(json.dumps({'profiles': [{'id': 'default', 'harness': 'codex',
        'command': str(codex), 'codex_home': str(account), 'roots': [str(home)]}]}))
    service = Service(home, config)
    c = service.client
    env = {**service.env, 'MONTY_CODEX_COMMAND': str(codex), 'MONTY_CODEX_YOLO': 'false'}

    def cli(args, environment=env, cwd=home, code=0, input=''):
        result = subprocess.run([BIN, *args, '--home', str(home)], env=environment,
            cwd=cwd, input=input, text=True, capture_output=True, timeout=20)
        assert result.returncode == code, (args, result.stdout, result.stderr)
        return result

    def terminal_rows():
        return [json.loads(line) for line in log.read_text().splitlines()] if log.exists() else []

    def start(name, workspaces, environment=None):
        return c.cmd('start-' + name, {'op': 'start', 'profile': 'default', 'terminal': True,
            'title': name, 'context': {'workspaces': [str(repo) for repo in workspaces],
                                     'environment': environment or {}}})['sessions'][0]['native_id']

    def attach(args, selected, environment=env, cwd=home):
        before = len(terminal_rows())
        cli(args, environment=environment, cwd=cwd)
        rows = until(terminal_rows, lambda rows: len(rows) > before)
        assert rows[-1]['native'] == selected, rows[-1]
        catalog = c.req('conversations')
        sess = next(sess for chat in catalog for sess in chat['sessions'] if sess['native_id'] == selected)
        until(lambda: c.req('terminals/' + sess['id'])['running'], lambda running: not running)
        return rows[-1]

    def rejected(args, environment=env, cwd=home):
        before = terminal_rows()
        result = cli(args, environment=environment, cwd=cwd, code=1)
        assert 'no registered session' in result.stderr, result.stderr
        assert terminal_rows() == before, 'rejected selector opened another terminal'

    def worker(name, workspaces):
        directory = home / '.monty/runs/selectors/workers' / name
        directory.mkdir(parents=True)
        context = home / (name + '.md')
        context.write_text('Isolated native selector fixture.\n')
        job = {'id': name, 'title': name, 'repo': str(workspaces[0]), 'branch': 'test/' + name,
            'status': 'launch-requested', 'worker_dir': str(directory), 'run_dir': str(directory.parent.parent),
            'context': str(context), 'worktree_mode': 'never', 'last_known_worktree': str(workspaces[0]),
            'workspaces': [{'repo': str(repo), 'branch': 'test/' + name, 'worktree': str(repo)} for repo in workspaces]}
        (directory / 'job.json').write_text(json.dumps(job))
        bindings = {'MONTY_JOB_ID': name, 'MONTY_WORKER_DIR': str(directory),
                    'MONTY_JOB_FILE': str(directory / 'job.json')}
        return directory, bindings

    try:
        cli(['settings', 'set', 'harness', 'codex'])
        worker_a, bindings_a = worker('worker-a', repos)
        worker_b, bindings_b = worker('worker-b', repos)
        worker_env = {**env, **bindings_a}
        native_b = start('worker-b', repos, bindings_b)
        sidecar_b = worker_b / 'codex-session-id'
        original_b = sidecar_b.read_bytes()
        # The only saved conversation belongs to B: A's missing-sidecar picker
        # must not silently choose it, even though their workspaces are identical.
        assert not (worker_a / 'codex-session-id').exists()
        rejected(['terminal', '--worker', '--mode', 'picker'], worker_env)
        # An existing sidecar is just a selector; it cannot authorize another worker.
        (worker_a / 'codex-session-id').write_text(native_b)
        rejected(['terminal', '--worker', '--mode', 'exact', '--session', native_b], worker_env)
        assert (worker_a / 'codex-session-id').read_text() == native_b
        rejected(['continue', native_b])

        native_a = start('worker-a', repos, bindings_a)
        (worker_a / 'codex-session-id').unlink()
        row = attach(['terminal', '--worker', '--mode', 'picker'], native_a, worker_env)
        assert row['worker'] == 'worker-a' and row['cwd'] == str(repos[0]), row
        assert (worker_a / 'codex-session-id').read_text() == native_a
        attach(['terminal', '--worker', '--mode', 'exact', '--session', native_a], native_a, worker_env)
        assert sidecar_b.read_bytes() == original_b, "selecting A changed B's native session"
        # Workspace order is part of the same worker identity; an old context
        # must not become eligible merely because its native ID matches.
        path = worker_a / 'job.json'
        original = path.read_bytes()
        changed = json.loads(original)
        changed['workspaces'].reverse()
        changed['repo'] = changed['workspaces'][0]['repo']
        changed['last_known_worktree'] = changed['workspaces'][0]['worktree']
        path.write_text(json.dumps(changed))
        rejected(['terminal', '--worker', '--mode', 'exact', '--session', native_a], worker_env)
        path.write_bytes(original)

        head_butler = start('head-butler', [home])
        unrelated = start('unrelated', [repos[1]])
        start('newer-worker', repos, bindings_b)
        task_free_same_repo = start('task-free-in-worker-repo', repos)
        rejected(['terminal', '--worker', '--mode', 'exact', '--session', task_free_same_repo], worker_env)
        jobs = {path: path.read_bytes() for path in (worker_a / 'job.json', worker_b / 'job.json')}
        sidecars = {path: path.read_bytes() for path in (worker_a / 'codex-session-id',)}
        attach(['continue', '--last'], head_butler)
        attach(['continue'], head_butler)
        # Exact task-free selection may deliberately cross cwd; worker selectors
        # remain rejected even when explicitly supplied.
        attach(['continue', unrelated], unrelated)
        assert all(path.read_bytes() == contents for path, contents in jobs.items())
        assert all(path.read_bytes() == contents for path, contents in sidecars.items())
        assert not (home / '.monty/tasks.local.json').exists()
        assert not list((home / '.monty').glob('handoffs/**/*.json'))
        help_text = ' '.join(cli(['continue', '--help=plain']).stdout.split())
        assert 'registered native session ID' in help_text and 'session name' not in help_text
        print('Terminal selectors: worker identity, sidecars, ordered workspaces and task-free continuation boundaries passed')
    finally:
        service.close()
        native.close()


if __name__ == '__main__':
    with tempfile.TemporaryDirectory(prefix='monty-terminal-selectors-', dir='/tmp') as directory:
        check(Path(directory).resolve())
