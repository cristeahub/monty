#!/usr/bin/env python3
"""Fake rex CLI for checkout-binary tests: persistent JSON state, no terminals.

Environment knobs:
  REX_FAKE_STATE    JSON state file (sessions, windows, blocks, clients, calls).
  REX_FAKE_LOG      append one JSON argv line per invocation.
  REX_FAKE_DOWN=1   every command fails as if the server were unreachable.
  REX_FAKE_FAIL=cmd the named subcommand exits 7 before changing state.
  REX_FAKE_INVALID=cmd the named subcommand mutates state, then prints junk
                    (a lost or garbled response after a successful creation).
  REX_FAKE_NO_APP=1 / REX_FAKE_TWO_APPS=1  app client inventory variants.
  REX_FAKE_NO_SELECT=1  the app offers no session.select action.
"""
import json
import os
import sys

ENDPOINT = 'unix:///fake/rex/server.sock'
APP = 'client:01a1-app'
OTHER_APP = 'client:01a1-other-app'


def load():
    path = os.environ['REX_FAKE_STATE']
    if os.path.exists(path):
        with open(path) as handle:
            return json.load(handle)
    return {'sessions': {}, 'blocks': {}, 'windows': {}, 'counter': 0, 'selected': []}


def save(state):
    with open(os.environ['REX_FAKE_STATE'], 'w') as handle:
        json.dump(state, handle)


def fresh(state, kind):
    state['counter'] += 1
    return '%s:%08x-fake-%04d' % (kind, 0x1a14100 + state['counter'], state['counter'])


def fail(message, code=1):
    print(message, file=sys.stderr)
    sys.exit(code)


def not_found(kind, target):
    fail('Error: %s target "%s" not found' % (kind, target), 3)


args = sys.argv[1:]
if os.environ.get('REX_FAKE_LOG'):
    with open(os.environ['REX_FAKE_LOG'], 'a') as log:
        log.write(json.dumps(args) + '\n')

# Global flags may precede the subcommand.
globals_ = {}
rest = []
i = 0
while i < len(args):
    if args[i] in ('-S', '-C', '--timeout'):
        globals_[args[i]] = args[i + 1]
        i += 2
    elif args[i].startswith('--autostart') or args[i].startswith('--color'):
        i += 1
    else:
        rest.append(args[i])
        i += 1
args = rest
command = args[0] if args else ''
sub = args[1] if len(args) > 1 else ''

if os.environ.get('REX_FAKE_DOWN'):
    fail('Error: connect to %s: connection refused' % ENDPOINT, 1)
if os.environ.get('REX_FAKE_FAIL') == command:
    fail('Error: fake %s failure' % command, 7)


def option(name, default=None):
    return args[args.index(name) + 1] if name in args else default


def tail():
    return args[args.index('--') + 1:] if '--' in args else []


def positional():
    """Positionals before -- excluding option values and flags."""
    stop = args.index('--') if '--' in args else len(args)
    values = []
    skip = False
    for token in args[1:stop]:
        if skip:
            skip = False
            continue
        if token in ('-s', '-b', '-w', '-c', '--label', '--window', '--shell', '--split', '--args', '--ratio'):
            skip = True
            continue
        if token.startswith('-'):
            continue
        values.append(token)
    return values


def output(value):
    if os.environ.get('REX_FAKE_INVALID') == command:
        print('{not json')
    else:
        print(json.dumps(value))


state = load()


def session_or_die(session):
    if session not in state['sessions']:
        not_found('session', session)
    return state['sessions'][session]


def new_block(session, window, label, cwd, cmd):
    block = fresh(state, 'block')
    state['blocks'][block] = {'session': session, 'window': window, 'label': label, 'cwd': cwd,
                              'command': cmd, 'child': {'pid': 4242, 'user_id': 501, 'name': 'zsh'},
                              'last_exit': None, 'placement': 'placed',
                              'status': [{'state': 'working', 'app': 'claude-code', 'updated_at': 'now'}]}
    state['windows'][window]['blocks'].append(block)
    return block


def new_window(session, label):
    window = fresh(state, 'window')
    state['windows'][window] = {'session': session, 'label': label, 'blocks': []}
    state['sessions'][session]['windows'].append(window)
    return window


if command == 'whoami':
    output({'endpoint': ENDPOINT, 'account': 'tester', 'session_count': len(state['sessions'])})
elif command == 'ls':
    output({'sessions': [{'session_id': k, 'label': v['label']} for k, v in state['sessions'].items()]})
elif command == 'session' and sub == 'inspect':
    session = session_or_die(positional()[1])
    output({'session_id': positional()[1], 'label': session['label'],
            'windows': [{'window_id': w, 'label': state['windows'][w]['label']} for w in session['windows']]})
elif command == 'block' and sub == 'inspect':
    session = option('-s')
    block = positional()[1]
    if block not in state['blocks'] or state['blocks'][block]['session'] != session:
        not_found('block', block)
    entry = state['blocks'][block]
    output({'session_id': session, 'block': {'block_id': block, 'label': entry['label'],
            'placement': entry['placement'], 'window_id': entry['window']}})
elif command == 'block' and sub == 'call':
    session = option('-s')
    block = option('-b')
    method = positional()[1]
    if block not in state['blocks'] or state['blocks'][block]['session'] != session:
        not_found('block', block)
    entry = state['blocks'][block]
    if method == 'process':
        output({'last_exit': entry['last_exit'], 'child': entry['child'], 'home': '/fake/home'})
    elif method == 'program_status':
        output({'records': entry['status']})
    else:
        fail('Error: unknown method ' + method, 2)
elif command == 'client' and sub == 'ls':
    clients = [{'client_id': 'client:01a1-peer'},
               {'client_id': 'client:01a1-null', 'info': None},
               {'client_id': 'client:01a1-unregistered', 'info': 'unknown'},
               {'client_id': 'client:01a1-cli', 'info': {'kind': 'cli'}}]
    if not os.environ.get('REX_FAKE_NO_APP'):
        clients.append({'client_id': APP, 'info': {'kind': 'app', 'label': 'Fake Mac'}})
    if os.environ.get('REX_FAKE_TWO_APPS'):
        clients.append({'client_id': OTHER_APP, 'info': {'kind': 'app', 'label': 'Other Mac'}})
    output({'clients': clients})
elif command == 'actions':
    actions = [{'name': 'client.window.new'}, {'name': 'client.tab.goto'}]
    if not os.environ.get('REX_FAKE_NO_SELECT'):
        actions.append({'name': 'session.select', 'args': {'properties': {'session_id': {}, 'window_id': {}}}})
    output({'actions': actions})
elif command == 'do':
    payload = json.loads(option('--args', '{}'))
    session_or_die(payload['session_id'])
    state['selected'].append({'client': globals_.get('-C'), **payload})
    save(state)
    print('null')
elif command == 'new':
    label = positional()[0] if positional() else 'generated-label'
    session = fresh(state, 'session')
    state['sessions'][session] = {'label': label, 'windows': []}
    window = new_window(session, option('--window', 'Window'))
    block = new_block(session, window, option('--window', 'shell'), option('-c'), tail())
    save(state)
    output({'session_id': session, 'initial_windows': [{'window_id': window, 'block_ids': [block], 'revision': 1}]})
elif command == 'run':
    session = option('-s')
    session_or_die(session)
    window = new_window(session, option('--label', 'Window'))
    block = new_block(session, window, option('--label', ''), option('-c'), tail())
    save(state)
    output({'window_id': window, 'block_ids': [block], 'revision': 2})
elif command == 'split':
    session = option('-s')
    anchor = option('-b')
    session_or_die(session)
    if anchor not in state['blocks'] or state['blocks'][anchor]['session'] != session:
        not_found('block', anchor)
    window = state['blocks'][anchor]['window']
    block = new_block(session, window, option('--label', ''), option('-c'), tail())
    save(state)
    output({'anchor_block_id': anchor, 'block_ids': [block], 'split_ids': ['split:fake'], 'revision': 3})
elif command == 'focus':
    session = option('-s')
    block = positional()[0]
    if block not in state['blocks'] or state['blocks'][block]['session'] != session:
        not_found('block', block)
    state.setdefault('focused', []).append(block)
    save(state)
else:
    fail('Error: unsupported fake rex command: ' + ' '.join(args), 2)
