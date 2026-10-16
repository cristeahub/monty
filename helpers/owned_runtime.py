"""Private server pipe/PTY guard. EOF means the owning server died, not UI detach."""
import base64
import errno
import fcntl
import json
import os
import pty
import re
import select
import selectors
import signal
import struct
import subprocess
import sys
import termios
import time


def main():
    mode, cwd, lock_paths, *command = sys.argv[1:]
    options = json.loads(mode) if mode.startswith('{') else {'mode': mode}
    mode = options['mode']
    dimensions = mode.split(':')
    mode = dimensions[0]
    rows, cols = map(int, dimensions[1:3]) if len(dimensions) > 1 else (24, 80)
    vim = len(dimensions) > 3 and dimensions[3] == 'vim'
    vim_initialized = False
    footer = b''
    os.umask(0o077)
    locks = []
    paths = json.loads(lock_paths) if lock_paths.startswith('[') else ([lock_paths] if lock_paths else [])
    for lock_path in paths:
        lock = os.open(lock_path, os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600)
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        os.set_inheritable(lock, True)
        locks.append(lock)
    if mode == 'lease':
        target = options['target_pid']
        print(json.dumps({'type': 'ready'}), flush=True)
        # Explicit release is allowed only by the live server. Owner death
        # keeps the fence until the native ownership guard finishes cleanup.
        while True:
            try:os.kill(target, 0)
            except ProcessLookupError:return 0
            ready, _, _ = select.select([0], [], [], .1)
            if ready:
                line = sys.stdin.buffer.readline()
                if line == b'release\n':return 0
                if not line:
                    while True:
                        try:os.kill(target, 0)
                        except ProcessLookupError:return 0
                        time.sleep(.1)
                else:raise ValueError('invalid ownership lease control')
    master = None
    if mode == 'pty':
        master, slave = pty.openpty()
        fcntl.ioctl(slave, termios.TIOCSWINSZ, struct.pack("HHHH", rows, cols, 0, 0))
        # forkpty provides a controlling terminal without unsafe preexec_fn.
        pid = os.fork()
        if pid == 0:
            os.close(master)
            os.setsid()
            fcntl.ioctl(slave, termios.TIOCSCTTY, 0)
            for fd in (0, 1, 2):
                os.dup2(slave, fd)
            if slave > 2:
                os.close(slave)
            os.chdir(cwd)
            os.execvpe(command[0], command, os.environ)
        os.close(slave)
        child_in = child_out = master
        process = None
    else:
        child_env = dict(os.environ, MONTY_SERVER_PARENT_PID=str(os.getpid()))
        process = subprocess.Popen(command, cwd=cwd, env=child_env, stdin=subprocess.PIPE,
                                   stdout=subprocess.PIPE, start_new_session=True, pass_fds=tuple(locks))
        pid = process.pid
        child_in, child_out = process.stdin.fileno(), process.stdout.fileno()
    stopped = False

    def stop(*_):
        nonlocal stopped
        if stopped:
            return
        stopped = True
        # Descendants share the process group; leave it fenced until cleanup finishes.
        try:
            os.killpg(pid, signal.SIGTERM)
        except ProcessLookupError:
            pass

    for sig in (signal.SIGTERM, signal.SIGINT, signal.SIGHUP):
        signal.signal(sig, stop)
    if mode == 'pty':
        print(json.dumps({'type': 'ready'}), flush=True)
    selector = selectors.DefaultSelector()
    selector.register(0, selectors.EVENT_READ, 'owner')
    selector.register(child_out, selectors.EVENT_READ, 'child')
    os.set_blocking(child_in, False)
    queued = bytearray()

    def enqueue(data):
        queued.extend(data)
        if len(queued) > 4 * 1024 * 1024:
            raise ValueError('native input backlog exceeds 4 MiB')
        if child_in == child_out:
            selector.modify(child_in, selectors.EVENT_READ | selectors.EVENT_WRITE, 'child')
        elif not selector.get_map().get(child_in):
            selector.register(child_in, selectors.EVENT_WRITE, 'input')
    buffer = b''
    code = None
    owner_deadline = None
    child_open = True
    try:
        while not stopped and (code is None or child_open):
            if code is None:
                if process is not None:
                    code = process.poll()
                else:
                    finished, status = os.waitpid(pid, os.WNOHANG)
                    if finished:
                        code = os.waitstatus_to_exitcode(status)
            if code is not None:
                try:
                    os.killpg(pid, signal.SIGKILL)
                except ProcessLookupError:
                    pass
            if owner_deadline is not None and time.monotonic() >= owner_deadline:
                stop()
                break
            for key, events in selector.select(0.1):
                if events & selectors.EVENT_WRITE:
                    try:
                        del queued[:os.write(child_in, queued)]
                    except BlockingIOError:
                        pass
                    if not queued:
                        if child_in == child_out:
                            selector.modify(child_in, selectors.EVENT_READ, 'child')
                        else:
                            selector.unregister(child_in)
                    if not events & selectors.EVENT_READ:
                        continue
                try:
                    data = os.read(key.fd, 16384)
                except OSError as error:
                    if error.errno == errno.EIO and key.data == 'child':
                        selector.unregister(key.fd)
                        child_open = False
                        continue
                    raise
                if not data:
                    if key.data == 'owner':
                        selector.unregister(key.fd)
                        if mode == 'pipe':
                            process.stdin.close()
                            owner_deadline = time.monotonic() + 3
                        elif code is None:
                            stop()
                    else:
                        selector.unregister(key.fd)
                        child_open = False
                    continue
                if key.data == 'child':
                    if mode == 'pty':
                        if vim and not vim_initialized:
                            footer = (footer + data)[-8192:]
                            match = re.search(rb'Vim: (Insert|Normal)', footer)
                            if match:
                                vim_initialized = True
                                if match[1] == b'Insert':
                                    enqueue(b'\x1b')
                        print(json.dumps({'type': 'output', 'data': base64.b64encode(data).decode()}), flush=True)
                    else:
                        sys.stdout.buffer.write(data)
                        sys.stdout.buffer.flush()
                elif mode == 'pipe':
                    enqueue(data)
                else:
                    buffer += data
                    if len(buffer) > 262144:
                        raise ValueError('terminal control frame too large')
                    while b'\n' in buffer:
                        line, buffer = buffer.split(b'\n', 1)
                        request = json.loads(line)
                        if request['type'] == 'input':
                            value = base64.b64decode(request['data'], validate=True)
                            enqueue(value)
                        elif request['type'] == 'resize':
                            rows, cols = request['rows'], request['cols']
                            if not all(isinstance(x, int) and 1 <= x <= 1000 for x in (rows, cols)):
                                raise ValueError('invalid terminal dimensions')
                            fcntl.ioctl(master, termios.TIOCSWINSZ, struct.pack('HHHH', rows, cols, 0, 0))
                        else:
                            raise ValueError('invalid terminal control')
    finally:
        stop()
        deadline = time.monotonic() + 3
        while code is None and time.monotonic() < deadline:
            if process is not None:
                code = process.poll()
            else:
                done, status = os.waitpid(pid, os.WNOHANG)
                if done:
                    code = os.waitstatus_to_exitcode(status)
            if code is None:
                time.sleep(0.02)
        # Also kill descendants after the leader exits; they must not outlive ownership.
        try:
            os.killpg(pid, signal.SIGKILL)
        except ProcessLookupError:
            pass
        if code is None:
            if process is not None:
                code = process.wait()
            else:
                _, status = os.waitpid(pid, 0)
                code = os.waitstatus_to_exitcode(status)
        if master is not None:
            os.close(master)
        for lock in locks:
            os.close(lock)
    if mode == 'pty':
        print(json.dumps({'type': 'exit', 'code': code if code >= 0 else 128 - code}), flush=True)
    return max(0, code) if code >= 0 else 128 - code


if __name__ == '__main__':
    sys.exit(main())
