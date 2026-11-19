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
    # True when the display this terminal is shown on supports the Program Status
    # Protocol (OSC 7501); the owned terminal then answers support probes itself.
    status_display = bool(options.get('program_status'))
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

    def signal_group(sig):
        # macOS answers EPERM, not ESRCH, when only zombies remain in the group.
        try:
            os.killpg(pid, sig)
        except (ProcessLookupError, PermissionError):
            pass

    def stop(*_):
        nonlocal stopped
        if stopped:
            return
        stopped = True
        # Descendants share the process group; leave it fenced until cleanup finishes.
        signal_group(signal.SIGTERM)

    for sig in (signal.SIGTERM, signal.SIGINT, signal.SIGHUP):
        signal.signal(sig, stop)
    if mode == 'pty':
        # The owner records the native PID so lock errors can name the process.
        print(json.dumps({'type': 'ready', 'pid': pid}), flush=True)
    selector = selectors.DefaultSelector()
    selector.register(0, selectors.EVENT_READ, 'owner')
    os.set_blocking(child_in, False)
    queued = bytearray()

    def watch(fd, events, data):
        """Register fd for exactly these selector events; no events removes it."""
        key = selector.get_map().get(fd)
        if key is None:
            if events:
                selector.register(fd, events, data)
        elif not events:
            selector.unregister(fd)
        elif key.events != events:
            selector.modify(fd, events, data)

    def enqueue(data):
        queued.extend(data)
        if len(queued) > 4 * 1024 * 1024:
            raise ValueError('native input backlog exceeds 4 MiB')

    # Output for the owner is written only as fast as the owner reads it. A blocking
    # write would hold the guard inside the write while the owner stalls: SIGTERM, owner
    # EOF and the cleanup deadline would all wait on the owner. Above the limit the guard
    # stops reading the program, which then waits as it would on a slow terminal.
    output_limit = 4 * 1024 * 1024
    output = bytearray()
    os.set_blocking(1, False)

    def send(data):
        output.extend(data)
        try:
            while output:
                del output[:os.write(1, output)]
        except BlockingIOError:
            pass
        except BrokenPipeError:
            # The owner is gone; its EOF on stdin ends the runtime.
            output.clear()

    def emit_output(data):
        send(json.dumps({'type': 'output', 'data': base64.b64encode(data).decode()}).encode() + b'\n')

    def relay(data):
        """Forward program output. Past the limit only a program that has ended or is
        being stopped is still read, and its remaining bytes are dropped."""
        if len(output) >= output_limit:
            return
        if mode == 'pty':
            emit_output(data)
        else:
            send(data)

    # Program Status Protocol support probe. A program sends it once at startup and
    # treats the protocol as unsupported when the reply is late, so when the display is
    # known to support the protocol the owned terminal answers at once, the way a
    # multiplexer would, and keeps the probe away from the display. Status reports
    # themselves pass through untouched.
    PROBE = b'\x1b]7501;?'
    PROBE_ENDS = (b'\x07', b'\x1b\\')
    PROBE_REPLY = b'\x1b]7501;?:states=idle,working,done,blocked,error:kinds=permission,question,auth\x1b\\'
    held = b''

    def answer_probes(data):
        """Strip complete probes from data and answer each; hold back a trailing partial probe."""
        nonlocal held
        data = held + data
        held = b''
        out = bytearray()
        while True:
            start = data.find(PROBE)
            if start < 0:
                break
            rest = data[start + len(PROBE):]
            end = next((end for end in PROBE_ENDS if rest.startswith(end)), None)
            if end is None:
                if len(rest) < 2:
                    out += data[:start]
                    held = data[start:]
                    return bytes(out)
                out += data[:start + len(PROBE)]
                data = rest
                continue
            enqueue(PROBE_REPLY)
            out += data[:start]
            data = rest[len(end):]
        for length in range(len(PROBE) - 1, 0, -1):
            if data.endswith(PROBE[:length]):
                held = data[-length:]
                data = data[:-length]
                break
        out += data
        return bytes(out)

    def poll():
        if process is not None:
            return process.poll()
        finished, status = os.waitpid(pid, os.WNOHANG)
        return os.waitstatus_to_exitcode(status) if finished else None

    def read(fd):
        """Read what fd has; a PTY whose program side has closed reads as EOF."""
        try:
            return os.read(fd, 16384)
        except OSError as error:
            if error.errno == errno.EIO and fd == child_out:
                return b''
            raise

    buffer = b''
    code = None
    owner_deadline = None
    child_open = True

    def wait_exit(seconds):
        """Wait up to seconds for the program to end, still reading its output: a program
        blocked writing to a terminal nobody reads cannot finish exiting."""
        nonlocal code, child_open
        deadline = time.monotonic() + seconds
        while code is None and time.monotonic() < deadline:
            if select.select([child_out] if child_open else [], [], [], 0.02)[0]:
                data = read(child_out)
                child_open = bool(data)
                if data:
                    relay(data)
            code = poll()

    try:
        while not stopped and (code is None or child_open):
            if code is None:
                code = poll()
            if code is not None:
                signal_group(signal.SIGKILL)
            if owner_deadline is not None and time.monotonic() >= owner_deadline:
                stop()
                break
            # An ended program is read to its end, so its terminal closes and locks release.
            reading = child_open and (code is not None or len(output) < output_limit)
            if child_in == child_out:
                watch(child_in, (selectors.EVENT_READ if reading else 0)
                      | (selectors.EVENT_WRITE if queued and child_open else 0), 'child')
            else:
                watch(child_out, selectors.EVENT_READ if reading else 0, 'child')
                watch(child_in, selectors.EVENT_WRITE if queued else 0, 'input')
            watch(1, selectors.EVENT_WRITE if output else 0, 'output')
            ready = selector.select(0.1)
            # A lone trailing ESC is held only until the next read; nothing more follows a stalled program.
            if not ready and held == b'\x1b':
                emit_output(held)
                held = b''
            for key, events in ready:
                if key.data == 'output':
                    send(b'')
                    continue
                if events & selectors.EVENT_WRITE:
                    try:
                        del queued[:os.write(child_in, queued)]
                    except BlockingIOError:
                        pass
                    if not events & selectors.EVENT_READ:
                        continue
                data = read(key.fd)
                if not data:
                    if key.data == 'owner':
                        selector.unregister(key.fd)
                        if mode == 'pipe':
                            queued.clear()
                            watch(child_in, 0, 'input')
                            process.stdin.close()
                            owner_deadline = time.monotonic() + 3
                        elif code is None:
                            stop()
                    else:
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
                        if status_display:
                            data = answer_probes(data)
                        if data:
                            relay(data)
                    else:
                        relay(data)
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
        if held:
            emit_output(held)
        stop()
        wait_exit(3)
        # Also kill descendants after the leader exits; they must not outlive ownership.
        signal_group(signal.SIGKILL)
        # Closing the terminal releases a program the kernel holds in exit until its
        # output drains, so the wait below ends; the bound covers anything else.
        if master is not None:
            os.close(master)
            child_open = False
        wait_exit(2)
        for lock in locks:
            os.close(lock)
    status = 1 if code is None else code if code >= 0 else 128 - code
    if mode == 'pty' and code is not None:
        send(json.dumps({'type': 'exit', 'code': status}).encode() + b'\n')
    # Hand the owner what it will still read, without waiting on an owner that stalls.
    deadline = time.monotonic() + 3
    while output and time.monotonic() < deadline:
        select.select([], [1], [], 0.1)
        send(b'')
    return status


if __name__ == '__main__':
    sys.exit(main())
