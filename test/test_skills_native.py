"""Opt-in discovery check using installed Codex/Pi, fresh homes and no inference.

python3 test/test_skills_native.py _build/default/bin/main.exe
"""
import json
import os
from pathlib import Path
import select
import shutil
import subprocess
import sys
import tempfile
import time

binary = str(Path(sys.argv[1]).resolve())
with tempfile.TemporaryDirectory(prefix="monty-skill-native-", dir="/tmp") as tmp:
    root = Path(tmp)
    env = {k: v for k, v in os.environ.items() if k in ("PATH", "LANG", "TMPDIR")}
    env.update(HOME=tmp, CODEX_HOME=str(root / "codex"), PI_CODING_AGENT_DIR=str(root / "pi"),
               MONTY_HOME=str(root / "monty"), PI_TELEMETRY="0")
    for placement, harness in [(p, h) for p in ("profile", "shared") for h in ("codex", "pi")]:
        subprocess.run([binary, "skills", "install", "--harness", harness],
                       cwd=root, env=env, check=True, capture_output=True)
        if placement == "shared":
            source = Path(env["CODEX_HOME" if harness == "codex" else "PI_CODING_AGENT_DIR"]) / "skills/monty"
            target = root / ".agents/skills/monty"
            target.parent.mkdir(parents=True, exist_ok=True)
            if target.exists():
                shutil.rmtree(target)
            shutil.move(str(source), target)
        args = ["codex", "app-server"] if harness == "codex" else [
            "pi", "--mode", "rpc", "--offline", "--no-approve", "--no-tools",
            "--no-extensions", "--no-prompt-templates", "--no-context-files"]
        with (root / (harness + ".log")).open("wb") as errors:
            process = subprocess.Popen(args, cwd=root, env=env, stdin=subprocess.PIPE,
                                       stdout=subprocess.PIPE, stderr=errors)
            buffer = b""

            def send(value):
                process.stdin.write(json.dumps(value).encode() + b"\n")
                process.stdin.flush()

            def reply(identity):
                global buffer
                deadline = time.monotonic() + 30
                while time.monotonic() < deadline:
                    while b"\n" in buffer:
                        line, buffer = buffer.split(b"\n", 1)
                        try:
                            value = json.loads(line)
                        except json.JSONDecodeError:
                            continue
                        if value.get("id") == identity:
                            return value
                    if select.select([process.stdout], [], [], .2)[0]:
                        chunk = os.read(process.stdout.fileno(), 65536)
                        if not chunk:
                            raise AssertionError((harness, "exited before discovery", (root / (harness + ".log")).read_text()))
                        buffer += chunk
                raise AssertionError((harness, "discovery timed out"))

            try:
                if harness == "codex":
                    send({"id": 1, "method": "initialize", "params": {
                        "clientInfo": {"name": "monty-skill-test", "version": "1"},
                        "capabilities": {"experimentalApi": True}}})
                    assert "error" not in reply(1)
                    send({"method": "initialized"})
                    send({"id": 2, "method": "skills/list", "params": {"cwds": [tmp], "forceReload": True}})
                    result = reply(2)
                    skills = [s for entry in result["result"]["data"] for s in entry["skills"]]
                    found = [s for s in skills if s["name"] == "monty" and s["enabled"]]
                else:
                    send({"id": "skills", "type": "get_commands"})
                    result = reply("skills")
                    assert result["success"], result
                    found = [s for s in result["data"]["commands"] if s["name"] == "skill:monty"]
                assert found, (harness, result)
                print(harness + " (" + placement + "): native discovery of automatically installed Monty skill passed")
            finally:
                process.terminate()
                try:
                    process.wait(timeout=10)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()
