"""Checkout-binary MCP and harness E2E, with no real client/model/config writes."""
import json
import fcntl
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


BINARY = str(Path(sys.argv[1]).resolve())


def request(method, params=None, id=1):
    return dict(jsonrpc="2.0", id=id, method=method, params={} if params is None else params)


def initialize(version="2025-11-25"):
    return request("initialize", dict(protocolVersion=version, capabilities={},
                                     clientInfo=dict(name="portable-test", version="1")))


READY = dict(jsonrpc="2.0", method="notifications/initialized")


with tempfile.TemporaryDirectory(prefix="monty-mcp-") as directory:
    root = Path(directory).resolve()
    home = root / "monty home"
    home.mkdir()
    agent = root / "pi agent"
    config = agent / "mcp.json"
    # Spaces and shell metacharacters must stay literal in every client format.
    executable = root / "monty 'quoted' $(touch INJECTED)"
    shutil.copy2(BINARY, executable)
    env = dict(os.environ, HOME=str(root), MONTY_HOME=str(home),
               PI_CODING_AGENT_DIR=str(agent), CODEX_HOME=str(root / "codex"),
               MONTY_HARNESS="codex", MANTLE_CONTEXT="", MONTY_CODEX_YOLO="false")

    def cli(*args, ok=True):
        result = subprocess.run([str(executable), *args], cwd=root, env=env,
                                text=True, capture_output=True, timeout=15)
        if ok:
            assert result.returncode == 0, result.stderr
        else:
            assert result.returncode != 0, result.stdout
        return result

    def exchange(messages, ok=True):
        data = "\n".join(json.dumps(m) if not isinstance(m, str) else m for m in messages) + "\n"
        result = subprocess.run([str(executable), "mcp", "serve"], cwd=root, env=env,
                                input=data, text=True, capture_output=True, timeout=15)
        assert (result.returncode == 0) == ok, result.stderr
        if ok:
            assert result.stderr == "", result.stderr
        return [json.loads(line) for line in result.stdout.splitlines()]

    def call(name, arguments):
        return request("tools/call", dict(name=name, arguments=arguments), id="call")

    def text(result):
        return result["result"]["content"][0]["text"]

    for version in ["2024-11-05", "2025-03-26", "2025-06-18", "2025-11-25", "future"]:
        reply = exchange([initialize(version), READY, request("tools/list", id=2)])
        assert len(reply) == 2  # No response to a notification or startup banner.
        assert reply[0]["result"]["protocolVersion"] == ("2025-11-25" if version == "future" else version)
        tools = reply[1]["result"]["tools"]
        assert {tool["name"] for tool in tools} == {"monty_help", "monty_command_help"}
        assert all(tool["annotations"]["readOnlyHint"] for tool in tools)

    # Bad requests don't corrupt the session; unknown notifications are ignored.
    replies = exchange([request("tools/list"), "{broken", "[]", initialize(), READY,
                        dict(jsonrpc="2.0", method="notifications/cancelled", params={"requestId":99}),
                        request("ping", id="p"), request("missing", id=4),
                        request("tools/call", [], id=5)])
    assert [r["error"]["code"] for r in replies[:3]] == [-32002, -32700, -32600]
    assert replies[-2]["error"]["code"] == -32601
    assert replies[4] == dict(jsonrpc="2.0", id="p", result={})
    assert exchange(["x" * 65537], ok=False) == []
    for info in [dict(name=None, version="1"), dict(name="client", version=1), []]:
        invalid = initialize()
        invalid["params"]["clientInfo"] = info
        replies = exchange([invalid, initialize(), READY, request("tools/list")])
        assert replies[0]["error"]["code"] == -32602
        assert "tools" in replies[-1]["result"]
    replies = exchange([initialize(), READY,
                        dict(jsonrpc="2.0", id=1, method="tools/list", params=None)])
    assert replies[-1]["error"]["code"] == -32602

    topics = next(tool for tool in tools if tool["name"] == "monty_help")["inputSchema"]["properties"]["topic"]["enum"]
    assert {"index", "launch", "headless", "lifecycle", "workers", "findings", "handoffs", "mcp"} <= set(topics)
    for topic in topics:
        reply = exchange([initialize(), READY, call("monty_help", dict(topic=topic))])[-1]
        assert "isError" not in reply["result"]
        assert text(reply) + "\n" == cli("mcp", "guide", topic).stdout

    # CLI parser help, not a second hand-maintained flag catalog. Even mutation
    # subcommands are safe to inspect with missing positional arguments.
    for command in [[], ["task", "workspace", "add"], ["settings", "set"],
                    ["headless", "run"], ["findings", "acknowledge"], ["mcp", "serve"]]:
        reply = exchange([initialize(), READY, call("monty_command_help", dict(command=command))])[-1]
        assert text(reply) == cli(*command, "--help=plain").stdout
    reply = exchange([initialize(), READY, call("monty_help", dict(topic="../../secret")),
                      call("monty_command_help", dict(command=["unknown"]))])
    assert all(r["result"]["isError"] for r in reply[1:])
    for arguments in [dict(command=["--version"]), dict(command=["a;touch INJECTED"]),
                      dict(command=["$(touch INJECTED)"]), dict(command="task"),
                      dict(command=["x"] * 5), dict(command=[], extra=True),
                      dict(command=None), None]:
        reply = exchange([initialize(), READY, call("monty_command_help", arguments)])[-1]
        assert reply["error"]["code"] == -32602
    for arguments in [dict(topic=None), dict(topic="index", extra=True), None]:
        reply = exchange([initialize(), READY, call("monty_help", arguments)])[-1]
        assert reply["error"]["code"] == -32602
    for name in ["monty_help", "monty_command_help"]:
        reply = exchange([initialize(), READY, request("tools/call", dict(name=name))])[-1]
        assert "content" in reply["result"]
    assert list(home.iterdir()) == []
    assert not agent.exists()
    assert not (root / "INJECTED").exists()

    generated = json.loads(cli("mcp", "config").stdout)
    assert generated == {"mcpServers": {"monty": {"command": str(executable), "args": ["mcp", "serve"]}}}
    assert "[mcp_servers.monty]" in cli("mcp", "config", "--format", "toml").stdout

    # Pi setup merges only its named entry, preserving preferences and servers.
    agent.mkdir()
    existing = dict(autoEnableCodemode=False, mcpServers=dict(existing=dict(command="other", args=["--keep"])))
    config.write_text(json.dumps(existing))
    cli("mcp", "setup-pi")
    merged = json.loads(config.read_text())
    assert merged["autoEnableCodemode"] is False
    assert merged["mcpServers"]["existing"] == existing["mcpServers"]["existing"]
    assert merged["mcpServers"]["monty"]["command"] == str(executable)
    assert merged["mcpServers"]["monty"]["exposure"] == "direct"
    before = config.stat().st_mtime_ns, config.read_bytes()
    cli("mcp", "setup-pi")
    assert before == (config.stat().st_mtime_ns, config.read_bytes())
    # A portable config has no Pi-specific exposure. Normalize only that missing
    # preference while retaining unrelated per-server and global configuration.
    portable_entry = dict(generated["mcpServers"]["monty"], env={"KEEP": "value"},
                          toolExposure={"monty_help": "direct"})
    config.write_text(json.dumps(dict(existing, mcpServers=dict(existing["mcpServers"], monty=portable_entry))))
    cli("mcp", "setup-pi")
    merged = json.loads(config.read_text())
    assert merged["mcpServers"]["monty"] == dict(portable_entry, exposure="direct")
    assert merged["mcpServers"]["existing"] == existing["mcpServers"]["existing"]
    assert merged["autoEnableCodemode"] is False
    for preference in [dict(exposure="codemode"), dict(exposure="deferred"), dict(exposure="hidden"),
                       dict(toolExposure={"monty_help": "hidden"}),
                       dict(toolExposure={"*": "codemode"})]:
        value = {"mcpServers": {"monty": dict(portable_entry, **preference)}}
        config.write_text(json.dumps(value))
        result = cli("mcp", "setup-pi", ok=False)
        assert "direct" in result.stderr
        assert json.loads(config.read_text()) == value
    for broken in ["not json", "[]", '{"mcpServers":[]}',
                   '{"mcpServers":{"monty":{"command":"some-other-server"}}}',
                   json.dumps({"mcpServers": {"monty": dict(portable_entry, type="http")}}),
                   json.dumps({"mcpServers": {"monty": dict(portable_entry, enabled="true")}}),
                   json.dumps({"mcpServers": {"monty": dict(generated["mcpServers"]["monty"], enabled=False)}})]:
        config.write_text(broken)
        cli("mcp", "setup-pi", ok=False)
        assert config.read_text() == broken
    config.unlink()
    victim = root / "untouched.json"
    victim.write_text("{}")
    config.symlink_to(victim)
    cli("mcp", "setup-pi", ok=False)
    assert victim.read_text() == "{}"
    config.unlink()

    # Real simultaneous CLI invocations converge on one valid entry.
    processes = [subprocess.Popen([str(executable), "mcp", "setup-pi"], cwd=root, env=env,
                                  stdout=subprocess.PIPE, stderr=subprocess.PIPE) for _ in range(6)]
    for process in processes:
        _, err = process.communicate(timeout=15)
        assert process.returncode == 0, err
    assert json.loads(config.read_text())["mcpServers"]["monty"]["command"] == str(executable)

    # Pi profiles may be shared by different Monty homes. A concurrent config
    # update must survive the waiting setup's fresh read/merge/write.
    config.write_text(json.dumps(existing))
    with Path(str(config) + ".monty.lock").open("r+") as lock:
        fcntl.lockf(lock, fcntl.LOCK_EX)
        process = subprocess.Popen([str(executable), "mcp", "setup-pi"], cwd=root,
                                   env=dict(env, MONTY_HOME=str(root / "second home")),
                                   stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        try:
            try:
                process.communicate(timeout=0.2)
                raise AssertionError("setup did not wait for the shared Pi config lock")
            except subprocess.TimeoutExpired:
                pass
            updated = dict(existing, addedWhileWaiting=True)
            config.write_text(json.dumps(updated))
        finally:
            fcntl.lockf(lock, fcntl.LOCK_UN)
        _, err = process.communicate(timeout=15)
        assert process.returncode == 0, err
    merged = json.loads(config.read_text())
    assert merged["addedWhileWaiting"] is True
    assert merged["mcpServers"]["existing"] == existing["mcpServers"]["existing"]
    assert merged["mcpServers"]["monty"]["command"] == str(executable)

    # Capture actual startup argv: no model or client installation involved.
    fake = root / "fake harness"
    log = root / "argv.json"
    fake.write_text(f"#!{sys.executable}\nimport json,sys\nfrom pathlib import Path\nPath({str(log)!r}).write_text(json.dumps(sys.argv[1:]))\n")
    fake.chmod(0o755)
    import shlex
    for harness in ["codex", "pi"]:
        for command in [["start"], ["continue", "--last"], ["continue", "saved-session"]]:
            cli(*command, "--harness", harness, "--" + harness + "-command", shlex.quote(str(fake)))
            argv = json.loads(log.read_text())
            if harness == "codex":
                entries = [value for value in argv if value.startswith("mcp_servers.monty=")]
                assert len(entries) == 1 and 'args=["mcp","serve"]' in entries[0]
                assert str(executable) in entries[0]
            else:
                assert json.loads(config.read_text())["mcpServers"]["monty"]["command"] == str(executable)
    assert not (root / "INJECTED").exists()
    assert not (home / ".monty/tasks.local.json").exists()
    assert not (home / ".monty/runs").exists()

print("MCP protocol, portable help, config preservation and harness startup: passed")
