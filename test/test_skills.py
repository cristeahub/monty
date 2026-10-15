"""Checkout-binary skill installation and migration; isolated homes, no model calls."""
import concurrent.futures
import fcntl
import json
import os
from pathlib import Path
import shlex
import shutil
import subprocess
import sys
import tempfile

BINARY = str(Path(sys.argv[1]).resolve())

with tempfile.TemporaryDirectory(prefix="monty-skills-") as tmp:
    root = Path(tmp).resolve()
    home = root / "monty home"
    home.mkdir()
    executable = root / "monty 'quoted' $(touch INJECTED)"
    shutil.copy2(BINARY, executable)
    env = dict(os.environ, HOME=str(root), MONTY_HOME=str(home), MONTY_HARNESS="codex",
               CODEX_HOME=str(root / "codex profile"), PI_CODING_AGENT_DIR=str(root / "pi profile"),
               CLAUDE_CONFIG_DIR=str(root / "claude profile"), MANTLE_CONTEXT="", MONTY_CODEX_YOLO="false")

    def cli(*args, ok=True, environment=env):
        result = subprocess.run([str(executable), *args], cwd=root, env=environment,
                                text=True, capture_output=True, timeout=20)
        assert (result.returncode == 0) == ok, (args, result.stdout, result.stderr)
        return result

    index = cli("guide").stdout
    assert "skills" in index and "mcp:" not in index
    assert cli("mcp", "guide").stdout == index
    assert cli("mcp", "guide", "workers").stdout == cli("guide", "workers").stdout
    cli("guide", "not-a-topic", ok=False)
    cli("mcp", "serve", ok=False)
    cli("mcp", "setup-pi", ok=False)
    cli("skills", "install", "--help=plain")
    assert list(home.iterdir()) == []
    assert not Path(env["CODEX_HOME"]).exists()
    assert not Path(env["PI_CODING_AGENT_DIR"]).exists()

    originals = {}
    for harness, key in [("codex", "CODEX_HOME"), ("pi", "PI_CODING_AGENT_DIR"), ("claude", "CLAUDE_CONFIG_DIR")]:
        agent = Path(env[key])
        target = agent / "skills/monty"
        cli("skills", "install", "--harness", harness)
        skill = target / "SKILL.md"
        manual = target / "references/workflows.md"
        assert skill.read_text().startswith("---\n")
        assert "name: monty\n" in skill.read_text()
        assert "monty guide" in skill.read_text()
        for line in index.splitlines():
            if line.startswith("- "):
                topic = line[2:].split(":")[0]
                assert cli("guide", topic).stdout.strip() in manual.read_text()
        originals[harness] = skill.read_bytes(), manual.read_bytes()
        before = [(p.stat().st_mtime_ns, p.read_bytes()) for p in [skill, manual]]
        cli("skills", "install", "--harness", harness)
        assert before == [(p.stat().st_mtime_ns, p.read_bytes()) for p in [skill, manual]]
        skill.write_text("---\n# Managed by Monty.\nname: monty\ndescription: old\n---\n")
        manual.write_text("<!-- Managed by Monty. -->\nold reference\n")
        unrelated = agent / "skills/custom/SKILL.md"
        unrelated.parent.mkdir()
        unrelated.write_text("custom skill")
        cli("skills", "install", "--harness", harness)
        assert (skill.read_bytes(), manual.read_bytes()) == originals[harness]
        assert unrelated.read_text() == "custom skill"

        # Unowned files and symlinks must not be replaced or followed.
        skill.write_text("my own monty skill")
        assert "unmanaged" in cli("skills", "install", "--harness", harness, ok=False).stderr
        assert skill.read_text() == "my own monty skill"
        skill.unlink()
        victim = root / (harness + "-victim")
        victim.write_text("untouched")
        skill.symlink_to(victim)
        cli("skills", "install", "--harness", harness, ok=False)
        assert victim.read_text() == "untouched"
        skill.unlink()
        cli("skills", "install", "--harness", harness)
        shutil.rmtree(target)
        target.symlink_to(root, target_is_directory=True)
        cli("skills", "install", "--harness", harness, ok=False)
        assert not (root / "SKILL.md").exists()
        target.unlink()
        cli("skills", "install", "--harness", harness)

        # Separate Monty homes share a native profile without corrupting files.
        with concurrent.futures.ThreadPoolExecutor(max_workers=6) as pool:
            list(pool.map(lambda n: cli("skills", "install", "--harness", harness,
                          environment=dict(env, MONTY_HOME=str(root / ("other-home-" + str(n))))), range(6)))
        assert (skill.read_bytes(), manual.read_bytes()) == originals[harness]

    # Default native homes work independently from explicitly selected profiles.
    defaults = {k: v for k, v in env.items() if k not in ("CODEX_HOME", "PI_CODING_AGENT_DIR")}
    for harness, directory in [("codex", ".codex"), ("pi", ".pi/agent")]:
        cli("skills", "install", "--harness", harness, environment=defaults)
        assert (root / directory / "skills/monty/SKILL.md").read_bytes() == originals[harness][0]
        missing_home = {k: v for k, v in defaults.items() if k != "HOME"}
        assert "or HOME" in cli("skills", "install", "--harness", harness,
                                environment=missing_home, ok=False).stderr
        cli("skills", "install", "--harness", harness,
            environment={k: v for k, v in env.items() if k != "HOME"})

    agent = Path(env["PI_CODING_AGENT_DIR"])
    config = agent / "mcp.json"
    existing = {"autoEnableCodemode": False, "mcpServers": {"other": {"command": "keep"}}}
    legacy = {"command": str(executable), "args": ["mcp", "serve"], "exposure": "direct"}
    config.write_text(json.dumps(dict(existing, mcpServers=dict(existing["mcpServers"], monty=legacy))))
    cli("skills", "install", "--harness", "pi")
    assert json.loads(config.read_text()) == existing
    before = config.stat().st_mtime_ns, config.read_bytes()
    cli("skills", "install", "--harness", "pi")
    assert (config.stat().st_mtime_ns, config.read_bytes()) == before
    # A user-managed server with the same name is not ours to remove.
    custom = dict(existing, mcpServers={"monty": dict(legacy, command="other-server")})
    config.write_text(json.dumps(custom))
    cli("skills", "install", "--harness", "pi")
    assert json.loads(config.read_text()) == custom

    def migrate_after_replacement(replace, ok=True):
        config.write_text(json.dumps(dict(existing, mcpServers=dict(existing["mcpServers"], monty=legacy))))
        with Path(str(config) + ".monty.lock").open("r+") as lock:
            fcntl.lockf(lock, fcntl.LOCK_EX)
            process = subprocess.Popen([str(executable), "skills", "install", "--harness", "pi"],
                                       cwd=root, env=env, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
            try:
                try:
                    process.communicate(timeout=.2)
                    raise AssertionError("migration did not wait for native config lock")
                except subprocess.TimeoutExpired:
                    pass
                replace()
            finally:
                fcntl.lockf(lock, fcntl.LOCK_UN)
        _, err = process.communicate(timeout=20)
        assert (process.returncode == 0) == ok, err

    # Migration rereads and revalidates after acquiring the native-config lock.
    migrate_after_replacement(lambda: config.write_text(json.dumps(
        dict(json.loads(config.read_text()), addedWhileWaiting=True))))
    assert json.loads(config.read_text()) == dict(existing, addedWhileWaiting=True)
    victim = root / "config-victim.json"
    victim.write_text(json.dumps({"mcpServers": {"monty": legacy}}))
    before = victim.read_bytes()

    def replace_with_symlink():
        config.unlink()
        config.symlink_to(victim)

    migrate_after_replacement(replace_with_symlink, ok=False)
    assert config.is_symlink() and victim.read_bytes() == before
    config.unlink()

    # Real head-butler start/continue reaches the harness only after installing.
    log = root / "argv.json"
    fake = root / "fake harness"
    fake.write_text(f"#!{sys.executable}\nimport json,os,sys\nfrom pathlib import Path\n"
                    "skill=Path(os.environ['PI_CODING_AGENT_DIR'])/'skills/monty/SKILL.md'\n"
                    "assert skill.is_file()\n"
                    f"Path({str(log)!r}).write_text(json.dumps(sys.argv[1:]))\n")
    fake.chmod(0o755)
    config=root/'server.json'
    config.write_text(json.dumps({'profiles':[{'id':'pi','harness':'pi','command':str(fake),'pi_home':str(agent),'roots':[str(root)]}]}))
    log.unlink(missing_ok=True)
    cli('start','--harness','pi','--pi-command',shlex.quote(str(fake)),ok=False)
    assert not log.exists(), 'unavailable server launched an independent harness'
    cli('service','start','--config',str(config))
    try:
        cli('start','--harness','pi','--pi-command',shlex.quote(str(fake)))
        for _ in range(100):
            if log.exists():break
            import time;time.sleep(.02)
        args=json.loads(log.read_text());assert '--session-id' in args
        # API/terminal ownership and exact continuation have dedicated suites.
        assert (agent/'skills/monty/SKILL.md').exists()
    finally:cli('service','stop')
    assert not (root / "INJECTED").exists()
    assert not (home / ".monty/tasks.local.json").exists()
    assert not (home / ".monty/runs").exists()

print("CLI skills: isolated installation, refresh, migration, concurrency and Pi startup passed")
