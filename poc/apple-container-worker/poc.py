#!/usr/bin/env python3
"""Run and benchmark the isolated Apple container worker proof of concept."""

import argparse
import csv
import fcntl
import hashlib
import json
import math
import os
import platform
import secrets
import shlex
import stat
import statistics
import subprocess
import sys
import tempfile
import time
from pathlib import Path


PREFIX = "monty-poc-"
IMAGE = "monty-poc-apple-worker:1"
CONTAINER = "monty-poc-apple-worker"
REUSE_CONTAINER = "monty-poc-apple-worker-reuse"
VOLUME = "monty-poc-apple-worker-volume"
CPUS = "2"
MEMORY = "1g"
LABEL_KEY = "com.monty.poc"
IMAGE_ENV = [
    "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin",
    "HOME=/monty/home",
]
ARTIFACT_CONTENT = b"artifact crossed the VM boundary\n"
INVALID_FALLBACK_DIGEST = (
    "sha256:4f9488b7295baec153a9953479690f835ad4699b1d9f11e3897a4485c224fc3e"
)
VERIFIED_DIGEST = (
    "sha256:cc10ad155a1846f0b110643e4450c848e787ed81b9b1029bfd7d365dece74fbf"
)


class Runner:
    def __init__(self, command_log):
        self.command_log = command_log

    def run(self, *args, check=True):
        command = [str(arg) for arg in args]
        self.command_log.append(shlex.join(command))
        result = subprocess.run(command, text=True, capture_output=True)
        if check and result.returncode:
            raise RuntimeError(
                f"command failed ({result.returncode}): {shlex.join(command)}\n"
                f"{result.stdout}{result.stderr}"
            )
        return result


def guard_resource_name(name):
    if not name.startswith(PREFIX) or not all(
        character.isalnum() or character in ".-_:/" for character in name
    ):
        raise ValueError(f"refusing unsafe PoC resource name: {name!r}")
    return name


def percentile_95(samples):
    """Nearest-rank p95; for 20 samples this reports the 19th ordered sample."""
    return sorted(samples)[math.ceil(0.95 * len(samples)) - 1]


def summary(samples):
    return {
        "iterations": len(samples),
        "minimum_ms": round(min(samples) * 1000, 3),
        "median_ms": round(statistics.median(samples) * 1000, 3),
        "p95_ms": round(percentile_95(samples) * 1000, 3),
        "maximum_ms": round(max(samples) * 1000, 3),
    }


def image_digest(image_inspect):
    try:
        digest = image_inspect[0]["configuration"]["descriptor"]["digest"]
    except (IndexError, KeyError, TypeError) as error:
        raise RuntimeError("image inspect did not contain an index digest") from error
    if not digest.startswith("sha256:"):
        raise RuntimeError(f"image inspect contained an invalid digest: {digest!r}")
    return digest


def image_action(digest):
    if digest is None:
        return "load-archive"
    if digest == VERIFIED_DIGEST:
        return "reuse"
    if digest == INVALID_FALLBACK_DIGEST:
        return "delete-and-load-archive"
    raise RuntimeError(f"refusing to replace unexpected image {IMAGE}@{digest}")


def verify_built_image(image_inspect):
    try:
        variant = next(
            item
            for item in image_inspect[0]["variants"]
            if item.get("platform", {}).get("architecture") == "arm64"
            and item.get("platform", {}).get("os") == "linux"
        )
        config = variant["config"]["config"]
    except (IndexError, KeyError, TypeError, StopIteration) as error:
        raise RuntimeError("image inspect has no arm64 configuration") from error
    if (
        config.get("Cmd") != ["sleep", "infinity"]
        or config.get("Entrypoint") not in (None, [])
        or config.get("Env") != IMAGE_ENV
        or config.get("Labels", {}).get(LABEL_KEY) != "apple-container-worker-v1"
    ):
        raise RuntimeError("image configuration does not match the PoC Containerfile")
    return config


def verify_native_builder(properties):
    try:
        rosetta = properties["build"]["rosetta"]
    except (KeyError, TypeError) as error:
        raise RuntimeError("cannot determine Apple container build.rosetta") from error
    if rosetta is not False:
        raise RuntimeError(
            "Apple container prerequisite is not satisfied: set [build] "
            "rosetta = false in ~/.config/container/config.toml, restart the "
            "service, and rerun"
        )


def builder_is_running(runner):
    status = runner.run("container", "builder", "status", "--quiet")
    return bool(status.stdout.strip())


def file_sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for chunk in iter(lambda: handle.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def verify_image_archive(path):
    if path is None:
        raise RuntimeError(
            f"{IMAGE} is absent; provide --image-archive with the checksummed "
            "OCI archive from a previous verified run"
        )
    try:
        archive = path.resolve(strict=True)
    except OSError as error:
        raise RuntimeError(f"cannot read image archive: {path}") from error
    checksum_path = archive.with_name(f"{archive.name}.sha256")
    try:
        fields = checksum_path.read_text(encoding="utf-8").split()
    except OSError as error:
        raise RuntimeError(
            f"cannot read image archive checksum: {checksum_path}"
        ) from error
    if len(fields) != 2 or fields[1] != archive.name:
        raise RuntimeError(f"invalid image archive checksum file: {checksum_path}")
    actual = file_sha256(archive)
    if not secrets.compare_digest(actual, fields[0]):
        raise RuntimeError(
            f"image archive checksum mismatch: expected {fields[0]}, got {actual}"
        )
    return archive


def open_lock_file(path):
    try:
        descriptor = os.open(
            path, os.O_CREAT | os.O_RDWR | os.O_NOFOLLOW, 0o600
        )
    except OSError as error:
        raise RuntimeError(f"cannot safely open PoC lock file: {path}") from error
    try:
        details = os.fstat(descriptor)
        if not stat.S_ISREG(details.st_mode) or details.st_uid != os.getuid():
            raise RuntimeError(f"refusing unsafe PoC lock file: {path}")
        os.fchmod(descriptor, 0o600)
        return os.fdopen(descriptor, "r+")
    except Exception:
        os.close(descriptor)
        raise


def mount_records(value):
    records = []
    if isinstance(value, dict):
        for key, child in value.items():
            if key.lower() == "mounts" and isinstance(child, list):
                records.extend(item for item in child if isinstance(item, dict))
            records.extend(mount_records(child))
    elif isinstance(value, list):
        for child in value:
            records.extend(mount_records(child))
    return records


def verify_mounts(container_inspect):
    mounts = mount_records(container_inspect)
    named_volumes = []
    for mount in mounts:
        mount_type = mount.get("type")
        if isinstance(mount_type, str):
            kind = mount_type.lower()
        elif isinstance(mount_type, dict):
            kind = next(iter(mount_type), "").lower()
        else:
            kind = ""
        if kind in {"bind", "virtiofs"}:
            raise RuntimeError(f"container has a host bind mount: {mount}")
        if kind == "volume":
            volume = (
                mount_type.get("volume", {}) if isinstance(mount_type, dict) else {}
            )
            named_volumes.append((mount, volume.get("name")))
    if len(mounts) != 1 or len(named_volumes) != 1:
        raise RuntimeError(f"expected exactly one named mount: {mounts}")
    mount, volume_name = named_volumes[0]
    if volume_name != VOLUME or mount.get("destination") != "/monty":
        raise RuntimeError(f"named /monty volume is absent from inspect output: {mounts}")
    return mounts


def verify_container_configuration(container_inspect, digest, owner):
    try:
        config = container_inspect[0]["configuration"]
    except (IndexError, KeyError, TypeError) as error:
        raise RuntimeError("container inspect has no configuration") from error
    actual_digest = config.get("image", {}).get("descriptor", {}).get("digest")
    isolated = {
        "publishedPorts": [],
        "publishedSockets": [],
        "rosetta": False,
        "ssh": False,
        "virtualization": False,
    }
    if actual_digest != digest:
        raise RuntimeError(
            f"container image digest {actual_digest!r} does not match {digest!r}"
        )
    if config.get("platform", {}).get("architecture") != "arm64":
        raise RuntimeError("container is not running as native arm64")
    if config.get("labels", {}).get(LABEL_KEY) != owner:
        raise RuntimeError("container does not carry this run's ownership label")
    if config.get("useInit") is not True:
        raise RuntimeError("container does not use Apple's signal-forwarding init")
    mismatches = {
        key: config.get(key)
        for key, expected in isolated.items()
        if config.get(key) != expected
    }
    if mismatches:
        raise RuntimeError(f"container has a broad host integration: {mismatches}")
    init = config.get("initProcess", {})
    if init.get("executable") != "sleep" or init.get("arguments") != ["infinity"]:
        raise RuntimeError(f"container init is not inert sleep: {init}")
    return config


def wait_ready(runner, name):
    runner.run("container", "exec", name, "true")


def stop(runner, name):
    runner.run("container", "stop", "--time", "1", name)


def resource_ids(runner, *args):
    result = runner.run("container", *args, "--quiet")
    return {line.strip() for line in result.stdout.splitlines() if line.strip()}


def create_container(
    runner, owner_label, owned_containers, name=CONTAINER, mount_volume=True
):
    command = [
        "container",
        "create",
        "--name",
        name,
        "--cpus",
        CPUS,
        "--memory",
        MEMORY,
        "--platform",
        "linux/arm64",
        "--label",
        owner_label,
        "--init",
        "--env",
        "HOME=/monty/home",
    ]
    if mount_volume:
        command.extend(
            ["--mount", f"type=volume,source={VOLUME},target=/monty"]
        )
    owned_containers.add(name)
    runner.run(*command, "--entrypoint", "sleep", IMAGE, "infinity")


def start_container(runner, name=CONTAINER):
    runner.run("container", "start", name)


def resource_owner(runner, kind, name):
    args = ("volume", "inspect", name) if kind == "volume" else ("inspect", name)
    inspected = json.loads(runner.run("container", *args).stdout)
    try:
        return inspected[0]["configuration"]["labels"].get(LABEL_KEY)
    except (IndexError, KeyError, TypeError, AttributeError) as error:
        raise RuntimeError(f"cannot verify {kind} ownership for {name}") from error


def delete_owned_container(runner, owned_containers, owner, name=CONTAINER):
    guard_resource_name(name)
    if name not in owned_containers:
        return
    if name in resource_ids(runner, "list", "--all"):
        if resource_owner(runner, "container", name) != owner:
            raise RuntimeError(
                f"refusing to delete container not owned by this run: {name}"
            )
        runner.run("container", "stop", "--time", "1", name, check=False)
        runner.run("container", "delete", name, check=False)
    if name in resource_ids(runner, "list", "--all"):
        raise RuntimeError(f"cleanup left container behind: {name}")
    owned_containers.remove(name)


def delete_owned_volume(runner, owned_volume, owner):
    if not owned_volume:
        return False
    if VOLUME in resource_ids(runner, "volume", "list"):
        if resource_owner(runner, "volume", VOLUME) != owner:
            raise RuntimeError(
                f"refusing to delete volume not owned by this run: {VOLUME}"
            )
        runner.run("container", "volume", "delete", VOLUME, check=False)
    if VOLUME in resource_ids(runner, "volume", "list"):
        raise RuntimeError(f"cleanup left volume behind: {VOLUME}")
    return False


def volume_hash(runner):
    result = runner.run(
        "container",
        "exec",
        CONTAINER,
        "sh",
        "-c",
        "find /monty -type f -exec sha256sum '{}' ';' | sort | sha256sum | cut -d' ' -f1",
    )
    return result.stdout.strip()


INSPECTION_COMMAND = (
    "set -eu; "
    "export GIT_OPTIONAL_LOCKS=0; "
    "git -C /monty/worktrees/task status --short; "
    "git -C /monty/worktrees/task diff --no-ext-diff -- README.md; "
    "ps -o pid,comm,args"
)


def inspect_workspace(runner):
    return runner.run(
        "container", "exec", CONTAINER, "sh", "-c", INSPECTION_COMMAND
    ).stdout


def deliver_inspection_result(iteration, output, stream=None):
    stream = stream or sys.stdout
    stream.write(
        json.dumps(
            {"event": "inspection-result", "iteration": iteration, "output": output},
            separators=(",", ":"),
        )
        + "\n"
    )
    stream.flush()


def benchmark_new_container(runner, iterations, owned_containers, owner, owner_label):
    samples = []
    for iteration in range(iterations):
        started = time.perf_counter()
        create_container(runner, owner_label, owned_containers)
        start_container(runner)
        wait_ready(runner, CONTAINER)
        samples.append(time.perf_counter() - started)
        delete_owned_container(runner, owned_containers, owner)
        print(f"create/start {iteration + 1}/{iterations}", flush=True)
    return samples


def benchmark_wake(runner, iterations):
    samples = []
    for iteration in range(iterations):
        started = time.perf_counter()
        runner.run("container", "start", CONTAINER)
        wait_ready(runner, CONTAINER)
        samples.append(time.perf_counter() - started)
        stop(runner, CONTAINER)
        print(f"wake {iteration + 1}/{iterations}", flush=True)
    return samples


def benchmark_inspection(runner, iterations, deliver=deliver_inspection_result):
    samples = {
        "start_readiness": [],
        "inspection_exec": [],
        "result_delivery": [],
        "time_to_result": [],
        "stop": [],
        "total": [],
    }
    first_output = ""
    for iteration in range(iterations):
        started = time.perf_counter()
        runner.run("container", "start", CONTAINER)
        wait_ready(runner, CONTAINER)
        ready = time.perf_counter()
        output = inspect_workspace(runner)
        inspected = time.perf_counter()
        deliver(iteration + 1, output)
        delivered = time.perf_counter()
        stop_started = time.perf_counter()
        stop(runner, CONTAINER)
        stopped = time.perf_counter()
        samples["start_readiness"].append(ready - started)
        samples["inspection_exec"].append(inspected - ready)
        samples["result_delivery"].append(delivered - inspected)
        samples["time_to_result"].append(delivered - started)
        samples["stop"].append(stopped - stop_started)
        samples["total"].append(stopped - started)
        if not first_output:
            first_output = output
        print(f"inspect cycle {iteration + 1}/{iterations}", flush=True)
    return samples, first_output


def write_report(output, facts, samples, inspection_samples):
    with (output / "samples.csv").open("w", newline="", encoding="utf-8") as handle:
        writer = csv.writer(handle)
        writer.writerow(["benchmark", "iteration", "seconds"])
        all_samples = dict(samples)
        all_samples.update(
            {
                f"inspection_{name}": values
                for name, values in inspection_samples.items()
                if name != "total"
            }
        )
        for benchmark, values in all_samples.items():
            for iteration, value in enumerate(values, 1):
                writer.writerow([benchmark, iteration, f"{value:.9f}"])

    facts["benchmarks"] = {name: summary(values) for name, values in samples.items()}
    facts["inspection_components"] = {
        name: summary(values) for name, values in inspection_samples.items()
    }
    facts["startup_acceptance"] = {
        "cached_create_start": facts["benchmarks"]["create_start"],
        "stopped_wake": facts["benchmarks"]["wake"],
        "stopped_wake_to_result": facts["inspection_components"]["time_to_result"],
    }
    facts["inspection_result_delivery"] = {
        "channel": "stdout-jsonl",
        "flush_completed_before_stop": True,
        "included_in_time_to_result": True,
    }
    ordered = sorted(
        range(len(inspection_samples["total"])),
        key=inspection_samples["total"].__getitem__,
    )
    tail_iteration = ordered[math.ceil(0.95 * len(ordered)) - 1]
    phases = {
        name: inspection_samples[name][tail_iteration]
        for name in ("start_readiness", "inspection_exec", "result_delivery", "stop")
    }
    facts["inspection_p95_iteration"] = {
        "iteration": tail_iteration + 1,
        "dominant_phase": max(phases, key=phases.get),
        "phase_ms": {name: round(value * 1000, 3) for name, value in phases.items()},
        "time_to_result_ms": round(
            inspection_samples["time_to_result"][tail_iteration] * 1000, 3
        ),
        "total_ms": round(inspection_samples["total"][tail_iteration] * 1000, 3),
    }
    (output / "benchmark.json").write_text(
        json.dumps(facts, indent=2, sort_keys=True) + "\n", encoding="utf-8"
    )

    target_lines = []
    for name, values in facts["startup_acceptance"].items():
        target = (
            "pass"
            if values["median_ms"] < 1000 and values["p95_ms"] < 2000
            else "miss"
        )
        target_lines.append(
            f"| {name} | {values['iterations']} | {values['minimum_ms']:.3f} | "
            f"{values['median_ms']:.3f} | {values['p95_ms']:.3f} | "
            f"{values['maximum_ms']:.3f} | {target} |"
        )
    component_lines = [
        f"| {name} | {values['minimum_ms']:.3f} | {values['median_ms']:.3f} | "
        f"{values['p95_ms']:.3f} | {values['maximum_ms']:.3f} |"
        for name, values in facts["inspection_components"].items()
    ]
    max_phase = max(
        ("start_readiness", "inspection_exec", "result_delivery", "stop"),
        key=lambda name: facts["inspection_components"][name]["maximum_ms"],
    )
    removal_note = (
        "Verified invalid-tag removal is preserved in "
        "`invalid-fallback-removal.json`."
        if (output / "invalid-fallback-removal.json").exists()
        else "No known invalid fallback tag was present."
    )
    if facts["image_origin"] == "verified-local-cache":
        recovery_note = "The verified local image was reused without a build or pull."
    else:
        recovery_note = (
            "The absent image was restored from a checksum-verified OCI archive; "
            "archive load time is excluded from all warm samples."
        )
    missed_targets = [
        name
        for name, values in facts["startup_acceptance"].items()
        if values["median_ms"] >= 1000 or values["p95_ms"] >= 2000
    ]
    recommendation = (
        f"REVISE startup targets ({', '.join(missed_targets)}) before "
        "production integration"
        if missed_targets
        else "PROCEED WITH THE STOP-OFF-CRITICAL-PATH WORKAROUND"
    )
    runtime_line = next(
        line
        for line in facts["runtime_version"].splitlines()
        if line.startswith("container ")
    )
    report = f"""# Apple container worker benchmark

Recommendation: **{recommendation}**.

- Image: `{facts['image']}@{facts['image_digest']}`
- Image source: `{facts['image_origin']}`
- Runtime: `{runtime_line}`
- Resources: {CPUS} CPUs, {MEMORY} memory, one named ext4 volume
- Init: Apple `--init` forwarding signals to inert `sleep infinity`
- p95 method: nearest rank (`ceil(0.95 × n)`)
- OCI archive: `{facts['image_archive']}`
- Archive SHA-256: `{facts['image_archive_sha256']}`

| path | n | min ms | median ms | p95 ms | max ms | targets¹ |
| --- | ---: | ---: | ---: | ---: | ---: | :---: |
{os.linesep.join(target_lines)}

¹ User-visible gates: median under one second and p95 under two seconds.

| inspection phase | min ms | median ms | p95 ms | max ms |
| --- | ---: | ---: | ---: | ---: |
{os.linesep.join(component_lines)}

Time-to-inspection-result is `time_to_result`; time-to-return-to-stopped is
`total`. The p95-ranked total was iteration
{facts['inspection_p95_iteration']['iteration']} at
{facts['inspection_p95_iteration']['total_ms']:.3f} ms. Its dominant phase was
`{facts['inspection_p95_iteration']['dominant_phase']}` with component timings
{facts['inspection_p95_iteration']['phase_ms']} and result latency
{facts['inspection_p95_iteration']['time_to_result_ms']:.3f} ms.
Across all iterations, result latency topped out at
{facts['inspection_components']['time_to_result']['maximum_ms']:.3f} ms versus
{facts['inspection_components']['total']['maximum_ms']:.3f} ms to return to
stopped. `{max_phase}` owns the remaining outlier tail, reaching
{facts['inspection_components'][max_phase]['maximum_ms']:.3f} ms.
Full-cycle and stop latency are lifecycle diagnostics, not startup gates; the
inspection result is emitted as flushed JSONL before stop begins. `time_to_result`
includes that externally observable delivery; `result_delivery` records its host
write/flush duration.

Cold/setup timings were kept out of warm samples: service start
{facts['cold_setup_seconds']['container_service_start']:.3f}s, invalid-tag removal
{facts['cold_setup_seconds']['invalid_tag_delete']:.3f}s, verified archive load
{facts['cold_setup_seconds']['image_archive_load']:.3f}s, volume creation
{facts['cold_setup_seconds']['volume_create']:.3f}s, initial container start
{facts['cold_setup_seconds']['initial_container_start']:.3f}s, and bundle seed
{facts['cold_setup_seconds']['git_bundle_seed']:.3f}s. Archive save took
{facts['cold_setup_seconds']['image_archive_save']:.3f}s separately. The container
service was already running. The global builder was observed but never started or
stopped by this runner (before={facts['builder_running_before']},
after={facts['builder_running_after']}). Invalid-tag removal time covers this
invocation. {removal_note} {recovery_note}

All isolation and persistence checks passed. The retrieved packet and artifact
are in `retrieved/`; raw configuration and inspection evidence are adjacent to
this report. Direct `container cp` from the named volume was
{'supported' if facts['direct_volume_cp_supported'] else 'not supported; rootfs staging was required'}.
Production gaps are listed in `poc/apple-container-worker/ARCHITECTURE.md`.

Restore the retained image without rebuilding with
`container image load --input {facts['image_archive']}` after verifying
`{facts['image_archive']}.sha256`.
"""
    (output / "REPORT.md").write_text(report, encoding="utf-8")


def self_check():
    assert percentile_95(list(range(1, 21))) == 19
    assert summary([0.001, 0.002, 0.003])["median_ms"] == 2.0
    assert guard_resource_name(CONTAINER) == CONTAINER
    try:
        guard_resource_name("unrelated")
    except ValueError:
        pass
    else:
        raise AssertionError("unsafe resource name was accepted")
    with tempfile.TemporaryDirectory(
        prefix=".lock-check-", dir=Path(__file__).resolve().parent
    ) as temporary:
        target = Path(temporary, "target")
        target.write_text("unchanged\n", encoding="utf-8")
        symlink = Path(temporary, "lock")
        symlink.symlink_to(target)
        try:
            open_lock_file(symlink)
        except RuntimeError:
            pass
        else:
            raise AssertionError("symlink lock file was accepted")
        assert target.read_text(encoding="utf-8") == "unchanged\n"
    volume_mount = {
        "configuration": {
            "mounts": [
                {
                    "destination": "/monty",
                    "source": f"/Users/example/volumes/{VOLUME}/volume.img",
                    "type": {"volume": {"name": VOLUME, "format": "ext4"}},
                }
            ]
        }
    }
    assert verify_mounts(volume_mount)[0]["destination"] == "/monty"
    volume_mount["configuration"]["mounts"][0]["type"]["volume"]["name"] += "-other"
    try:
        verify_mounts(volume_mount)
    except RuntimeError:
        pass
    else:
        raise AssertionError("wrong named volume was accepted")
    image = [
        {
            "configuration": {
                "descriptor": {"digest": "sha256:" + "a" * 64}
            },
            "variants": [
                {
                    "platform": {"architecture": "arm64", "os": "linux"},
                    "config": {
                        "config": {
                            "Cmd": ["sleep", "infinity"],
                            "Env": IMAGE_ENV.copy(),
                            "Labels": {LABEL_KEY: "apple-container-worker-v1"},
                        }
                    },
                }
            ]
        }
    ]
    assert image_digest(image) == "sha256:" + "a" * 64
    assert image_action(None) == "load-archive"
    assert image_action(VERIFIED_DIGEST) == "reuse"
    assert image_action(INVALID_FALLBACK_DIGEST) == "delete-and-load-archive"
    try:
        image_action("sha256:" + "b" * 64)
    except RuntimeError:
        pass
    else:
        raise AssertionError("unexpected image digest was accepted")
    assert verify_built_image(image)["Cmd"] == ["sleep", "infinity"]
    image[0]["variants"][0]["config"]["config"]["Env"].append(
        "CODEX_API_KEY=leaked"
    )
    try:
        verify_built_image(image)
    except RuntimeError:
        pass
    else:
        raise AssertionError("unexpected image environment was accepted")
    image[0]["variants"][0]["config"]["config"]["Env"].pop()
    verify_native_builder({"build": {"rosetta": False}})
    try:
        verify_native_builder({"build": {"rosetta": True}})
    except RuntimeError:
        pass
    else:
        raise AssertionError("Rosetta builder setting was accepted")
    image[0]["variants"][0]["config"]["config"]["Entrypoint"] = ["git"]
    try:
        verify_built_image(image)
    except RuntimeError:
        pass
    else:
        raise AssertionError("fallback image configuration was accepted")
    with tempfile.TemporaryDirectory(
        prefix=".archive-check-", dir=Path(__file__).resolve().parent
    ) as temporary:
        archive = Path(temporary, "image.oci.tar")
        archive.write_bytes(b"verified archive")
        archive.with_name(f"{archive.name}.sha256").write_text(
            f"{file_sha256(archive)}  {archive.name}\n", encoding="utf-8"
        )
        assert verify_image_archive(archive) == archive.resolve()
    events = []

    class FakeRunner:
        def run(self, *args, check=True):
            events.append(args)
            return subprocess.CompletedProcess(args, 0, "inspection\n", "")

    benchmark_inspection(
        FakeRunner(), 1, deliver=lambda iteration, output: events.append("delivered")
    )
    assert events.index("delivered") < next(
        index
        for index, event in enumerate(events)
        if isinstance(event, tuple) and event[:2] == ("container", "stop")
    )
    class LostResponseRunner:
        def run(self, *args, check=True):
            raise RuntimeError("lost response")

    owned = set()
    try:
        create_container(LostResponseRunner(), "owner", owned)
    except RuntimeError:
        pass
    else:
        raise AssertionError("lost create response was accepted")
    assert CONTAINER in owned
    print("self-check passed")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--iterations", type=int, default=20)
    parser.add_argument(
        "--output",
        type=Path,
        default=(
            Path(os.environ["MONTY_WORKER_DIR"])
            / "artifacts"
            / "apple-container-worker-poc"
            if os.environ.get("MONTY_WORKER_DIR")
            else None
        ),
    )
    parser.add_argument(
        "--image-archive",
        type=Path,
        help="checksummed OCI archive used only when the verified image is absent",
    )
    parser.add_argument("--self-check", action="store_true")
    args = parser.parse_args()
    if args.self_check:
        self_check()
        return
    if args.iterations < 20:
        parser.error("--iterations must be at least 20")
    if args.output is None:
        parser.error("--output is required outside a Monty worker")

    for resource in (IMAGE, CONTAINER, REUSE_CONTAINER, VOLUME):
        guard_resource_name(resource)

    lock = open_lock_file(Path(tempfile.gettempdir(), f"{CONTAINER}.lock"))
    try:
        fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
    except BlockingIOError as error:
        raise RuntimeError("another Apple container worker PoC is running") from error
    lock.seek(0)
    lock.truncate()
    lock.write(f"{os.getpid()}\n")
    lock.flush()

    source_dir = Path(__file__).resolve().parent
    repo_root = source_dir.parents[1]
    output = args.output.resolve()
    if output == repo_root or repo_root in output.parents:
        parser.error("--output must be outside the ephemeral code worktree")
    if os.environ.get("MONTY_WORKER_DIR"):
        artifact_root = (Path(os.environ["MONTY_WORKER_DIR"]) / "artifacts").resolve()
        if output != artifact_root and artifact_root not in output.parents:
            parser.error("--output must be under $MONTY_WORKER_DIR/artifacts")
    try:
        output.mkdir(parents=True)
    except FileExistsError:
        parser.error("--output must not already exist; use a fresh path")
    command_log = []
    runner = Runner(command_log)
    owned_containers = set()
    owned_volume = False
    owner = f"apple-container-worker-v1-{secrets.token_hex(16)}"
    owner_label = f"{LABEL_KEY}={owner}"

    status = runner.run("container", "system", "status", check=False)
    system_start_seconds = 0.0
    if status.returncode:
        raise RuntimeError(
            "Apple container service must already be running with "
            "build.rosetta=false; start/reconfigure it outside this PoC"
        )

    existing_containers = resource_ids(runner, "list", "--all")
    for name in (CONTAINER, REUSE_CONTAINER):
        if name in existing_containers:
            raise RuntimeError(f"refusing pre-existing container: {name}")
    if VOLUME in resource_ids(runner, "volume", "list"):
        raise RuntimeError(f"refusing pre-existing volume: {VOLUME}")

    properties_raw = runner.run(
        "container", "system", "property", "list", "--format", "json"
    ).stdout
    verify_native_builder(json.loads(properties_raw))
    (output / "system-properties.json").write_text(
        properties_raw, encoding="utf-8"
    )
    builder_running_before = builder_is_running(runner)

    invalid_tag_delete_seconds = 0.0
    image_archive_load_seconds = 0.0
    image_origin = "verified-local-cache"
    existing_image = runner.run(
        "container", "image", "inspect", IMAGE, check=False
    )
    if existing_image.returncode == 0:
        (output / "prebuild-image-inspect.json").write_text(
            existing_image.stdout, encoding="utf-8"
        )
        existing_data = json.loads(existing_image.stdout)
        existing_digest = image_digest(existing_data)
        action = image_action(existing_digest)
        if action == "reuse":
            verify_built_image(existing_data)
            (output / "prebuild-image-action.log").write_text(
                f"retained previously verified image {IMAGE}@{existing_digest}\n",
                encoding="utf-8",
            )
    else:
        if "image not found" not in existing_image.stderr.lower():
            raise RuntimeError(
                f"cannot safely inspect pre-existing image tag: {existing_image.stderr}"
            )
        (output / "prebuild-image-inspect.json").write_text(
            "[]\n", encoding="utf-8"
        )
        (output / "prebuild-image-action.log").write_text(
            f"image tag was absent: {IMAGE}\n", encoding="utf-8"
        )
        action = image_action(None)

    archive_to_load = (
        verify_image_archive(args.image_archive) if action != "reuse" else None
    )
    if action == "delete-and-load-archive":
        (output / "invalid-fallback-image-inspect.json").write_text(
            existing_image.stdout, encoding="utf-8"
        )
        delete_started = time.perf_counter()
        deleted = runner.run("container", "image", "delete", IMAGE)
        invalid_tag_delete_seconds = time.perf_counter() - delete_started
        (output / "invalid-image-delete.log").write_text(
            deleted.stdout + deleted.stderr, encoding="utf-8"
        )
        if runner.run(
            "container", "image", "inspect", IMAGE, check=False
        ).returncode == 0:
            raise RuntimeError(f"invalid fallback tag still exists: {IMAGE}")
        (output / "invalid-fallback-removal.json").write_text(
            json.dumps(
                {
                    "deleted_digest": existing_digest,
                    "image": IMAGE,
                    "verified_absent_after_delete": True,
                },
                indent=2,
                sort_keys=True,
            )
            + "\n",
            encoding="utf-8",
        )
    if archive_to_load:
        load_started = time.perf_counter()
        loaded = runner.run("container", "image", "load", "--input", archive_to_load)
        image_archive_load_seconds = time.perf_counter() - load_started
        image_origin = "verified-oci-archive"
        (output / "image-recovery.log").write_text(
            loaded.stdout + loaded.stderr, encoding="utf-8"
        )

    image_raw = runner.run("container", "image", "inspect", IMAGE).stdout
    image_data = json.loads(image_raw)
    verify_built_image(image_data)
    (output / "image-inspect.json").write_text(image_raw, encoding="utf-8")
    digest = image_digest(image_data)
    if digest != VERIFIED_DIGEST:
        raise RuntimeError(
            f"image digest {digest} does not match verified digest {VERIFIED_DIGEST}"
        )

    try:
        volume_started = time.perf_counter()
        owned_volume = True
        runner.run(
            "container",
            "volume",
            "create",
            "--label",
            owner_label,
            "--opt",
            "size=2g",
            VOLUME,
        )
        volume_create_seconds = time.perf_counter() - volume_started
        volume_raw = runner.run("container", "volume", "inspect", VOLUME).stdout
        (output / "volume-inspect.json").write_text(volume_raw, encoding="utf-8")

        initial_started = time.perf_counter()
        create_container(runner, owner_label, owned_containers)
        start_container(runner)
        wait_ready(runner, CONTAINER)
        initial_container_seconds = time.perf_counter() - initial_started

        seed_started = time.perf_counter()
        expected_head = runner.run(
            "git", "-C", repo_root, "rev-parse", "HEAD"
        ).stdout.strip()
        expected_report = {
            "schema": "monty.worker-report/v1",
            "state": "awaiting-host-review",
            "head": expected_head,
        }
        with tempfile.TemporaryDirectory(prefix="seed-", dir=output) as temporary:
            bundle = Path(temporary) / "monty.bundle"
            runner.run("git", "-C", repo_root, "bundle", "create", bundle, "HEAD")
            runner.run("git", "bundle", "verify", bundle)
            runner.run(
                "container", "cp", bundle, f"{CONTAINER}:/monty-context.bundle"
            )
            runner.run(
                "container",
                "exec",
                CONTAINER,
                "sh",
                "-c",
                "set -eu; "
                "mkdir -p /monty/home/.cache /monty/repos /monty/worktrees "
                "/monty/context /monty/outbox /monty/artifacts; "
                "mv /monty-context.bundle /monty/context/monty.bundle; "
                "git clone /monty/context/monty.bundle /monty/repos/monty; "
                "git -C /monty/repos/monty worktree add -b monty-poc/change "
                "/monty/worktrees/task HEAD; "
                "printf '\\nApple container PoC tracked change.\\n' "
                ">> /monty/worktrees/task/README.md; "
                "printf 'untracked worker artifact\\n' "
                "> /monty/worktrees/task/poc-untracked.txt; "
                "head=$(git -C /monty/worktrees/task rev-parse HEAD); "
                "printf '{\"schema\":\"monty.worker-report/v1\","
                "\"state\":\"awaiting-host-review\",\"head\":\"%s\"}\\n' "
                "\"$head\" > /monty/outbox/report-v1.json; "
                "printf 'artifact crossed the VM boundary\\n' "
                "> /monty/artifacts/proof.txt; "
                "cp /monty/artifacts/proof.txt /monty/outbox/artifact.txt",
            )
        seed_seconds = time.perf_counter() - seed_started

        before_restart = volume_hash(runner)
        before_status = inspect_workspace(runner)
        stop(runner, CONTAINER)
        runner.run("container", "start", CONTAINER)
        wait_ready(runner, CONTAINER)
        after_restart = volume_hash(runner)
        if before_restart != after_restart:
            raise RuntimeError("private volume content changed across stop/start")
        process_evidence = runner.run(
            "container", "exec", CONTAINER, "ps", "-o", "pid,comm,args"
        ).stdout
        pid_1_is_inert_init = any(
            len(fields) == 3
            and fields[0] == "1"
            and fields[2].split() == ["/.cz-init", "--", "sleep", "infinity"]
            for fields in (
                line.split(maxsplit=2) for line in process_evidence.splitlines()
            )
        )
        if not pid_1_is_inert_init:
            raise RuntimeError(
                "container PID 1 is not Apple's init wrapping inert sleep:\n"
                f"{process_evidence}"
            )
        isolation_evidence = runner.run(
            "container",
            "exec",
            CONTAINER,
            "sh",
            "-c",
            "set -eu; test ! -e /Users; test ! -S /run/container.sock; "
            "test ! -S /var/run/docker.sock; test -d /monty/home; "
            "test -d /monty/repos; test -d /monty/worktrees; "
            "test -d /monty/context; test -d /monty/outbox; "
            "test -d /monty/artifacts; cat /proc/mounts",
        ).stdout
        (output / "inspection-before.txt").write_text(
            before_status, encoding="utf-8"
        )
        (output / "process-after-restart.txt").write_text(
            process_evidence, encoding="utf-8"
        )
        (output / "guest-mounts.txt").write_text(
            isolation_evidence, encoding="utf-8"
        )
        delete_owned_container(runner, owned_containers, owner)

        samples = {}
        samples["create_start"] = benchmark_new_container(
            runner, args.iterations, owned_containers, owner, owner_label
        )

        create_container(runner, owner_label, owned_containers)
        start_container(runner)
        wait_ready(runner, CONTAINER)
        if volume_hash(runner) != before_restart:
            raise RuntimeError("private volume content changed across container recreation")
        stop(runner, CONTAINER)

        samples["wake"] = benchmark_wake(runner, args.iterations)
        pre_inspection_hash = before_restart
        inspection_samples, first_inspection = benchmark_inspection(
            runner, args.iterations
        )
        samples["inspection_cycle"] = inspection_samples["total"]

        runner.run("container", "start", CONTAINER)
        wait_ready(runner, CONTAINER)
        post_inspection_hash = volume_hash(runner)
        if pre_inspection_hash != post_inspection_hash:
            raise RuntimeError("read-only inspection changed private volume content")
        final_inspection = inspect_workspace(runner)
        (output / "inspection-cycle-first.txt").write_text(
            first_inspection, encoding="utf-8"
        )
        (output / "inspection-after.txt").write_text(
            final_inspection, encoding="utf-8"
        )

        container_raw = runner.run("container", "inspect", CONTAINER).stdout
        container_data = json.loads(container_raw)
        mounts = verify_mounts(container_data)
        verify_container_configuration(container_data, digest, owner)
        (output / "container-inspect.json").write_text(
            container_raw, encoding="utf-8"
        )
        (output / "isolation.json").write_text(
            json.dumps(
                {
                    "checks": {
                        "host_users_path_absent": True,
                        "container_control_socket_absent": True,
                        "docker_socket_absent": True,
                        "only_named_task_mount": True,
                        "no_broad_host_integrations": True,
                        "native_image_digest_matches": True,
                        "volume_survived_restart": before_restart == after_restart,
                        "inspection_content_unchanged": (
                            pre_inspection_hash == post_inspection_hash
                        ),
                        "pid_1_is_inert_init": pid_1_is_inert_init,
                    },
                    "mounts": mounts,
                    "volume_sha256": post_inspection_hash,
                },
                indent=2,
                sort_keys=True,
            )
            + "\n",
            encoding="utf-8",
        )

        retrieved = output / "retrieved"
        retrieved.mkdir(exist_ok=True)
        direct_copy_errors = []
        for source, destination in (
            ("report-v1.json", retrieved / "report-v1.json"),
            ("artifact.txt", retrieved / "artifact.txt"),
        ):
            if destination.exists():
                destination.unlink()
            direct_copy = runner.run(
                "container",
                "cp",
                f"{CONTAINER}:/monty/outbox/{source}",
                destination,
                check=False,
            )
            if direct_copy.returncode:
                direct_copy_errors.append(
                    f"$ container cp {CONTAINER}:/monty/outbox/{source} {destination}\n"
                    f"{direct_copy.stdout}{direct_copy.stderr}"
                )
                staged = f"/tmp/{PREFIX}{source}"
                runner.run(
                    "container",
                    "exec",
                    CONTAINER,
                    "cp",
                    f"/monty/outbox/{source}",
                    staged,
                )
                runner.run(
                    "container", "cp", f"{CONTAINER}:{staged}", destination
                )
        (output / "direct-volume-copy.log").write_text(
            "\n".join(direct_copy_errors)
            if direct_copy_errors
            else "direct named-volume copy succeeded\n",
            encoding="utf-8",
        )
        report_packet = json.loads(
            (retrieved / "report-v1.json").read_text(encoding="utf-8")
        )
        if report_packet != expected_report:
            raise RuntimeError("retrieved worker report does not match its source")
        if (retrieved / "artifact.txt").read_bytes() != ARTIFACT_CONTENT:
            raise RuntimeError("retrieved artifact does not match its source")

        delete_owned_container(runner, owned_containers, owner)
        create_container(
            runner,
            owner_label,
            owned_containers,
            name=REUSE_CONTAINER,
            mount_volume=False,
        )
        start_container(runner, REUSE_CONTAINER)
        wait_ready(runner, REUSE_CONTAINER)
        reuse_raw = runner.run("container", "inspect", REUSE_CONTAINER).stdout
        reuse_data = json.loads(reuse_raw)
        verify_container_configuration(reuse_data, digest, owner)
        if mount_records(reuse_data):
            raise RuntimeError("cached-image reuse container unexpectedly has mounts")
        current_image_raw = runner.run(
            "container", "image", "inspect", IMAGE
        ).stdout
        if image_digest(json.loads(current_image_raw)) != digest:
            raise RuntimeError("retained image tag digest changed before reuse proof")
        (output / "cached-image-reuse.json").write_text(
            reuse_raw, encoding="utf-8"
        )
        delete_owned_container(
            runner, owned_containers, owner, name=REUSE_CONTAINER
        )

        archive = output / "monty-poc-apple-worker-1-linux-arm64.oci.tar"
        archive.unlink(missing_ok=True)
        archive_started = time.perf_counter()
        runner.run(
            "container",
            "image",
            "save",
            "--platform",
            "linux/arm64",
            "--output",
            archive,
            IMAGE,
        )
        archive_seconds = time.perf_counter() - archive_started
        archive_sha256 = file_sha256(archive)
        (output / f"{archive.name}.sha256").write_text(
            f"{archive_sha256}  {archive.name}\n", encoding="utf-8"
        )

        runtime_version = runner.run("container", "system", "version").stdout.strip()
        builder_running_after = builder_is_running(runner)
        facts = {
            "host": {
                "machine": platform.machine(),
                "macos": platform.mac_ver()[0],
            },
            "runtime_version": runtime_version,
            "image": IMAGE,
            "image_digest": digest,
            "image_origin": image_origin,
            "image_archive": str(archive),
            "image_archive_sha256": archive_sha256,
            "image_reuse_verified": True,
            "direct_volume_cp_supported": not direct_copy_errors,
            "builder_running_before": builder_running_before,
            "builder_running_after": builder_running_after,
            "cpus": CPUS,
            "memory": MEMORY,
            "volume": VOLUME,
            "volume_size": "2g",
            "iterations": args.iterations,
            "p95_method": "nearest-rank",
            "system_was_running": True,
            "cold_setup_seconds": {
                "container_service_start": round(system_start_seconds, 6),
                "invalid_tag_delete": round(invalid_tag_delete_seconds, 6),
                "image_archive_load": round(image_archive_load_seconds, 6),
                "volume_create": round(volume_create_seconds, 6),
                "initial_container_start": round(initial_container_seconds, 6),
                "git_bundle_seed": round(seed_seconds, 6),
                "image_archive_save": round(archive_seconds, 6),
            },
            "exact_commands": {
                "create_start": (
                    f"container create --name {CONTAINER} --cpus {CPUS} "
                    f"--memory {MEMORY} --platform linux/arm64 --label "
                    f"{owner_label} --init "
                    f"--env HOME=/monty/home --mount "
                    f"type=volume,source={VOLUME},target=/monty --entrypoint sleep "
                    f"{IMAGE} infinity; container start {CONTAINER}; "
                    f"container exec {CONTAINER} true"
                ),
                "wake": (
                    f"container start {CONTAINER}; container exec {CONTAINER} true"
                ),
                "inspection_cycle": (
                    f"container start {CONTAINER}; container exec {CONTAINER} "
                    f"true; container exec {CONTAINER} "
                    f"sh -c {shlex.quote(INSPECTION_COMMAND)}; "
                    "host emit inspection-result JSONL and flush stdout; "
                    f"container stop --time 1 {CONTAINER}"
                ),
                "already_running_inspection": (
                    f"container exec {CONTAINER} "
                    f"sh -c {shlex.quote(INSPECTION_COMMAND)}"
                ),
            },
        }
        write_report(output, facts, samples, inspection_samples)
        print(f"image: {IMAGE}@{digest}")
        for name, values in samples.items():
            print(f"{name}: {summary(values)}")
    finally:
        primary_error = sys.exc_info()[1]
        cleanup_errors = []
        try:
            (output / "commands.log").write_text(
                "\n".join(command_log) + "\n", encoding="utf-8"
            )
        except Exception as error:
            cleanup_errors.append(f"cannot persist command log: {error}")
        for name in tuple(owned_containers):
            try:
                delete_owned_container(
                    runner, owned_containers, owner, name=name
                )
            except Exception as error:
                cleanup_errors.append(str(error))
        try:
            owned_volume = delete_owned_volume(runner, owned_volume, owner)
        except Exception as error:
            cleanup_errors.append(str(error))
        remaining_containers = None
        try:
            remaining_containers = sorted(
                {CONTAINER, REUSE_CONTAINER}
                & resource_ids(runner, "list", "--all")
            )
        except Exception as error:
            cleanup_errors.append(f"cannot inspect remaining containers: {error}")
        remaining_volumes = None
        try:
            remaining_volumes = sorted(
                {VOLUME} & resource_ids(runner, "volume", "list")
            )
        except Exception as error:
            cleanup_errors.append(f"cannot inspect remaining volumes: {error}")
        retained_digest = None
        try:
            retained_image = runner.run(
                "container", "image", "inspect", IMAGE, check=False
            )
            if retained_image.returncode == 0:
                retained_digest = image_digest(json.loads(retained_image.stdout))
        except Exception as error:
            cleanup_errors.append(f"cannot inspect retained image: {error}")
        builder_running = None
        try:
            builder_running = builder_is_running(runner)
        except Exception as error:
            cleanup_errors.append(f"cannot inspect builder status: {error}")
        if remaining_containers or remaining_volumes:
            cleanup_errors.append(
                "cleanup left exact PoC resources behind: "
                f"containers={remaining_containers}, volumes={remaining_volumes}"
            )
        if retained_digest != digest:
            cleanup_errors.append(
                f"retained image digest is {retained_digest!r}, expected {digest!r}"
            )
        try:
            (output / "commands.log").write_text(
                "\n".join(command_log) + "\n", encoding="utf-8"
            )
        except Exception as error:
            cleanup_errors.append(f"cannot update command log: {error}")
        try:
            (output / "cleanup.json").write_text(
                json.dumps(
                    {
                        "builder_running_before": builder_running_before,
                        "builder_running_after": builder_running,
                        "builder_modified": False,
                        "remaining_containers": remaining_containers,
                        "remaining_volumes": remaining_volumes,
                        "retained_image": IMAGE,
                        "retained_image_digest": retained_digest,
                        "primary_error": (
                            str(primary_error) if primary_error else None
                        ),
                        "cleanup_errors": cleanup_errors,
                    },
                    indent=2,
                    sort_keys=True,
                )
                + "\n",
                encoding="utf-8",
            )
        except Exception as error:
            cleanup_errors.append(f"cannot persist cleanup evidence: {error}")
        if cleanup_errors and primary_error is None:
            raise RuntimeError("; ".join(cleanup_errors))


if __name__ == "__main__":
    main()
