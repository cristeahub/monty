#!/usr/bin/env python3
"""Read-only provider boundary. One JSON request on stdin, one versioned result.
All scheduling, durable commits and analysis belong to Monty, not this helper.
"""
import copy
import hashlib
import json
import os
import re
import stat
import subprocess
import sys
import time
import tempfile
from pathlib import Path
import urllib.parse


class SourceError(Exception):
    def __init__(self, code, retry=0):
        self.code, self.retry = code, retry


def private_file(path):
    st = os.lstat(path)
    if not stat.S_ISREG(st.st_mode) or st.st_mode & 0o077 or st.st_uid != os.getuid():
        raise SourceError("credential_file must be an owned regular file with mode 0600")
    return path


def digest(value):
    return hashlib.sha256(json.dumps(value, sort_keys=True).encode()).hexdigest()


def observation(native, conversation, timestamp, author, content, url, deleted=False):
    return dict(native_id=str(native), conversation=conversation, source_time=float(timestamp),
                author=str(author), content=content.encode("utf-8")[:16000].decode("utf-8", errors="ignore"), url=url, deleted=deleted,
                revision=digest([content, deleted]))


def reconcile_page(state, normalized, complete):
    """Compare revisions and infer disappearances only after a complete scan."""
    previous = {o["native_id"]: o.get("revision") for o in state.get("previous", []) + state["seen"]}
    items = [o for o in normalized if previous.get(o["native_id"]) != o["revision"]]
    state["seen"].extend({k: o[k] for k in ("native_id", "conversation", "source_time", "url", "revision")} for o in normalized)
    if complete:
        seen = {o["native_id"] for o in state["seen"]}
        for prior in state.get("previous", []):
            if prior["source_time"] >= state["oldest"] and prior["native_id"] not in seen:
                items.append(observation(prior["native_id"], prior["conversation"], prior["source_time"], "unknown", "", prior["url"], True))
        state = {"through": state["until"], "previous": state["seen"]}
    return items, state


class Slack:
    METHODS = {"auth.test", "conversations.info", "conversations.list",
               "conversations.history", "conversations.replies"}

    def __init__(self, config):
        self.config = config
        self.cli = config.get("slack_cli") or "slack"
        token = open(private_file(config["credential_file"])).read().strip()
        if not token or "\n" in token:
            raise SourceError("invalid Slack token file")
        self.env = {k: v for k, v in os.environ.items() if not k.startswith("SLACK_")}
        self.env["SLACK_BOT_TOKEN"] = token
        self.env["SLACK_SKIP_UPDATE"] = "1"
        # Capability check, not a developer login or an app installation.
        with tempfile.TemporaryDirectory(prefix="slack-help-", dir=".") as config_dir:
            help_result = subprocess.run([self.cli, "api", "--help", "--skip-update", "--no-color",
                                          "--config-dir", os.path.abspath(config_dir)],
                                         stdin=subprocess.DEVNULL, capture_output=True, timeout=15, env=self.env)
        if help_result.returncode or b"--include" not in help_result.stdout or b"--json" not in help_result.stdout:
            raise SourceError("Slack CLI needs api --include and --json support")
        self.identity = self.api("auth.test")
        if self.identity.get("team_id") != config["account"]:
            raise SourceError("Slack token belongs to a different account")
        self.base = self.identity.get("url", "")
        if not self.base.startswith("https://"):
            raise SourceError("Slack auth.test returned no HTTPS workspace URL")

    def api(self, method, **params):
        assert method in self.METHODS
        # RawRequest in Slack CLI retries 429/503 internally before --include
        # prints headers. Its private debug log exposes that sleep. Stop the
        # child immediately and let Monty honor the delay across process restarts.
        with tempfile.TemporaryDirectory(prefix="slack-", dir=".") as config_dir:
            proc = subprocess.Popen([self.cli, "api", method, "--json", json.dumps(params),
                "--include", "--skip-update", "--no-color", "--config-dir", os.path.abspath(config_dir)],
                stdin=subprocess.DEVNULL, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=self.env)
            deadline = time.monotonic() + 30
            try:
                while True:
                    try:
                        stdout, stderr = proc.communicate(timeout=0.1)
                        break
                    except subprocess.TimeoutExpired:
                        for log in Path(config_dir).glob("logs/*.log"):
                            if log.stat().st_size > 8000000:
                                raise SourceError("Slack CLI log exceeded output budget")
                            retry = re.search(r"responded with status (?:429|503)\. Retrying request in ([0-9.hms]+)", log.read_text(errors="replace"))
                            if retry:
                                delay = sum(float(n) * {"h":3600,"m":60,"s":1}[unit]
                                            for n, unit in re.findall(r"([0-9.]+)([hms])", retry[1]))
                                raise SourceError("Slack rate limited", max(1, int(delay + 1)))
                        if time.monotonic() >= deadline:
                            raise SourceError("Slack CLI timed out; coverage incomplete", 60)
            finally:
                if proc.poll() is None:
                    proc.kill()
                    proc.communicate()
            returncode = proc.returncode
        # --include emits HTTP <status>, headers, blank line, raw JSON. API-level
        # {ok:false} can exit zero; HTTP 429 exits nonzero but carries Retry-After.
        raw = stdout.decode("utf-8").replace("\r\n", "\n")
        headers, sep, body = raw.partition("\n\n")
        if not sep or not headers.startswith("HTTP "):
            raise SourceError("unexpected Slack CLI output; capability/version mismatch")
        retry = re.search(r"(?im)^retry-after:\s*(\d+)", headers)
        status = int(headers.splitlines()[0].split()[1])
        if status == 429:
            raise SourceError("Slack rate limited", int(retry[1]) if retry else 60)
        if returncode or not 200 <= status < 300:
            raise SourceError("Slack HTTP/CLI request failed")
        data = json.loads(body)
        if data.get("ok") is not True:
            code = data.get("error", "unknown_error")
            code = code if re.fullmatch(r"[a-z_]{1,80}", code) else "unknown_error"
            raise SourceError("Slack API: " + code, 60 if code == "ratelimited" else 0)
        return data

    def check(self):
        for channel in self.config["channels"]:
            self.api("conversations.info", channel=channel)
        if self.config["include_dms"]:
            self.api("conversations.list", types="im,mpim", limit=1)
        return {"account": self.config["account"], "user": self.identity.get("user_id"),
                "capabilities": ["history", "thread_reads", "bounded_reconciliation"],
                "coverage": "Selected channels verified; history and replies permissions are checked during collection."}

    def normalize(self, channel, message):
        ts = message["ts"]
        deleted = message.get("subtype") == "tombstone"
        item = observation(channel + ":" + ts, channel + ":" + message.get("thread_ts", ts),
                           ts, message.get("user", message.get("bot_id", "unknown")),
                           message.get("text", ""), self.base.rstrip("/") + "/archives/" +
                           channel + "/p" + ts.replace(".", ""), deleted)
        item["revision"] = digest([item["revision"], message.get("edited", {}).get("ts")])
        return item

    def collect(self, checkpoint, now, budget):
        cp = copy.deepcopy(checkpoint or {})
        cp.setdefault("channels", {})
        cp.setdefault("threads", {})
        cp.setdefault("turn", 0)
        items, error, retry, pages = [], None, 0, 0
        completed = set()
        try:
            # DM discovery is paginated and retained; never lists public/private
            # channels to silently expand the user's explicitly selected scope.
            if self.config["include_dms"]:
                try:
                    data = self.api("conversations.list", types="im,mpim", limit=200,
                                    cursor=cp.get("dm_cursor", ""))
                except SourceError as ex:
                    if ex.code == "Slack API: invalid_cursor" and cp.get("dm_cursor"):
                        cp["dm_cursor"] = ""
                        raise SourceError("Slack DM cursor expired; discovery will restart")
                    raise
                cp["dm_cursor"] = data.get("response_metadata", {}).get("next_cursor", "")
                for c in data["channels"]:
                    cp["channels"].setdefault(c["id"], {})
                budget -= 1
            for channel in self.config["channels"]:
                cp["channels"].setdefault(channel, {})
            # Round-robin pages across both history and known roots. Even roots
            # that had no replies are revisited, including ones older than lookback.
            jobs = [("history", k) for k in cp["channels"]] + [("thread", k) for k in cp["threads"]]
            for _ in range(budget):
                if not jobs:
                    break
                kind, key = jobs[cp["turn"] % len(jobs)]
                cp["turn"] += 1
                if (kind, key) in completed:
                    if len(completed) >= len(jobs):
                        break
                    continue
                state = copy.deepcopy(cp["channels"][key] if kind == "history" else cp["threads"][key])
                if "until" not in state:
                    state["until"] = now
                    state["oldest"] = min(state.get("through", now - self.config["lookback_hours"] * 3600),
                                          now - self.config["retention_days"] * 86400) if state.get("through") else now - self.config["lookback_hours"] * 3600
                    state["seen"] = []
                channel, root = (key, None) if kind == "history" else key.split(":", 1)
                params = dict(channel=channel, limit=100, cursor=state.get("cursor", ""), latest=str(state["until"]), inclusive=True)
                if root is None:
                    params["oldest"] = str(state["oldest"])
                    method = "conversations.history"
                else:
                    params["ts"] = root
                    method = "conversations.replies"
                try:
                    data = self.api(method, **params)
                except SourceError as ex:
                    if ex.code == "Slack API: invalid_cursor" and state.get("cursor"):
                        # Replay the same bounded interval, retaining the prior
                        # completed scan for deletion reconciliation.
                        state.pop("cursor")
                        state["seen"] = []
                        cp["channels" if kind == "history" else "threads"][key] = state
                        raise SourceError("Slack cursor expired; covered interval will restart")
                    if root and ex.code.endswith("thread_not_found"):
                        for prior in state.get("previous", []):
                            items.append(observation(prior["native_id"], prior["conversation"], prior["source_time"], "unknown", "", prior["url"], True))
                        del cp["threads"][key]
                        jobs.remove((kind,key))
                        continue
                    raise
                pages += 1
                if data.get("is_limited"):
                    cp["history_limited"] = True
                normalized = [self.normalize(channel, m) for m in data["messages"]]
                for m in data["messages"]:
                    if "thread_ts" not in m or m["thread_ts"] == m["ts"]:
                        thread = channel + ":" + m["ts"]
                        if thread not in cp["threads"] and len(cp["threads"]) < self.config["max_threads"]:
                            cp["threads"][thread] = {}
                            jobs.append(("thread",thread))
                        elif thread not in cp["threads"]:
                            cp["thread_limit_hit"] = True
                cursor = data.get("response_metadata", {}).get("next_cursor", "")
                if data.get("has_more") and not cursor:
                    raise SourceError("Slack incomplete page without cursor; checkpoint retained")
                state["cursor"] = cursor
                changes, state = reconcile_page(state, normalized, complete=not cursor)
                items.extend(changes)
                if not cursor:
                    completed.add((kind, key))
                # Bound checkpoint size as well as the observation store. A full
                # backlog is recoverable by raising the configured cap.
                if len(state.get("seen", state.get("previous", []))) > self.config["max_observations"]:
                    raise SourceError("Slack reconciliation capacity reached; increase max_observations")
                cp["channels" if kind == "history" else "threads"][key] = state
        except SourceError as ex:
            error, retry = ex.code, ex.retry
        coverage = "Polling selected channels and discovered DMs; rotating known thread reads. Edits/deletions reconciled within retention. Unknown roots older than initial lookback are not covered."
        if cp.get("dm_cursor") or any("until" in s for s in cp["channels"].values()):
            coverage += " History/DM pagination remains pending."
        if cp.get("thread_limit_hit"):
            coverage += " Thread index limit reached: some roots are not monitored; increase max_threads and retriage/reconfigure."
        if cp.get("history_limited"):
            coverage += " Provider reports restricted message history."
        return result(items, cp, coverage, error, retry, pages, self.identity.get("user_id"))


class Zulip:
    def __init__(self, config):
        import zulip
        self.config = config
        self.client = zulip.Client(config_file=private_file(config["credential_file"]), retry_on_errors=False, insecure=False)
        self.base = self.client.base_url.rstrip("/")
        # Client base_url includes /api; the account is the configured realm URL.
        realm = self.base.removesuffix("/api").rstrip("/")
        if realm != config["account"].rstrip("/") or not realm.startswith("https://"):
            raise SourceError("Zulip credentials belong to a different HTTPS realm")
        self.realm = realm
        self.identity = self.checked(self.client.get_profile())

    @staticmethod
    def checked(data):
        if data.get("result") != "success":
            code = data.get("code", "API_ERROR")
            code = code if re.fullmatch(r"[A-Z_]{1,80}", code) else "API_ERROR"
            raise SourceError("Zulip API: " + code, data.get("retry-after", 60) if code == "RATE_LIMIT_HIT" else 0)
        return data

    def check(self):
        data = self.checked(self.client.get_subscriptions())
        accessible = {str(c["stream_id"]) for c in data["subscriptions"]}
        if not set(self.config["channels"]) <= accessible:
            raise SourceError("selected Zulip stream IDs must be subscribed and accessible")
        return {"account": self.realm, "user": str(self.identity["user_id"]),
                "capabilities": ["history", "topic_reads", "bounded_reconciliation"],
                "coverage": "Selected subscriptions verified; DMs limited to authenticated user."}

    def normalize(self, m):
        direct = m["type"] == "private"
        conversation = ("dm:" + ",".join(str(u["id"]) for u in sorted(m["display_recipient"], key=lambda u:u["id"]))) if direct else str(m["stream_id"]) + ":" + m["subject"]
        item = observation(m["id"], conversation, m["timestamp"], m["sender_id"], m["content"],
                           self.realm + "/#narrow/id/" + str(m["id"]))
        item["revision"] = digest([item["revision"], m.get("last_edit_timestamp")])
        return item

    def collect(self, checkpoint, now, budget):
        cp = copy.deepcopy(checkpoint or {"scopes": {}, "turn": 0})
        scopes = list(self.config["channels"]) + (["dm"] if self.config["include_dms"] else [])
        items, error, retry, pages = [], None, 0, 0
        completed = set()
        try:
            for _ in range(budget):
                scope = scopes[cp["turn"] % len(scopes)]
                cp["turn"] += 1
                if scope in completed:
                    if len(completed) >= len(scopes):
                        break
                    continue
                state = copy.deepcopy(cp["scopes"].get(scope, {}))
                if "until" not in state:
                    state["until"] = now
                    state["oldest"] = min(state.get("through", now), now - self.config["retention_days"]*86400) if state.get("through") else now - self.config["lookback_hours"]*3600
                    state["seen"] = []
                    state["anchor"] = "newest"
                narrow = [{"operator":"is", "operand":"dm"}] if scope == "dm" else [{"operator":"stream", "operand":int(scope)}]
                data = self.checked(self.client.get_messages(dict(narrow=narrow, anchor=state["anchor"],
                    num_before=100, num_after=0, include_anchor=state["anchor"] == "newest", apply_markdown=False)))
                pages += 1
                if data.get("history_limited"):
                    cp["history_limited"] = True
                messages = data["messages"]
                normalized = [self.normalize(m) for m in messages if m["timestamp"] >= state["oldest"]]
                complete = data.get("found_oldest") or not messages or messages[0]["timestamp"] < state["oldest"]
                changes, state = reconcile_page(state, normalized, complete)
                items.extend(changes)
                if len(state.get("seen", state.get("previous", []))) > self.config["max_observations"]:
                    raise SourceError("Zulip reconciliation capacity reached; increase max_observations")
                if complete:
                    completed.add(scope)
                else:
                    state["anchor"] = messages[0]["id"]
                cp["scopes"][scope] = state
        except SourceError as ex:
            error, retry = ex.code, ex.retry
        coverage = "Selected streams and DMs, including new replies in old topics. Rolling history reconciliation: edits/deletions outside retention are not covered; disappeared messages may be deleted or inaccessible."
        if any("until" in s for s in cp["scopes"].values()):
            coverage += " History pagination remains pending."
        if cp.get("history_limited"):
            coverage += " Provider reports restricted message history."
        return result(items, cp, coverage, error, retry, pages, self.identity.get("user_id"))


def result(items, checkpoint, coverage, error=None, retry=0, pages=0, user=None):
    # Keep the latest revision for a message encountered in both history/thread reads.
    unique = {o["native_id"]:o for o in items}
    return dict(schema=1, observations=list(unique.values()), checkpoint=checkpoint,
                coverage=coverage, error=error, retry_after=retry, successful_pages=pages, user=str(user))


def main():
    request = json.load(sys.stdin)
    if request["schema"] != 1:
        raise SourceError("unsupported adapter schema")
    config = request["config"]
    source = {"slack":Slack, "zulip":Zulip}[config["source"]](config)
    if request["operation"] == "check":
        return dict(schema=1, **source.check())
    if request["operation"] != "collect":
        raise SourceError("unsupported operation")
    return source.collect(request.get("checkpoint"), request["now"], config["max_pages"])


if __name__ == "__main__":
    try:
        output = main()
    except SourceError as ex:
        output = dict(schema=1, error=ex.code, retry_after=ex.retry)
    except (OSError, ValueError, KeyError, TypeError, ImportError, subprocess.SubprocessError):
        # Do not print exception strings: client exceptions can contain secrets.
        output = dict(schema=1, error="source dependency, response or configuration failure", retry_after=0)
    print(json.dumps(output))
