# Service API

The Monty client service documents its own API. You can list every operation,
read an operation's arguments and result, and call it, all from the CLI:

```sh
monty api list
monty api describe command.send
monty api call health
```

Because the description comes from the service you are connected to, it always
matches that service's version. Without a running service the CLI shows the
installed binary's catalog and says so.

The API is a loopback HTTP API under `/v1`, authenticated with the bearer token
in `.monty/client-service-token`; see the client-service workflow in
[docs/monty-workflows.md](monty-workflows.md#client-service) for its startup,
authentication and safety rules. This page covers discovery and calling.

## Discovering operations

`monty api list` prints every operation with a short description:

```
$ monty api list
OPERATION                 DESCRIPTION
health                    Service capabilities, configured profiles, the service epoch and the current event cursor.
inventory                 The read-only task inventory with the CLI's exact ordering, values, canonical keys, …
tasks.show                One task with its worker identity and the revision its actions require.
…
command.task_create       Add a Monty-owned local task; it does not start a worker.
command.send              Send text to the selected native session.
…
```

Queries are `GET` requests named after their resource. Actions are commands:
`POST /v1/commands/{id}` with an `op` field and a durable receipt. Each `op` is
its own operation named `command.<op>`, so it can be described and called on its
own.

Use `-q` to print only the names, which is handy for searching:

```
$ monty api list -q | grep task
tasks.show
command.task_create
command.task_preflight
command.task_apply
command.task_complete
```

## Reading an operation's documentation

`monty api describe` prints everything the service knows about one operation:
what it does, how to invoke it, the arguments it takes, what it returns, and an
example of each.

```
$ monty api describe command.task_create
Add a Monty-owned local task; it does not start a worker.

Operation: command.task_create
Invoke: POST /v1/commands/{id} with op=task_create

Input schema:
{
  "type": "object",
  "properties": {
    "op": { "type": "string", "const": "task_create" },
    "project": { "type": "string", "description": "A registered project ID." },
    "title": { "type": "string", "description": "At most 500 characters." },
    "notes": { "type": "string", "description": "At most 64 KiB." }
  },
  "required": [ "op", "project", "title" ],
  "additionalProperties": false
}

Output schema:
{ "type": "object", "properties": { "key": { "type": "string" } }, "required": [ "key" ], "additionalProperties": true }

Request example:
{ "op": "task_create", "project": "monty", "title": "Add API help" }

Validation notes:
- Submitting reserves the command ID and answers HTTP 202 with its receipt; …
```

The schemas are [JSON Schema](https://json-schema.org). To read one:

- `properties` lists each argument and its type. For a `GET` operation they are
  path parameters, named in the path as `{key}`, and query parameters.
- `required` lists the arguments that must be present.
- `additionalProperties: false` means a misspelled argument is an error rather
  than being ignored. An `additionalProperties: true` output is an open shape;
  its notes say what to expect.

The request example is usually the quickest way to see how a call should look.
Validation notes describe rules the schema cannot express: which fields fence
on a revision, what is rejected with 409, what a receipt does and does not
prove.

## Calling an operation

`monty api call` calls an operation and prints its result as JSON:

```
$ monty api call health
{
  "api_version": 1,
  "epoch": "3f9c…",
  …
}
```

Arguments are a single JSON object. Pass it directly, read it from a file with
`@`, or read it from standard input with `-`:

```sh
monty api call inventory '{"scope":"all","all_groups":"true"}'
monty api call command.start @start.json
echo '{"conversation":"c1"}' | monty api call command.sync -
```

Path parameters, query parameters and body fields all come from that object:
`{"key":"local:3"}` fills `/v1/tasks/{key}` for `tasks.show`, and the rest of a
`GET` object becomes the query string. For a command, Monty adds the `op` and a
generated command ID; pass `--request-id ID` to choose the ID, which makes the
request repeatable and its receipt recoverable. The result is indented when
printed to a terminal and kept on one line when piped, so it is ready for
[`jq`](https://jqlang.org):

```sh
monty api call inventory | jq '.tasks | length'
```

A command answers with its receipt. `monty api call` waits until the receipt
leaves `accepted` and `running` and prints it, so `.status` and `.result` are
usually final; pass `--no-wait` to print the accepted receipt immediately, or
`--timeout SECONDS` to wait longer than the default 30 seconds. A receipt that
is still `running` after the timeout is printed as it stands and can be read
again with `commands.show`. `finished` means the command finished, not that a
model turn or task finished. A `rejected` or `outcome_unknown` receipt exits
with status 1.

Two operations are not plain JSON: `llms` prints Markdown, and `events` is a
server-sent event stream, which the CLI refuses; read it with
`curl -N -H 'Authorization: Bearer …' http://127.0.0.1:PORT/v1/events`.

### Errors

A call that fails prints the service's message and exits with a nonzero
status:

```
$ monty api call command.sync '{"conversation":"missing"}'
{"id":"…","status":"rejected","result":{"code":404,"message":"record not found"},…}
$ monty api call tasks.show '{"key":"nope"}'
Error: a canonical local task key is required (status=400)
```

A `400` means the arguments were wrong, `404` that the operation or record does
not exist, `409` that the revision, session or receipt changed and you should
refresh before retrying, and `503` that the service is not running.

## Machine-readable documentation

Both discovery commands take `--json`, which prints the service's own
description of its API:

```sh
monty api list --json          # every operation, with schemas and examples
monty api describe inventory --json
```

Each operation has a `name`, a `description`, an `invoke` object (`method`,
`path` and, for commands, `op`), an `input_schema`, an `output_schema`, and,
where the service has them, a `request_example`, a `response_example` and
`validation_notes`. The service serves the same document at `GET /v1/help`,
and as Markdown at `GET /v1/llms.txt`, for agents and tools that read the API
directly.

This is useful for generating bindings, for checking a client against a service
before running it, and for giving a coding agent the exact shape of the API it
is about to call. The catalog lives in one place in the source,
`lib/api_catalog.ml`, and the test suite checks that every documented command
`op` reaches its handler and that the service's copy equals the binary's.

The CLI remains the authoritative interface for task lifecycle: launching,
resuming, completing and acknowledging through `monty` commands. The API
exposes the same operations with the same safeguards; it adds no shortcut
around them.
