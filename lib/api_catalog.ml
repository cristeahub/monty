(* The service API described by itself, the way Rex's server documents its own
   methods: every operation with its HTTP shape, JSON schemas, examples and the
   validation rules a schema cannot express. The service serves this catalog as
   /v1/help and /v1/llms.txt, and `monty api` reads it, so the CLI, the service
   and the documentation never disagree about what the API accepts. *)

type operation = {
  name : string;
  description : string;
  meth : string;
  path : string;  (* under /v1, with {param} placeholders *)
  op : string option;  (* the command op for POST /v1/commands/{id} *)
  input : Yojson.Safe.t;
  output : Yojson.Safe.t;
  request_example : Yojson.Safe.t option;
  response_example : Yojson.Safe.t option;
  notes : string list;
}

(* ---- JSON Schema helpers -------------------------------------------------- *)

let strings values = `List (List.map (fun value -> `String value) values)
let described doc fields = if doc = "" then fields else ("description", `String doc) :: fields
let typed ?(doc = "") kind fields = `Assoc (("type", `String kind) :: described doc fields)
let string ?(doc = "") () = typed ~doc "string" []
let enum ?(doc = "") values = typed ~doc "string" [ ("enum", strings values) ]
let const ?(doc = "") value = typed ~doc "string" [ ("const", `String value) ]
let const_bool ?(doc = "") value = typed ~doc "boolean" [ ("const", `Bool value) ]
let int ?(doc = "") ?minimum ?maximum () =
  typed ~doc "integer"
    ((match minimum with Some n -> [ ("minimum", `Int n) ] | None -> [])
    @ match maximum with Some n -> [ ("maximum", `Int n) ] | None -> [])
let number ?(doc = "") () = typed ~doc "number" []
let bool ?(doc = "") () = typed ~doc "boolean" []
let nullable ?(doc = "") kind = `Assoc (("type", strings [ kind; "null" ]) :: described doc [])
let array ?(doc = "") items = typed ~doc "array" [ ("items", items) ]
let obj ?(doc = "") ?(required = []) ?(additional = false) properties =
  typed ~doc "object"
    [ ("properties", `Assoc properties); ("required", strings required); ("additionalProperties", `Bool additional) ]
let any ?(doc = "") () = typed ~doc "object" [ ("additionalProperties", `Bool true) ]
let none = obj []
let j = Yojson.Safe.from_string

(* ---- Shared shapes ----------------------------------------------------- *)

let receipt_status = [ "accepted"; "running"; "finished"; "rejected"; "outcome_unknown" ]

let receipt =
  obj ~doc:"A durable command receipt." ~required:[ "id"; "status"; "created_at"; "request" ]
    [ ("id", string ());
      ("fingerprint", string ~doc:"SHA-256 of the request; a reused ID with another request conflicts." ());
      ("status", enum receipt_status);
      ("created_at", number ());
      ("updated_at", number ());
      ("request", any ~doc:"The submitted command with secrets and terminal bytes redacted." ());
      ("result", any ~doc:"The operation's result, or {code, message} after a rejection." ()) ]

let process_result =
  obj ~doc:"A Monty CLI process run by the service." ~required:[ "exit_code"; "stdout"; "stderr" ]
    [ ("exit_code", int ()); ("stdout", string ()); ("stderr", string ()) ]

let task_workspace = obj ~required:[ "repo"; "branch" ] [ ("repo", string ()); ("branch", string ()) ]

let local_task =
  obj ~doc:"A Monty-owned local task." ~required:[ "id"; "project"; "title"; "status" ]
    [ ("id", string ()); ("project", string ()); ("title", string ()); ("status", string ());
      ("branch", string ()); ("workspaces", array task_workspace); ("notes", string ());
      ("worker_id", string ()); ("worker_key", string ()); ("external_key", string ());
      ("external_url", string ()); ("external_source", string ()); ("created_at", string ());
      ("updated_at", string ()) ]

let task_detail =
  obj ~doc:"A task with the revision its actions require." ~required:[ "key"; "id"; "title"; "status"; "project" ] ~additional:true
    [ ("key", string ~doc:"Canonical key: local:ID, or worker:ID for an unlinked read-only worker row." ());
      ("revision", string ~doc:"Opaque; task_preflight and task_complete must repeat it." ());
      ("project_repo", string ());
      ("read_only", bool ~doc:"True for an unlinked worker row, which cannot be managed until reconciled." ());
      ("worker", nullable ~doc:"{id, status, directory} of the linked worker." "object");
      ("workspaces", array task_workspace) ]

let conversation =
  obj ~doc:"A conversation manifest: Monty's durable record of a native chat." ~required:[ "id"; "title"; "revision"; "sessions" ] ~additional:true
    [ ("schema", int ()); ("id", string ()); ("title", string ());
      ("revision", int ~doc:"Repeat it in stateful commands; a changed revision rejects them." ());
      ("task_keys", array (string ())); ("created_at", number ());
      ("sessions", array (any ~doc:"Native sessions: id, harness, profile, scope, native_id, context, runtime, observed, terminal." ()));
      ("selected_session", nullable "string"); ("transfers", array (any ()));
      ("archived", bool ()); ("purged", bool ()) ]

let context =
  obj ~doc:"Where and how a native session runs." ~required:[ "workspaces" ]
    [ ("workspaces", array ~doc:"One to sixteen absolute directories inside the profile's roots; the first is the working directory." (string ()));
      ("instructions", string ~doc:"Trusted instructions, at most 64 KiB." ());
      ("objective", string ());
      ("environment", any ~doc:"Monty worker variables only (MONTY_*)." ()) ]

let terminal_environment =
  any ~doc:"The caller's REX_SESSION, REX_BLOCK, REX_SERVER and REX_CLIENT, applied to this attachment only." ()

let command_input ?(required = []) ?(additional = false) op properties =
  obj ~required:("op" :: required) ~additional (("op", const op) :: properties)

let conversation_fields = [ ("conversation", string ()); ("revision", int ()) ]

(* ---- Operations --------------------------------------------------------- *)

let query name ~description ?(input = none) ?(output = any ()) ?request ?response ?(notes = []) path =
  { name; description; meth = "GET"; path; op = None; input; output; request_example = request;
    response_example = response; notes }

let command op ~description ?(required = []) ?(additional = false) ?(output = any ()) ?request ?response ?(notes = []) properties =
  { name = "command." ^ op; description; meth = "POST"; path = "/v1/commands/{id}"; op = Some op;
    input = command_input ~required ~additional op properties; output; request_example = request;
    response_example = response;
    notes = notes @ [ "Submitting reserves the command ID and answers HTTP 202 with its receipt; read GET /v1/commands/{id} for the outcome. `finished` means the command finished, not that a model turn or task finished." ] }

let operations =
  [ query "health" "/v1/health" ~description:"Service capabilities, configured profiles, the service epoch and the current event cursor."
      ~output:(obj ~required:[ "api_version"; "epoch"; "cursor"; "profiles" ] ~additional:true
         [ ("api_version", int ()); ("epoch", string ~doc:"Changes at every restart; cursors and shutdown are scoped to it." ());
           ("cursor", string ()); ("profiles", array (obj ~required:[ "id"; "harness" ] ~additional:true
             [ ("id", string ()); ("harness", enum [ "codex"; "pi"; "claude" ]); ("scope", string ~doc:"SHA-256 of the canonical account directory." ()); ("roots", array (string ())) ]));
           ("request_limit", int ()); ("event_retention", int ()) ])
      ~response:(j {|{"api_version":1,"epoch":"3f9c…","cursor":"3f9c…:42","profiles":[{"id":"default","harness":"codex","scope":"7b1e…","roots":["/Users/me/repos"]}]}|});
    query "help" "/v1/help" ~description:"This catalog: every operation with its schemas, examples and validation notes."
      ~output:(obj ~required:[ "http"; "operations" ] [ ("http", any ()); ("operations", array (any ())) ]);
    query "llms" "/v1/llms.txt" ~description:"The same catalog as Markdown for agents and people (text/markdown)."
      ~output:(string ~doc:"Markdown." ());
    query "inventory" "/v1/inventory" ~description:"The read-only task inventory with the CLI's exact ordering, values, canonical keys, every workspace, the profile and group banner, and warnings."
      ~input:(obj [ ("scope", enum ~doc:"Default active." [ "active"; "archived"; "all" ]); ("project", string ());
        ("group", string ()); ("all_groups", const ~doc:"Include every project group." "true"); ("run", string ~doc:"A run directory name or path." ()) ])
      ~output:(obj ~required:[ "profile"; "group"; "banner"; "warnings"; "tasks" ]
        [ ("profile", nullable ~doc:"The active Mantle profile." "string"); ("group", nullable "string"); ("banner", string ());
          ("warnings", array (string ()));
          ("tasks", array (obj ~required:[ "key"; "id"; "project"; "status"; "title"; "branch"; "workspaces" ]
            [ ("key", string ()); ("id", string ()); ("project", string ()); ("status", string ()); ("title", string ());
              ("branch", nullable "string"); ("workspaces", array task_workspace) ])) ])
      ~request:(j {|{"scope":"active","all_groups":"true"}|})
      ~notes:[ "Queries never reconcile tasks, acknowledge handoffs, import or resume chats, or complete work; the CLI lifecycle commands stay authoritative."; "Selection follows the CLI: lists default to the active Mantle profile's project group." ];
    query "projects" "/v1/projects" ~description:"Registered projects, for task creation."
      ~output:(array (obj ~required:[ "id"; "repo"; "group" ] [ ("id", string ()); ("repo", string ()); ("group", nullable "string") ]));
    query "tasks.show" "/v1/tasks/{key}" ~description:"One task with its worker identity and the revision its actions require."
      ~input:(obj ~required:[ "key" ] [ ("key", string ~doc:"Canonical key such as local:3 or worker:abc; percent-encode it in the path." ()) ])
      ~output:task_detail
      ~response:(j {|{"key":"local:3","id":"3","project":"monty","title":"Add API help","status":"open","revision":"c0ffee…","project_repo":"/Users/me/repos/monty","worker":null}|});
    query "discovery.list" "/v1/discovery" ~description:"Native conversations in one configured account, without importing or resuming them."
      ~input:(obj ~required:[ "profile" ] [ ("profile", string ()); ("cursor", string ~doc:"The previous page's next_cursor." ());
        ("archived", const ~doc:"Codex only." "true"); ("search", string ~doc:"Codex only." ()) ])
      ~output:(obj ~required:[ "data"; "next_cursor"; "errors"; "coverage" ]
        [ ("data", array (obj ~required:[ "profile"; "harness"; "scope"; "native_id" ] ~additional:true
            [ ("profile", string ()); ("harness", string ()); ("scope", string ()); ("native_id", string ());
              ("conversation", nullable ~doc:"The Monty conversation already holding this session." "string"); ("title", string ()); ("cwd", string ()); ("updated_at", string ()) ]));
          ("next_cursor", nullable "string"); ("errors", array (any ())); ("coverage", string ()) ])
      ~notes:[ "Pi reports unreadable or skipped files in `errors` with partial coverage." ];
    query "discovery.history" "/v1/discovery/history" ~description:"Read one native session's history without importing it."
      ~input:(obj ~required:[ "profile" ] [ ("profile", string ()); ("native_id", string ~doc:"Codex and Claude." ()); ("session_file", string ~doc:"Pi: the absolute session file." ()) ])
      ~notes:[ "Viewing history does not import, attach, resume or create tasks." ];
    query "conversations.list" "/v1/conversations" ~description:"Every conversation manifest Monty holds." ~output:(array conversation);
    query "conversations.show" "/v1/conversations/{id}" ~description:"One conversation manifest with its revision, selected session, transfers and coverage."
      ~input:(obj ~required:[ "id" ] [ ("id", string ()) ]) ~output:conversation;
    query "conversations.history" "/v1/conversations/{id}/history" ~description:"Owned history: raw batches, normalized item revisions and effective item identities, eight batches per session per page."
      ~input:(obj ~required:[ "id" ] [ ("id", string ()); ("offset", int ~minimum:0 ()) ])
      ~output:(obj ~required:[ "id"; "sessions" ] ~additional:true
        [ ("id", string ()); ("sessions", array (obj ~required:[ "id"; "records" ] ~additional:true [ ("id", string ()); ("records", array (any ())); ("next_offset", nullable "integer") ])) ])
      ~notes:[ "Follow each session's next_offset to read beyond the first eight batches."; "History reads use only Monty's files; the native source may be offline." ];
    query "terminals.output" "/v1/terminals/{session}" ~description:"Retained bytes of an owned native terminal, from a byte offset."
      ~input:(obj ~required:[ "session" ] [ ("session", string ~doc:"The native session ID in the conversation." ()); ("offset", int ~minimum:0 ()) ])
      ~output:(obj ~required:[ "offset"; "data"; "running"; "code" ]
        [ ("offset", int ~doc:"The next offset to read." ()); ("data", string ~doc:"Base64, at most 32 KiB per read." ()); ("running", bool ()); ("code", nullable ~doc:"Exit code, or outcome_unknown after a restart." "integer") ])
      ~notes:[ "A cursor beyond the retained output is rejected with 409." ];
    query "blobs.read" "/v1/blobs/{sha256}" ~description:"Bytes of an owned attachment (application/octet-stream)."
      ~input:(obj ~required:[ "sha256" ] [ ("sha256", string ~doc:"64 lowercase hex characters." ()) ]) ~output:(string ~doc:"Raw bytes." ());
    query "commands.show" "/v1/commands/{id}" ~description:"A command's current receipt, without dispatching anything."
      ~input:(obj ~required:[ "id" ] [ ("id", string ()) ]) ~output:receipt
      ~response:(j {|{"id":"req-1","fingerprint":"9a2b…","status":"finished","created_at":1760000000.1,"updated_at":1760000000.9,"request":{"op":"sync","conversation":"c1"},"result":{"id":"c1","revision":4}}|});
    { name = "commands.submit"; description = "Reserve and run one command; the `command.*` operations below describe each `op`."; meth = "POST";
      path = "/v1/commands/{id}"; op = None;
      input = obj ~required:[ "id"; "op" ] ~additional:true
        [ ("id", string ~doc:"Your request ID: letters, digits, hyphen or underscore, at most 160 characters." ()); ("op", enum [ "task_create"; "task_preflight"; "task_apply"; "task_complete"; "start"; "import"; "attach"; "resume"; "send"; "interrupt"; "set_model"; "detach"; "release_runtime"; "respond"; "sync"; "switch"; "recover_switch"; "select_session"; "archive"; "purge"; "handoff"; "terminal_open"; "terminal_input"; "terminal_resize"; "headless_start"; "cancel_execution" ]) ];
      output = receipt; request_example = Some (j {|{"id":"req-1","op":"sync","conversation":"c1"}|});
      response_example = Some (j {|{"id":"req-1","status":"accepted","created_at":1760000000.1,"fingerprint":"9a2b…","request":{"op":"sync","conversation":"c1"}}|});
      notes = [ "Requires Content-Type: application/json and a body of at most 256 KiB."; "Repeating an identical command under its ID returns the existing receipt; the same ID with another body is rejected with 409. A rejected ID stays rejected, so a revised command uses a new ID."; "Stateful conversation commands require the conversation's current revision unless noted; fetch a fresh manifest after a 409." ] };
    query "events" "/v1/events" ~description:"Server-sent events: `update` with invalidations and provisional native activity, `resync` when the cursor expired."
      ~input:(obj [ ("Last-Event-ID", string ~doc:"Header, not a query: an `epoch:sequence` cursor from /v1/health or an earlier event." ()) ])
      ~output:(obj ~doc:"The data of an `update` event." ~additional:true [ ("type", enum [ "command_changed"; "inventory_changed"; "history_changed"; "terminal_changed" ]); ("command", string ()); ("conversation", string ()); ("session", string ()) ])
      ~notes:[ "Only the latest 256 events are retained, each at most 4 KiB; a previous-epoch or expired cursor answers 409 before streaming."; "Provisional token deltas are not durable message copies; read authoritative snapshots." ];
    { name = "shutdown"; description = "Stop the service through its shared cleanup path."; meth = "POST"; path = "/v1/shutdown"; op = None;
      input = obj ~required:[ "epoch" ] [ ("epoch", string ~doc:"The current epoch from /v1/health." ()) ];
      output = obj ~required:[ "epoch" ] [ ("epoch", string ()) ]; request_example = Some (j {|{"epoch":"3f9c…"}|});
      response_example = Some (j {|{"epoch":"3f9c…"}|}); notes = [ "Answers HTTP 202; a stale epoch is rejected with 409."; "monty service stop does this for you." ] };
    { name = "supervisors.authorize"; description = "Internal: a headless supervisor the service started proves it is a server child."; meth = "POST"; path = "/v1/supervisors/authorize"; op = None;
      input = obj ~required:[ "execution"; "nonce"; "parent_pid" ] [ ("execution", string ()); ("nonce", string ()); ("parent_pid", int ()) ];
      output = obj ~required:[ "authorized" ] [ ("authorized", const_bool true) ]; request_example = None; response_example = None;
      notes = [ "Only the process the service spawned holds the nonce; any other caller is rejected with 403." ] };
    (* Task commands *)
    command "task_create" ~description:"Add a Monty-owned local task; it does not start a worker." ~required:[ "project"; "title" ]
      [ ("project", string ~doc:"A registered project ID." ()); ("title", string ~doc:"At most 500 characters." ()); ("notes", string ~doc:"At most 64 KiB." ()) ]
      ~output:(obj ~required:[ "key" ] ~additional:true [ ("key", string ()) ])
      ~request:(j {|{"op":"task_create","project":"monty","title":"Add API help"}|});
    command "task_preflight" ~description:"Plan a launch or resume: Monty adds the title, task identity and ordered workspace map, saves the plan in its run directory, and runs doctor plus the complete dry run." ~required:[ "task"; "revision"; "profile"; "action" ]
      [ ("task", string ~doc:"Canonical local task key." ()); ("revision", string ~doc:"From GET /v1/tasks/{key}." ()); ("profile", string ~doc:"The account profile to launch into." ());
        ("action", enum [ "launch"; "resume" ]); ("context", string ~doc:"Standalone Markdown task context; required for launch, at most 64 KiB." ()) ]
      ~output:(obj ~required:[ "task"; "preflight"; "action"; "ready" ] ~additional:true
        [ ("task", string ()); ("preflight", string ~doc:"This command's ID; task_apply names it." ()); ("action", string ()); ("ready", bool ()); ("doctor", process_result); ("preview", process_result); ("terminal", string ()); ("workspaces", array task_workspace) ])
      ~request:(j {|{"op":"task_preflight","task":"local:3","revision":"c0ffee…","profile":"default","action":"launch","context":"## Summary\nAdd /v1/help…"}|})
      ~notes:[ "An open task without a worker can be launched; a task with a worker is resumed."; "The plan records the terminal (ghostty or rex); an apply whose terminal changed is refused." ];
    command "task_apply" ~description:"Request the terminal once for a successful preflight, after checking the task, account, role, settings, terminal and context again." ~required:[ "preflight"; "confirm" ]
      [ ("preflight", string ~doc:"A finished, ready task_preflight command ID." ()); ("confirm", const_bool true) ]
      ~output:(obj ~required:[ "task"; "requested" ] ~additional:true [ ("task", string ()); ("requested", bool ()); ("action", string ()); ("terminal", string ()); ("process", process_result); ("doctor", process_result); ("preview", process_result) ])
      ~request:(j {|{"op":"task_apply","preflight":"req-preflight-1","confirm":true}|})
      ~notes:[ "A consumed plan cannot be submitted again under another ID."; "A requested launch is not evidence that implementation succeeded or that the task is complete; inspect `process`." ];
    command "task_complete" ~description:"Complete a task: a linked worker goes through monty done, which refuses dirty worktrees." ~required:[ "task"; "revision"; "confirm" ]
      [ ("task", string ()); ("revision", string ()); ("confirm", const_bool true) ]
      ~output:(obj ~required:[ "task"; "action" ] ~additional:true [ ("task", string ()); ("action", const "complete"); ("process", process_result) ])
      ~request:(j {|{"op":"task_complete","task":"local:3","revision":"c0ffee…","confirm":true}|})
      ~notes:[ "There is no force-discard action; failing doctor state checks are reported instead." ];
    (* Conversation commands *)
    command "start" ~description:"Start a fresh task-free native conversation in one account profile; no model turn is sent." ~required:[ "profile"; "context" ]
      [ ("profile", string ()); ("title", string ()); ("context", context); ("task_keys", array ~doc:"Exact existing inventory keys to link; no lifecycle side effect." (string ()));
        ("model", string ()); ("modelProvider", string ()); ("terminal", bool ~doc:"True when a native terminal will attach instead of an RPC adapter." ()); ("codex_yolo", bool ~doc:"Codex: no approvals or sandbox for this thread; an explicit high-risk choice." ());
        ("command", string ~doc:"Must resolve to the profile's trusted command." ()); ("fork", string ~doc:"Pi terminal only: a session to fork." ()); ("terminal_environment", terminal_environment) ]
      ~output:conversation
      ~request:(j {|{"op":"start","profile":"default","title":"Planning","context":{"workspaces":["/Users/me/repos/monty"],"instructions":"Current trusted instructions","objective":"Investigate the issue","environment":{}},"task_keys":[]}|})
      ~notes:[ "Workspace paths must exist inside the profile's roots, with the first as cwd; one to sixteen, ordered."; "Naming a fresh Codex thread persists its empty rollout so another client can resume it before any model work." ];
    command "import" ~description:"Register an existing native session without starting its runtime; Pi keeps mirroring the selected file." ~required:[ "profile"; "context" ]
      [ ("profile", string ()); ("context", context); ("task_keys", array (string ())); ("native_id", string ~doc:"Codex and Claude." ()); ("session_file", string ~doc:"Pi: the absolute session file." ()) ]
      ~output:conversation;
    command "attach" ~description:"Codex only: rejoin a thread already loaded in the selected server and mirror its live activity." ~required:[ "profile"; "context"; "native_id" ]
      [ ("profile", string ()); ("context", context); ("task_keys", array (string ())); ("native_id", string ()) ]
      ~output:conversation ~notes:[ "Claude and Pi reject attach: import the session, then resume it after detaching other writers." ];
    command "resume" ~description:"Resume an imported session's runtime after the operator confirms every other runtime was detached." ~required:[ "conversation"; "revision"; "detached" ]
      (conversation_fields @ [ ("detached", const_bool ~doc:"Operator confirmation, not protocol proof." true) ])
      ~output:conversation ~notes:[ "An active source turn is rejected; reach an idle boundary first." ];
    command "send" ~description:"Send text to the selected native session." ~required:[ "conversation"; "text" ]
      (conversation_fields @ [ ("selected_session", string ~doc:"Fence on the exact session instead of the revision." ()); ("text", string ~doc:"At most 8192 bytes." ()); ("delivery", enum ~doc:"Pi only; default prompt." [ "prompt"; "steer"; "follow_up" ]) ])
      ~request:(j {|{"op":"send","conversation":"c1","revision":4,"text":"Summarize the failing test."}|})
      ~notes:[ "Give `revision` or `selected_session`; a changed session is rejected with 409."; "A session owned by a native terminal routes input through the terminal operations instead." ];
    command "interrupt" ~description:"Interrupt the native turn." ~required:[ "conversation" ]
      (conversation_fields @ [ ("selected_session", string ()); ("turn_id", string ~doc:"Codex: the exact turn." ()) ])
      ~notes:[ "Pi clears queued inputs and aborts active work." ];
    command "set_model" ~description:"Pi only: change the session's provider and model while idle." ~required:[ "conversation"; "revision"; "provider"; "model" ]
      (conversation_fields @ [ ("provider", string ()); ("model", string ()) ]);
    command "detach" ~description:"Acknowledge a client's detachment without cancelling execution or releasing the runtime." ~required:[ "conversation"; "revision" ] conversation_fields
      ~output:(obj ~required:[ "conversation"; "detached" ] [ ("conversation", string ()); ("detached", const_bool true) ]);
    command "release_runtime" ~description:"Release an idle runtime's ownership; exit its native terminal first." ~required:[ "conversation"; "revision" ] conversation_fields ~output:conversation;
    command "respond" ~description:"Answer one pending native approval or question with the upstream JSON shape." ~required:[ "conversation"; "pending_id"; "pending_revision"; "response" ]
      (conversation_fields @ [ ("pending_id", string ()); ("pending_revision", int ()); ("response", any ~doc:"The native response object; Monty does not translate it." ()) ])
      ~output:(obj ~required:[ "status"; "pending_id" ] [ ("status", const "submitted"); ("pending_id", string ()) ])
      ~notes:[ "Exactly one response per pending request and connection generation; a second or stale response conflicts." ];
    command "sync" ~description:"Refresh the owned snapshot of the selected session; no model work and no revision needed." ~required:[ "conversation" ] [ ("conversation", string ()) ] ~output:conversation;
    command "switch" ~description:"Branch the conversation into another profile with a bounded context package." ~required:[ "conversation"; "revision"; "profile"; "context"; "mode" ]
      (conversation_fields @ [ ("profile", string ~doc:"The destination profile." ()); ("context", context); ("mode", const "branch"); ("model", string ()); ("modelProvider", string ());
        ("attachments", array ~doc:"Source attachment SHA-256 hashes to carry; their bytes count toward the 128 KiB package." (string ())); ("detached", bool ~doc:"Required for a Pi source with file mirroring only." ()) ])
      ~output:conversation ~notes:[ "Requires idle source history and no pending requests; the destination may run one continuation turn." ];
    command "recover_switch" ~description:"Select an already accepted switch destination after reading it, without creating or replaying it." ~required:[ "conversation"; "revision"; "transfer"; "confirm_branch" ]
      (conversation_fields @ [ ("transfer", string ()); ("confirm_branch", const_bool true) ]) ~output:conversation;
    command "select_session" ~description:"Make another registered session of the conversation the selected one." ~required:[ "conversation"; "revision"; "session" ]
      (conversation_fields @ [ ("session", string ()) ]) ~output:conversation;
    command "archive" ~description:"Mark the conversation archived; no native or task mutation and no revision needed." ~required:[ "conversation" ] [ ("conversation", string ()) ] ~output:conversation;
    command "purge" ~description:"Permanently remove Monty's retained record of a conversation." ~required:[ "conversation"; "confirm" ]
      [ ("conversation", string ()); ("confirm", const_bool true) ] ~output:conversation
      ~notes:[ "Rejected while an owned runtime, live terminal or Codex worker lease exists; native session files are never deleted." ];
    command "handoff" ~description:"Not available: exclusive takeover of a standalone native runtime cannot be verified." [] ~notes:[ "Always rejected with 501 and an explanation; detach manually, import, then resume." ];
    (* Terminal commands *)
    command "terminal_open" ~description:"Start the selected session's native process in a server-owned PTY." ~required:[ "conversation"; "revision"; "selected_session" ]
      (conversation_fields @ [ ("selected_session", string ()); ("rows", int ~minimum:1 ~maximum:1000 ()); ("cols", int ~minimum:1 ~maximum:1000 ()); ("prompt", string ~doc:"Initial native TUI input." ());
        ("fresh", bool ~doc:"Pi: start a distinct session." ()); ("command", string ~doc:"Must resolve to the profile's trusted command." ()); ("terminal_environment", terminal_environment) ])
      ~notes:[ "monty terminal is the client for this; it forwards bytes and size changes and detaches on exit." ];
    command "terminal_input" ~description:"Write bytes to the owned terminal." ~required:[ "conversation"; "selected_session"; "data" ]
      [ ("conversation", string ()); ("selected_session", string ()); ("data", string ~doc:"Base64, at most 4 KiB decoded." ()) ]
      ~output:(obj ~required:[ "submitted" ] [ ("submitted", const_bool true) ]) ~notes:[ "Input bytes are omitted from the receipt." ];
    command "terminal_resize" ~description:"Resize the owned terminal." ~required:[ "conversation"; "selected_session"; "rows"; "cols" ]
      [ ("conversation", string ()); ("selected_session", string ()); ("rows", int ~minimum:1 ~maximum:1000 ()); ("cols", int ~minimum:1 ~maximum:1000 ()) ]
      ~output:(obj ~required:[ "submitted" ] [ ("submitted", const_bool true) ]);
    (* Headless *)
    command "headless_start" ~description:"Submit a server-owned headless supervisor for one active worker, with the worker's locks, pinned profile, phase sequencing, artifacts and handoff." ~required:[ "profile"; "worker" ]
      [ ("profile", string ()); ("worker", string ~doc:"Worker id, branch, task key or title slug." ()); ("resume", bool ()); ("codex_yolo", bool ());
        ("wt_command", string ~doc:"Must resolve to the service's wt." ()); ("command", string ~doc:"Must resolve to the profile's trusted command." ()); ("environment", any ~doc:"Monty worker variables only." ()) ]
      ~output:(obj ~required:[ "worker_id"; "status"; "command"; "log" ] ~additional:true [ ("worker_id", string ()); ("status", const "submitted"); ("command", string ()); ("log", string ()); ("run_status", string ~doc:"Later receipt results carry the run's status." ()) ])
      ~notes:[ "monty headless run and run-many submit this; the receipt stays running while phases execute."; "A startup receipt is not evidence of successful implementation, and completion leaves the task open." ];
    command "cancel_execution" ~description:"Stop a running headless supervisor." ~required:[ "command"; "updated_at" ]
      [ ("command", string ~doc:"The headless_start command ID." ()); ("updated_at", number ~doc:"The receipt's current updated_at; a changed receipt is rejected." ()) ]
      ~output:receipt ~notes:[ "The receipt becomes outcome_unknown; inspect artifacts before any fresh execution." ] ]

let find name = List.find_opt (fun operation -> operation.name = name) operations

let names () = List.map (fun operation -> operation.name) operations

(* ---- Renderings --------------------------------------------------------- *)

let invoke operation =
  `Assoc
    ([ ("method", `String operation.meth); ("path", `String operation.path) ]
    @ match operation.op with Some op -> [ ("op", `String op) ] | None -> [])

let operation_json operation =
  `Assoc
    ([ ("name", `String operation.name); ("description", `String operation.description); ("invoke", invoke operation);
       ("input_schema", operation.input); ("output_schema", operation.output) ]
    @ (match operation.request_example with Some example -> [ ("request_example", example) ] | None -> [])
    @ (match operation.response_example with Some example -> [ ("response_example", example) ] | None -> [])
    @ [ ("validation_notes", strings operation.notes) ])

let http =
  `Assoc
    [ ("base_path", `String "/v1"); ("help_path", `String "/v1/help"); ("llms_txt_path", `String "/v1/llms.txt");
      ("events_path", `String "/v1/events");
      ("authentication", `String "Authorization: Bearer <token from .monty/client-service-token>, exact Host 127.0.0.1:PORT, and no Origin or the identical http origin.") ]

let to_json () = `Assoc [ ("http", http); ("operations", `List (List.map operation_json operations)) ]

let of_json json =
  match Yojson.Safe.Util.member "operations" json with
  | `List operations ->
      List.filter_map
        (fun operation ->
          let member name = Yojson.Safe.Util.member name operation in
          let text name = match member name with `String value -> value | _ -> "" in
          let invoke = member "invoke" in
          match (member "name", member "invoke") with
          | `String name, `Assoc _ ->
              Some
                { name; description = text "description";
                  meth = (match Yojson.Safe.Util.member "method" invoke with `String value -> value | _ -> "GET");
                  path = (match Yojson.Safe.Util.member "path" invoke with `String value -> value | _ -> "");
                  op = (match Yojson.Safe.Util.member "op" invoke with `String value -> Some value | _ -> None);
                  input = member "input_schema"; output = member "output_schema";
                  request_example = (match member "request_example" with `Null -> None | value -> Some value);
                  response_example = (match member "response_example" with `Null -> None | value -> Some value);
                  notes = (match member "validation_notes" with `List notes -> List.filter_map (function `String note -> Some note | _ -> None) notes | _ -> []) }
          | _ -> None)
        operations
  | _ -> []

let pretty json = Yojson.Safe.pretty_to_string ~std:true json

let describe operation =
  String.concat "\n"
    ([ operation.description; "";
       Printf.sprintf "Operation: %s" operation.name;
       Printf.sprintf "Invoke: %s %s%s" operation.meth operation.path (match operation.op with Some op -> " with op=" ^ op | None -> "");
       ""; "Input schema:"; pretty operation.input; ""; "Output schema:"; pretty operation.output ]
    @ (match operation.request_example with Some example -> [ ""; "Request example:"; pretty example ] | None -> [])
    @ (match operation.response_example with Some example -> [ ""; "Response example:"; pretty example ] | None -> [])
    @ (match operation.notes with [] -> [] | notes -> "" :: "Validation notes:" :: List.map (fun note -> "- " ^ note) notes))
  ^ "\n"

let table operations =
  let width = List.fold_left (fun width operation -> max width (String.length operation.name)) 9 operations in
  let pad value = value ^ String.make (width - String.length value) ' ' in
  String.concat "\n" ((pad "OPERATION" ^ "  DESCRIPTION") :: List.map (fun operation -> pad operation.name ^ "  " ^ operation.description) operations) ^ "\n"

let llms_txt () =
  let section title operations =
    "## " ^ title ^ "\n\n"
    ^ String.concat "\n"
        (List.map
           (fun operation ->
             Printf.sprintf "- `%s`: %s %s%s. %s" operation.name operation.meth operation.path
               (match operation.op with Some op -> " with `op: \"" ^ op ^ "\"`" | None -> "")
               operation.description)
           operations)
    ^ "\n\n"
  in
  let queries, commands = List.partition (fun operation -> operation.op = None) operations in
  "# Monty service API\n\n\
   Monty's client service is a loopback HTTP API under `/v1`. Queries are `GET` requests; actions are `POST /v1/commands/{id}` with an `op` field and a durable receipt. Every operation is described with JSON Schemas, examples and validation notes at `GET /v1/help`, which `monty api list --json` and `monty api describe NAME` print.\n\n\
   Authentication: `Authorization: Bearer` with the token in `.monty/client-service-token`, the exact `Host: 127.0.0.1:PORT`, and no `Origin` or the identical http origin. JSON bodies require `Content-Type: application/json` and are limited to 256 KiB.\n\n"
  ^ section "Queries and endpoints" queries ^ section "Commands" commands
  ^ "## From the command line\n\n```sh\nmonty api list\nmonty api describe command.send\nmonty api call inventory '{\"scope\":\"all\"}'\nmonty api call command.sync '{\"conversation\":\"c1\"}'\n```\n"
