type t = Pi | Codex | Claude

let of_string value =
  match String.lowercase_ascii value with
  | "pi" -> Ok Pi
  | "codex" -> Ok Codex
  | "claude" -> Ok Claude
  | _ -> Error (`Msg (Printf.sprintf "unknown harness %S" value))

let to_string = function Pi -> "pi" | Codex -> "codex" | Claude -> "claude"

let supports_fork = function Pi -> true | Codex | Claude -> false

(* Each harness selects its native account directory with one environment
   variable; profiles record the directory under the matching field. *)
let account_variable = function
  | Pi -> "PI_CODING_AGENT_DIR" | Codex -> "CODEX_HOME" | Claude -> "CLAUDE_CONFIG_DIR"

let account_field = function Pi -> "pi_home" | Codex -> "codex_home" | Claude -> "claude_home"

let default_account_directory ~getenv harness =
  match getenv "HOME" with
  | Some home when String.trim home <> "" ->
      Some (Filename.concat home (match harness with Pi -> ".pi/agent" | Codex -> ".codex" | Claude -> ".claude"))
  | _ -> None

let same_directory a b =
  let real path = try Unix.realpath path with Unix.Unix_error _ -> path in
  a = b || real a = real b

(* Claude Code keys its stored login by whether CLAUDE_CONFIG_DIR is set, even
   when the variable names the default ~/.claude, so the plain `claude` login is
   invisible to a process that exports it. The default Claude account, which is
   what a shell without an active Mantle profile uses, is therefore selected by
   leaving the variable unset. Codex and Pi read credentials from the directory
   itself, so their variable is always exported. *)
let uses_default_login ~getenv harness directory =
  harness = Claude
  && (match default_account_directory ~getenv harness with
      | Some default -> same_directory default directory
      | None -> false)

let account_environment ~getenv harness directory =
  if uses_default_login ~getenv harness directory then []
  else [ account_variable harness ^ "=" ^ directory ]

let check_review_command harness command =
  let forbidden = match harness with
    | Pi -> ["--extension";"-e";"--skill";"--prompt-template";"--tools";"-t";"--builtin-tools"]
    | Codex -> ["--config";"-c";"--profile";"-p";"--enable";"--sandbox";"-s";"--worktree";
        "--dangerously-bypass-approvals-and-sandbox";"--dangerously-bypass-hook-trust";"--approve-for-me";"--yolo";"--full-auto"]
    | Claude -> ["--settings";"--setting-sources";"--mcp-config";"--plugin-dir";"--plugin-url";
        "--agents";"--agent";"--tools";"--allowedtools";"--allowed-tools";
        "--permission-mode";"--dangerously-skip-permissions";"--allow-dangerously-skip-permissions";
        "--worktree";"-w";"--bg";"--exec";"--desktop";"--chrome";"--ide";"--remote";"--teleport"] in
  let rec check remaining =
    if String.trim remaining="" then Ok () else
    match Process.command_word remaining with
    | Error _ as error -> error
    | Ok (word,finish) ->
        let flag=String.lowercase_ascii(List.hd(String.split_on_char '=' word)) in
        if flag="--" || List.exists (fun blocked -> flag=blocked ||
          (String.length blocked=2 && String.starts_with ~prefix:blocked flag)) forbidden then
          Error (Printf.sprintf "reviewer command cannot include %s; configure model/provider arguments without tools, integrations or permission overrides" word)
        else check (String.sub remaining finish (String.length remaining-finish)) in
  (* Native disable flags do not remove explicitly supplied integrations. *)
  match Process.command_word command with Error _ as error->error|Ok (_,finish)->
    check(String.sub command finish (String.length command-finish))

let pi_headless_args ~writable =
  " --print --mode json --no-session --tools "
  ^ (if writable then "read,bash,edit,write,grep,find,ls"
     else "read,grep,find,ls --no-extensions --no-skills --no-prompt-templates")

let codex_headless_permissions ~codex_yolo ~writable =
  if not writable then
    " --sandbox read-only --ignore-user-config --ignore-rules"
    ^ " -c 'mcp_servers={}' -c 'features.hooks=false' -c 'features.multi_agent=false' -c 'features.apps=false' -c 'features.plugins=false'"
  else if codex_yolo then " --dangerously-bypass-approvals-and-sandbox"
  else " --sandbox workspace-write"
