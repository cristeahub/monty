type t = Pi | Codex | Claude

let of_string value =
  match String.lowercase_ascii value with
  | "pi" -> Ok Pi
  | "codex" -> Ok Codex
  | "claude" -> Ok Claude
  | _ -> Error (`Msg (Printf.sprintf "unknown harness %S" value))

let to_string = function Pi -> "pi" | Codex -> "codex" | Claude -> "claude"

let supports_fork = function Pi -> true | Codex | Claude -> false

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
