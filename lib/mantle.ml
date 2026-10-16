(* The active Mantle profile, read from the shell-local environment Mantle
   exports. It scopes project groups and the Rex session tabs land in. *)

let profile getenv =
  let env name = Option.value ~default:"" (getenv name) in
  let name = env "MANTLE_CONTEXT" in
  (* Match Mantle's shell-local active_context contract without invoking a CLI. *)
  if String.length name > 0 && String.length name <= 32
     && name.[0] >= 'a' && name.[0] <= 'z'
     && String.for_all (function 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true | _ -> false) name
     && not (List.mem name [ "init"; "list"; "create"; "off"; "status"; "prompt" ])
     && List.mem (env "_MANTLE_PREV_STATE") [ "unset"; "local"; "export" ]
     && env "_MANTLE_SELECTED_HOME" <> ""
     && env "CODEX_HOME" = env "_MANTLE_SELECTED_HOME"
  then Some name else None

(* A profile name typed by hand must satisfy the same contract. *)
let valid_name name =
  if String.length name > 0 && String.length name <= 32
     && name.[0] >= 'a' && name.[0] <= 'z'
     && String.for_all (function 'a' .. 'z' | '0' .. '9' | '-' | '_' -> true | _ -> false) name
  then Ok name
  else Error (Printf.sprintf "%S is not a Mantle profile name (lowercase letters, digits, - and _)" name)
