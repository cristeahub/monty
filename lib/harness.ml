type t = Pi | Codex

let of_string value =
  match String.lowercase_ascii value with
  | "pi" -> Ok Pi
  | "codex" -> Ok Codex
  | _ -> Error (`Msg (Printf.sprintf "unknown harness %S" value))

let to_string = function Pi -> "pi" | Codex -> "codex"

let supports_fork = function Pi -> true | Codex -> false

let pi_headless_args ~writable =
  " --print --mode json --no-session --tools "
  ^ (if writable then "read,bash,edit,write,grep,find,ls"
     else "read,grep,find,ls")
