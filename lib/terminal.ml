type backend = Ghostty | Dry_run

type target = Tab | Window | Split

type focus_policy = Background | Foreground

let backend_of_string = function
  | "ghostty" -> Ok Ghostty
  | "dry-run" | "dry_run" | "dryrun" -> Ok Dry_run
  | value -> Error (`Msg (Printf.sprintf "unknown terminal backend %S" value))

let backend_to_string = function Ghostty -> "ghostty" | Dry_run -> "dry-run"

let target_of_string = function
  | "tab" -> Ok Tab
  | "window" -> Ok Window
  | "split" -> Ok Split
  | value -> Error (`Msg (Printf.sprintf "unknown launch target %S" value))

let target_to_string = function Tab -> "tab" | Window -> "window" | Split -> "split"

let focus_policy_of_string = function
  | "background" -> Ok Background
  | "foreground" -> Ok Foreground
  | value -> Error (`Msg (Printf.sprintf "unknown focus policy %S" value))

let focus_policy_to_string = function
  | Background -> "background"
  | Foreground -> "foreground"

let default_backend () =
  match Sys.getenv_opt "MONTY_TERMINAL" with
  | Some value -> value
  | None -> "ghostty"

let default_target () =
  match Sys.getenv_opt "MONTY_TARGET" with Some value -> value | None -> "tab"

let default_focus_policy () =
  match Sys.getenv_opt "MONTY_FOCUS" with
  | Some value when String.trim value <> "" -> (
      match focus_policy_of_string (String.trim value) with
      | Ok policy -> policy
      | Error _ -> Background)
  | _ -> Background
