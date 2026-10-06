(* Browser credentials are short-lived and never persisted with the service token. *)
open Conversation_store

type t = {
  mutable codes : (string * float) list;
  mutable sessions : (string * float) list;
  mutable assets : (string * (string * string)) list;
}

let create () = { codes=[]; sessions=[]; assets=[] }
let load_assets t directory =
  try
  let directory=Unix.realpath directory in
  let assets=List.map (fun (route,name,mime) ->
    let path=Filename.concat directory name in
    if Unix.realpath path<>path then fail 400 "web assets must not be symlinks";
    let st=Unix.lstat path in
    if st.Unix.st_kind<>Unix.S_REG || st.st_size>67_108_864 then fail 400 "web asset must be a regular file under 64 MiB";
    let ic=open_in_bin path in
    let bytes=Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
      if not(State_store.same_file st (Unix.fstat (Unix.descr_of_in_channel ic))) then fail 409 "web asset changed during open";
      really_input_string ic st.st_size) in
    route,(mime,bytes)) ["/","index.html","text/html; charset=utf-8";
      "/main.bc.js","main.bc.js","text/javascript; charset=utf-8";
      "/style.css","style.css","text/css; charset=utf-8"] in
  t.assets <- assets
  with Sys_error _ | Unix.Unix_error _ ->
    fail 400 "web assets unavailable; finish building the client before running monty web"

let asset t path = List.assoc_opt path t.assets
let serve_asset (mime,bytes) =
  Cohttp_lwt_unix.Server.respond_string ~status:`OK ~headers:(Cohttp.Header.of_list
    ["content-type",mime;"cache-control","no-store";"x-content-type-options","nosniff";
      "referrer-policy","no-referrer";"x-frame-options","DENY";
      "content-security-policy","default-src 'none'; script-src 'self'; style-src 'self'; connect-src 'self'; img-src 'self' data:; font-src 'self'; base-uri 'none'; form-action 'self'; frame-ancestors 'none'"])
    ~body:bytes ()
let fresh entries = List.filter (fun (_,until) -> until > Unix.gettimeofday ()) entries
let contains secret entries = List.exists (fun (key,_) -> Eqaf.equal key secret) (fresh entries)
let remove secret entries = List.filter (fun (key,_) -> not (Eqaf.equal key secret)) entries
let issue t =
  t.codes <- fresh t.codes;
  if List.length t.codes >= 8 then fail 429 "too many unused browser codes; wait five minutes";
  let code=random_id () in
  t.codes <- (code,Unix.gettimeofday () +. 300.) :: t.codes;
  obj ["code",string code;"expires_in",`Int 300]

let cookie_name port = "monty_session_" ^ string_of_int port
let cookie ~port headers =
  let name=cookie_name port ^ "=" in
  let values=Cohttp.Header.get_multi headers "cookie" |> List.concat_map (String.split_on_char ';')
    |> List.map String.trim |> List.filter (String.starts_with ~prefix:name) in
  match values with
  | [value] -> String.sub value (String.length name) (String.length value-String.length name)
  | _ -> ""
let authenticated t ~port headers = contains (cookie ~port headers) t.sessions
let set_cookie ~port value seconds =
  Printf.sprintf "%s=%s; Path=/; HttpOnly; SameSite=Strict; Max-Age=%d" (cookie_name port) value seconds
let login t ~port code =
  if not (contains code t.codes) then fail 401 "browser code is invalid or expired; run monty web again";
  t.sessions <- fresh t.sessions;
  if List.length t.sessions >= 32 then fail 429 "too many browser sessions; sign out of an existing session";
  t.codes <- remove code t.codes;
  let session=random_id () in
  t.sessions <- (session,Unix.gettimeofday () +. 43200.) :: t.sessions;
  set_cookie ~port session 43200
let logout t ~port headers =
  t.sessions <- remove (cookie ~port headers) t.sessions;
  set_cookie ~port "" 0
