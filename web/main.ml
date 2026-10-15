open! Core
open! Bonsai_web.Cont
open Bonsai.Let_syntax
open Js_of_ocaml
open Poly

module N = Vdom.Node
module A = Vdom.Attr
let field key = function `Assoc xs -> List.Assoc.find xs ~equal:String.equal key |> Option.value ~default:`Null | _ -> `Null
let text = function `String s -> s | `Int i -> Int.to_string i | _ -> ""
let s key j = text (field key j)
let array = function `List xs -> xs | _ -> []
let xs key j = array (field key j)
let obj xs = `Assoc xs
let str s = `String s
let put key value j = match j with `Assoc xs -> `Assoc ((key,value)::List.Assoc.remove xs ~equal:String.equal key) | _ -> j
let encode = Uri.pct_encode ~component:`Generic
let query fields = Uri.encoded_of_query (List.map fields ~f:(fun (k,v)->k,[v]))
let parse_json text =
  (* js_of_ocaml 5 converts the lexer buffer between bytes and strings for
     tokens. Feeding chunks avoids copying the entire response for each token. *)
  let offset=ref 0 in
  let lexbuf=Stdlib.Lexing.from_function ~with_positions:false (fun buffer length->
    let length=Int.min length (String.length text - !offset) in
    Stdlib.Bytes.blit_string text !offset buffer 0 length;
    offset:= !offset+length;
    length) in
  Yojson.Safe.from_lexbuf (Yojson.init_lexer ()) lexbuf
let api (meth,path,body) =
  Async_kernel.Deferred.create (fun result ->
    let xhr=XmlHttpRequest.create () in
    let finish value = Async_kernel.Ivar.fill_if_empty result value in
    xhr##_open (Js.string meth) (Js.string ("/v1/" ^ path)) Js._true;
    Js.Unsafe.set xhr "timeout" 20000;
    xhr##.onreadystatechange := Js.wrap_callback (fun () ->
      if Poly.equal xhr##.readyState XmlHttpRequest.DONE then (
        let json=try parse_json (Js.Opt.case xhr##.responseText (fun () -> "") Js.to_string) with _ -> `Null in
        if xhr##.status>=200 && xhr##.status<300 then finish (Ok json)
        else finish (Error (xhr##.status,if s "error" json<>"" then s "error" json else "Connection interrupted. Your draft is still here."))));
    if Option.is_some body then xhr##setRequestHeader (Js.string "Content-Type") (Js.string "application/json");
    xhr##send (match body with None->Js.null|Some j->Js.some(Js.string(Yojson.Safe.to_string j))))
let request = Effect.of_deferred_fun api
let get path = request ("GET",path,None)
let post path body = request ("POST",path,Some body)

let rec content j = match j with
  | `String value -> value
  | `List values -> String.concat ~sep:"\n" (List.filter_map values ~f:(fun j->let t=content j in if t="" then None else Some t))
  | `Assoc _ ->
      if s "text" j<>"" then s "text" j else
      if not(Poly.equal (field "content" j) `Null) then content(field "content" j) else
      if not(Poly.equal (field "message" j) `Null) then content(field "message" j) else ""
  | _ -> ""
let item_key item = Yojson.Safe.to_string (`List[field "turn_id" item;field "source_id" item])
let session_items sess =
  let latest=xs "records" sess |> List.concat_map ~f:(xs "items") |> List.fold ~init:String.Map.empty
    ~f:(fun acc item->Map.set acc ~key:(item_key item) ~data:item) in
  List.filter_map(xs "effective" sess) ~f:(fun key->Map.find latest (text key))
let history_items history = xs "sessions" history |> List.concat_map ~f:session_items
let settle_live history live =
  List.filter live ~f:(fun preview ->
    if s "conversation" preview<>s "id" history || not(Poly.equal(field "complete" preview)(`Bool true)) then true else
    let sess=List.find(xs "sessions" history) ~f:(fun sess->s "id" sess=s "session" preview) |> Option.value ~default:`Null in
    not(List.exists(session_items sess) ~f:(fun item ->
      if s "source_id" preview="pi-live" then s "kind" item="agentMessage" &&
        not(List.mem (xs "seen" preview) (str(item_key item)) ~equal:Poly.equal) && content(field "content" item)=s "text" preview
      else item_key item=item_key preview &&
        (if s "kind" item="commandExecution" then s "aggregatedOutput" (field "content" item) else content(field "content" item))=s "text" preview)))
let now_seconds () = Time_ns.Span.to_sec(Time_ns.to_span_since_epoch(Time_ns.now ()))
let needs_refresh=ref true
let last_refresh=ref 0.
type selection = Task of string | Chat of string | Native of Yojson.Safe.t
let selection_view = function Task _->"tasks"|Chat _|Native _->"chats"
let selection_route = function Task id->"#tasks/" ^ encode id|Chat id->"#chats/" ^ encode id
  | Native row->"#native/" ^ encode(Yojson.Safe.to_string(obj(List.map ["profile";"native_id";"session_file";"cwd";"title"] ~f:(fun key->key,field key row))))
let route_seen=ref "\000"
let location_hash ()=Js.to_string Dom_html.window##.location##.hash
let set_route route =
  route_seen:=route;
  if location_hash ()<>route then Dom_html.window##.location##.hash:=Js.string route
let focus_detail = Effect.of_sync_fun(fun ()->
  ignore(Dom_html.window##requestAnimationFrame(Js.wrap_callback(fun _->
    Js.Opt.iter(Dom_html.document##getElementById(Js.string "detail-title")) (fun element->
      ignore(Js.Unsafe.meth_call element "focus" [||])))))) ()
type model = {
  storage_key:string; storage_error:string;
  online:bool; events_connected:bool; live:Yojson.Safe.t list;
  authenticated:bool; health:Yojson.Safe.t; inventory:Yojson.Safe.t; chats:Yojson.Safe.t list;
  view:string; search:string; filter_status:string; filter_project:string; filter_account:string;
  selected:selection option; history:Yojson.Safe.t;
  code:string; error:string; loading:bool; profile:string; native:Yojson.Safe.t list;
  new_chat:bool; new_title:string; new_workspace:string; detached:bool;
  projects:Yojson.Safe.t list; new_task:bool; project:string; new_notes:string;
  task_contexts:string String.Map.t; task_plan:Yojson.Safe.t; confirm_complete:bool;
  answers:string String.Map.t;
  native_cursor:string option; discovery_errors:Yojson.Safe.t list; native_archived:bool;
  discovery_loading:bool; discovery_loaded:bool; native_query:string;
  drafts:string String.Map.t; pending:(string * Yojson.Safe.t) list; receipts:Yojson.Safe.t String.Map.t;
}
let initial = {storage_key="";storage_error="";online=false;events_connected=false;live=[];authenticated=false;health=`Null;inventory=`Null;chats=[];view="tasks";
  search="";filter_status="all";filter_project="";filter_account="";selected=None;history=`Null;code="";error="";loading=true;profile="";
  new_chat=false;new_title="";new_workspace="";detached=false;
  projects=[];new_task=false;project="";new_notes="";task_contexts=String.Map.empty;task_plan=`Null;confirm_complete=false;
  answers=String.Map.empty;
  native=[];native_cursor=None;discovery_errors=[];native_archived=false;discovery_loading=false;discovery_loaded=false;native_query="";
  drafts=String.Map.empty;pending=[];receipts=String.Map.empty}
let dispatching=ref String.Set.empty
let command_key body = match s "op" body with
  | "respond" -> "response:" ^ s "pending_id" body
  | "interrupt" -> "interrupt:" ^ s "conversation" body
  | _ -> "command"
let saved_request body = obj(List.filter_map ["op";"conversation";"task";"pending_id";"text"] ~f:(fun key->
  if Poly.equal(field key body) `Null || (key="text" && s "op" body<>"send") then None else Some(key,field key body)))
let storage ()=Js.Unsafe.get Js.Unsafe.global "localStorage"
let storage_warning="Browser storage was unavailable or full. Some drafts or action receipts may not survive a page reload."
let remember model key value = Effect.of_sync_fun(fun ()->
  try
    if model.storage_key="" then raise Exit;
    ignore(Js.Unsafe.meth_call (storage ()) "setItem" [|Js.Unsafe.inject(Js.string(model.storage_key ^ key));
      Js.Unsafe.inject(Js.string(Yojson.Safe.to_string value))|]);None
  with _->Some storage_warning) ()
let edit_text model inject key value update =
  let open Effect.Let_syntax in
  let%bind error=remember model key (str value) in
  inject(fun m->let m=update m in {m with storage_error=Option.value error ~default:m.storage_error})
let saved_state m =
  List.concat[
    List.map(Map.to_alist m.drafts) ~f:(fun (k,v)->"draft:" ^ k,str v);
    List.map(Map.to_alist m.task_contexts) ~f:(fun (k,v)->"context:" ^ k,str v);
    List.map m.pending ~f:(fun (k,v)->"command:" ^ k,saved_request v);
    ["selected",(match m.selected with Some(Chat id)->obj["chat",str id]|Some(Task id)->obj["task",str id]|_->`Null)]]
  |> String.Map.of_alist_exn
let persist old m =
  if m.storage_key="" then m else try
    (* Separate keys keep another tab's drafts and pending receipts intact. *)
    Map.iter2 (saved_state old) (saved_state m) ~f:(fun ~key ~data->
      let key=Js.Unsafe.inject(Js.string(m.storage_key ^ key)) in
      match data with
      | `Both(a,b) when Poly.equal a b -> ()
      | `Left previous ->
          let current=Js.Unsafe.meth_call (storage ()) "getItem" [|key|] in
          if Js.Opt.case current (fun ()->false) (fun v->Js.to_string v=Yojson.Safe.to_string previous) then
            ignore(Js.Unsafe.meth_call (storage ()) "removeItem" [|key|])
      | `Both(_,value)|`Right value -> ignore(Js.Unsafe.meth_call (storage ()) "setItem"
          [|key;Js.Unsafe.inject(Js.string(Yojson.Safe.to_string value))|]));
    m
  with _->{m with storage_error=storage_warning}
let restore health m =
  let key="monty.web.v1." ^ s "storage_scope" health ^ "." in
  if m.storage_key=key then m else
  let m={m with storage_key=key} in
  try
    let saved=List.range 0 (Js.Unsafe.get (storage ()) "length") |> List.filter_map ~f:(fun index->
      let name=Js.to_string(Js.Unsafe.meth_call (storage ()) "key" [|Js.Unsafe.inject index|]) in
      Option.map(String.chop_prefix name ~prefix:key) ~f:(fun name'->
        name',parse_json(Js.to_string(Js.Unsafe.meth_call (storage ()) "getItem" [|Js.Unsafe.inject(Js.string name)|])))) in
    let fields prefix=List.filter_map saved ~f:(fun (key,value)->Option.map(String.chop_prefix key ~prefix) ~f:(fun key->key,value)) in
    let strings prefix=List.filter_map(fields prefix) ~f:(function k,`String v->Some(k,v)|_->None) |> String.Map.of_alist_reduce ~f:(fun _ b->b) in
    let pending=fields "command:" |> List.map ~f:(fun (id,body)->id,saved_request body) in
    dispatching:=String.Set.of_list(List.map pending ~f:(fun (_,body)->command_key body));
    let selected=List.Assoc.find saved ~equal:String.equal "selected" |> Option.value ~default:`Null in
    let selected=if s "chat" selected<>"" then Some(Chat(s "chat" selected)) else
      if s "task" selected<>"" then Some(Task(s "task" selected)) else m.selected in
    {m with drafts=strings "draft:";task_contexts=strings "context:";pending;selected;
      view=(match selected with Some(Chat _)->"chats"|Some(Task _)->"tasks"|_->m.view)}
  with _->{m with storage_key="";storage_error="Saved browser state could not be read. New edits stay in memory; existing stored data has not been removed."}
let failure inject (code,message) = inject (fun m ->
  {m with error=message;loading=false;online=(m.online && code<>0 && code<>401 && code<500);authenticated=(m.authenticated && code<>401);answers=(if code=401 then String.Map.empty else m.answers)})
let load_history cid =
  let rec page offset previous =
    let open Effect.Let_syntax in
    let%bind result=get ("conversations/" ^ encode cid ^ "/history?offset=" ^ Int.to_string offset) in
    match result with Error _ -> Effect.return result | Ok current ->
      let sessions=xs "sessions" current |> List.map ~f:(fun sess ->
        let old=List.find (xs "sessions" previous) ~f:(fun old -> String.equal (s "id" old) (s "id" sess)) in
        put "records" (`List (Option.value_map old ~default:[] ~f:(xs "records") @ xs "records" sess)) sess) in
      let current=put "sessions" (`List sessions) current in
      if List.exists sessions ~f:(fun sess -> not(Poly.equal (field "next_offset" sess) `Null)) then page (offset+8) current
      else Effect.return (Ok current)
  in page 0 `Null
let read_selection = function
  | Task key->get ("tasks/" ^ encode key)
  | Chat cid->load_history cid
  | Native row->get ("discovery/history?" ^ query ["profile",s "profile" row;"native_id",s "native_id" row;"session_file",s "session_file" row])
let refresh model inject =
  let open Effect.Let_syntax in
  let%bind health=get "health" in
  match health with
  | Error (401,_) -> inject (fun m->{m with authenticated=false;online=false;loading=false;answers=String.Map.empty;
      error=(if m.authenticated then "Session expired. Run monty web to connect again. Your drafts are still here." else m.error)})
  | Error e->failure inject e | Ok health ->
  let%bind ()=inject (fun m->let m=restore health m in
    {m with authenticated=true;online=true;health;
      error=(if m.error="Connection interrupted. Your draft is still here." then "" else m.error);
      profile=(if m.profile="" then s "id" (List.hd (xs "profiles" health) |> Option.value ~default:`Null) else m.profile)}) in
  let%bind inventory=get "inventory?all_groups=true&scope=all" in
  let%bind chats=get "conversations" in
  let%bind projects=get "projects" in
  let%bind ()=(match projects with Error e->failure inject e|Ok projects->inject(fun m->{m with projects=array projects;
    project=(if m.project="" then s "id" (List.hd(array projects) |> Option.value ~default:`Null) else m.project)})) in
  let%bind ()=(match inventory,chats with
    | Ok inventory,Ok chats->inject (fun m->{m with inventory;chats=array chats;loading=false})
    | Error e,_ | _,Error e->failure inject e) in
  match model.selected with
  | None -> Effect.Ignore
  | Some selected ->
      let%bind result=read_selection selected in
      (match result with Error e->failure inject e|Ok history->inject(fun m->
        let newer=match selected,field "revision" history,field "revision" m.history with
          | Chat _,`Int a,`Int b->a>=b | _->true in
        if Poly.equal m.selected (Some selected) && newer then {m with history;live=settle_live history m.live} else m))
let select selected inject =
  let open Effect.Let_syntax in
  let%bind ()=Effect.of_sync_fun(fun ()->set_route(selection_route selected)) () in
  let%bind ()=inject (fun m->{m with selected=Some selected;view=selection_view selected;history=`Null;live=[];error="";new_chat=false;new_task=false;detached=false;task_plan=`Null;confirm_complete=false}) in
  let%bind result=read_selection selected in
  let%bind ()=match result with Error e->failure inject e|Ok history->inject (fun m ->
    if Poly.equal m.selected (Some selected) then {m with history;live=settle_live history m.live} else m) in
  focus_detail
let navigate view inject =
  let open Effect.Let_syntax in
  let%bind ()=Effect.of_sync_fun(fun ()->set_route("#" ^ view)) () in
  inject(fun m->{m with view;selected=None;search="";history=`Null;new_chat=false;new_task=false})
let follow_route model inject =
  let open Effect.Let_syntax in
  let%bind route=Effect.of_sync_fun(fun ()->
    let route=location_hash () in
    if not model.authenticated || route= !route_seen then None else (route_seen:=route;Some route)) () in
  match route with None->Effect.Ignore|Some route->
    try match String.lsplit2 (String.chop_prefix_if_exists route ~prefix:"#") ~on:'/' with
    | Some("tasks",id) when id<>""->select(Task(Uri.pct_decode id)) inject
    | Some("chats",id) when id<>""->select(Chat(Uri.pct_decode id)) inject
    | Some("native",data)->select(Native(parse_json(Uri.pct_decode data))) inject
    | _ when route="#chats"->navigate "chats" inject
    | _ when route="#tasks"->navigate "tasks" inject
    | _ when route=""->(match model.selected with Some selected->select selected inject|None->navigate model.view inject)
    | _->inject(fun m->{m with error="This workspace link is not valid."})
    with _->inject(fun m->{m with error="This workspace link is not valid."})
let discover model inject ~more =
  let open Effect.Let_syntax in
  let search=if more then model.native_query else model.search in
  let params=["profile",model.profile;"search",search;"archived",Bool.to_string model.native_archived] @
    (if more then Option.to_list (Option.map model.native_cursor ~f:(fun c->"cursor",c)) else []) in
  let%bind ()=inject(fun m->{m with discovery_loading=true;native_query=search;error=""}) in
  let%bind result=get ("discovery?" ^ query params) in
  let%bind ()=inject(fun m->if m.profile=model.profile then {m with discovery_loading=false} else m) in
  match result with Error e->failure inject e|Ok page->inject (fun m ->
    if m.profile<>model.profile || m.native_archived<>model.native_archived then m else
    {m with native=(if more then m.native else []) @ xs "data" page;
      discovery_loaded=true;
      native_cursor=(match field "next_cursor" page with `String s->Some s|_->None);
      discovery_errors=(if more then m.discovery_errors else []) @ xs "errors" page})
let command model inject body =
  let open Effect.Let_syntax in
  let key=command_key body in
  let%bind id=Effect.of_sync_fun (fun () ->
    if Set.mem !dispatching key then None else (
      dispatching:=Set.add !dispatching key;
      let crypto=Js.Unsafe.get Js.Unsafe.global "crypto" in
      Some(Js.to_string (Js.Unsafe.meth_call crypto "randomUUID" [||])))) () in
  match id with None->Effect.Ignore|Some id ->
  let%bind storage_error=remember model ("command:" ^ id) (saved_request body) in
  match storage_error with Some message->
    let%bind ()=Effect.of_sync_fun(fun ()->dispatching:=Set.remove !dispatching key) () in
    inject(fun m->{m with storage_error=message;error="The action was not submitted because its receipt ID could not be saved. Free browser storage, then try again."})
  | None ->
  let%bind ()=inject (fun m->{m with pending=(id,saved_request body)::m.pending;
    receipts=Map.set m.receipts ~key:id ~data:(obj["id",str id;"status",str "submitting"]);error=""}) in
  let%bind result=post ("commands/" ^ id) body in
  match result with
  | Error ((code,message) as e) when code>=400 && code<500 && code<>408 ->
      let%bind ()=Effect.of_sync_fun (fun ()->dispatching:=Set.remove !dispatching key) () in
      let%bind ()=inject (fun m->{m with pending=List.Assoc.remove m.pending ~equal:String.equal id;
        receipts=Map.set m.receipts ~key:id ~data:(obj["id",str id;"status",str "rejected";"message",str message])}) in
      failure inject e
  | Error e->
      let%bind ()=inject(fun m->{m with receipts=Map.set m.receipts ~key:id ~data:(obj["id",str id;"status",str "checking"])}) in
      failure inject e
  | Ok receipt->inject (fun m->{m with receipts=Map.set m.receipts ~key:id ~data:receipt})
let check_command ?(force=false) model inject =
  Effect.all_unit (List.filter_map model.pending ~f:(fun (id,request_body) ->
    let previous=Map.find model.receipts id |> Option.value ~default:`Null in
    if s "status" previous="submitting" || (not force && s "status" previous="outcome_unknown") then None else Some(
    let open Effect.Let_syntax in
    let%bind result=get ("commands/" ^ id) in
    let result=match result with Error(404,_)->Ok(obj["id",str id;"status",str "outcome_unknown";
      "result",obj["message",str "No receipt was found. Inspect the task or conversation before repeating this action."]])|_->result in
    match result with Error e->failure inject e|Ok receipt ->
    let status=s "status" receipt in
    let complete=List.mem ["finished";"rejected"] status ~equal:String.equal in
    let%bind ()=(if complete then Effect.of_sync_fun (fun ()->dispatching:=Set.remove !dispatching (command_key request_body)) () else Effect.Ignore) in
    let%bind ()=inject (fun m ->
      let result=field "result" receipt in
      let failed_process=List.find_map ["process";"doctor";"preview"] ~f:(fun key->let process=field key result in
        if not(Poly.equal process `Null) && not(Poly.equal(field "exit_code" process)(`Int 0)) then Some process else None) in
      let drafts=if status="finished" && s "op" request_body="send" then
        let cid=s "conversation" request_body in
        if Option.value (Map.find m.drafts cid) ~default:""=s "text" request_body then Map.remove m.drafts cid else m.drafts
        else m.drafts in
      {m with receipts=Map.set m.receipts ~key:id ~data:receipt;drafts;
        answers=(if status="finished" && s "op" request_body="respond" then
          Map.filter_keys m.answers ~f:(fun key->not(String.is_prefix key ~prefix:(s "pending_id" request_body ^ ":"))) else m.answers);
        task_plan=(if status="finished" && s "op" request_body="task_preflight" &&
          Poly.equal m.selected (Some(Task(s "task" request_body))) then result else m.task_plan);
        pending=(if complete then List.Assoc.remove m.pending ~equal:String.equal id else m.pending);
        error=(if List.mem ["rejected";"outcome_unknown"] status ~equal:String.equal then s "message" result else
          if status="finished" && Option.is_some failed_process then
            let process=Option.value_exn failed_process in "The task action needs attention. " ^ s "stderr" process ^ s "stdout" process
          else m.error)}) in
    let result=field "result" receipt in
    if status="finished" && s "id" result<>"" && not(List.is_empty(xs "sessions" result)) then select (Chat(s "id" result)) inject
    else if status="finished" && s "op" request_body="task_create" then select (Task(s "key" result)) inject
    else if status="finished" && List.mem ["task_apply";"task_complete"] (s "op" request_body) ~equal:String.equal then
      let key=if s "task" result<>"" then s "task" result else s "key" result in
      let%bind detail=get ("tasks/" ^ encode key) in
      (match detail with Error e->failure inject e|Ok history->inject(fun m->
        if Poly.equal m.selected (Some(Task key)) then {m with history;task_plan=`Null;confirm_complete=false} else m))
    else Effect.Ignore)))

let el ?(attrs=[]) tag children = N.create tag ~attrs children
let txt s = N.text s
let cls s = A.class_ s
let button ?(kind="button") ?(disabled=false) label effect =
  N.button ~attrs:([A.type_ "button";cls kind;A.on_click(fun _->effect)] @ if disabled then [A.disabled] else []) [txt label]
let tag value = N.span ~attrs:[cls "tag"] [txt value]
let note text = N.p ~attrs:[cls "muted"] [txt text]
let heading eyebrow title = N.div ~attrs:[cls "section-heading"] [N.p ~attrs:[cls "eyebrow"] [txt eyebrow];N.h2 ~attrs:[A.id "detail-title";A.create "tabindex" "-1"] [txt title]]
let dropdown label value choices change = el "label" [N.span[txt label];N.select
  ~attrs:[A.create "aria-label" label;A.value_prop value;A.on_change(fun _ value->change value)]
  (List.map choices ~f:(fun (value,label)->N.option ~attrs:[A.value value] [txt label]))]
let input label value set = el "label" [N.span [txt label];N.input ~attrs:[A.value_prop value;A.on_input(fun _ value->set value)] ()]
let textarea ?(disabled=false) label value set = el "label" [N.span[txt label];N.textarea
  ~attrs:([A.value_prop value;A.on_input(fun _ value->set value)] @ if disabled then [A.disabled] else []) []]
let focus_element id = Effect.of_sync_fun (fun () ->
  Js.Opt.iter (Dom_html.document##getElementById (Js.string id)) (fun element ->
    ignore(Js.Unsafe.meth_call element "scrollIntoView" [|Js.Unsafe.inject(Js.Unsafe.obj [|"block",Js.Unsafe.inject(Js.string "center")|])|]);
    ignore(Js.Unsafe.meth_call element "focus" [||]))) ()
let transcript ?(live=[]) history =
  let sessions=xs "sessions" history in
  List.concat_map sessions ~f:(fun sess ->
    let previews=List.filter live ~f:(fun preview->s "conversation" preview=s "id" history && s "session" preview=s "id" sess) in
    let items=session_items sess |>
      List.filter ~f:(fun item->not(List.exists previews ~f:(fun p->item_key p=item_key item))) in
    [N.div ~attrs:[cls "session-line"] [txt(s "profile" sess ^ " · " ^ s "harness" sess);
       tag (s "state" (field "coverage" sess))]] @
    List.map (xs "gaps" (field "coverage" sess)) ~f:(fun gap->note(text gap)) @
    List.map items ~f:(fun item ->
      let kind=s "kind" item in
      let body=content(field "content" item) in
      el "article" ~attrs:[cls ("message " ^ if kind="userMessage" then "user" else "assistant")]
        [N.div ~attrs:[cls "message-role"] [txt (if kind="userMessage" then "You" else if kind="agentMessage" then "Assistant" else kind)];
         (if body<>"" then N.pre [txt body] else el "details" [el "summary" [txt "View activity"];N.pre [txt(Yojson.Safe.pretty_to_string (field "content" item))]]);
         N.div (List.map (xs "attachments" item) ~f:(fun attachment ->
           if Poly.equal (field "available" attachment) (`Bool true) then
             N.a ~attrs:[A.href ("/v1/blobs/" ^ s "sha256" attachment);A.create "download" "attachment"] [txt "Download attachment"]
           else note ("Attachment unavailable: " ^ s "reason" attachment)))]) @
    List.map previews ~f:(fun preview->el "article" ~attrs:[cls "message assistant live-message"] [
      N.div ~attrs:[cls "message-role"] [txt(if s "kind" preview="commandExecution" then "Running command" else "Assistant · live")];
      N.pre[txt(s "text" preview)];
      (if Poly.equal(field "truncated" preview)(`Bool true) then note "Live preview limit reached. The captured output will appear in history." else N.none)]))

let active_turn history sess =
  let captured=List.find (xs "sessions" history) ~f:(fun value -> s "id" value=s "id" sess) |> Option.value ~default:`Null in
  let raw=List.rev(xs "records" captured) |> List.find_map ~f:(fun batch ->
    let raw=field "raw" batch in if Poly.equal raw `Null then None else Some raw) |> Option.value ~default:`Null in
  if List.mem ["pi";"claude"] (s "harness" sess) ~equal:String.equal then
    if s "runtime" sess="running" then Some "" else None
  else List.find_map (xs "turns" (field "thread" raw)) ~f:(fun turn->
    if s "status" turn="inProgress" then Some(s "id" turn) else None)

let pending_ui m inject chat sess =
  List.map (xs "pending" sess) ~f:(fun pending ->
    let id=s "id" pending in
    let params=field "params" pending in
    let meth=s "method" pending in
    let expires=match field "expires_at" pending with `Float n->n|`Int n->Float.of_int n|_->0. in
    let expired=expires>0. && expires<=Time_ns.Span.to_sec(Time_ns.to_span_since_epoch(Time_ns.now ())) in
    let busy=not m.online || expired || s "state" pending<>"pending" || List.exists m.pending ~f:(fun (_,body)->s "pending_id" body=id) in
    let respond response=command m inject(obj["op",str "respond";"conversation",field "id" chat;"revision",field "revision" chat;
      "pending_id",str id;"pending_revision",field "revision" pending;"response",response]) in
    let answer key=Map.find m.answers (id ^ ":" ^ key) |> Option.value ~default:"" in
    let update key value=inject(fun m->{m with answers=Map.set m.answers ~key:(id ^ ":" ^ key) ~data:value}) in
    let decision label value=button ~disabled:busy label (respond(obj["decision",str value])) in
    let payload label=el "details" [el "summary"[txt label];N.pre[txt(Yojson.Safe.pretty_to_string params)]] in
    let body=if s "harness" sess="pi" then
      [N.strong[txt(s "title" params)];note(s "message" params)] @
      (match meth with
      | "confirm" -> [button ~disabled:busy "Confirm" (respond(obj["confirmed",`Bool true]));
          button ~disabled:busy "Decline" (respond(obj["confirmed",`Bool false]))]
      | "select" -> List.map (xs "options" params) ~f:(fun option->button ~disabled:busy (text option) (respond(obj["value",option])))
      | "input"|"editor" -> [textarea ~disabled:busy "Your response" (answer "value") (update "value");
          button ~disabled:busy "Submit response" (respond(obj["value",str(answer "value")]))]
      | _ -> [note "This native dialog is not supported."]) @
      [button ~disabled:busy "Cancel request" (respond(obj["cancelled",`Bool true]))]
    else match meth with
    | "claude/approval" -> [N.strong[txt("Approve " ^ s "tool" params ^ " once?")];
        N.pre[txt(Yojson.Safe.pretty_to_string(field "input" params))];
        decision "Approve once" "allow";decision "Decline" "deny"]
    | "claude/question" ->
        let questions=xs "questions" (field "input" params) in
        [N.strong[txt "Your input is needed"]] @ List.map questions ~f:(fun q ->
          let key=s "question" q in
          el "fieldset" ~attrs:(if busy then [A.disabled] else []) [el "legend"[txt key];
            N.div(List.map(xs "options" q) ~f:(fun option->button ~disabled:busy (s "label" option) (update key (s "label" option))));
            textarea ~disabled:busy "Answer" (answer key) (update key)]) @
        [button ~disabled:(busy || List.exists questions ~f:(fun q->String.strip(answer(s "question" q))="")) "Submit answers"
          (respond(obj["decision",str "allow";"answers",obj(List.map questions ~f:(fun q->s "question" q,str(answer(s "question" q))))]));
         decision "Decline" "deny"]
    | "item/commandExecution/requestApproval"|"item/fileChange/requestApproval" ->
        let network=field "networkApprovalContext" params in
        let title=if not(Poly.equal network `Null) then "Network access requested" else
          if meth="item/fileChange/requestApproval" then "File changes requested" else
          if s "kind" params="writeStdin" then "Terminal input requested" else "Command approval requested" in
        let offered=match field "availableDecisions" params with `List values->values|_->List.map ["accept";"decline";"cancel"] ~f:str in
        let proposed=xs "sessions" m.history |> List.filter ~f:(fun captured->s "id" captured=s "id" sess) |>
          List.concat_map ~f:(xs "records") |> List.rev |> List.find_map ~f:(fun batch->
            let raw=field "raw" batch in let event=field "params" raw in let item=field "item" event in
            if List.mem ["item/started";"item/completed"] (s "method" raw) ~equal:String.equal && s "type" item="fileChange" &&
              Poly.equal(field "id" item)(field "itemId" params) && Poly.equal(field "turnId" event)(field "turnId" params)
            then Some item else List.find_map(List.rev(xs "items" batch)) ~f:(fun item->
              if s "kind" item="fileChange" && Poly.equal(field "source_id" item)(field "itemId" params) &&
                Poly.equal(field "turn_id" item)(field "turnId" params) then Some(field "content" item) else None)) in
        [N.strong[txt title];note(s "reason" params);
          (if Poly.equal network `Null then N.pre[txt(s "command" params)] else N.pre[txt(s "protocol" network ^ "://" ^ s "host" network)]);
          note(s "cwd" params);
          (if s "grantRoot" params="" then N.none else note("Requested write root: " ^ s "grantRoot" params ^ ". This grant may cover the session."));
          (if Poly.equal(field "additionalPermissions" params) `Null then N.none else N.pre[txt(Yojson.Safe.pretty_to_string(field "additionalPermissions" params))]);
          (if meth<>"item/fileChange/requestApproval" then N.none else
            match proposed with None->note "The source has not supplied the proposed file changes yet."|Some item->
              N.div(List.map (xs "changes" item) ~f:(fun change->N.div[N.strong[txt(s "path" change)];N.pre[txt(s "diff" change)]])));
          payload "Request details"] @
        List.filter_map [(if meth="item/fileChange/requestApproval" then "Approve file changes" else "Approve once"),"accept";"Decline","decline";"Cancel turn","cancel"] ~f:(fun (label,value)->
          if List.mem offered (str value) ~equal:Poly.equal then
            Some(if value="accept" && meth="item/fileChange/requestApproval" && Option.is_none proposed then
              button ~disabled:true label Effect.Ignore else decision label value)
          else None)
    | "item/tool/requestUserInput" ->
        let questions=xs "questions" params in
        [N.strong[txt "Your input is needed"]] @
        List.map questions ~f:(fun question ->
          let key=s "id" question in
          el "fieldset" ~attrs:(if busy then [A.disabled] else []) [el "legend"[txt(s "question" question)];
            N.div(List.map (xs "options" question) ~f:(fun option->
              el "label" ~attrs:[cls "confirmation"] [N.input ~attrs:[A.type_ "radio";A.name(id ^ ":" ^ key);A.value(s "label" option);
                A.bool_property "checked" (answer key=s "label" option);(if busy then A.disabled else A.empty);
                A.on_change(fun _ _->update key (s "label" option))] ();
                N.span[N.strong[txt(s "label" option)];N.span ~attrs:[cls "muted"] [txt(s "description" option)]]]));
            (if List.is_empty(xs "options" question) || Poly.equal(field "isOther" question)(`Bool true) then
              el "label" [N.span[txt(if List.is_empty(xs "options" question) then "Answer" else "Answer (choose above or write your own)")];
                N.input ~attrs:[A.type_(if Poly.equal(field "isSecret" question)(`Bool true) then "password" else "text");
                  A.value_prop(answer key);A.on_input(fun _ value->update key value)] ()] else N.none)]) @
        [button ~disabled:(busy || List.exists questions ~f:(fun q->String.strip(answer(s "id" q))="")) "Submit answers"
          (respond(obj["answers",obj(List.map questions ~f:(fun q->s "id" q,obj["answers",`List[str(answer(s "id" q))]]))]));
         button ~disabled:busy "Skip questions" (respond(obj["answers",obj(List.map questions ~f:(fun q->s "id" q,obj["answers",`List[]]))]))]
    | "item/permissions/requestApproval" ->
        [N.strong[txt "Permissions requested for this turn"];note(s "reason" params);note(s "cwd" params);
          N.pre[txt(Yojson.Safe.pretty_to_string(field "permissions" params))];
          button ~disabled:busy "Grant for this turn" (respond(obj["permissions",field "permissions" params;"scope",str "turn"]));
          button ~disabled:busy "Decline permissions" (respond(obj["permissions",obj[];"scope",str "turn"]))]
    | "mcpServer/elicitation/request" ->
        [N.strong[txt("Request from " ^ s "serverName" params)];note(s "message" params);payload "Request details";
          note "Complete this form in a native client, or decline it here.";
          button ~disabled:busy "Decline request" (respond(obj["action",str "decline";"content",`Null]))]
    | _ -> [N.strong[txt "Native request needs attention"];payload "Request details";note "This request needs a native client."] in
    N.div ~attrs:[cls "notice pending-request";A.create "role" "region";A.create "aria-label" "Pending request"]
      ((if expired then [note "This request has expired. Wait for the runner to continue, or interrupt the turn."] else
        if busy then [note "Response submitted; waiting for the runner."] else []) @ body))

let apply_live event m =
  let cid=s "conversation" event and sid=s "session" event in
  if not(Poly.equal m.selected (Some(Chat cid))) then m else
  let native=field "native" event in
  let params=field "params" native in
  let meth=s "method" native in
  let kind=s "type" native in
  let item=field "item" params in
  let item_id=if s "id" item<>"" then s "id" item else s "itemId" params in
  let same preview=s "conversation" preview=cid && s "session" preview=sid &&
    (if kind<>"" then s "source_id" preview=(if kind="sdk_message" then "claude-live" else "pi-live") else s "source_id" preview=item_id && s "turn_id" preview=s "turnId" params) in
  let previous=List.find m.live ~f:same in
  let base text kind source turn=obj["conversation",str cid;"session",str sid;"source_id",str source;"turn_id",str turn;
    "kind",str kind;"text",str text;"complete",`Bool false;
    "seen",`List(List.map(history_items m.history) ~f:(fun i->str(item_key i)))] in
  let updated=match meth,kind with
    | "item/started",_ when List.mem ["agentMessage";"commandExecution"] (s "type" item) ~equal:String.equal ->
        Some(base (s "text" item) (s "type" item) item_id (s "turnId" params))
    | ("item/agentMessage/delta"|"item/commandExecution/outputDelta"),_ ->
        Option.map previous ~f:(fun preview ->
          if Poly.equal(field "truncated" preview)(`Bool true) then preview else
          let value=s "text" preview ^ s "delta" params in
          (* ponytail: bound each live preview to 1 MiB; full output is retained by Monty. *)
          if String.length value>1_048_576 then put "truncated" (`Bool true) preview else put "text" (str value) preview)
    | "item/completed",_ -> Option.map previous ~f:(fun preview->
        let value=if s "type" item="commandExecution" then s "aggregatedOutput" item else s "text" item in
        put "complete" (`Bool true) (put "text" (str value) preview))
    | _,"sdk_message" ->
        let message=field "message" native in
        let event=field "event" message in
        (match s "kind" native,s "type" event with
        | "StreamEvent","message_start" -> Some(base "" "agentMessage" "claude-live" "")
        | "StreamEvent","content_block_delta" when s "type" (field "delta" event)="text_delta" ->
            let preview=Option.value previous ~default:(base "" "agentMessage" "claude-live" "") in
            let value=s "text" preview ^ s "text" (field "delta" event) in
            Some(if String.length value>1_048_576 then put "truncated" (`Bool true) preview else put "text" (str value) preview)
        | "AssistantMessage",_ ->
            let value=content message in
            let preview=Option.value previous ~default:(base "" "agentMessage" "claude-live" "") in
            Some(put "complete" (`Bool true)(put "text" (str value) preview))
        | _ -> previous)
    | _,"message_start" when s "role" (field "message" native)="assistant" ->
        Some(put "blocks" (obj[]) (base (content(field "message" native)) "agentMessage" "pi-live" ""))
    | _,"message_update" ->
        let update=field "assistantMessageEvent" native in
        Option.map previous ~f:(fun preview ->
          if Poly.equal(field "truncated" preview)(`Bool true) then preview else
          let blocks=field "blocks" preview in
          let index=s "contentIndex" update in
          let value=match s "type" update with
            | "text_start"->Some "" | "text_delta"->Some(s index blocks ^ s "delta" update)
            | "text_end"->Some(s "content" update) | _->None in
          match value with None->preview|Some value->
          let blocks=put index (str value) blocks in
          let text=match blocks with `Assoc fields->
            List.sort fields ~compare:(fun (a,_) (b,_)->Int.compare(Int.of_string a)(Int.of_string b)) |>
            List.map ~f:(fun (_,v)->text v) |> String.concat ~sep:"\n" | _->"" in
          if String.length text>1_048_576 then put "truncated" (`Bool true) preview
          else put "blocks" blocks (put "text" (str text) preview))
    | _,"message_end" ->
        let message=field "message" native in
        if s "role" message<>"assistant" then previous else
        let value=content message in
        let preview=Option.value previous ~default:(base "" "agentMessage" "pi-live" "") in
        Some(put "complete" (`Bool(kind="message_end"))
          (if String.length value>1_048_576 then put "truncated" (`Bool true) preview else put "text" (str value) preview))
    | _ -> previous in
  let live=if meth="monty/disconnected" || kind="monty/disconnected" then
      List.filter m.live ~f:(fun preview->s "conversation" preview<>cid || s "session" preview<>sid)
    else match updated with None->m.live|Some preview->
      List.filter m.live ~f:(fun p->not(same p)) @ [preview] in
  {m with live}

(* One connection for this browser app. Reconnection only reads snapshots. *)
let event_source : Js.Unsafe.any option ref = ref None
let source_epoch=ref ""
let retry_events_at=ref 0.
let close_events () =
  Option.iter !event_source ~f:(fun source->ignore(Js.Unsafe.meth_call source "close" [||]));
  event_source:=None
let ensure_events m inject = Effect.of_sync_fun (fun () ->
  if not m.authenticated then close_events () else (
  let epoch=s "epoch" m.health in
  if epoch<> !source_epoch then (close_events ();source_epoch:=epoch);
  if Option.is_none !event_source && epoch<>"" && now_seconds ()>= !retry_events_at then (
    let source=Js.Unsafe.new_obj (Js.Unsafe.get Js.Unsafe.global "EventSource") [|Js.Unsafe.inject(Js.string "/v1/events")|] in
    event_source:=Some source;
    let active ()=match !event_source with Some current->phys_equal current source|None->false in
    let publish f=if active () then Effect.Expert.handle_non_dom_event_exn(inject f) in
    let listen name callback=ignore(Js.Unsafe.meth_call source "addEventListener"
      [|Js.Unsafe.inject(Js.string name);Js.Unsafe.inject(Js.wrap_callback(fun event->if active () then callback event))|]) in
    listen "open" (fun _->needs_refresh:=true;publish(fun m->{m with events_connected=true;live=[]}));
    let resync ()=needs_refresh:=true;publish(fun m->{m with live=[]}) in
    listen "update" (fun event->
      try let event=parse_json(Js.to_string(Js.Unsafe.get event "data")) in
        let native=field "native" event in
        if not(List.mem ["item/agentMessage/delta";"item/commandExecution/outputDelta"] (s "method" native) ~equal:String.equal || s "type" native="message_update") then needs_refresh:=true;
        if s "type" event="resync" then resync () else publish(apply_live event)
      with _->resync ());
    listen "resync" (fun _->resync ());
    listen "session_expired" (fun _->publish(fun m->{m with authenticated=false;online=false;events_connected=false;live=[];answers=String.Map.empty;
      error="Session expired. Run monty web to connect again. Your drafts are still here."});close_events ());
    listen "error" (fun _->publish(fun m->{m with events_connected=false;live=[]});close_events ();
      retry_events_at:=now_seconds ()+.2.;needs_refresh:=true)))) ()

let application graph =
  let model,inject=Bonsai.state_machine0 ~default_model:initial ~apply_action:(fun _ m f->
    let next=f m in if next.storage_key=m.storage_key then persist m next else next) graph in
  let tick=let%map model and inject in
    let open Effect.Let_syntax in
    let%bind ()=follow_route model inject in
    let%bind ()=ensure_events model inject in
    let%bind ()=(if model.authenticated then check_command model inject else Effect.Ignore) in
    let%bind refresh_now=Effect.of_sync_fun (fun ()->
      if !needs_refresh || now_seconds () -. !last_refresh>=10. then (
        needs_refresh:=false;last_refresh:=now_seconds ();true) else false) () in
    if refresh_now then refresh model inject else Effect.Ignore in
  Bonsai.Clock.every ~when_to_start_next_effect:`Wait_period_after_previous_effect_finishes_blocking
    ~trigger_on_activate:true (Time_ns.Span.of_sec 0.5) tick graph;
  let%map m=model and inject in
  let set f=inject f in
  let error=if m.error="" then N.none else N.div ~attrs:[cls "notice error";A.create "role" "alert"] [txt m.error;button "Dismiss" (set(fun m->{m with error=""}))] in
  if not m.authenticated then
    N.main ~attrs:[cls "login"] [N.div ~attrs:[cls "login-card"] [N.div ~attrs:[cls "brand"] [txt "m"];heading "MONTY / LOCAL WORKSPACE" "Welcome back.";
      note "Run monty web in your terminal, then paste the one-use pairing code here.";
      el "form" ~attrs:[A.on_submit(fun _ -> Effect.Many [Effect.Prevent_default;
        (let open Effect.Let_syntax in let%bind result=post "browser/session" (obj["code",str m.code]) in
         match result with Error e->failure inject e|Ok _->
           let%bind ()=Effect.of_sync_fun(fun ()->close_events ();needs_refresh:=true) () in
           set(fun m->{m with code="";authenticated=true;error=""}))])]
        [input "Pairing code" m.code (fun code->set(fun m->{m with code}));
         N.button ~attrs:[A.type_ "submit";cls "primary"] [txt "Connect to Monty"]];error;
      note "This workspace stays on your machine."]]
  else
  let matches json=m.search="" || String.is_substring (String.lowercase(Yojson.Safe.to_string json)) ~substring:(String.lowercase m.search) in
  let selected_class selection=if Poly.equal m.selected (Some selection) then "row selected" else "row" in
  let row selection title subtitle status=
    N.button ~attrs:([A.type_ "button";cls(selected_class selection);A.on_click(fun _->select selection inject)] @
      if Poly.equal m.selected (Some selection) then [A.create "aria-current" "true"] else [])
      [N.div ~attrs:[cls "row-top"] [N.strong [txt (if title="" then "Untitled conversation" else title)];tag status];note subtitle] in
  let list=if m.view="tasks" then
      let tasks=List.filter (xs "tasks" m.inventory) ~f:(fun task->matches task &&
        (m.filter_project="" || s "project" task=m.filter_project ||
          List.exists m.projects ~f:(fun project->s "id" project=m.filter_project &&
            List.exists(xs "workspaces" task) ~f:(fun workspace->s "repo" workspace=s "repo" project))) &&
        (m.filter_status="all" || (if m.filter_status="done" then s "status" task="done" else s "status" task<>"done"))) in
      if List.is_empty tasks then [] else
      [N.div ~attrs:[cls "task-table"] [el "table" [
        el "thead" [el "tr" (List.map ["#";"ID";"Project";"Status";"Title";"Branch"] ~f:(fun label->el "th" ~attrs:[A.create "scope" "col"] [txt label]))];
        el "tbody" (List.mapi tasks ~f:(fun index task->
          el "tr" ~attrs:[cls(if Poly.equal m.selected (Some(Task(s "key" task))) then "selected" else "")] [
            el "td" [txt(Int.to_string(index+1))];el "td" [txt(s "id" task)];el "td" [txt(s "project" task)];
            el "td" [tag(s "status" task)];el "td" [button ~kind:"task-title" (s "title" task) (select (Task(s "key" task)) inject)];
            el "td" (if List.is_empty(xs "workspaces" task) then [N.code[txt(s "branch" task)]] else
              List.map (xs "workspaces" task) ~f:(fun workspace->N.div ~attrs:[A.title(s "repo" workspace)] [N.code[txt(s "branch" workspace)]]))]))]]]
    else List.filter m.chats ~f:(fun c -> not(Poly.equal (field "purged" c) (`Bool true)) && matches c &&
      (m.filter_account="" || List.exists(xs "sessions" c) ~f:(fun sess->s "profile" sess=m.filter_account))) |> List.map ~f:(fun c ->
      row (Chat(s "id" c)) (s "title" c) (String.concat ~sep:" · " (List.map (xs "sessions" c) ~f:(s "profile")))
        (if List.exists(xs "sessions" c) ~f:(fun s->not(List.is_empty(xs "pending" s))) then "needs input" else
          if Poly.equal (field "archived" c) (`Bool true) then "archived" else "registered")) in
  let profile_select label=dropdown label m.profile
    (List.map(xs "profiles" m.health) ~f:(fun p->s "id" p,s "id" p ^ " · " ^ s "harness" p))
    (fun profile->set(fun m->
      let account=List.find(xs "profiles" m.health) ~f:(fun p->s "id" p=profile) |> Option.value ~default:`Null in
      {m with profile;native=[];native_cursor=None;task_plan=`Null;discovery_loaded=false;discovery_loading=false;discovery_errors=[];native_archived=false;
        new_workspace=(if m.new_chat then List.hd(xs "roots" account) |> Option.value_map ~default:"" ~f:text else m.new_workspace)})) in
  let detail=if m.new_task then [heading "MAKE ROOM FOR THE WORK" "New task";
    N.div ~attrs:[cls "new-chat"] [
      el "label" [N.span[txt "Project"];N.select ~attrs:[A.create "aria-label" "Project";A.value_prop m.project;A.on_change(fun _ project->set(fun m->{m with project}))]
        (List.map m.projects ~f:(fun p->N.option ~attrs:[A.value(s "id" p)] [txt(s "id" p)]))];
      input "Task title" m.new_title (fun new_title->set(fun m->{m with new_title}));
      textarea "Notes" m.new_notes (fun new_notes->set(fun m->{m with new_notes}));
      note "Creating a task saves it to Monty. You can prepare and launch its worker from the task detail.";
      button ~kind:"primary" ~disabled:(not m.online || not(List.is_empty m.pending) || String.strip m.new_title="" || m.project="") "Create task"
        (command m inject(obj["op",str "task_create";"project",str m.project;"title",str m.new_title;"notes",str m.new_notes]))]]
  else if m.new_chat then [heading "A FRESH START" "New conversation";
    N.div ~attrs:[cls "new-chat"] [profile_select "Chat account";
      input "Title" m.new_title (fun new_title->set(fun m->{m with new_title}));
      input "Workspace path" m.new_workspace (fun new_workspace->set(fun m->{m with new_workspace}));
      note "Choose a workspace within this account’s configured roots.";
      button ~kind:"primary" ~disabled:(not m.online || not(List.is_empty m.pending) || String.strip m.new_title="" || String.strip m.new_workspace="") "Start conversation"
        (command m inject (obj["op",str "start";"profile",str m.profile;"title",str m.new_title;
          "context",obj["workspaces",`List[str m.new_workspace]]]))]]
  else if Option.is_some m.selected && Poly.equal m.history `Null then
    [heading "SELECTED ITEM" (if m.error="" then "Loading…" else "Unable to load this item.");
      note(if m.error="" then "Reading Monty’s saved history and context." else "Refresh to try again, or choose another item.")]
  else match m.selected with
    | None->[N.div ~attrs:[cls "empty"] [heading "A LITTLE ROOM TO THINK" "Everything has its place.";note "Choose a task or conversation to see its context and activity."]]
    | Some(Task key)->
        let task=m.history in
        let worker=field "worker" task in
        let has_worker=not(Poly.equal worker `Null) in
        let closed=s "status" task="done" in
        let context=Map.find m.task_contexts key |> Option.value ~default:(s "notes" task) in
        let busy=not m.online || not(List.is_empty m.pending) in
        let task_command op extra=command m inject(obj(["op",str op;"task",str key;"revision",field "revision" task] @ extra)) in
        [heading key (s "title" task);tag(s "status" task);note(s "project" task);
        (if s "notes" task="" then N.none else N.pre[txt(s "notes" task)]);
        N.h3 [txt "Workspaces"];N.div (List.map (xs "workspaces" task) ~f:(fun w->N.div ~attrs:[cls "workspace"] [N.strong[txt(s "branch" w)];N.code[txt(s "repo" w)]]));
        (if List.is_empty(xs "workspaces" task) then N.code[txt(s "project_repo" task)] else N.none);
        N.h3[txt "Manage task"];
        (if Poly.equal task `Null then note "Loading task…" else if Poly.equal (field "read_only" task) (`Bool true) then
          note "Reconcile this worker with a local task before managing it. The inventory warnings describe its current link." else
          N.div ~attrs:[cls "new-chat"] [
            (if has_worker then note("Worker " ^ s "id" worker ^ " · " ^ s "status" worker) else N.none);
            (if closed && not has_worker then note "This task is complete." else
              N.div ~attrs:[cls "task-controls"] [profile_select "Worker account";
                (if has_worker then N.none else N.div [
                  textarea "Worker context" context (fun value->edit_text m inject ("context:" ^ key) value (fun m->{m with task_contexts=Map.set m.task_contexts ~key ~data:value;task_plan=`Null}));
                  note "Include the objective, relevant links, constraints, acceptance criteria, and planning notes. Monty adds the workspace map."]);
                button ~disabled:(busy || (not has_worker && String.strip context=""))
                  (if has_worker then (if closed then "Review reopening" else "Review resume") else "Run launch preflight")
                  (task_command "task_preflight" ["action",str(if has_worker then "resume" else "launch");"profile",str m.profile;"context",str context])]);
            (if Poly.equal m.task_plan `Null then N.none else
              let plan=m.task_plan in
              let ready=Poly.equal (field "ready" plan) (`Bool true) in
              N.div ~attrs:[cls "notice preflight";A.create "role" "status"] [
                N.strong[txt(if ready then "Preflight passed" else "Preflight needs attention")];
                note("Account: " ^ s "profile" plan ^ " · " ^ s "agent_profile" plan ^ " · Ghostty");
                N.div(List.map (xs "workspaces" plan) ~f:(fun w->N.p[txt(s "repo" w ^ " · " ^ s "branch" w)]));
                (if s "context" plan="" then N.none else el "details" [el "summary"[txt "Worker context to launch"];N.pre[txt(s "context" plan)]]);
                el "details" ~attrs:(if ready then [] else [A.create "open" ""]) [el "summary"[txt "Preflight output"];
                  N.pre[txt(s "stdout" (field "doctor" plan) ^ s "stderr" (field "doctor" plan) ^ s "stdout" (field "preview" plan) ^ s "stderr" (field "preview" plan))]];
                (if ready then button ~kind:"primary" ~disabled:busy
                  (if s "action" plan="launch" then "Launch worker in Ghostty" else "Resume worker in Ghostty")
                  (command m inject(obj["op",str "task_apply";"preflight",field "preflight" plan;"confirm",`Bool true])) else N.none)]);
            (if closed then N.none else if not m.confirm_complete then button ~disabled:busy "Complete task…" (set(fun m->{m with confirm_complete=true})) else
              N.div ~attrs:[cls "notice"] [note(if has_worker then "Complete this task and archive its worker. Monty removes clean worktrees and refuses to discard local changes." else "Mark this task complete?");
                button ~disabled:busy "Confirm completion" (task_command "task_complete" ["confirm",`Bool true]);
                button "Cancel" (set(fun m->{m with confirm_complete=false}))])]);
        N.h3 [txt "Linked conversations"];N.div (List.filter_map m.chats ~f:(fun c ->
          if List.mem (xs "task_keys" c) (str key) ~equal:Poly.equal then Some(button (s "title" c) (select (Chat(s "id" c)) inject)) else None))]
    | Some(Native native)->[heading (s "profile" native ^ " / NATIVE HISTORY") (if s "title" native="" then "Native conversation" else s "title" native);
        N.div ~attrs:[cls "notice"] [txt "This conversation is not registered with Monty. Import it before taking control."];
        button ~kind:"primary" ~disabled:(not m.online || not(List.is_empty m.pending)) "Import conversation" (command m inject (obj["op",str "import";"profile",field "profile" native;"native_id",field "native_id" native;
          "session_file",field "session_file" native;"title",field "title" native;"context",obj["workspaces",`List[field "cwd" native]] ]));
        N.div (transcript m.history);
        el "details" [el "summary" [txt "Source record"];N.pre ~attrs:[cls "native-history"] [txt(Yojson.Safe.pretty_to_string(field "snapshot" m.history))]]]
    | Some(Chat cid)->
        let chat=List.find m.chats ~f:(fun c->s "id" c=cid) |> Option.value ~default:m.history in
        let sess=List.find (xs "sessions" chat) ~f:(fun s->Poly.equal (field "id" s) (field "selected_session" chat)) |> Option.value ~default:`Null in
        let draft=Map.find m.drafts cid |> Option.value ~default:"" in
        [heading (s "profile" sess ^ " / CONVERSATION") (s "title" chat)] @ transcript ~live:m.live m.history @
        [N.div ~attrs:[A.id "pending-requests";A.create "tabindex" "-1"] (pending_ui m inject chat sess)] @
        [(if Poly.equal (field "observed" sess) (`Bool true) then N.none else
          N.div ~attrs:[cls "notice"] [note "This session is detached. Resume it in Monty after detaching any other native session.";
            el "label" ~attrs:[cls "confirmation"] [N.input ~attrs:[A.type_ "checkbox";A.bool_property "checked" m.detached;
              A.on_change(fun _ _->set(fun m->{m with detached=not m.detached}))] ();
              N.span[txt "Other native sessions are detached"]];
            button ~disabled:(not m.online || not m.detached || not(List.is_empty m.pending)) "Resume in Monty"
              (command m inject(obj["op",str "resume";"conversation",str cid;"revision",field "revision" chat;"detached",`Bool true]))]);
         (match active_turn m.history sess with None->N.none|Some turn ->
            button ~disabled:(not m.online) "Interrupt turn" (command m inject(obj["op",str "interrupt";"conversation",str cid;"selected_session",field "id" sess;"revision",field "revision" chat;"turn_id",str turn])));
         el "form" ~attrs:[cls "composer";A.on_submit(fun _->Effect.Many[Effect.Prevent_default;
          if not m.online || not(Poly.equal(field "observed" sess)(`Bool true)) || String.is_empty(String.strip draft) || not(List.is_empty m.pending) then Effect.Ignore else
          command m inject (obj["op",str "send";"conversation",str cid;"selected_session",field "id" sess;"revision",field "revision" chat;"text",str draft])])]
          [el "label" [N.span[txt "Message"];N.textarea ~attrs:[A.id "message-input";A.value_prop draft;A.placeholder "Think out loud…";A.on_input(fun _ value->edit_text m inject ("draft:" ^ cid) value (fun m->{m with drafts=Map.set m.drafts ~key:cid ~data:value}))] []];
           N.div ~attrs:[cls "composer-actions"] [note "Drafts are saved in this browser.";
             N.button ~attrs:([A.type_ "submit";cls "primary"] @ if not m.online || String.strip draft="" || not(List.is_empty m.pending) || not(Poly.equal(field "observed" sess)(`Bool true)) then [A.disabled] else []) [txt "Send message"]]]]
  in
  N.div ~attrs:[cls "app"] [el "aside" ~attrs:[cls "sidebar"] [N.div ~attrs:[cls "wordmark"] [N.span ~attrs:[cls "brand"] [txt "m"];N.strong[txt "Monty"]];
      N.p ~attrs:[cls "eyebrow"] [txt "YOUR WORKSPACE"];
      el "nav" (List.map ["tasks","Tasks";"chats","Chats"] ~f:(fun (view,label)->button ~kind:(if m.view=view then "nav-item active" else "nav-item") label (navigate view inject)));
      N.div ~attrs:[cls "sidebar-footer"] [tag(if not m.online then "Reconnecting" else if m.events_connected then "Live updates" else "Connecting updates");button "Sign out" (let open Effect.Let_syntax in
        let%bind result=post "browser/logout" (obj[]) in match result with Error e->failure inject e|Ok _->set(fun m->{m with authenticated=false;history=`Null;chats=[];inventory=`Null;answers=String.Map.empty}))]];
    N.main ~attrs:[cls "workspace-main"] [el "header" ~attrs:[cls "topbar"] [N.div [N.p ~attrs:[cls "eyebrow"] [txt "THE CONTROL ROOM"];N.h1[txt(if m.view="tasks" then "Tasks" else "Conversations")]];
        N.div ~attrs:[cls "toolbar"] [
          (match m.selected with Some(Chat cid) when List.exists m.chats ~f:(fun c->s "id" c=cid &&
              List.exists(xs "sessions" c) ~f:(fun sess->not(List.is_empty(xs "pending" sess)))) ->
            button ~kind:"primary" "Review request" (focus_element "pending-requests") | _->N.none);
          (match m.selected with Some(Chat _) when not(Poly.equal m.history `Null)->button "Write a message" (focus_element "message-input")|_->N.none);
          button "Refresh" (refresh m inject);
          (if m.view="chats" then button ~kind:"primary" "New conversation" (set(fun m ->
            let profile=List.find (xs "profiles" m.health) ~f:(fun p->s "id" p=m.profile) |> Option.value ~default:`Null in
            {m with new_chat=true;new_task=false;selected=None;new_title="";new_workspace=(List.hd(xs "roots" profile) |> Option.value_map ~default:"" ~f:text)})) else
              button ~kind:"primary" "New task" (set(fun m->{m with new_task=true;new_chat=false;selected=None;new_title="";new_notes=""})))]];error;
      (if m.storage_error="" then N.none else N.div ~attrs:[cls "notice error";A.create "role" "alert"] [txt m.storage_error]);
      N.div (List.map (xs "warnings" m.inventory) ~f:(fun warning->N.div ~attrs:[cls "notice"] [txt(text warning)]));
      (if Map.is_empty m.receipts then N.none else N.div ~attrs:[cls "notice";A.create "role" "status"]
        [txt (if List.is_empty m.pending then "Actions settled" else "Working…");
         el "details" [el "summary" [txt "Action details"];
           N.div (List.map (Map.data m.receipts) ~f:(fun receipt->N.pre [txt(Yojson.Safe.pretty_to_string receipt)]))];
         N.div(List.filter_map m.pending ~f:(fun (id,body)->
           let receipt=Map.find m.receipts id |> Option.value ~default:`Null in
           if s "status" receipt<>"outcome_unknown" then None else Some(N.div[
             N.strong[txt("Outcome needs review: " ^ s "op" body)];note(s "message" (field "result" receipt));
             note("Receipt: " ^ id ^ ". Check the current task or chat before submitting another action.");
             button "I’ve checked this outcome" (let open Effect.Let_syntax in
               let%bind ()=Effect.of_sync_fun(fun ()->dispatching:=Set.remove !dispatching (command_key body)) () in
               set(fun m->{m with pending=List.Assoc.remove m.pending ~equal:String.equal id}))])));
         (if not(List.is_empty m.pending) then button "Check status" (check_command ~force:true m inject) else N.none)]);
      N.div ~attrs:[cls ("panels " ^ m.view)] [el "section" ~attrs:[cls "catalog"] [N.div ~attrs:[cls "search"] [input "Search this view" m.search (fun search->set(fun m->{m with search}))];
          N.div ~attrs:[cls "filters"] (if m.view="tasks" then [
            dropdown "Task status" m.filter_status ["all","All statuses";"active","Open tasks";"done","Completed tasks"]
              (fun filter_status->set(fun m->{m with filter_status}));
            dropdown "Task project" m.filter_project (("","All projects")::List.map m.projects ~f:(fun p->s "id" p,s "id" p))
              (fun filter_project->set(fun m->{m with filter_project}))]
          else [dropdown "Conversation account" m.filter_account (("","All accounts")::List.map(xs "profiles" m.health) ~f:(fun p->s "id" p,s "id" p))
            (fun filter_account->set(fun m->{m with filter_account}))]);
          note (if m.view="tasks" then (if s "profile" m.inventory="" then "" else "Profile: " ^ s "profile" m.inventory ^ " · ") ^ s "banner" m.inventory else "REGISTERED WITH MONTY");
          N.div (if List.is_empty list then [note(if m.loading then "Loading your workspace…" else "No items match this view.")] else list);
          (if m.view<>"chats" then N.none else N.div ~attrs:[cls "discovery"] [N.h3[txt "Native conversations"];
            profile_select "Account";
            (if List.exists(xs "profiles" m.health) ~f:(fun p->s "id" p=m.profile && s "harness" p="codex") then
              el "label" ~attrs:[cls "confirmation"] [N.input ~attrs:[A.type_ "checkbox";A.bool_property "checked" m.native_archived;
                A.on_change(fun _ _->set(fun m->{m with native_archived=not m.native_archived;native=[];native_cursor=None;discovery_loaded=false;discovery_errors=[]}))] ();
                N.span[txt "Archived native conversations"]] else N.none);
            button ~disabled:(m.discovery_loading || not m.online) "Browse native history" (discover m inject ~more:false);
            note(if m.discovery_loading then "Reading native history…" else if not m.discovery_loaded then
              "Choose an account and browse its native history. Viewing does not import or start a session." else
              Int.to_string(List.length m.native) ^ " native conversations loaded" ^
                (if Option.is_some m.native_cursor then "; more pages are available." else "; end of the available listing for this source and filters.") ^
                (if List.is_empty m.discovery_errors then "" else " Some sources could not be read; coverage is partial."));
            N.div (List.map m.discovery_errors ~f:(fun e->note(s "message" e)));
            N.div (List.filter m.native ~f:matches |> List.map ~f:(fun n->
              let registered=List.find m.chats ~f:(fun chat->not(Poly.equal(field "purged" chat)(`Bool true)) &&
                List.exists(xs "sessions" chat) ~f:(fun sess->List.for_all ["harness";"scope";"native_id"] ~f:(fun key->Poly.equal(field key sess)(field key n)))) in
              match registered with
              | Some chat->row (Chat(s "id" chat)) (s "title" n) (s "cwd" n) "registered"
              | None->row (Native n) (s "title" n) (s "cwd" n) "native"));
            (if Option.is_some m.native_cursor then button ~disabled:m.discovery_loading "Load more" (discover m inject ~more:true) else N.none)])];
        el "section" ~attrs:[cls "detail";A.create "aria-label" "Selected item"] detail]]]

let () = Bonsai_web.Start.start application
