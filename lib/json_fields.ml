let member_string json name =
  match Yojson.Safe.Util.member name json with
  | `String value when String.trim value <> "" -> Ok value
  | _ -> Error (Printf.sprintf "missing string field %S" name)

let optional_string json name =
  match Yojson.Safe.Util.member name json with
  | `Null -> Ok None
  | `String value when String.trim value = "" -> Ok None
  | `String value -> Ok (Some value)
  | _ -> Error (Printf.sprintf "field %S must be a string when present" name)

let list_field json name =
  match Yojson.Safe.Util.member name json with
  | `Null -> Ok []
  | `List values -> Ok values
  | _ -> Error (Printf.sprintf "field %S must be an array" name)

let read_json_file path =
  try Ok (Yojson.Safe.from_file path) with
  | Sys_error msg -> Error msg
  | Yojson.Json_error msg -> Error ("invalid JSON in " ^ path ^ ": " ^ msg)

let fold_results values ~init ~f =
  List.fold_left
    (fun acc value ->
      match acc with Error _ as err -> err | Ok acc -> f acc value)
    (Ok init) values

let ( let* ) = Result.bind

let field name = function
  | `Assoc fields -> Option.value ~default:`Null (List.assoc_opt name fields)
  | _ -> `Null

let str = function `String value -> value | _ -> invalid_arg "expected string"
let int = function `Int value -> value | _ -> invalid_arg "expected integer"
let num = function
  | `Float value -> value
  | `Int value -> float_of_int value
  | `Null -> 0.
  | _ -> invalid_arg "expected time"
let list = function `List values -> values | _ -> invalid_arg "expected list"
let s key json = str (field key json)
let n key json = num (field key json)
let i key json = int (field key json)
let xs key json = list (field key json)
let put key value = function
  | `Assoc fields -> `Assoc ((key, value) :: List.remove_assoc key fields)
  | _ -> invalid_arg "expected object"
let puts fields json = List.fold_left (fun json (key, value) -> put key value json) json fields
let bounded_string max = function
  | `String value when String.length value > 0 && String.length value <= max -> value
  | _ -> invalid_arg "missing or oversized string"
let text key max json = bounded_string max (field key json)
