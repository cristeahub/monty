let toml_basic_string value =
  let buffer = Buffer.create (String.length value + 8) in
  Buffer.add_char buffer '"';
  String.iter
    (fun character ->
      match character with
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\b' -> Buffer.add_string buffer "\\b"
      | '\t' -> Buffer.add_string buffer "\\t"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\012' -> Buffer.add_string buffer "\\f"
      | '\r' -> Buffer.add_string buffer "\\r"
      | character when Char.code character < 0x20 || Char.code character = 0x7f ->
          Buffer.add_string buffer (Printf.sprintf "\\u%04X" (Char.code character))
      | character -> Buffer.add_char buffer character)
    value;
  Buffer.add_char buffer '"';
  Buffer.contents buffer

let argument path =
  let path =
    try Unix.realpath path
    with Unix.Unix_error _ -> Shell.normalize (Shell.abs_path path)
  in
  " -c "
  ^ Shell.quote
      ("projects." ^ toml_basic_string path ^ ".trust_level=\"trusted\"")

let arguments paths = paths |> List.map argument |> String.concat ""
