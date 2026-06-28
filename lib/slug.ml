let is_alpha_num = function
  | 'a' .. 'z' | '0' .. '9' -> true
  | _ -> false

let trim_dashes s =
  let len = String.length s in
  let left = ref 0 in
  while !left < len && s.[!left] = '-' do
    incr left
  done;
  let right = ref (len - 1) in
  while !right >= !left && s.[!right] = '-' do
    decr right
  done;
  if !right < !left then "task" else String.sub s !left (!right - !left + 1)

let of_title title =
  let b = Buffer.create (String.length title) in
  let add_dash () =
    let len = Buffer.length b in
    if len > 0 && Buffer.nth b (len - 1) <> '-' then Buffer.add_char b '-'
  in
  String.iter
    (fun c ->
      let c = Char.lowercase_ascii c in
      if is_alpha_num c then Buffer.add_char b c else add_dash ())
    title;
  Buffer.contents b |> trim_dashes

let branch ?index title =
  let slug = of_title title in
  match index with
  | None -> "monty/" ^ slug
  | Some index -> Printf.sprintf "monty/%02d-%s" index slug
