open! Core

type t =
  { name : string
  ; text : string
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind name = Json.string_field j "name" in
  let%map text = Json.string_field j "text" in
  { name; text }
;;

let to_json t = `Object [ "name", `String t.name; "text", `String t.text ]
let list_to_json l = `Array (List.map l ~f:to_json)

let name_problem name =
  if String.is_empty name
  then Some "a snippet needs a name"
  else if String.exists name ~f:Char.is_whitespace
  then Some (sprintf "snippet names are one word, not %S" name)
  else None
;;

let find l name = List.find l ~f:(fun t -> String.equal t.name name)

let add l t =
  if Option.is_some (find l t.name)
  then List.map l ~f:(fun s -> if String.equal s.name t.name then t else s)
  else l @ [ t ]
;;

let remove l name = List.filter l ~f:(fun t -> not (String.equal t.name name))

let first_line t =
  String.split_lines t.text
  |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
  |> Option.value_map ~default:"" ~f:String.strip
;;
