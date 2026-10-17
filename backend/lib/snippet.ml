open! Core
open! Import

type t =
  { name : string
  ; text : string
  }
[@@deriving sexp_of, equal]

let to_json t = `Object [ "name", `String t.name; "text", `String t.text ]

let of_json ~index json =
  let where = sprintf "config.snippets[%d]" index in
  match json with
  | `Object fields ->
    let find name = List.Assoc.find fields ~equal:String.equal name in
    (match find "name", find "text" with
     | Some (`String name), _
       when String.is_empty name || String.exists name ~f:Char.is_whitespace ->
       Or_error.errorf
         "%s.name %S must be one word: non-empty, without spaces"
         where
         name
     | Some (`String name), Some (`String text) ->
       if String.is_empty (String.strip text)
       then
         Or_error.errorf
           "%s (%S) has an empty text; give it some or remove it"
           where
           name
       else Ok { name; text }
     | Some (`String name), _ ->
       Or_error.errorf "%s (%S) needs a \"text\" string" where name
     | _ -> Or_error.errorf "%s needs a \"name\" string" where)
  | _ ->
    Or_error.errorf
      "%s must be an object like {\"name\": \"review\", \"text\": \"...\"}"
      where
;;

let list_of_json = function
  | `Null -> Ok []
  | `Array items ->
    let open Or_error.Let_syntax in
    let%bind snippets =
      Or_error.all (List.mapi items ~f:(fun index -> of_json ~index))
    in
    (match
       List.find_a_dup snippets ~compare:(fun a b ->
         String.compare a.name b.name)
     with
     | Some dup ->
       Or_error.errorf
         "config.snippets has more than one snippet named %S; rename or remove \
          all but one"
         dup.name
     | None -> Ok snippets)
  | _ ->
    Or_error.error_string
      "config.snippets must be an array of {\"name\": ..., \"text\": ...} \
       objects"
;;
