open! Core
open! Import

exception Invalid of string

let invalid fmt = ksprintf (fun s -> raise (Invalid s)) fmt

let field json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> invalid "arguments must be a JSON object"
;;

let string_opt json name =
  match field json name with
  | None | Some `Null -> None
  | Some (`String s) -> Some s
  | Some _ -> invalid "argument %S must be a string" name
;;

let string json name =
  match string_opt json name with
  | Some s -> s
  | None -> invalid "missing required argument %S" name
;;

let int_opt json name =
  match field json name with
  | None | Some `Null -> None
  | Some (`Number n) ->
    (match Int.of_string_opt n with
     | Some i -> Some i
     | None ->
       (match Float.of_string_opt n with
        | Some f when Float.is_integer f -> Some (Float.to_int f)
        | _ -> invalid "argument %S must be an integer" name))
  | Some _ -> invalid "argument %S must be an integer" name
;;

let bool_opt json name =
  match field json name with
  | None | Some `Null -> None
  | Some `True -> Some true
  | Some `False -> Some false
  | Some _ -> invalid "argument %S must be a boolean" name
;;

let list_opt json name =
  match field json name with
  | None | Some `Null -> None
  | Some (`Array l) -> Some l
  | Some _ -> invalid "argument %S must be an array" name
;;

let schema ?(required = []) properties =
  let property (name, kind, description) =
    let typed =
      match kind with
      | `String -> [ "type", `String "string" ]
      | `Integer -> [ "type", `String "integer" ]
      | `Boolean -> [ "type", `String "boolean" ]
      | `Array items -> [ "type", `String "array"; "items", items ]
    in
    name, `Object (typed @ [ "description", `String description ])
  in
  `Object
    [ "type", `String "object"
    ; "properties", `Object (List.map properties ~f:property)
    ; "required", `Array (List.map required ~f:(fun r -> `String r))
    ; "additionalProperties", `False
    ]
;;
