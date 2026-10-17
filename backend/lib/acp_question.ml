open! Core
open! Import

let of_event questions =
  User_question.of_arguments (`Object [ "questions", questions ])
;;

let message = function
  | [ (q : User_question.t) ] -> q.question
  | questions ->
    String.concat
      ~sep:"\n"
      (List.mapi questions ~f:(fun i (q : User_question.t) ->
         sprintf "%d. %s" (i + 1) (Option.value q.header ~default:q.question)))
;;

let key i = sprintf "q%d" (i + 1)
let other_key i = key i ^ "_other"

let choice_option (c : User_question.Choice.t) =
  `Object
    [ "const", `String c.label
    ; ( "title"
      , `String
          (match c.description with
           | Some d -> sprintf "%s: %s" c.label d
           | None -> c.label) )
    ]
;;

let schema questions =
  let properties =
    List.concat_mapi questions ~f:(fun i (q : User_question.t) ->
      let title = `String q.question in
      match q.choices with
      | [] -> [ key i, `Object [ "type", `String "string"; "title", title ] ]
      | choices ->
        let options = List.map choices ~f:choice_option in
        let main =
          if q.multiple
          then
            `Object
              [ "type", `String "array"
              ; "title", title
              ; "items", `Object [ "anyOf", `Array options ]
              ]
          else
            `Object [ "type", `String "string"; "title", title; "oneOf", `Array options ]
        in
        [ key i, main
        ; ( other_key i
          , `Object
              [ "type", `String "string"
              ; "title", `String "Or your own answer"
              ] )
        ])
  in
  `Object
    [ "type", `String "object"
    ; "properties", `Object properties
    ; "required", `Array []
    ]
;;

let answers questions content =
  let get name = Json.member name content in
  let text name =
    match get name with
    | Some (`String s) when not (String.is_empty (String.strip s)) -> [ s ]
    | _ -> []
  in
  List.mapi questions ~f:(fun i (_ : User_question.t) ->
    let picked =
      match get (key i) with
      | Some (`Array items) -> List.filter_map items ~f:Json.string
      | _ -> []
    in
    picked @ text (key i) @ text (other_key i))
;;

let single_choice = function
  | [ ({ choices = _ :: _; multiple = false; _ } : User_question.t) as q ] ->
    Some q.choices
  | _ -> None
;;
