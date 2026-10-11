open! Core
open! Import

let glyph : P.Origin.Kind.t -> string = function
  | Host -> "⇄"
  | Cwd -> "⌂"
  | Editor -> "✎"
  | Plan_mode -> "◇"
  | Delivery -> "↩"
  | Handover -> "↪"
  | Goal -> "◎"
  | Goal_continuation -> "◷"
  | Resume -> "↻"
  | Shell -> "$"
  | Summary -> "≡"
  | Review -> "⚑"
  | Init -> "▤"
  | Other _ -> "•"
;;

let typed (user : P.Message.User.t) =
  match P.Message.User.source user with
  | Some _ -> ""
  | None ->
    let body = P.Message.User.body user in
    (match Skill_message.parse body with
     | Some skill -> Skill_message.invocation skill
     | None -> body)
;;

let first_line text =
  String.split_lines text
  |> List.find ~f:(fun l -> not (String.is_empty (String.strip l)))
  |> Option.value_map ~default:"" ~f:String.strip
;;

let label (user : P.Message.User.t) =
  match P.Message.User.source user with
  | Some { kind; title } -> glyph kind ^ " " ^ title
  | None ->
    (match first_line (typed user), user.images, user.origin with
     | "", image :: _, _ -> P.Image.to_string_hum image
     | "", [], Some { notes = note :: _; _ } ->
       glyph note.kind ^ " " ^ note.title
     | line, _, _ -> line)
;;
