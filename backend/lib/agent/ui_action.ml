open! Core
open! Import

type t =
  { call_id : string
  ; component : string
  ; title : string
  ; values : Json.t
  }
[@@deriving sexp_of]

let is_control c = Char.to_int c < 0x20 || Char.to_int c = 0x7f

(* Runs of whitespace and control characters as one space: a title is
   quoted on one line of the model's text and is the note's title. *)
let one_line s =
  let buf = Buffer.create (String.length s) in
  let space = ref false in
  String.iter s ~f:(fun c ->
    if Char.is_whitespace c || is_control c
    then space := true
    else (
      if !space && Buffer.length buf > 0 then Buffer.add_char buf ' ';
      space := false;
      Buffer.add_char buf c));
  Buffer.contents buf
;;

let of_json (json : Json.t) =
  let field name =
    match json with
    | `Object fields -> List.Assoc.find fields name ~equal:String.equal
    | _ -> None
  in
  let string name =
    match field name with
    | Some (`String s) when not (String.is_empty (String.strip s)) ->
      Ok (Some s)
    | None | Some `Null -> Ok None
    | Some _ -> Or_error.errorf "ui_action.%s must be a non-empty string" name
  in
  let required name =
    Or_error.bind (string name) ~f:(function
      | None -> Or_error.errorf "ui_action needs %S" name
      | Some s when String.exists s ~f:is_control ->
        Or_error.errorf
          "ui_action.%s %S contains control characters: send the id as the \
           render_ui call has it"
          name
          s
      | Some s -> Ok s)
  in
  match json with
  | `Object _ ->
    let open Or_error.Let_syntax in
    let%bind call_id = required "call_id" in
    let%bind component = required "component" in
    let%bind title = string "title" in
    let%map values =
      match field "values" with
      | Some values -> Ok values
      | None -> Or_error.error_string "ui_action needs \"values\""
    in
    let title =
      match Option.map title ~f:one_line with
      | Some title when not (String.is_empty title) -> title
      | Some _ | None -> component
    in
    { call_id; component; title; values }
  | _ ->
    Or_error.error_string
      "param \"ui_action\" must be an object {call_id, component, title, \
       values}"
;;

let text t =
  sprintf
    "The user responded through \"%s\" (render_ui call %s, component %s):\n%s"
    t.title
    t.call_id
    t.component
    (Json.to_string t.values)
;;

let max_summary = 160

let summary t =
  let rec value : Json.t -> string = function
    | `String s -> s
    | `Number n -> n
    | `True -> "yes"
    | `False -> "no"
    | `Null -> "nothing"
    | `Array [] -> "none"
    | `Array items -> String.concat ~sep:", " (List.map items ~f:value)
    | `Object [] -> "nothing"
    | `Object fields ->
      String.concat
        ~sep:", "
        (List.map fields ~f:(fun (k, v) ->
           match v with
           | `Array (_ :: _ :: _) -> sprintf "%s = [%s]" k (value v)
           | v -> sprintf "%s = %s" k (value v)))
  in
  let line = one_line (sprintf "Sent \"%s\": %s" t.title (value t.values)) in
  if String.length line > max_summary
  then
    fst (Utf8.split_incomplete_suffix (String.prefix line (max_summary - 3)))
    ^ "..."
  else line
;;

let note t =
  { Injected.Note.kind =
      Ui_action
        { call_id = t.call_id; component = t.component; summary = summary t }
  ; text = text t
  }
;;

let check t messages =
  let calls =
    List.concat_map messages ~f:(function
      | Message.Assistant a ->
        List.filter (Message.Assistant.tool_calls a) ~f:(fun call ->
          String.equal call.name Tool_render_ui.name)
      | User _ | Tool_result _ -> [])
  in
  let failed =
    List.exists messages ~f:(function
      | Message.Tool_result r ->
        r.is_error && String.equal r.tool_call_id t.call_id
      | User _ | Assistant _ -> false)
  in
  match
    List.find calls ~f:(fun (call : Content.Tool_call.t) ->
      String.equal call.id t.call_id)
  with
  | None ->
    Or_error.errorf
      "unknown render_ui call %S in this session: answer a component the \
       session shows"
      t.call_id
  | Some _ when failed ->
    Or_error.errorf
      "render_ui call %s failed, so nothing of it was shown: answer a \
       component the session shows"
      t.call_id
  | Some call ->
    let ids =
      match Content.Tool_call.parse_arguments call with
      | Ok args -> Ui_components.interactive_ids args
      | Error _ -> []
    in
    if List.mem ids t.component ~equal:String.equal
    then Ok ()
    else
      Or_error.errorf
        "render_ui call %s has no interactive component %S%s"
        t.call_id
        t.component
        (match ids with
         | [] -> " (it has none)"
         | ids ->
           sprintf
             " (its forms, choices and checklists with submit: %s)"
             (String.concat ~sep:", " ids))
;;
