open! Core
open! Import

type t =
  { call_id : string
  ; component : string
  ; title : string
  ; values : Json.t
  }

let of_json (json : Json.t) =
  let field name =
    match json with
    | `Object fields -> List.Assoc.find fields name ~equal:String.equal
    | _ -> None
  in
  let string name =
    match field name with
    | Some (`String s) when not (String.is_empty (String.strip s)) -> Ok (Some s)
    | None | Some `Null -> Ok None
    | Some _ -> Or_error.errorf "ui_action.%s must be a non-empty string" name
  in
  let required name =
    Or_error.bind (string name) ~f:(function
      | Some s -> Ok s
      | None -> Or_error.errorf "ui_action needs %S" name)
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
    { call_id; component; title = Option.value title ~default:component; values }
  | _ ->
    Or_error.error_string
      "param \"ui_action\" must be an object {call_id, component, title, values}"
;;

let text t =
  sprintf
    "The user responded through %S (render_ui call %s, component %s):\n%s"
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
  let line = sprintf "Sent %S: %s" t.title (value t.values) in
  let line = String.tr line ~target:'\n' ~replacement:' ' in
  if String.length line > max_summary
  then fst (Utf8.split_incomplete_suffix (String.prefix line (max_summary - 3))) ^ "..."
  else line
;;

let note t =
  { Injected.Note.kind =
      Ui_action { call_id = t.call_id; component = t.component; summary = summary t }
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
  match
    List.find calls ~f:(fun (call : Content.Tool_call.t) ->
      String.equal call.id t.call_id)
  with
  | None ->
    Or_error.errorf
      "unknown render_ui call %S in this session: answer a component the \
       session shows"
      t.call_id
  | Some call ->
    let ids =
      match Content.Tool_call.parse_arguments call with
      | Ok args -> Ui_components.ids args
      | Error _ -> []
    in
    if List.mem ids t.component ~equal:String.equal
    then Ok ()
    else
      Or_error.errorf
        "render_ui call %s has no component %S%s"
        t.call_id
        t.component
        (match ids with
         | [] -> ""
         | ids -> sprintf " (its ids: %s)" (String.concat ~sep:", " ids))
;;
