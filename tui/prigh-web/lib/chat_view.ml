open! Core
open! Import

let div ?(cls = "") children =
  Node.div
    ~attrs:(if String.is_empty cls then [] else [ Attr.class_ cls ])
    children
;;

let span ?(cls = "") text =
  Node.span
    ~attrs:(if String.is_empty cls then [] else [ Attr.class_ cls ])
    [ Node.text text ]
;;

let image_src (image : Image.t) =
  sprintf "data:%s;base64,%s" image.mime_type image.data
;;

let images ?(cls = "images") list =
  match list with
  | [] -> Node.none
  | list ->
    div
      ~cls
      (List.map list ~f:(fun (image : Image.t) ->
         Node.img
           ~attrs:
             [ Attr.class_ "thumb"
             ; Attr.src (image_src image)
             ; Attr.alt (Image.to_string_hum image)
             ; Attr.title (Image.to_string_hum image)
             ]
           ()))
;;

let tool_summary (call : Tool_call.t) =
  match Json.parse call.arguments with
  | Ok json ->
    List.find_map
      [ "command"; "path"; "pattern"; "task"; "prefix" ]
      ~f:(fun key ->
        match Json.field json key with
        | Some (`String s) -> Some s
        | _ -> None)
    |> Option.value ~default:""
  | Error _ -> ""
;;

let first_line s =
  match String.lsplit2 s ~on:'\n' with
  | Some (line, _) -> line ^ " …"
  | None -> s
;;

let rec tool_view chat (call : Tool_call.t) =
  let tool = Chat.tool chat call.id in
  let status, status_cls =
    match tool with
    | Some { result = Some { is_error = true; _ }; _ } ->
      "error", "status-error"
    | Some { result = Some _; _ } -> "done", "status-ok"
    | Some { result = None; _ } | None -> "running", "status-running"
  in
  let body =
    match tool with
    | None -> []
    | Some { result = Some result; _ } ->
      [ (if String.is_empty result.text
         then Node.none
         else
           Node.pre
             ~attrs:[ Attr.class_ "tool-output" ]
             [ Node.text result.text ])
      ; images result.images
      ]
    | Some { output; _ } ->
      if String.is_empty output
      then []
      else
        [ Node.pre ~attrs:[ Attr.class_ "tool-output" ] [ Node.text output ] ]
  in
  let subagent =
    match tool with
    | Some { subagent = Some s; _ } ->
      div
        ~cls:"subagent"
        [ div ~cls:"subagent-task" [ Node.text s.task ]; view s.chat ]
    | _ -> Node.none
  in
  Node.details
    ~attrs:[ Attr.class_ ("tool " ^ status_cls) ]
    ([ Node.summary
         [ span ~cls:"tool-name" call.name
         ; span ~cls:"tool-arg" (first_line (tool_summary call))
         ; span ~cls:("tool-status " ^ status_cls) status
         ]
     ; subagent
     ]
     @ body)

and entry_view chat (entry : Chat.Entry.t) =
  match entry with
  | User { text; images = list } ->
    div ~cls:"msg user" [ div ~cls:"bubble" [ Node.text text ]; images list ]
  | Notice text -> div ~cls:"msg notice" [ Node.text text ]
  | Assistant { message; streaming } ->
    let blocks =
      List.map message.content ~f:(function
        | Text "" -> Node.none
        | Text text -> Markdown_view.render text
        | Thinking "" -> Node.none
        | Thinking text ->
          Node.details
            ~attrs:[ Attr.class_ "thinking" ]
            [ Node.summary [ Node.text "Thinking" ]
            ; div ~cls:"thinking-text" [ Node.text text ]
            ]
        | Tool_call call -> tool_view chat call)
    in
    let error =
      match message.stop_reason with
      | Error e -> div ~cls:"error-box" [ Node.text e ]
      | Aborted -> div ~cls:"aborted" [ Node.text "aborted" ]
      | End_turn | Tool_use | Length -> Node.none
    in
    div
      ~cls:(if streaming then "msg assistant streaming" else "msg assistant")
      (blocks @ [ error ])

and view chat =
  div ~cls:"entries" (List.map (Chat.entries chat) ~f:(entry_view chat))
;;
