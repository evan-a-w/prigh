open! Core
open! Import

open Html

let chip ?(cls = "") text = span (String.strip ("chip " ^ cls)) text

(* The first line of a multi-line argument, marked as continuing. *)
let first_line s =
  let s = String.strip s in
  match String.lsplit2 s ~on:'\n' with
  | Some (line, _) -> line ^ " …"
  | None -> s
;;

let summary (call : Tool_call.t) =
  let args = Tool_args.of_call call in
  List.find_map
    [ "command"; "path"; "pattern"; "task"; "id"; "prefix" ]
    ~f:(Tool_args.string args)
  |> Option.value
       ~default:(String.concat ~sep:" " (Tool_args.strings args "ids"))
;;

module Status = struct
  type t =
    | Preparing
    | Running
    | Done
    | Failed

  let cls = function
    | Preparing | Running -> "running"
    | Done -> "ok"
    | Failed -> "error"
  ;;

  let icon t =
    match t with
    | Preparing | Running -> Node.span ~attrs:[ Attr.class_ "spinner" ] []
    | Done -> span "icon" "✓"
    | Failed -> span "icon" "✕"
  ;;
end

(* bash appends how a failed command ended as its last line. *)
let bash_outcome text =
  let lines = String.split_lines (String.rstrip text) in
  match List.last lines with
  | Some last
    when String.is_prefix last ~prefix:"["
         && String.is_suffix last ~suffix:"]"
         && List.exists
              [ "[exit code "; "[timed out after "; "[killed by "; "[cancelled" ]
              ~f:(fun prefix -> String.is_prefix last ~prefix) ->
    ( Some (String.sub last ~pos:1 ~len:(String.length last - 2))
    , String.concat ~sep:"\n" (List.drop_last_exn lines) )
  | _ -> None, text
;;

let job_started text =
  let open Option.Let_syntax in
  let%bind rest = String.chop_prefix text ~prefix:"started job " in
  let%map id, _ = String.lsplit2 rest ~on:':' in
  id
;;

let output ?(error = false) ?(head = 6) ?(tail = 0) text =
  if String.is_empty (String.strip text)
  then Node.none
  else Output_view.view ~error ~head ~tail text
;;

let error_output (r : Message.Tool_result.t) = output ~error:true ~head:12 r.text

(* The subagent's latest step, for a running card. *)
let last_activity chat =
  List.rev (Chat.entries chat)
  |> List.find_map ~f:(fun (entry : Chat.Entry.t) ->
    match entry with
    | Assistant { message; _ } ->
      List.rev message.content
      |> List.find_map ~f:(fun (content : Content.t) ->
        match content with
        | Tool_call call -> Some (call.name ^ " " ^ first_line (summary call))
        | Text t when not (String.is_empty (String.strip t)) ->
          List.last (String.split_lines (String.strip t))
        | Thinking t when not (String.is_empty (String.strip t)) ->
          Some "thinking…"
        | Text _ | Thinking _ -> None)
    | User _ | Notice _ | Compaction _ -> None)
;;

let subagent_body ~nested (s : Chat.Subagent.t) ~running =
  let transcript =
    match Chat.entries s.chat with
    | [] -> Node.none
    | entries ->
      Node.details
        ~attrs:[ Attr.class_ "transcript" ]
        [ Node.summary [ Node.text (sprintf "Transcript · %s" (plural (List.length entries) "message")) ]
        ; nested s.chat
        ]
  in
  let activity =
    if running
    then (
      match last_activity s.chat with
      | Some a -> div "activity" [ span "arrow" "↳"; span "text" a ]
      | None -> div "activity" [ span "arrow" "↳"; span "text" "starting…" ])
    else Node.none
  in
  let report =
    match s.result with
    | Some { text; is_error = false } when not (String.is_empty (String.strip text)) ->
      folded
        ~cls:"report"
        ~label:"Report"
        ~preview:(first_line text)
        (Markdown_view.render text)
    | Some { text; is_error = true } -> output ~error:true ~head:12 text
    | Some _ | None -> Node.none
  in
  [ activity; report; transcript ]
;;

let task_details task =
  if String.mem (String.strip task) '\n'
  then
    folded
      ~cls:"task"
      ~label:"Task"
      ~preview:(first_line task)
      (div "task-text" [ Node.text task ])
  else Node.none
;;

let view ~nested ~streaming (call : Tool_call.t) (tool : Chat.Tool.t option) =
  let args = Tool_args.of_call call in
  let str key = Option.value (Tool_args.string args key) ~default:"" in
  let result = Option.bind tool ~f:(fun t -> t.result) in
  let live = Option.value_map tool ~default:"" ~f:(fun t -> t.output) in
  let subagent = Option.bind tool ~f:(fun t -> t.subagent) in
  let status : Status.t =
    match result, subagent with
    | _, Some { result = None; _ } -> Running
    | _, Some { result = Some { is_error = true; _ }; _ } -> Failed
    | Some { is_error = true; _ }, _ -> Failed
    | Some _, _ -> Done
    | None, _ -> if streaming then Preparing else Running
  in
  let failed = function
    | Some ({ is_error = true; _ } : Message.Tool_result.t) -> true
    | _ -> false
  in
  let arg, chips, body =
    match call.name with
    | "bash" ->
      let command = str "command" in
      let background = Option.value (Tool_args.bool args "background") ~default:false in
      let outcome, text =
        match result with
        | Some r -> bash_outcome r.text
        | None -> None, live
      in
      let job = Option.bind result ~f:(fun r -> job_started r.text) in
      let chips =
        List.filter_opt
          [ Option.some_if (background && Option.is_none job) (chip "background")
          ; Option.map job ~f:(fun id -> chip ~cls:"job" ("job " ^ id))
          ; Option.map outcome ~f:(chip ~cls:"bad")
          ]
      in
      let body =
        [ (if String.mem (String.strip command) '\n'
           then Node.pre ~attrs:[ Attr.class_ "command" ] [ Node.text command ]
           else Node.none)
        ; (if Option.is_some job
           then Node.none
           else (
             match result with
             | None -> output ~head:0 ~tail:8 text
             | Some r -> output ~error:r.is_error ~head:4 ~tail:8 text))
        ]
      in
      Some (span "arg command" (first_line command)), chips, body
    | "read" ->
      let range =
        match Tool_args.int args "offset", Tool_args.int args "limit" with
        | None, None -> []
        | Some o, None -> [ chip (sprintf "from line %d" o) ]
        | None, Some l -> [ chip (plural l "line") ]
        | Some o, Some l -> [ chip (sprintf "lines %d–%d" o (o + l - 1)) ]
      in
      let body =
        match result with
        | None -> []
        | Some ({ is_error = true; _ } as r) -> [ error_output r ]
        | Some { images = _ :: _ as images; text; _ } ->
          [ Image_view.thumbs images; span "caption" text ]
        | Some { text; _ } ->
          [ Node.details
              ~attrs:[ Attr.class_ "file" ]
              [ Node.summary [ Node.text (plural (Output_view.line_count text) "line") ]
              ; Node.pre [ Node.text text ]
              ]
          ]
      in
      Some (span "arg path" (str "path")), range, body
    | "write" ->
      let content = str "content" in
      let lines = Output_view.line_count content in
      ( Some (span "arg path" (str "path"))
      , [ chip (plural lines "line") ]
      , [ (match result with
           | Some ({ is_error = true; _ } as r) -> error_output r
           | _ -> Node.none)
        ; (if String.is_empty content
           then Node.none
           else Output_view.view ~head:(if streaming then 0 else 8) ~tail:(if streaming then 8 else 0) content)
        ] )
    | "edit" ->
      let edits = Tool_args.edits args in
      let diff =
        match result with
        | Some { is_error = false; text; _ } when Diff_view.is_unified text -> `Unified text
        | _ -> `Edits edits
      in
      let added, removed =
        match diff with
        | `Unified text -> Diff_view.unified_counts text
        | `Edits edits -> Diff_view.edits_counts edits
      in
      ( Some (span "arg path" (str "path"))
      , (if added + removed = 0
         then []
         else [ span "chip add" (sprintf "+%d" added); span "chip del" (sprintf "−%d" removed) ])
      , [ (match result with
           | Some ({ is_error = true; _ } as r) -> error_output r
           | _ -> Node.none)
        ; (match diff with
           | `Unified text -> Diff_view.unified text
           | `Edits [] -> Node.none
           | `Edits edits -> Diff_view.edits edits)
        ] )
    | ("ls" | "grep" | "find") as name ->
      let arg =
        match name with
        | "ls" -> Option.value (Tool_args.string args "path") ~default:"."
        | _ -> str "pattern"
      in
      let chips =
        List.filter_opt
          [ (match name with
             | "ls" -> None
             | _ -> Option.map (Tool_args.string args "path") ~f:(fun p -> chip ("in " ^ p)))
          ; Option.map (Tool_args.string args "glob") ~f:chip
          ; Option.bind (Tool_args.bool args "ignore_case") ~f:(fun b ->
              Option.some_if b (chip "ignore case"))
          ]
      in
      let body =
        match result with
        | None -> []
        | Some r -> [ output ~error:r.is_error ~head:8 r.text ]
      in
      Some (span ("arg " ^ if String.equal name "ls" then "path" else "pattern") arg), chips, body
    | "subagent" ->
      let task = str "task" in
      let model =
        match subagent with
        | Some s -> Some s.model
        | None -> Tool_args.string args "model"
      in
      let chips =
        List.filter_opt
          [ Option.map model ~f:(chip ~cls:"model")
          ; Option.bind subagent ~f:(fun s ->
              Option.some_if (s.turns > 0) (chip (plural s.turns "turn")))
          ; Option.bind subagent ~f:(fun s ->
              Option.map s.cost_usd ~f:(fun c -> chip (sprintf "$%.2f" c)))
          ]
      in
      let body =
        task_details task
        ::
        (match subagent with
         | Some s -> subagent_body ~nested s ~running:(Option.is_none s.result)
         | None ->
           (match result with
            | Some ({ is_error = true; _ } as r) -> [ error_output r ]
            | Some _ | None -> []))
      in
      Some (span "arg task" (first_line task)), chips, body
    | _ ->
      let arg = first_line (summary call) in
      let body =
        match result with
        | None -> [ output ~head:0 ~tail:6 live ]
        | Some r -> [ output ~error:r.is_error ~head:8 r.text; Image_view.thumbs r.images ]
      in
      Option.some_if (not (String.is_empty arg)) (span "arg" arg), [], body
  in
  let status = if failed result && Poly.equal status Status.Done then Status.Failed else status in
  let body = present body in
  div
    (String.concat ~sep:" " [ "tool"; "tool-" ^ call.name; Status.cls status ])
    [ div
        "tool-head"
        ([ Status.icon status; span "name" call.name ]
         @ Option.to_list arg
         @ chips)
    ; (match body with
       | [] -> Node.none
       | body -> div "tool-body" body)
    ]
;;
