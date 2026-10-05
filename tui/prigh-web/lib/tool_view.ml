open! Core
open! Import
open Chat_html

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
    [ "command"; "path"; "pattern"; "task"; "id"; "url"; "query" ]
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
    | Interrupted

  let cls = function
    | Preparing | Running -> "running"
    | Done -> "ok"
    | Failed -> "error"
    | Interrupted -> "interrupted"
  ;;

  let icon t =
    match t with
    | Preparing | Running -> Node.span ~attrs:[ Attr.class_ "spinner" ] []
    | Done -> span "icon" "✓"
    | Failed -> span "icon" "✕"
    | Interrupted -> span "icon" "■"
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
              [ "[exit code "
              ; "[timed out after "
              ; "[killed by "
              ; "[cancelled"
              ]
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

let error_output (r : Message.Tool_result.t) =
  output ~error:true ~head:12 r.text
;;

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

let report text =
  if String.is_empty (String.strip text)
  then Node.none
  else
    folded
      ~cls:"report"
      ~label:"Report"
      ~preview:(Markdown.preview text)
      (Markdown_view.render text)
;;

let subagent_body ~nested (s : Chat.Subagent.t) =
  let transcript =
    match Chat.entries s.chat with
    | [] -> Node.none
    | entries ->
      folded
        ~cls:"transcript"
        ~label:"Transcript"
        ~preview:(plural (List.length entries) "message")
        (nested s.chat)
  in
  let activity =
    match s.result with
    | Some _ -> Node.none
    | None ->
      div
        "activity"
        [ span "arrow" "↳"
        ; span "text" (Option.value (last_activity s.chat) ~default:"starting…")
        ]
  in
  let result =
    match s.result with
    | Some { text; is_error = false } ->
      report (Subagent_report.of_string text).text
    | Some { text; is_error = true } ->
      output ~error:true ~head:12 (Subagent_report.of_string text).text
    | None -> Node.none
  in
  [ activity; result; transcript ]
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

(* Reports of finished jobs or subagents ([job_wait], [job_kill],
   [subagent_wait]), then perhaps a note of what is still running. *)
let reports text =
  let text, note =
    match
      String.substr_index_all text ~may_overlap:false ~pattern:"\n\n"
      |> List.last
    with
    | Some i
      when List.exists
             [ "still running: "; "timed out; still running: " ]
             ~f:(fun prefix ->
               String.is_substring_at text ~pos:(i + 2) ~substring:prefix) ->
      String.prefix text i, String.drop_prefix text (i + 2)
    | _ -> text, ""
  in
  match Prigh_ui.Delivery.parse text with
  | Some sections -> [ Delivery_view.view sections; output ~head:8 note ]
  | None ->
    [ output
        ~head:8
        (String.concat
           ~sep:"\n\n"
           (List.filter [ text; note ] ~f:(Fn.non String.is_empty)))
    ]
;;

let cancelled (r : Message.Tool_result.t) =
  let text = String.strip r.text in
  r.is_error
  && (String.is_prefix text ~prefix:"[cancelled]"
      || String.is_suffix text ~suffix:"[cancelled]")
;;

type presentation =
  { arg : Node.t option
  ; chips : Node.t list
  ; body : Node.t list
  }

let present ~nested ~streaming ~live (call : Tool_call.t) ~result ~subagent =
  let args = Tool_args.of_call call in
  let str key = Option.value (Tool_args.string args key) ~default:"" in
  let error_body =
    match result with
    | Some ({ is_error = true; _ } as r : Message.Tool_result.t) ->
      error_output r
    | _ -> Node.none
  in
  match call.name with
  | "bash" ->
    let command = str "command" in
    let background =
      Option.value (Tool_args.bool args "background") ~default:false
    in
    let outcome, text =
      match result with
      | Some r -> bash_outcome r.text
      | None -> None, live
    in
    let job = Option.bind result ~f:(fun r -> job_started r.text) in
    { arg = Some (span "arg command" (first_line command))
    ; chips =
        List.filter_opt
          [ Option.some_if
              (background && Option.is_none job)
              (chip "background")
          ; Option.map job ~f:(fun id -> chip ~cls:"job" ("job " ^ id))
          ; Option.map outcome ~f:(fun outcome ->
              chip
                ~cls:(if String.equal outcome "cancelled" then "" else "bad")
                outcome)
          ]
    ; body =
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
    }
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
      | Some { is_error = true; _ } -> [ error_body ]
      | Some { images = _ :: _ as images; text; _ } ->
        [ Image_view.thumbs images; span "caption" text ]
      | Some { text; _ } ->
        [ Node.details
            ~attrs:[ Attr.class_ "file" ]
            [ Node.summary
                [ Node.text (plural (Output_view.line_count text) "line") ]
            ; Node.pre [ Node.text text ]
            ]
        ]
    in
    { arg = Some (span "arg path" (str "path")); chips = range; body }
  | "write" ->
    let content = str "content" in
    let overwrote =
      match result with
      | Some { is_error = false; text; _ } ->
        String.is_prefix text ~prefix:"overwrote "
      | _ -> false
    in
    { arg = Some (span "arg path" (str "path"))
    ; chips =
        List.filter_opt
          [ Some (chip (plural (Output_view.line_count content) "line"))
          ; Option.some_if overwrote (chip "overwrote")
          ]
    ; body =
        [ error_body
        ; (if String.is_empty content
           then Node.none
           else if streaming
           then Output_view.view ~head:0 ~tail:8 content
           else Output_view.view ~head:8 ~tail:0 content)
        ]
    }
  | "edit" ->
    let diff =
      match result with
      | Some { is_error = false; text; _ } when Diff_view.is_unified text ->
        `Unified text
      | _ -> `Edits (Tool_args.edits args)
    in
    let added, removed =
      match diff with
      | `Unified text -> Diff_view.unified_counts text
      | `Edits edits -> Diff_view.edits_counts edits
    in
    { arg = Some (span "arg path" (str "path"))
    ; chips =
        (if added + removed = 0
         then []
         else
           [ chip ~cls:"add" (sprintf "+%d" added)
           ; chip ~cls:"del" (sprintf "−%d" removed)
           ])
    ; body =
        [ error_body
        ; (match diff with
           | `Unified text -> Diff_view.unified text
           | `Edits [] -> Node.none
           | `Edits edits -> Diff_view.edits edits)
        ]
    }
  | ("ls" | "grep" | "find") as name ->
    let ls = String.equal name "ls" in
    let arg =
      if ls
      then Option.value (Tool_args.string args "path") ~default:"."
      else str "pattern"
    in
    { arg = Some (span (if ls then "arg path" else "arg pattern") arg)
    ; chips =
        List.filter_opt
          [ (if ls
             then None
             else
               Option.map (Tool_args.string args "path") ~f:(fun p ->
                 chip ("in " ^ p)))
          ; Option.map (Tool_args.string args "glob") ~f:chip
          ; Option.bind (Tool_args.bool args "ignore_case") ~f:(fun b ->
              Option.some_if b (chip "ignore case"))
          ]
    ; body =
        (match result with
         | None -> []
         | Some r -> [ output ~error:r.is_error ~head:8 r.text ])
    }
  | "subagent" ->
    let task = str "task" in
    let model =
      match subagent with
      | Some (s : Chat.Subagent.t) -> Some s.model
      | None -> Tool_args.string args "model"
    in
    let started =
      match result with
      | Some { is_error = false; text; _ } -> Subagent_report.started text
      | _ -> None
    in
    { arg = Some (span "arg task" (first_line task))
    ; chips =
        List.filter_opt
          [ Option.map started ~f:(fun id -> chip ~cls:"job" ("agent " ^ id))
          ; Option.map model ~f:(chip ~cls:"model")
          ; Option.bind subagent ~f:(fun s ->
              Option.some_if (s.turns > 0) (chip (plural s.turns "turn")))
          ; Option.bind subagent ~f:(fun s ->
              Option.map s.cost_usd ~f:(fun c -> chip (sprintf "$%.2f" c)))
          ]
    ; body =
        task_details task
        ::
        (match subagent, result with
         | Some s, _ -> subagent_body ~nested s
         | None, Some { is_error = true; _ } -> [ error_body ]
         | None, Some { text; _ } when Option.is_none started ->
           [ report (Subagent_report.of_string text).text ]
         | None, _ -> [])
    }
  | "job_wait" | "job_kill" | "subagent_wait" | "subagent_cancel" ->
    let ids = Tool_args.strings args "ids" @ Tool_args.strings args "id" in
    { arg =
        Option.some_if
          (not (List.is_empty ids))
          (span "arg" (String.concat ~sep:" " ids))
    ; chips = []
    ; body =
        (match result with
         | None -> []
         | Some { is_error = true; _ } -> [ error_body ]
         | Some { text; _ } -> reports text)
    }
  | _ ->
    let arg = first_line (summary call) in
    { arg = Option.some_if (not (String.is_empty arg)) (span "arg" arg)
    ; chips = []
    ; body =
        (match result with
         | None -> [ output ~head:0 ~tail:6 live ]
         | Some r ->
           [ output ~error:r.is_error ~head:8 r.text
           ; Image_view.thumbs r.images
           ])
    }
;;

let view
      ~nested
      ~streaming
      ~running
      (call : Tool_call.t)
      (tool : Chat.Tool.t option)
  =
  let result = Option.bind tool ~f:(fun t -> t.result) in
  let cancelled = Option.exists result ~f:cancelled in
  (* What a cancelled call managed to do is not an error. *)
  let result =
    Option.map result ~f:(fun r ->
      if cancelled
      then
        { r with
          is_error = false
        ; text =
            (if String.equal (String.strip r.text) "[cancelled]"
             then ""
             else r.text)
        }
      else r)
  in
  let live = Option.value_map tool ~default:"" ~f:(fun t -> t.output) in
  let subagent = Option.bind tool ~f:(fun t -> t.subagent) in
  let status : Status.t =
    match result, subagent with
    | _ when cancelled -> Interrupted
    | Some { is_error = true; _ }, _
    | _, Some { result = Some { is_error = true; _ }; _ } -> Failed
    | _, Some { result = None; _ } -> Running
    | _, Some { result = Some _; _ } -> Done
    | Some _, _ -> Done
    | None, _ ->
      if streaming then Preparing else if running then Running else Interrupted
  in
  let { arg; chips; body } =
    present ~nested ~streaming ~live call ~result ~subagent
  in
  let chips =
    match status with
    | Interrupted when Option.is_none result -> chips @ [ chip "no result" ]
    | Interrupted when not (String.equal call.name "bash") ->
      chips @ [ chip "cancelled" ]
    | Preparing | Running | Done | Failed | Interrupted -> chips
  in
  let body = Chat_html.present body in
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
