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

let patch_files ?result args =
  Prigh_ui.Patch.parse
    ~input:(Option.value (Tool_args.string args "input") ~default:"")
    ~result
;;

let summary (call : Tool_call.t) =
  let args = Tool_args.of_call call in
  if String.equal call.name "apply_patch"
  then
    List.hd (patch_files args)
    |> Option.value_map ~default:"" ~f:Prigh_ui.Patch.File.name
  else
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
    | Preparing | Running -> Html.lamp ()
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

(* A few lines of each output: the first, or the last of an error. *)
let output ~fold ?(error = false) ?(lines = 6) ?elided text =
  if String.is_empty (String.strip text)
  then Node.none
  else Output_view.view ~fold ~error ~lines ?elided text
;;

let live_tail = 6

let live ~lines text =
  if String.is_empty (String.strip text)
  then Node.none
  else Output_view.live ~lines ~tail:live_tail text
;;

let error_output ~fold (r : Message.Tool_result.t) =
  output ~fold ~error:true ?elided:r.elided r.text
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
        | Server_tool s -> Some (Tool_summary.server_tool_line s)
        | Text t when not (String.is_empty (String.strip t)) ->
          List.last (String.split_lines (String.strip t))
        | Thinking t when not (String.is_empty (String.strip t)) ->
          Some "thinking…"
        | Text _ | Thinking _ -> None)
    | User _ | Notice _ | Compaction _ | Shell _ -> None)
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

let subagent_body ~nested ~fold (s : Chat.Subagent.t) =
  let transcript =
    match Chat.entries s.chat with
    | [] -> Node.none
    | entries ->
      folded
        ~cls:"transcript"
        ~label:"Transcript"
        ~preview:(plural (List.length entries) "message")
        (nested s)
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
      output ~fold ~error:true (Subagent_report.of_string text).text
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
let reports ~fold text =
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
  | Some sections ->
    (* Toggled with the card. *)
    [ Delivery_view.view ~fold:(fst fold) ~key:(fun _ -> snd fold) sections
    ; output ~fold note
    ]
  | None ->
    [ output
        ~fold
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

let link ~cls url text =
  Node.a
    ~attrs:
      [ Attr.class_ cls
      ; Attr.href url
      ; Attr.target "_blank"
      ; Attr.create "rel" "noopener noreferrer"
      ]
    [ Node.text text ]
;;

let safe_url url =
  String.is_prefix url ~prefix:"https://"
  || String.is_prefix url ~prefix:"http://"
;;

(* A web search's results, folded (verbose opens them), each a link. *)
let search_results (results : Tool_summary.Search_result.t list) =
  folded
    ~cls:"results"
    ~label:(plural (List.length results) "result")
    ~preview:
      (String.concat
         ~sep:"   "
         (List.map results ~f:(fun (r : Tool_summary.Search_result.t) ->
            r.title)))
    (Node.ol
       ~attrs:[ Attr.class_ "results" ]
       (List.map results ~f:(fun (r : Tool_summary.Search_result.t) ->
          Node.li
            (match r.url with
             | Some url when safe_url url ->
               [ link ~cls:"title" url r.title; span "url" url ]
             | Some url -> [ span "title" r.title; span "url" url ]
             | None -> [ span "title" r.title ]))))
;;

let parsed_arguments (call : Tool_call.t) =
  Result.ok (Json.parse call.arguments)
;;

let plan_of_arguments json = Option.join (Result.ok (Plan.of_arguments json))

let questions_of_call call =
  Option.bind (parsed_arguments call) ~f:(fun json ->
    Result.ok (Json.list_field json "questions" ~f:Question.of_json))
  |> Option.value ~default:[]
;;

let plan_list (plan : Plan.t) =
  Node.div
    ~attrs:[ Attr.class_ "plan" ]
    [ Option.value_map plan.explanation ~default:Node.none ~f:(fun e ->
        Node.p ~attrs:[ Attr.class_ "plan-explanation" ] [ Node.text e ])
    ; Node.ul
        ~attrs:[ Attr.class_ "plan-steps" ]
        (List.map plan.steps ~f:(fun step ->
           let status =
             match step.status with
             | Completed -> "step-done"
             | In_progress -> "step-now"
             | Pending -> "step-todo"
           in
           Node.li
             ~attrs:[ Attr.class_ ("plan-step " ^ status) ]
             [ span "mark" (Plan_view.mark step.status)
             ; Node.text (" " ^ step.step)
             ]))
    ]
;;

type presentation =
  { arg : Node.t option
  ; chips : Node.t list
  ; body : Node.t list
  }

let present
      ~nested
      ~fold
      ~streaming
      ~(tool : Chat.Tool.t option)
      (call : Tool_call.t)
      ~result
      ~subagent
  =
  let fold = fold, call.id in
  let running_output () =
    match tool with
    | Some t -> live ~lines:t.lines t.output
    | None -> Node.none
  in
  let args = Tool_args.of_call call in
  let str key = Option.value (Tool_args.string args key) ~default:"" in
  let error_body =
    match result with
    | Some ({ is_error = true; _ } as r : Message.Tool_result.t) ->
      error_output ~fold r
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
      | None -> None, ""
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
             | None -> running_output ()
             | Some r -> output ~fold ~error:r.is_error ?elided:r.elided text))
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
      | Some { text; elided; _ } -> [ output ~fold ~lines:0 ?elided text ]
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
           then live ~lines:(Chat.newlines content) content
           else output ~fold content)
        ]
    }
  | "edit" ->
    let diff =
      match result with
      | Some { is_error = false; text; _ } when Diff_view.is_unified text ->
        `Unified text
      | _ -> `Edits (Tool_args.edits args)
    in
    let lang = Highlight.Lang.of_path (str "path") in
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
           | `Unified text -> Diff_view.unified ?lang ~fold text
           | `Edits [] -> Node.none
           | `Edits edits -> Diff_view.edits ?lang ~fold edits)
        ]
    }
  | "apply_patch" ->
    let module Patch = Prigh_ui.Patch in
    let result_text =
      match result with
      | Some { is_error = false; text; _ } when Patch.is_success text ->
        Some text
      | _ -> None
    in
    let files = patch_files args ?result:result_text in
    let single = List.length files = 1 in
    let added, removed = Patch.counts files in
    let diff (file : Patch.File.t) =
      let lang = Highlight.Lang.of_path file.path in
      match file.kind, file.diff with
      | Delete, _ | _, Some [] -> Node.none
      | Update, Some lines ->
        Diff_view.unified ?lang ~fold (String.concat ~sep:"\n" lines)
      | Add, _ when Option.is_some result_text ->
        let lines = Patch.File.lines file in
        Diff_view.unified
          ?lang
          ~fold
          (String.concat
             ~sep:"\n"
             (sprintf "@@ -0,0 +1,%d @@" (List.length lines) :: lines))
      | (Add | Update), _ ->
        (match file.input with
         | [] -> Node.none
         | lines -> Diff_view.hunks ?lang ~fold lines)
    in
    let label (file : Patch.File.t) =
      let kind =
        match file.kind with
        | Add -> "add"
        | Update -> "update"
        | Delete -> "del"
      in
      div
        ("patch-file " ^ kind)
        [ span "kind" (Patch.Kind.letter file.kind)
        ; span "path" (Patch.File.name file)
        ]
    in
    { arg =
        List.hd files
        |> Option.map ~f:(fun file -> span "arg path" (Patch.File.name file))
    ; chips =
        ((match files with
          | [ { kind = Add; _ } ] -> [ chip "new file" ]
          | [ { kind = Delete; _ } ] -> [ chip "deleted" ]
          | [] | [ { kind = Update; _ } ] -> []
          | files -> [ chip (plural (List.length files) "file") ])
         @
         if added + removed = 0
         then []
         else
           [ chip ~cls:"add" (sprintf "+%d" added)
           ; chip ~cls:"del" (sprintf "−%d" removed)
           ])
    ; body =
        (match result with
         | Some { is_error = false; text; _ }
           when List.is_empty files || not (Patch.is_success text) ->
           [ output ~fold text ]
         | _ ->
           error_body
           :: List.concat_map files ~f:(fun file ->
             if single then [ diff file ] else [ label file; diff file ]))
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
         | Some r -> [ output ~fold ~error:r.is_error ?elided:r.elided r.text ])
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
         | Some s, _ -> subagent_body ~nested ~fold s
         | None, Some { is_error = true; _ } -> [ error_body ]
         | None, Some { text; _ } when Option.is_none started ->
           [ report (Subagent_report.of_string text).text ]
         | None, _ -> [])
    }
  | "web_search" ->
    let results =
      match result with
      | Some { is_error = false; text; _ } ->
        Tool_summary.Search_result.parse text
      | _ -> []
    in
    { arg =
        Option.map (Tool_args.string args "query") ~f:(fun query ->
          span "arg query" (Tool_summary.quoted ~width:Int.max_value query))
    ; chips =
        (match results with
         | [] -> []
         | results -> [ chip (plural (List.length results) "result") ])
    ; body =
        (match result, results with
         | None, _ -> []
         | Some { is_error = true; _ }, _ -> [ error_body ]
         | Some { text; _ }, [] -> [ output ~fold text ]
         | Some _, results -> [ search_results results ])
    }
  | "web_fetch" ->
    let url = str "url" in
    let fetched =
      match result with
      | Some { is_error = false; text; _ } -> Tool_summary.Fetched.parse text
      | _ -> None
    in
    { arg =
        Some
          (if safe_url url
           then link ~cls:"arg url" url url
           else span "arg url" url)
    ; chips =
        (match fetched with
         | None -> []
         | Some fetched ->
           (if String.equal fetched.url url
            then []
            else [ chip ("→ " ^ fetched.url) ])
           @ [ chip fetched.details ])
    ; body =
        (match result, fetched with
         | None, _ -> [ running_output () ]
         | Some { elided = Some elided; text; _ }, _ ->
           [ output ~fold ~elided text ]
         | Some _, Some fetched -> [ output ~fold fetched.body ]
         | Some r, None ->
           [ output ~fold ~error:r.is_error ?elided:r.elided r.text ])
    }
  | "job_input" ->
    { arg =
        Option.map (Tool_summary.job_input args) ~f:(fun arg ->
          span "arg input" arg)
    ; chips =
        (if Tool_summary.closes_stdin args then [ chip "close stdin" ] else [])
    ; body =
        (match result with
         | None -> []
         | Some r -> [ output ~fold ~error:r.is_error ?elided:r.elided r.text ])
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
         | Some { text; _ } -> reports ~fold text)
    }
  | "update_plan" ->
    (match parsed_arguments call |> Option.bind ~f:plan_of_arguments with
     | None ->
       { arg = None
       ; chips = []
       ; body =
           [ (match result with
              | Some r -> output ~fold ~error:r.is_error ?elided:r.elided r.text
              | None -> Node.none)
           ]
       }
     | Some plan ->
       { arg = None
       ; chips =
           [ chip
               (sprintf
                  "%d/%d done"
                  (Plan.completed plan)
                  (List.length plan.steps))
           ]
       ; body = [ error_body; plan_list plan ]
       })
  | "ask_user" ->
    (match result with
     | Some { is_error = false; text; _ } ->
       { arg = None
       ; chips = []
       ; body =
           [ output
               ~fold
               ~lines:12
               (Option.value
                  (String.chop_prefix text ~prefix:"The user answered:\n")
                  ~default:text)
           ]
       }
     | Some { is_error = true; _ } ->
       { arg = None; chips = [ chip "no answer" ]; body = [ error_body ] }
     | None ->
       { arg = None
       ; chips = [ chip ~cls:"waiting" "waiting for your answer" ]
       ; body =
           [ Node.ul
               ~attrs:[ Attr.class_ "questions" ]
               (List.map (questions_of_call call) ~f:(fun (q : Question.t) ->
                  Node.li [ Node.text q.question ]))
           ]
       })
  | "exit_plan_mode" ->
    let plan = str "plan" in
    { arg = None
    ; chips =
        (match result with
         | Some { is_error = false; _ } -> [ chip ~cls:"ok" "approved" ]
         | Some { is_error = true; text; _ }
           when String.is_prefix text ~prefix:"[the user did not approve" ->
           [ chip ~cls:"bad" "not approved" ]
         | Some _ -> []
         | None -> [ chip ~cls:"waiting" "waiting for your approval" ])
    ; body =
        [ (if String.is_empty (String.strip plan)
           then Node.none
           else div "plan" [ Markdown_view.render ~streaming plan ])
        ; error_body
        ]
    }
  | _ ->
    let arg = first_line (summary call) in
    { arg = Option.some_if (not (String.is_empty arg)) (span "arg" arg)
    ; chips = []
    ; body =
        (match result with
         | None -> [ running_output () ]
         | Some r ->
           [ output ~fold ~error:r.is_error ?elided:r.elided r.text
           ; Image_view.thumbs r.images
           ])
    }
;;

let card
      ?arg:arg_override
      ~nested
      ~fold
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
    present ~nested ~fold ~streaming ~tool call ~result ~subagent
  in
  let arg = Option.first_some arg_override arg in
  let chips =
    match status with
    | Interrupted when Option.is_none result -> chips @ [ chip "no result" ]
    | Interrupted when not (String.equal call.name "bash") ->
      chips @ [ chip "cancelled" ]
    | Preparing | Running | Done | Failed | Interrupted -> chips
  in
  let body = Chat_html.present body in
  (* A subagent's card opens it in the agents panel (the page listens for
     clicks on [data-agent]); [data-call] finds the card from the panel. *)
  let agent =
    match subagent, result with
    | Some s, _ -> Some s.agent_id
    | None, Some { is_error = false; text; _ } -> Subagent_report.started text
    | None, _ -> None
  in
  let head =
    [ Status.icon status; span "name" call.name ] @ Option.to_list arg @ chips
  in
  let cls =
    String.concat ~sep:" " [ "tool"; "tool-" ^ call.name; Status.cls status ]
  in
  let body =
    match body with
    | [] -> Node.none
    | body -> div "tool-body" body
  in
  if String.equal call.name "subagent"
  then
    Node.div
      ~attrs:[ Attr.class_ cls; Attr.create "data-call" call.id ]
      [ (match agent with
         | Some id ->
           Node.div
             ~attrs:
               [ Attr.class_ "tool-head"
               ; Attr.create "data-agent" id
               ; Attr.title "Follow it in the agents panel"
               ]
             (head
              @ [ (* The page opens the agent from a click on [data-agent]:
                     this button is the head's way in from the keyboard. *)
                  Node.button
                    ~attrs:
                      [ Attr.class_ "open-agent"
                      ; Attr.type_ "button"
                      ; Attr.create "aria-label" "Open in the agents panel"
                      ]
                    [ Node.text "Open ›" ]
                ])
         | None -> div "tool-head" head)
      ; body
      ]
  else div cls [ div "tool-head" head; body ]
;;

let view ~nested ~fold ~streaming ~running call tool =
  card ~nested ~fold ~streaming ~running call tool
;;

let server_tool ~fold ~running (s : Content.Server_tool.t) =
  let call, result = Tool_summary.as_call s in
  let arg =
    match s.name with
    | "web_search" -> None
    | _ ->
      Option.map (Tool_summary.server_tool_arg s) ~f:(fun input ->
        span "arg" input)
  in
  card
    ?arg
    ~nested:(fun _ -> Node.none)
    ~fold
    ~streaming:false
    ~running
    call
    (Some { call; output = ""; lines = 0; result; subagent = None })
;;
