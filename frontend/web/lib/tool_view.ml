open! Core
open! Import
open Chat_html

let chip ?(cls = "") text = span (String.strip ("chip " ^ cls)) text
let open_key id = id ^ "#open"

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
  else if String.equal call.name "worktree"
  then Option.value (Tool_summary.worktree args) ~default:""
  else if String.equal call.name "pull_request"
  then Option.value (Tool_summary.pull_request args) ~default:""
  else if String.equal call.name "issue"
  then Option.value (Tool_summary.issue args) ~default:""
  else if String.equal call.name "repo"
  then Option.value (Tool_summary.repo args) ~default:""
  else if String.equal call.name "sessions"
  then Option.value (Tool_summary.sessions args) ~default:""
  else if String.equal call.name "ask_session"
  then Option.value (Tool_summary.ask_session args) ~default:""
  else if String.equal call.name "answer_session"
  then Option.value (Tool_summary.answer_session args) ~default:""
  else if String.equal call.name "memory"
  then Option.value (Tool_summary.memory args) ~default:""
  else if String.equal call.name "computer"
  then
    Option.value_map
      (Computer_action.of_call call)
      ~default:""
      ~f:Computer_action.describe
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
    | Done -> Html.mark ~cls:"done" ~label:"Done" Check
    | Failed -> Html.mark ~cls:"failed" ~label:"Failed" Cross
    | Interrupted -> Html.mark ~cls:"stopped" ~label:"Interrupted" Square
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

let live ~lines ~missed text =
  if String.is_empty (String.strip text) && not missed
  then Node.none
  else Output_view.live ~lines ~missed ~tail:live_tail text
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
    | User _ | Compaction _ | Shell _ -> None)
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

(* A subagent's short name, else its task's first line. *)
let subagent_title (call : Tool_call.t) =
  let args = Tool_args.of_call call in
  match Tool_args.string args "name" with
  | Some name when not (String.is_empty (String.strip name)) ->
    String.strip name
  | _ -> first_line (Option.value (Tool_args.string args "task") ~default:"")
;;

let agent_name chat id =
  List.find_map (Chat.entries chat) ~f:(fun (entry : Chat.Entry.t) ->
    match entry with
    | Assistant { message; _ } ->
      List.find_map message.content ~f:(fun (content : Content.t) ->
        match content with
        | Tool_call call when String.equal call.name "subagent" ->
          let started =
            Option.bind (Chat.tool chat call.id) ~f:(fun (t : Chat.Tool.t) ->
              match t.subagent, t.result with
              | Some s, _ -> Some s.agent_id
              | None, Some { is_error = false; text; _ } ->
                Subagent_report.started text
              | None, _ -> None)
          in
          Option.some_if
            (Option.exists started ~f:(String.equal id))
            (subagent_title call)
        | Tool_call _ | Text _ | Thinking _ | Server_tool _ -> None)
    | User _ | Compaction _ | Shell _ -> None)
;;

(* The report of a job or subagent stopped by [job_kill] or
   [subagent_cancel]. *)
let reports ~fold text =
  match Prigh_ui.Delivery.parse text with
  | Some sections ->
    (* Toggled with the card. *)
    [ Delivery_view.view ~fold:(fst fold) ~key:(fun _ -> snd fold) sections ]
  | None -> [ output ~fold text ]
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
           let status, mark =
             match step.status with
             | Completed ->
               "step-done", Html.mark ~cls:"done" ~label:"Done" Check
             | In_progress ->
               "step-now", Html.mark ~cls:"now" ~label:"In progress" Next
             | Pending -> "step-todo", Html.mark ~cls:"todo" ~label:"To do" Ring
           in
           Node.li
             ~attrs:[ Attr.class_ ("plan-step " ^ status) ]
             [ mark; Node.text step.step ]))
    ]
;;

(* A call at rest is one line: its argument and the chips saying how it
   went; [details] (what it was asked, beyond its argument) are in the line's
   tooltip and atop its opened body. *)
type presentation =
  { arg : Node.t option
  ; chips : Node.t list
  ; details : string list
  ; body : Node.t list
  }

let present
      ~nested:_
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
    | Some t -> live ~lines:t.lines ~missed:t.missed t.output
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
    let moved =
      Option.bind result ~f:(fun (r : Message.Tool_result.t) ->
        Moved_job.parse r.text)
    in
    let job =
      match moved with
      | Some m -> Some m.id
      | None -> Option.bind result ~f:(fun r -> job_started r.text)
    in
    { arg = Some (span "arg command" (first_line command))
    ; details =
        List.filter_opt
          [ Option.some_if (background && Option.is_none job) "background"
          ; Option.map moved ~f:(fun m ->
              sprintf "moved to the background after %s" m.after)
          ]
    ; chips =
        List.filter_opt
          [ Option.map job ~f:(fun id -> chip ~cls:"job" ("job " ^ id))
          ; Option.map outcome ~f:(fun outcome ->
              chip
                ~cls:(if String.equal outcome "cancelled" then "" else "bad")
                outcome)
          ]
    ; body =
        [ (if String.mem (String.strip command) '\n'
           then Node.pre ~attrs:[ Attr.class_ "command" ] [ Node.text command ]
           else Node.none)
        ; (match moved with
           | Some m -> output ~fold m.output
           | None -> Node.none)
        ; (if Option.is_some job
           then Node.none
           else (
             match result with
             | None -> running_output ()
             | Some r -> output ~fold ~error:r.is_error ?elided:r.elided text))
        ]
    }
  | "computer" ->
    let action = Computer_action.of_call call in
    let body =
      match result with
      | None -> []
      | Some { is_error = true; _ } -> [ error_body ]
      | Some { images = []; text; _ } -> [ span "caption" (String.strip text) ]
      | Some { images = shot :: _; text; _ } ->
        let size =
          match Computer_action.screenshot_size text with
          | Some size -> Some size
          | None -> Image_size.dimensions shot
        in
        let mark =
          match Option.bind action ~f:Computer_action.point, size with
          | Some { x; y }, Some (w, h) when x >= 0 && y >= 0 && x <= w && y <= h
            ->
            let percent n d = Float.of_int n *. 100. /. Float.of_int d in
            [ Node.span
                ~attrs:
                  [ Attr.class_ "screen-mark"
                  ; Attr.create "aria-hidden" "true"
                  ; Attr.style
                      (Css_gen.create
                         ~field:"left"
                         ~value:(sprintf "%.3f%%" (percent x w))
                       |> Css_gen.combine
                            (Css_gen.create
                               ~field:"top"
                               ~value:(sprintf "%.3f%%" (percent y h))))
                  ]
                []
            ]
          | _ -> []
        in
        let label = Image.to_string_hum shot in
        let lines = String.split_lines (String.rstrip text) in
        (* The last line ("screenshot W×H") is the image itself. *)
        let caption =
          match List.rev lines with
          | last :: rest when String.is_prefix last ~prefix:"screenshot " ->
            List.rev rest
          | _ -> lines
        in
        [ Node.div
            ~attrs:
              [ Attr.class_ "screen"
              ; Attr.create Image_view.group_attribute ""
              ]
            [ Node.button
                ~attrs:
                  [ Attr.type_ "button"
                  ; Attr.class_ "image screen-shot"
                  ; Attr.title label
                  ; Attr.create "aria-label" ("View the screenshot, " ^ label)
                  ; Attr.create Image_view.viewer_attribute ""
                  ]
                ([ Node.img
                     ~attrs:
                       [ Attr.class_ "thumb"
                       ; Attr.src (Image_view.src shot)
                       ; Attr.alt "the screen after the action"
                       ]
                     ()
                 ]
                 @ mark)
            ]
        ; span "caption" (String.concat ~sep:"\n" caption)
        ]
    in
    { arg =
        Option.map action ~f:(fun a -> span "arg" (Computer_action.describe a))
    ; chips =
        (match result with
         | Some { is_error = false; text; _ } ->
           Option.value_map
             (Computer_action.screenshot_size text)
             ~default:[]
             ~f:(fun (w, h) -> [ chip (sprintf "%d×%d" w h) ])
         | _ -> [])
    ; details = []
    ; body
    }
  | "read" ->
    let range =
      match Tool_args.int args "offset", Tool_args.int args "limit" with
      | None, None -> []
      | Some o, None -> [ sprintf "from line %d" o ]
      | None, Some l -> [ plural l "line" ]
      | Some o, Some l -> [ sprintf "lines %d–%d" o (o + l - 1) ]
    in
    let body =
      match result with
      | None -> []
      | Some { is_error = true; _ } -> [ error_body ]
      | Some { images = _ :: _ as images; text; _ } ->
        [ Image_view.thumbs images; span "caption" text ]
      | Some { text; elided; _ } -> [ output ~fold ~lines:0 ?elided text ]
    in
    { arg = Some (span "arg path" (str "path"))
    ; chips = []
    ; details = range
    ; body
    }
  | "write" ->
    let content = str "content" in
    let overwrote =
      match result with
      | Some { is_error = false; text; _ } ->
        String.is_prefix text ~prefix:"overwrote "
      | _ -> false
    in
    { arg = Some (span "arg path" (str "path"))
    ; chips = [ chip (plural (Output_view.line_count content) "line") ]
    ; details = (if overwrote then [ "overwrote the file" ] else [])
    ; body =
        [ error_body
        ; (if String.is_empty content
           then Node.none
           else if streaming
           then live ~lines:(Chat.newlines content) ~missed:false content
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
    ; details = []
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
    ; details = []
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
    ; chips = []
    ; details =
        List.filter_opt
          [ (if ls
             then None
             else
               Option.map (Tool_args.string args "path") ~f:(fun p -> "in " ^ p))
          ; Option.map (Tool_args.string args "glob") ~f:(fun g -> "files " ^ g)
          ; Option.bind (Tool_args.bool args "ignore_case") ~f:(fun b ->
              Option.some_if b "ignoring case")
          ]
    ; body =
        (match result with
         | None -> []
         | Some r -> [ output ~fold ~error:r.is_error ?elided:r.elided r.text ])
    }
  | "subagent" ->
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
    (* Its name, how it stands and the way into the agents panel: the task,
       its log and its report are there. *)
    let name = subagent_title call in
    let agent_id =
      Option.first_some
        (Option.map subagent ~f:(fun (s : Chat.Subagent.t) -> s.agent_id))
        started
    in
    { arg = Some (span "arg task" name)
    ; chips = []
    ; details =
        List.filter_opt
          [ Option.map agent_id ~f:(fun id -> "agent " ^ id)
          ; Option.some_if
              (Option.exists subagent ~f:(fun s -> s.forked)
               || Option.value (Tool_args.bool args "fork") ~default:false)
              "forked"
          ; model
          ; Option.bind subagent ~f:(fun s ->
              Option.some_if (s.turns > 0) (plural s.turns "turn"))
          ; Option.bind subagent ~f:(fun s ->
              Option.map s.cost_usd ~f:(fun c -> sprintf "$%.2f" c))
          ]
    ; body =
        (match subagent, result with
         | Some { result = Some { text; is_error = true }; _ }, _ ->
           [ output ~fold ~error:true (Subagent_report.of_string text).text ]
         | None, Some { is_error = true; _ } -> [ error_body ]
         | None, Some { text; _ } when Option.is_none started ->
           (* Ran in the foreground: its report is its result. *)
           [ report (Subagent_report.of_string text).text ]
         | _ -> [])
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
    ; details = []
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
    (* The line is the card's toggle: the link is in the opened body. *)
    { arg = Some (span "arg url" url)
    ; chips = []
    ; details =
        (match fetched with
         | None -> []
         | Some fetched ->
           (if String.equal fetched.url url then [] else [ "→ " ^ fetched.url ])
           @ [ fetched.details ])
    ; body =
        (match result, fetched with
         | None, _ -> [ running_output () ]
         | Some { elided = Some elided; text; _ }, _ ->
           [ output ~fold ~elided text ]
         | Some _, Some fetched ->
           [ (if safe_url url then link ~cls:"url-link" url url else Node.none)
           ; output ~fold fetched.body
           ]
         | Some r, None ->
           [ output ~fold ~error:r.is_error ?elided:r.elided r.text ])
    }
  | "job_input" ->
    { arg =
        Option.map (Tool_summary.job_input args) ~f:(fun arg ->
          span "arg input" arg)
    ; chips = []
    ; details =
        (if Tool_summary.closes_stdin args then [ "closes its stdin" ] else [])
    ; body =
        (match result with
         | None -> []
         | Some r -> [ output ~fold ~error:r.is_error ?elided:r.elided r.text ])
    }
  | "job_kill" | "subagent_cancel" ->
    let ids = Tool_args.strings args "ids" @ Tool_args.strings args "id" in
    { arg =
        Option.some_if
          (not (List.is_empty ids))
          (span "arg" (String.concat ~sep:" " ids))
    ; chips = []
    ; details = []
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
       ; details = []
       ; body =
           [ (match result with
              | Some r -> output ~fold ~error:r.is_error ?elided:r.elided r.text
              | None -> Node.none)
           ]
       }
     | Some plan ->
       (* The step it is on, else the next to do: the list opens. *)
       let now =
         List.find plan.steps ~f:(fun s ->
           match s.status with
           | In_progress -> true
           | Completed | Pending -> false)
         |> Option.first_some
              (List.find plan.steps ~f:(fun s ->
                 match s.status with
                 | Pending -> true
                 | Completed | In_progress -> false))
       in
       { arg = Option.map now ~f:(fun s -> span "arg step" s.step)
       ; details = []
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
       ; chips = [ chip ~cls:"ok" "answered" ]
       ; details = []
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
       { arg = None
       ; chips = [ chip "no answer" ]
       ; details = []
       ; body = [ error_body ]
       }
     | None ->
       { arg = None
       ; details = []
       ; chips = [ chip ~cls:"waiting" "waiting for your answer" ]
       ; body =
           [ Node.ul
               ~attrs:[ Attr.class_ "questions" ]
               (List.map (questions_of_call call) ~f:(fun (q : Question.t) ->
                  Node.li [ Node.text q.question ]))
           ]
       })
  | "reply_to_comment" ->
    let edits = Tool_args.edits args in
    let resolve = Option.value (Tool_args.bool args "resolve") ~default:false in
    { arg =
        Option.some_if
          (not (String.is_empty (str "id")))
          (span "arg" (str "id"))
    ; chips =
        ((if List.is_empty edits then [] else [ chip "proposes a change" ])
         @ if resolve then [ chip ~cls:"ok" "resolved" ] else [])
    ; details = []
    ; body =
        [ div "comment-reply" [ Markdown_view.render ~streaming (str "text") ]
        ; (if List.is_empty edits
           then Node.none
           else Diff_view.edits ~fold edits)
        ; error_body
        ]
    }
  | _ ->
    let arg = first_line (summary call) in
    { arg = Option.some_if (not (String.is_empty arg)) (span "arg" arg)
    ; chips = []
    ; details = []
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
  let { arg; chips; details; body } =
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
  let tooltip =
    match details with
    | [] -> None
    | details -> Some (String.concat ~sep:" · " details)
  in
  let cls =
    String.concat ~sep:" " [ "tool"; "tool-" ^ call.name; Status.cls status ]
  in
  if String.equal call.name "subagent"
  then (
    (* "Subagent: its name": what the line is, then which one. *)
    let head =
      (Status.icon status :: span "kind" "Subagent:" :: Option.to_list arg)
      @ chips
    in
    Node.div
      ~attrs:[ Attr.class_ cls; Attr.create "data-call" call.id ]
      [ (match agent with
         | Some id ->
           Node.div
             ~attrs:
               ([ Attr.class_ "tool-head"; Attr.create "data-agent" id ]
                @ Option.to_list
                    (Option.map tooltip ~f:(fun t ->
                       Attr.title (t ^ " · follow it in the agents panel"))))
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
      ; (match body with
         | [] -> Node.none
         | body -> div "tool-body" body)
      ])
  else (
    (* One line at rest; the line opens the rest. A failure, a call still
       showing its live output, an image and a question waiting for an answer
       start open. *)
    let by_default =
      match status with
      | Failed -> true
      | Preparing | Running -> not (List.is_empty body)
      | Done | Interrupted ->
        Option.exists result ~f:(fun r -> not (List.is_empty r.images))
        || String.equal call.name "reply_to_comment"
    in
    let key = open_key call.id in
    let opened = Fold.opened ~by_default fold key in
    let line =
      [ Status.icon status; span "name" call.name ] @ Option.to_list arg
    in
    match body with
    | [] -> div cls [ div "tool-head" (line @ chips) ]
    | body ->
      Node.div
        ~attrs:
          [ Attr.classes
              (String.split cls ~on:' ' @ if opened then [ "open" ] else [])
          ]
        (Node.button
           ~attrs:
             ([ Attr.class_ "tool-head"
              ; Attr.type_ "button"
              ; Fold.toggle_attr fold key
              ; Attr.create "aria-expanded" (Bool.to_string opened)
              ]
              @ Option.to_list (Option.map tooltip ~f:Attr.title))
           (line @ chips @ [ span "chev" "" ])
         ::
         (if opened
          then
            [ div
                "tool-body"
                ((match tooltip with
                  | Some t -> span "details" t
                  | None -> Node.none)
                 :: body)
            ]
          else [])))
;;

(* A [render_ui] call shows its components once the backend accepted them;
   a skeleton until then, and a failed call's card if it refused them. *)
let render_ui ~ui ~nested ~fold ~streaming ~running (call : Tool_call.t) tool =
  let refused (r : Message.Tool_result.t) =
    String.is_prefix (String.lstrip r.text) ~prefix:"Nothing was shown."
  in
  let tool =
    Option.map tool ~f:(fun (t : Chat.Tool.t) ->
      match t.result with
      | Some r when refused r ->
        { t with result = Some { r with is_error = true } }
      | _ -> t)
  in
  let result = Option.bind tool ~f:(fun (t : Chat.Tool.t) -> t.result) in
  let shown =
    match result with
    | Some r when not r.is_error -> Ui_view.spec call
    | _ -> None
  in
  match shown, result with
  | Some spec, _ ->
    Node.div
      ~attrs:[ Attr.class_ "tool-ui"; Attr.create "data-call" call.id ]
      [ Ui_view.view (Ui_context.create ui ~call_id:call.id ~fold) spec ]
  | None, None when streaming || running ->
    Node.div ~attrs:[ Attr.class_ "tool-ui" ] [ Ui_view.skeleton () ]
  | None, _ -> card ~nested ~fold ~streaming ~running call tool
;;

let view
      ?(ui = Ui_context.Page.none)
      ~nested
      ~fold
      ~streaming
      ~running
      call
      tool
  =
  if String.equal call.Tool_call.name "render_ui"
  then render_ui ~ui ~nested ~fold ~streaming ~running call tool
  else card ~nested ~fold ~streaming ~running call tool
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
    (Some
       { call; output = ""; lines = 0; missed = false; result; subagent = None })
;;
