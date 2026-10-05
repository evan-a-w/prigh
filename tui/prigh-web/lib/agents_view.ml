open! Core
open! Import
open Html
module Action = App.Action
module Agent = Agents.Agent
module Item = Agents.Item

let plural = Chat_html.plural
let now (m : App.Model.t) = Option.value m.now ~default:Time_ns.epoch
let first_line = Chat_html.first_line

let job_status (j : Agents.Job.t) : Agents.Status.t =
  match j.info.running, j.info.exit with
  | true, _ -> Running
  | false, (None | Some "exited 0") -> Complete
  | false, Some _ -> Failed
;;

let status_cls (status : Agents.Status.t) = Agents.Status.to_string status

let status_icon (status : Agents.Status.t) =
  match status with
  | Running -> Node.span ~attrs:[ Attr.class_ "spinner" ] []
  | Complete -> span ~cls:"state-icon" "✓"
  | Failed -> span ~cls:"state-icon" "✕"
;;

let pill (status : Agents.Status.t) label =
  span ~cls:("pill " ^ status_cls status) label
;;

let selected (m : App.Model.t) item =
  Option.equal Item.equal m.agents.selected (Some item)
;;

let report_text (r : Event.Subagent_result.t) =
  (Subagent_report.of_string r.text).text
;;

(* What it is doing, or how it ended. *)
let activity (m : App.Model.t) (a : Agent.t) =
  match a.result with
  | Some r -> Markdown.preview (report_text r)
  | None ->
    let latest =
      Option.bind (Chat.find_subagent m.chat a.id) ~f:(fun s ->
        Tool_view.last_activity s.chat)
    in
    (match a.current_tool, latest with
     | Some (name, since), latest ->
       sprintf
         "%s · %s"
         (Option.value latest ~default:name)
         (Agents.format_span (Time_ns.diff (now m) since))
     | None, Some latest -> latest
     | None, None -> if a.turns = 0 then "starting…" else "thinking…")
;;

let number_badge n =
  if n >= 1 && n <= 9
  then
    Node.kbd
      ~attrs:[ Attr.class_ "row-num"; Attr.title (sprintf "Alt+%d" n) ]
      [ Node.text (Int.to_string n) ]
  else Node.span ~attrs:[ Attr.class_ "row-num none" ] []
;;

let row
      ~inject
      ~item
      ~status
      ~selected
      ~depth
      ~number
      ~title
      ~elapsed
      ~meta
      ~line
  =
  Node.button
    ~attrs:
      [ classes
          [ "agents-row"; status_cls status ]
          [ "selected", selected; "nested", depth > 0 ]
      ; Attr.type_ "button"
      ; Attr.create "style" (sprintf "--depth: %d" depth)
      ; Attr.on_click (fun _ -> inject (Action.Select_item item))
      ]
    [ number_badge number
    ; status_icon status
    ; div
        ~cls:"row-main"
        [ div
            ~cls:"row-top"
            [ span ~cls:"row-title" title; span ~cls:"row-elapsed" elapsed ]
        ; span ~cls:"row-meta" (String.concat ~sep:" · " meta)
        ; (if String.is_empty line then Node.none else span ~cls:"row-line" line)
        ]
    ]
;;

let agent_meta (m : App.Model.t) (a : Agent.t) =
  let cost =
    Option.bind (Chat.find_subagent m.chat a.id) ~f:(fun s -> s.cost_usd)
  in
  List.filter_opt
    [ Some a.model
    ; Option.some_if (a.turns > 0) (plural a.turns "turn")
    ; Option.some_if (a.tool_calls > 0) (plural a.tool_calls "tool call")
    ; Option.map cost ~f:(sprintf "$%.2f")
    ]
;;

let bytes n =
  if n < 1024
  then sprintf "%d B" n
  else if n < 1024 * 1024
  then sprintf "%.1f KB" (Float.of_int n /. 1024.)
  else sprintf "%.1f MB" (Float.of_int n /. 1048576.)
;;

let job_meta ?(exit = true) (j : Agents.Job.t) =
  List.filter_opt
    [ Some j.info.id
    ; Option.some_if
        (exit && not j.info.running)
        (Option.value j.info.exit ~default:"finished")
    ; Option.some_if (j.info.bytes > 0) (bytes j.info.bytes)
    ]
;;

(* [job_output]'s text starts with a line about the job (shown above) and
   which lines follow. *)
let split_output text =
  match String.lsplit2 text ~on:'\n' with
  | Some (header, body) when String.is_prefix header ~prefix:"[job " ->
    let lines =
      String.lsplit2 header ~on:';'
      |> Option.bind ~f:(fun (_, rest) -> String.lsplit2 rest ~on:']')
      |> Option.map ~f:(fun (lines, _) -> String.strip lines)
    in
    lines, body
  | None when String.is_prefix text ~prefix:"[job " -> None, ""
  | _ -> None, text
;;

let item_row (m : App.Model.t) ~inject ~number (item, depth) =
  let a = m.agents in
  match (item : Item.t) with
  | Agent id ->
    (match Agents.find_agent a id with
     | None -> Node.none
     | Some agent ->
       row
         ~inject
         ~item
         ~status:agent.status
         ~selected:(selected m item)
         ~depth
         ~number
         ~title:(first_line agent.task)
         ~elapsed:(Agents.format_span (Agents.agent_elapsed agent ~now:(now m)))
         ~meta:(agent.id :: agent_meta m agent)
         ~line:(activity m agent))
  | Job id ->
    (match Agents.find_job a id with
     | None -> Node.none
     | Some job ->
       row
         ~inject
         ~item
         ~status:(job_status job)
         ~selected:(selected m item)
         ~depth
         ~number
         ~title:(first_line job.info.command)
         ~elapsed:(Agents.format_span (Agents.job_elapsed job ~now:(now m)))
         ~meta:(job_meta job)
         ~line:(Option.value job.info.last_line ~default:"")
       |> fun node -> Node.div ~attrs:[ Attr.class_ "job" ] [ node ])
;;

let is_agent ((item : Item.t), _) =
  match item with
  | Agent _ -> true
  | Job _ -> false
;;

let section ~title ~count rows =
  match rows with
  | [] -> Node.none
  | rows ->
    Node.section
      ~attrs:[ Attr.class_ "agents-section" ]
      (div
         ~cls:"agents-section-head"
         [ span ~cls:"agents-section-title" title
         ; span ~cls:"agents-section-count" count
         ]
       :: rows)
;;

let counts ~running ~total =
  if running > 0 then sprintf "%d running" running else Int.to_string total
;;

let list_view (m : App.Model.t) ~inject =
  let a = m.agents in
  let listed = Agents.listed a in
  let numbered = List.mapi listed ~f:(fun i entry -> i + 1, entry) in
  let rows ~f =
    List.filter_map numbered ~f:(fun (number, entry) ->
      Option.some_if (f entry) (item_row m ~inject ~number entry))
  in
  let agents = rows ~f:is_agent in
  let jobs = rows ~f:(Fn.non is_agent) in
  let earlier = Agents.earlier a in
  if List.is_empty listed && List.is_empty earlier
  then
    div
      ~cls:"agents-empty"
      [ Node.p
          [ Node.text "No subagents or background jobs in this session yet." ]
      ; Node.p
          ~attrs:[ Attr.class_ "hint" ]
          [ Node.text
              "They show here while the agent delegates work (its subagent \
               tool) or runs commands in the background; !&command starts a \
               job yourself."
          ]
      ]
  else
    Node.fragment
      [ section
          ~title:"Subagents"
          ~count:
            (counts
               ~running:
                 (List.count listed ~f:(fun (item, _) ->
                    is_agent (item, 0) && Agents.running a item))
               ~total:(List.length agents))
          agents
      ; section
          ~title:"Jobs"
          ~count:
            (counts
               ~running:
                 (List.count listed ~f:(fun (item, _) ->
                    (not (is_agent (item, 0))) && Agents.running a item))
               ~total:(List.length jobs))
          jobs
      ; (if List.is_empty listed
         then
           div
             ~cls:"agents-empty"
             [ Node.p [ Node.text "Nothing since your last prompt." ] ]
         else Node.none)
      ; (match earlier with
         | [] -> Node.none
         | earlier ->
           Node.details
             ~attrs:[ Attr.class_ "agents-earlier" ]
             (Node.summary [ Node.textf "Earlier (%d)" (List.length earlier) ]
              :: List.map earlier ~f:(item_row m ~inject ~number:0)))
      ]
;;

let stop_button (m : App.Model.t) ~inject item ~label ~action ~title =
  if Set.mem m.agents.stopping item
  then
    button
      ~cls:"stop small"
      ~disabled:true
      ~on_click:Effect.Ignore
      [ Node.text "Stopping…" ]
  else
    button
      ~cls:"stop small"
      ~title
      ~on_click:(inject action)
      [ icon Stop; Node.text label ]
;;

let crumbs (m : App.Model.t) ~inject (a : Agent.t) =
  match Agents.lineage m.agents a.id with
  | [] | [ _ ] -> Node.none
  | lineage ->
    Node.nav
      ~attrs:[ Attr.class_ "agents-crumbs" ]
      (List.concat_map lineage ~f:(fun (b : Agent.t) ->
         if String.equal b.id a.id
         then [ span ~cls:"crumb current" (first_line b.task) ]
         else
           [ Node.button
               ~attrs:
                 [ Attr.class_ "crumb"
                 ; Attr.type_ "button"
                 ; Attr.title b.id
                 ; Attr.on_click (fun _ ->
                     inject (Action.Select_item (Agent b.id)))
                 ]
               [ Node.text (first_line b.task) ]
           ; span ~cls:"crumb-sep" "›"
           ]))
;;

let agent_detail (m : App.Model.t) ~inject (a : Agent.t) =
  let sub = Chat.find_subagent m.chat a.id in
  let children =
    Agents.children m.agents (Some a.id)
    |> List.map ~f:(fun (c : Agent.t) ->
      item_row m ~inject ~number:0 (Item.Agent c.id, 0))
  in
  let transcript =
    match sub with
    | Some s when not (List.is_empty (Chat.entries s.chat)) ->
      Chat_view.view s.chat
    | Some _ -> div ~cls:"agents-note" [ Node.text "Starting…" ]
    | None ->
      div
        ~cls:"agents-note"
        [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
        ; Node.text "Loading the transcript…"
        ]
  in
  (* The transcript ends with the report. *)
  let result =
    match a.result with
    | None -> Node.none
    | Some { is_error = false; _ } when Option.is_some sub -> Node.none
    | Some ({ is_error = false; _ } as r) ->
      div
        ~cls:"agents-result"
        [ div ~cls:"agents-result-head" [ Node.text "Report" ]
        ; Markdown_view.render (report_text r)
        ]
    | Some ({ is_error = true; _ } as r) ->
      let text = String.strip (report_text r) in
      let head, text =
        match String.chop_prefix text ~prefix:"[cancelled]" with
        | Some rest -> "Cancelled", String.strip rest
        | None -> "Failed", text
      in
      div
        ~cls:"agents-result failed"
        [ div ~cls:"agents-result-head" [ Node.text head ]
        ; (if String.is_empty text
           then Node.none
           else Node.pre [ Node.text text ])
        ]
  in
  div
    ~cls:"agents-detail"
    ~attrs:[ Attr.create "data-shown" ("agent " ^ a.id) ]
    [ crumbs m ~inject a
    ; div
        ~cls:"detail-head"
        [ div
            ~cls:"detail-title"
            [ status_icon a.status
            ; span ~cls:"detail-task" (first_line a.task)
            ]
        ; div
            ~cls:"detail-meta"
            ([ pill a.status (Agents.Status.to_string a.status)
             ; span
                 ~cls:"detail-elapsed"
                 (Agents.format_span (Agents.agent_elapsed a ~now:(now m)))
             ]
             @ List.map (a.id :: agent_meta m a) ~f:(span ~cls:"detail-chip"))
        ; div
            ~cls:"detail-actions"
            [ (match a.status with
               | Running ->
                 stop_button
                   m
                   ~inject
                   (Agent a.id)
                   ~label:"Cancel"
                   ~title:"Cancel this subagent (/agents lists the others)"
                   ~action:(Action.Cancel_subagent a.id)
               | Complete | Failed -> Node.none)
            ; button
                ~cls:"ghost small"
                ~title:"Show its card in the chat"
                ~on_click:(inject (Action.Show_in_chat a.id))
                [ icon Locate; Node.text "Show in chat" ]
            ]
        ]
    ; (match children with
       | [] -> Node.none
       | rows ->
         section
           ~title:"Its subagents"
           ~count:(Int.to_string (List.length rows))
           rows)
    ; div ~cls:"agents-transcript" [ transcript ]
    ; result
    ]
;;

let job_detail (m : App.Model.t) ~inject (j : Agents.Job.t) =
  let status = job_status j in
  let output =
    match m.agents.output with
    | Some (id, text) when String.equal id j.info.id -> Some (split_output text)
    | _ -> None
  in
  div
    ~cls:"agents-detail"
    ~attrs:[ Attr.create "data-shown" ("job " ^ j.info.id) ]
    [ div
        ~cls:"detail-head"
        [ div
            ~cls:"detail-title"
            [ status_icon status; span ~cls:"detail-command" j.info.command ]
        ; div
            ~cls:"detail-meta"
            ([ pill
                 status
                 (if j.info.running
                  then "running"
                  else Option.value j.info.exit ~default:"finished")
             ; span
                 ~cls:"detail-elapsed"
                 (Agents.format_span (Agents.job_elapsed j ~now:(now m)))
             ]
             @ List.map (job_meta ~exit:false j) ~f:(span ~cls:"detail-chip"))
        ; (if j.info.running
           then
             div
               ~cls:"detail-actions"
               [ stop_button
                   m
                   ~inject
                   (Job j.info.id)
                   ~label:"Kill"
                   ~title:"Kill this job"
                   ~action:(Action.Kill_job j.info.id)
               ]
           else Node.none)
        ]
    ; (match output with
       | None ->
         div
           ~cls:"agents-note"
           [ Node.span ~attrs:[ Attr.class_ "spinner" ] []
           ; Node.text "Loading the output…"
           ]
       | Some (_, text) when String.is_empty (String.strip text) ->
         div ~cls:"agents-note" [ Node.text "No output yet." ]
       | Some (lines, text) ->
         Node.fragment
           [ (match lines with
              | Some lines ->
                div ~cls:"agents-note small" [ Node.text ("Output, " ^ lines) ]
              | None -> Node.none)
           ; Node.pre ~attrs:[ Attr.class_ "job-output" ] [ Node.text text ]
           ])
    ; (if j.info.running
       then
         div
           ~cls:"agents-note small"
           [ Node.text "Its output refreshes while it runs." ]
       else Node.none)
    ]
;;

let summary (a : Agents.t) =
  let running_agents = Agents.running_agents a in
  let running_jobs = Agents.running_jobs a in
  let parts ~agents ~jobs =
    List.filter_opt
      [ Option.some_if (agents > 0) (plural agents "agent")
      ; Option.some_if (jobs > 0) (plural jobs "job")
      ]
    |> String.concat ~sep:" · "
  in
  if running_agents + running_jobs > 0
  then Some (parts ~agents:running_agents ~jobs:running_jobs ^ " running", true)
  else (
    let listed = Agents.listed a in
    let agents = List.count listed ~f:is_agent in
    let jobs = List.length listed - agents in
    if agents + jobs = 0 then None else Some (parts ~agents ~jobs, false))
;;

let title (m : App.Model.t) =
  match m.agents.selected with
  | None -> "Agents"
  | Some (Agent id) -> "Subagent " ^ id
  | Some (Job id) -> "Job " ^ id
;;

let view (m : App.Model.t) ~inject =
  let a = m.agents in
  if not a.open_
  then Node.none
  else (
    let body =
      match a.selected with
      | None -> list_view m ~inject
      | Some (Agent id) ->
        (match Agents.find_agent a id with
         | Some agent -> agent_detail m ~inject agent
         | None -> list_view m ~inject)
      | Some (Job id) ->
        (match Agents.find_job a id with
         | Some job -> job_detail m ~inject job
         | None -> list_view m ~inject)
    in
    Node.create
      "aside"
      ~attrs:
        [ Attr.class_ "agents-panel"
        ; Attr.id "agents-panel"
        ; Attr.create "aria-label" "Subagents and jobs"
        ]
      [ div
          ~cls:"agents-resize"
          ~attrs:[ Attr.title "Drag to resize"; Attr.role "separator" ]
          []
      ; div
          ~cls:"agents-head"
          [ (match a.selected with
             | Some _ ->
               button
                 ~cls:"icon ghost"
                 ~title:"All agents (Esc)"
                 ~on_click:(inject Action.Agents_back)
                 [ icon Back ]
             | None -> icon ~cls:"agents-head-icon" Bot)
          ; span ~cls:"agents-title" (title m)
          ; (match summary a with
             | Some (text, true) -> span ~cls:"agents-running" text
             | _ -> Node.none)
          ; button
              ~cls:"icon ghost agents-close"
              ~title:"Close (Alt+0)"
              ~on_click:(inject Action.Toggle_subagents)
              [ icon Close ]
          ]
      ; div ~cls:"agents-body" ~attrs:[ Attr.id "agents-body" ] [ body ]
      ])
;;

let toggle (m : App.Model.t) ~inject =
  let a = m.agents in
  if (not a.open_) && List.is_empty a.agents && List.is_empty a.jobs
  then Node.none
  else (
    let running = Agents.running_agents a + Agents.running_jobs a in
    button
      ~cls:
        (if a.open_
         then "icon ghost agents-toggle open"
         else "icon ghost agents-toggle")
      ~title:"Subagents and jobs (/agents)"
      ~on_click:(inject Action.Toggle_subagents)
      [ icon Bot
      ; (if running > 0
         then span ~cls:"badge" (Int.to_string running)
         else Node.none)
      ])
;;
