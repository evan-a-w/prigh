open! Core
open! Import
open Html
module Action = App.Action
module Agent = Agents.Agent
module Item = Agents.Item

let plural = Chat_html.plural
let now (m : App.Model.t) = Option.value m.now ~default:Time_ns.epoch
let first_line = Chat_html.first_line

(* How an agent or job stands: running, or how it ended. Cancelled,
   interrupted and killed work stopped without failing: it is shown quietly,
   not in brick. *)
module Outcome = struct
  type t =
    | Running
    | Done
    | Failed
    | Stopped of string

  let cls = function
    | Running -> "running"
    | Done -> "complete"
    | Failed -> "failed"
    | Stopped _ -> "stopped"
  ;;

  let word = function
    | Running -> "Running"
    | Done -> "Done"
    | Failed -> "Failed"
    | Stopped word -> word
  ;;

  let mark t =
    match t with
    | Running ->
      (* The row says it in words for screen readers. *)
      Node.span
        ~attrs:
          [ Attr.class_ "lamp"
          ; Attr.title "Running"
          ; Attr.create "aria-hidden" "true"
          ]
        []
    | Done -> Html.mark ~cls:"done" Check
    | Failed -> Html.mark ~cls:"failed" Cross
    | Stopped _ -> Html.mark ~cls:"stopped" Dash
  ;;
end

let report_text (r : Event.Subagent_result.t) =
  (Subagent_report.of_string r.text).text
;;

(* A failed report's prefix ("[cancelled]", "subagent failed: ") says how it
   ended: that is the outcome, the rest is what to show. *)
let ending (a : Agent.t) : Outcome.t * string =
  match a.result with
  | None -> Running, ""
  | Some ({ is_error = false; _ } as r) -> Done, report_text r
  | Some ({ is_error = true; _ } as r) ->
    let text = String.strip (report_text r) in
    let rest prefix =
      Option.map (String.chop_prefix text ~prefix) ~f:String.strip
    in
    (match rest "[cancelled]", rest "[interrupted]" with
     | Some rest, _ -> Stopped "Cancelled", rest
     | None, Some rest -> Stopped "Interrupted", rest
     | None, None ->
       Failed, Option.value (rest "subagent failed:") ~default:text)
;;

let job_outcome (j : Agents.Job.t) : Outcome.t =
  match j.info.running, j.info.exit with
  | true, _ -> Running
  | false, (None | Some "exited 0") -> Done
  | false, Some "killed" -> Stopped "Killed"
  | false, Some _ -> Failed
;;

let is_failed : Outcome.t -> bool = function
  | Failed -> true
  | Running | Done | Stopped _ -> false
;;

let selected (m : App.Model.t) item =
  Option.equal Item.equal m.agents.selected (Some item)
;;

(* Finished in under a second: no time worth showing. *)
let elapsed_label (outcome : Outcome.t) span =
  match outcome with
  | Running -> Some (Agents.format_span span)
  | Done | Failed | Stopped _ ->
    Option.some_if Time_ns.Span.(span >= second) (Agents.format_span span)
;;

(* What it is doing, or how it ended. *)
let activity (m : App.Model.t) (a : Agent.t) =
  match ending a with
  | Running, _ ->
    let latest =
      Option.bind (Chat.find_subagent m.chat a.id) ~f:(fun s ->
        Tool_view.last_activity s.chat)
    in
    (match a.current_tool, latest with
     | Some (name, _), latest -> Option.value latest ~default:name
     | None, Some latest -> latest
     | None, None -> if a.turns = 0 then "Starting…" else "Thinking…")
  | Done, text -> Markdown.preview text
  | Failed, text -> first_line text
  | Stopped "Interrupted", text when not (String.is_empty text) ->
    "Interrupted: " ^ first_line text
  | Stopped word, _ -> word
;;

let row
      ~inject
      ~item
      ~(outcome : Outcome.t)
      ~selected
      ~shortcut
      ~title
      ~elapsed
      ~meta
      ~line
  =
  Node.button
    ~attrs:
      ([ classes [ "agents-row"; Outcome.cls outcome ] [ "selected", selected ]
       ; Attr.type_ "button"
       ; Attr.on_click (fun _ -> inject (Action.Select_item item))
       ]
       @ (if selected then [ Attr.create "aria-current" "true" ] else [])
       @ Option.value_map shortcut ~default:[] ~f:(fun n ->
         [ Attr.create "aria-keyshortcuts" (sprintf "Alt+%d" n)
         ; Attr.title (sprintf "Alt+%d" n)
         ]))
    (List.filter_opt
       [ Some
           (Node.span
              ~attrs:[ Attr.class_ "row-mark" ]
              [ Outcome.mark outcome ])
       ; (* Stopped work's line says so. *)
         (match outcome with
          | Running | Done | Failed ->
            Some (span ~cls:"sr-only" (Outcome.word outcome))
          | Stopped _ -> None)
       ; Some (span ~cls:"row-title" title)
       ; Option.map elapsed ~f:(span ~cls:"row-time")
       ; Option.some_if (not (String.is_empty line)) (span ~cls:"row-line" line)
       ; (match List.filter meta ~f:(Fn.non String.is_empty) with
          | [] -> None
          | meta -> Some (span ~cls:"row-meta" (String.concat ~sep:"  " meta)))
       ])
;;

let agent_meta (m : App.Model.t) (a : Agent.t) =
  let cost =
    Option.bind (Chat.find_subagent m.chat a.id) ~f:(fun s -> s.cost_usd)
  in
  let queued =
    let q = Agent_queue.find m.agent_queues a.id in
    List.length q.steer + List.length q.follow_up
  in
  List.filter_opt
    [ Option.some_if (queued > 0) (sprintf "%d queued" queued)
    ; Some a.model
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

let job_meta (j : Agents.Job.t) =
  List.filter_opt
    [ Some j.info.id; Option.some_if (j.info.bytes > 0) (bytes j.info.bytes) ]
;;

(* Its last line; how it ended when that was not a success. *)
let job_line (j : Agents.Job.t) (outcome : Outcome.t) =
  let last = Option.value j.info.last_line ~default:"" in
  match outcome, j.info.exit with
  | Failed, Some exit when not (String.is_empty last) ->
    sprintf "%s: %s" exit last
  | Failed, Some exit -> exit
  | Stopped word, _ -> word
  | _ -> last
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

let item_row (m : App.Model.t) ~inject ~shortcut (item : Item.t) =
  let a = m.agents in
  match item with
  | Agent id ->
    (match Agents.find_agent a id with
     | None -> Node.none
     | Some agent ->
       let outcome, _ = ending agent in
       row
         ~inject
         ~item
         ~outcome
         ~selected:(selected m item)
         ~shortcut
         ~title:(first_line agent.task)
         ~elapsed:
           (elapsed_label outcome (Agents.agent_elapsed agent ~now:(now m)))
         ~meta:
           (match outcome with
            | Running -> agent.id :: agent_meta m agent
            | Done | Failed | Stopped _ -> [])
         ~line:(activity m agent))
  | Job id ->
    (match Agents.find_job a id with
     | None -> Node.none
     | Some job ->
       let outcome = job_outcome job in
       row
         ~inject
         ~item
         ~outcome
         ~selected:(selected m item)
         ~shortcut
         ~title:(first_line job.info.command)
         ~elapsed:(elapsed_label outcome (Agents.job_elapsed job ~now:(now m)))
         ~meta:
           (match outcome with
            | Running -> job_meta job
            | Done | Failed | Stopped _ -> [])
         ~line:(job_line job outcome)
       |> fun node -> Node.div ~attrs:[ Attr.class_ "job" ] [ node ])
;;

let is_agent ((item : Item.t), _) =
  match item with
  | Agent _ -> true
  | Job _ -> false
;;

module Branch = struct
  type t =
    { item : Item.t
    ; children : t list
    }

  (* The flat (item, depth) list as a tree: each item with what follows it
     deeper, until one at its depth or shallower. *)
  let rec nest = function
    | [] -> []
    | (item, depth) :: rest ->
      let inside, after = List.split_while rest ~f:(fun (_, d) -> d > depth) in
      { item; children = nest inside } :: nest after
  ;;
end

let tree_view (m : App.Model.t) ~inject ~shortcut entries =
  let rec branch ({ item; children } : Branch.t) =
    let row = item_row m ~inject ~shortcut:(shortcut item) item in
    match children with
    | [] -> Node.li [ row ]
    | children ->
      Node.li
        [ row
        ; Node.ul
            ~attrs:[ Attr.class_ "agents-tree nested" ]
            (List.map children ~f:branch)
        ]
  in
  Node.ul
    ~attrs:[ Attr.class_ "agents-tree" ]
    (List.map (Branch.nest entries) ~f:branch)
;;

let section ?(cls = "") ~title ~count body =
  Node.section
    ~attrs:[ Attr.class_ (String.strip ("agents-section " ^ cls)) ]
    [ Node.h3
        ~attrs:[ Attr.class_ "agents-section-head" ]
        [ Node.text title; span ~cls:"agents-section-count" count ]
    ; body
    ]
;;

let counts ~running ~total =
  if running > 0 then sprintf "%d running" running else Int.to_string total
;;

let list_view (m : App.Model.t) ~inject =
  let a = m.agents in
  let listed = Agents.listed a in
  let numbers =
    List.mapi listed ~f:(fun i (item, _) -> item, i + 1)
    |> Item.Map.of_alist_reduce ~f:Fn.const
  in
  let shortcut item =
    Option.bind (Map.find numbers item) ~f:(fun n -> Option.some_if (n <= 9) n)
  in
  let agents = List.filter listed ~f:is_agent in
  let jobs = List.filter listed ~f:(Fn.non is_agent) in
  let running entries =
    List.count entries ~f:(fun (item, _) -> Agents.running a item)
  in
  let group ~title entries =
    match entries with
    | [] -> Node.none
    | entries ->
      section
        ~title
        ~count:(counts ~running:(running entries) ~total:(List.length entries))
        (tree_view m ~inject ~shortcut entries)
  in
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
      [ group ~title:"Subagents" agents
      ; group ~title:"Jobs" jobs
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
             [ Node.summary [ Node.textf "Earlier %d" (List.length earlier) ]
             ; tree_view
                 m
                 ~inject
                 ~shortcut:(fun _ -> None)
                 (List.filter earlier ~f:is_agent)
             ; tree_view
                 m
                 ~inject
                 ~shortcut:(fun _ -> None)
                 (List.filter earlier ~f:(Fn.non is_agent))
             ])
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

(* Who started it, outermost first: each opens that agent. *)
let crumbs (m : App.Model.t) ~inject (a : Agent.t) =
  match List.drop_last (Agents.lineage m.agents a.id) with
  | None | Some [] -> Node.none
  | Some ancestors ->
    Node.nav
      ~attrs:
        [ Attr.class_ "agents-crumbs"; Attr.create "aria-label" "Started by" ]
      (List.concat_map ancestors ~f:(fun (b : Agent.t) ->
         [ Node.button
             ~attrs:
               [ Attr.class_ "crumb"
               ; Attr.type_ "button"
               ; Attr.title b.id
               ; Attr.on_click (fun _ ->
                   inject (Action.Select_item (Agent b.id)))
               ]
             [ Node.text (first_line b.task) ]
         ; Node.span
             ~attrs:
               [ Attr.class_ "crumb-sep"; Attr.create "aria-hidden" "true" ]
             [ Node.text "›" ]
         ]))
;;

(* The title, then one quiet line: how it stands (in its colour), for how
   long, and the rest; the action that stops it at the line's end. *)
let detail_head ~(outcome : Outcome.t) ~title ~elapsed ~meta ~action =
  div
    ~cls:("detail-head " ^ Outcome.cls outcome)
    [ Node.h2
        ~attrs:[ Attr.class_ "detail-title" ]
        [ Node.span ~attrs:[ Attr.class_ "row-mark" ] [ Outcome.mark outcome ]
        ; title
        ]
    ; div
        ~cls:"detail-meta"
        (action
         :: List.intersperse
              ~sep:(Node.text " ")
              (List.filter_opt
                 [ Some (span ~cls:"detail-state" (Outcome.word outcome))
                 ; Option.map elapsed ~f:(span ~cls:"detail-elapsed")
                 ; (match meta with
                    | [] -> None
                    | meta ->
                      Some
                        (span ~cls:"detail-more" (String.concat ~sep:"  " meta)))
                 ]))
    ]
;;

(* The composer writes to it (again, when it does: back to the main agent);
   a nested one, which takes no messages, says why. Finished, it has none. *)
let steer_button (m : App.Model.t) ~inject (a : Agent.t) =
  let steering = Recipient.equal m.recipient (Agent a.id) in
  match Recipient.refusal m.agents a.id with
  | Some _ when not (Agents.running m.agents (Agent a.id)) -> Node.none
  | Some why ->
    button
      ~cls:"ghost small agents-steer"
      ~title:why
      ~disabled:true
      ~on_click:Effect.Ignore
      [ icon Target; Node.text "Steer" ]
  | None ->
    button
      ~cls:
        (if steering
         then "ghost small agents-steer on"
         else "small agents-steer")
      ~title:
        (if steering
         then "Your messages go to it (Esc in the editor: to the main agent)"
         else sprintf "Write to %s from the composer (/agents steer)" a.id)
      ~attrs:[ Attr.create "aria-pressed" (Bool.to_string steering) ]
      ~on_click:
        (inject (Action.Set_recipient (if steering then Main else Agent a.id)))
      [ icon Target; Node.text (if steering then "Steering" else "Steer") ]
;;

let agent_detail (m : App.Model.t) ~inject (a : Agent.t) =
  let sub = Chat.find_subagent m.chat a.id in
  let outcome, ended = ending a in
  let children =
    match Agents.children m.agents (Some a.id) with
    | [] -> Node.none
    | children ->
      section
        ~cls:"agents-children"
        ~title:"Its subagents"
        ~count:(Int.to_string (List.length children))
        (tree_view
           m
           ~inject
           ~shortcut:(fun _ -> None)
           (List.map children ~f:(fun (c : Agent.t) -> Item.Agent c.id, 0)))
  in
  (* The task is the title: its prompt opens the transcript only when it
     says more than the title does. *)
  let task_in_title = not (String.mem (String.strip a.task) '\n') in
  let transcript =
    match sub with
    | Some s when not (List.is_empty (Chat.entries s.chat)) ->
      div
        ~cls:
          (if task_in_title
           then "agents-transcript task-in-title"
           else "agents-transcript")
        [ Chat_view.view
            ~fold:(Fold.scoped (App.Model.fold m) s.agent_id)
            (App.Model.message_time m)
            s.chat
        ]
    | Some _ -> div ~cls:"agents-note" [ Html.lamp (); Node.text "Starting…" ]
    | None ->
      let retry why =
        div
          ~cls:"agents-note failed"
          [ Node.text why
          ; button
              ~cls:"ghost small"
              ~title:"Ask the backend for its transcript again"
              ~on_click:(inject (Action.Select_item (Agent a.id)))
              [ Node.text "Retry" ]
          ]
      in
      (match Map.find m.agents.fetches a.id with
       | Some (Asked _) ->
         div
           ~cls:"agents-note"
           [ Html.lamp (); Node.text "Loading the transcript…" ]
       | Some (Failed e) ->
         retry (sprintf "Couldn't load the transcript: %s." e)
       | None -> retry "The transcript is not loaded.")
  in
  (* The transcript ends with the report; how it stopped or failed is said
     below it. *)
  let ending_note =
    match outcome, sub with
    | Running, _ -> Node.none
    | Done, Some _ -> Node.none
    | Done, None ->
      div
        ~cls:"agents-ending complete"
        [ div ~cls:"agents-ending-head" [ Node.text "Report" ]
        ; Markdown_view.render ended
        ]
    | (Failed | Stopped _), _ ->
      (* The transcript, once loaded, shows what it had got to. *)
      div
        ~cls:("agents-ending " ^ Outcome.cls outcome)
        [ Node.span ~attrs:[ Attr.class_ "row-mark" ] [ Outcome.mark outcome ]
        ; span ~cls:"agents-ending-head" (Outcome.word outcome)
        ; (if
             String.is_empty ended
             || (Option.is_some sub && not (is_failed outcome))
           then Node.none
           else Node.pre [ Node.text ended ])
        ]
  in
  ( div
      ~cls:"agents-detail"
      ~attrs:[ Attr.create "data-shown" ("agent " ^ a.id) ]
      [ children; transcript; ending_note ]
  , [ crumbs m ~inject a
    ; detail_head
        ~outcome
        ~title:
          (Node.span
             ~attrs:
               [ Attr.class_ "detail-task"; Attr.title (String.strip a.task) ]
             [ Node.text (first_line a.task) ])
        ~elapsed:(elapsed_label outcome (Agents.agent_elapsed a ~now:(now m)))
        ~meta:(agent_meta m a)
        ~action:
          (Node.fragment
             [ steer_button m ~inject a
             ; (match outcome with
                | Running ->
                  stop_button
                    m
                    ~inject
                    (Agent a.id)
                    ~label:"Cancel"
                    ~title:"Cancel this subagent (/agents lists the others)"
                    ~action:(Action.Cancel_subagent a.id)
                | Done | Failed | Stopped _ -> Node.none)
             ; button
                 ~cls:"ghost small agents-locate"
                 ~on_click:(inject (Action.Show_in_chat a.id))
                 [ icon Locate; Node.text "Show in chat" ]
             ])
    ] )
;;

let job_detail (m : App.Model.t) ~inject (j : Agents.Job.t) =
  let outcome = job_outcome j in
  let output =
    match m.agents.output with
    | Some (id, text) when String.equal id j.info.id -> Some (split_output text)
    | _ -> None
  in
  ( div
      ~cls:"agents-detail"
      ~attrs:[ Attr.create "data-shown" ("job " ^ j.info.id) ]
      [ (match output with
         | None ->
           div
             ~cls:"agents-note"
             [ Html.lamp (); Node.text "Loading the output…" ]
         | Some (_, text) when String.is_empty (String.strip text) ->
           div ~cls:"agents-note" [ Node.text "No output yet." ]
         | Some (lines, text) ->
           div
             ~cls:"job-output-block"
             [ Node.pre ~attrs:[ Attr.class_ "job-output" ] [ Node.text text ]
             ; div
                 ~cls:"agents-note small"
                 [ Node.text
                     (String.concat
                        ~sep:"  "
                        (List.filter_opt
                           [ Option.map lines ~f:(fun l -> "Output, " ^ l)
                           ; Option.some_if
                               j.info.running
                               "refreshes while it runs"
                           ]))
                 ]
             ])
      ]
  , [ detail_head
        ~outcome
        ~title:(span ~cls:"detail-command" j.info.command)
        ~elapsed:(elapsed_label outcome (Agents.job_elapsed j ~now:(now m)))
        ~meta:
          (List.filter_opt
             [ (match outcome, j.info.exit with
                | Failed, Some exit -> Some exit
                | _ -> None)
             ; Option.some_if (j.info.bytes > 0) (bytes j.info.bytes)
             ])
        ~action:
          (if j.info.running
           then
             stop_button
               m
               ~inject
               (Job j.info.id)
               ~label:"Kill"
               ~title:"Kill this job"
               ~action:(Action.Kill_job j.info.id)
           else Node.none)
    ] )
;;

let summary (a : Agents.t) =
  let running_agents = Agents.running_agents a in
  let running_jobs = Agents.running_jobs a in
  let parts ~agents ~jobs =
    List.filter_opt
      [ Option.some_if (agents > 0) (plural agents "agent")
      ; Option.some_if (jobs > 0) (plural jobs "job")
      ]
    |> String.concat ~sep:", "
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
  | None -> [ Node.text "Agents" ]
  | Some (Agent id) -> [ Node.text "Subagent "; span ~cls:"agents-id" id ]
  | Some (Job id) -> [ Node.text "Job "; span ~cls:"agents-id" id ]
;;

let view (m : App.Model.t) ~inject =
  let a = m.agents in
  if not a.open_
  then Node.none
  else (
    let shown =
      match a.selected with
      | None -> None
      | Some (Agent id) ->
        Option.map (Agents.find_agent a id) ~f:(fun agent -> `Agent agent)
      | Some (Job id) ->
        Option.map (Agents.find_job a id) ~f:(fun job -> `Job job)
    in
    (* What is shown in full: the transcript or output scrolls, what it is
       and its actions stay in sight below it. *)
    let body, foot =
      match shown with
      | None -> list_view m ~inject, []
      | Some (`Agent agent) -> agent_detail m ~inject agent
      | Some (`Job job) -> job_detail m ~inject job
    in
    Node.create
      "aside"
      ~attrs:
        [ Attr.class_ "agents-panel"
        ; Attr.id "agents-panel"
        ; Attr.tabindex (-1)
        ; Attr.create "aria-label" "Subagents and jobs"
        ]
      [ Resize_view.handle m ~inject Agents
      ; div
          ~cls:"agents-head"
          [ (match shown with
             | Some _ ->
               button
                 ~cls:"icon ghost"
                 ~title:"All agents (Esc)"
                 ~label:"All agents"
                 ~on_click:(inject Action.Agents_back)
                 [ icon Back ]
             | None -> icon ~cls:"agents-head-icon" Bot)
          ; Node.h2 ~attrs:[ Attr.class_ "agents-title" ] (title m)
          ; (match shown, summary a with
             | None, Some (text, true) -> span ~cls:"agents-running" text
             | _ -> Node.none)
          ; button
              ~cls:"icon ghost agents-close"
              ~title:"Close (Alt+0)"
              ~label:"Close subagents and jobs"
              ~on_click:(inject Action.Toggle_subagents)
              [ icon Close ]
          ]
      ; div ~cls:"agents-body" ~attrs:[ Attr.id "agents-body" ] [ body ]
      ; (match foot with
         | [] -> Node.none
         | foot ->
           Node.footer
             ~attrs:
               [ Attr.class_ "agents-foot"
               ; Attr.create
                   "aria-label"
                   (match shown with
                    | Some (`Job _) -> "This job"
                    | Some (`Agent _) | None -> "This subagent")
               ]
             foot)
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
      ~label:
        (if running > 0
         then sprintf "Subagents and jobs: %d running" running
         else "Subagents and jobs")
      ~attrs:[ Attr.create "aria-expanded" (Bool.to_string a.open_) ]
      ~on_click:(inject Action.Toggle_subagents)
      [ icon Bot
      ; (if running > 0
         then span ~cls:"badge" (Int.to_string running)
         else Node.none)
      ])
;;
