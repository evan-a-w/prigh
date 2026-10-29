open! Core
open! Import
open Html
module Action = App.Action

let tokens n =
  if n >= 1_000_000
  then sprintf "%.1fM" (Float.of_int n /. 1e6)
  else if n >= 10_000
  then sprintf "%dk" (n / 1000)
  else if n >= 1000
  then sprintf "%.1fk" (Float.of_int n /. 1000.)
  else Int.to_string n
;;

(* What a narrow bar leaves to the icon beside it. *)
let text_ text = span ~cls:"status-text" text

let item ?(cls = "") ?title ?(attrs = []) children =
  Node.span
    ~attrs:
      ([ Attr.class_ (String.strip ("status-item " ^ cls)) ]
       @ Option.value_map title ~default:[] ~f:(fun t -> [ Attr.title t ])
       @ attrs)
    children
;;

(* What the agent is doing, said (politely) when it changes: Working,
   Ready, Reconnecting… Not the counters, which change all the time. *)
let activity = [ Attr.role "status"; Attr.create "aria-atomic" "true" ]

let chip ?label ~cls ~title ~command ~inject children =
  Node.button
    ~attrs:
      [ Attr.class_ (String.strip ("status-item link " ^ cls))
      ; Attr.type_ "button"
      ; Attr.title title
      ; Option.value_map label ~default:Attr.empty ~f:(Attr.create "aria-label")
      ; Attr.on_click (fun _ -> inject (Action.Run command))
      ]
    children
;;

let context (state : State.t) ~inject =
  if state.model.context_window <= 0
  then Node.none
  else (
    let percent =
      100.
      *. Float.of_int state.context_tokens
      /. Float.of_int state.model.context_window
    in
    let level =
      if Float.(percent >= 85.)
      then "hot"
      else if Float.(percent >= 60.)
      then "warm"
      else ""
    in
    chip
      ~cls:("context " ^ level)
      ~title:
        (sprintf
           "Context: %s of %s tokens (/compact frees some). Click for the \
            session's details (/session)"
           (tokens state.context_tokens)
           (tokens state.model.context_window))
      ~command:"/session"
      ~label:(sprintf "Context %.0f%% full" percent)
      ~inject
      [ Node.span
          ~attrs:[ Attr.class_ "meter" ]
          [ Node.span
              ~attrs:
                [ Attr.class_ "meter-fill"
                ; Attr.style
                    (Css_gen.width
                       (`Percent
                           (Percent.of_percentage (Float.min 100. percent))))
                ]
              []
          ]
      ; span ~cls:"percent" (sprintf "%.0f%%" percent)
      ])
;;

let level_class : Prigh_ui.Loop_status.Level.t -> string = function
  | Normal -> ""
  | Warn -> "warm"
  | Over -> "hot"
;;

(* With a budget: spent / budget, warm from 80%, hot once spent. *)
let cost (state : State.t) ~inject =
  match Prigh_ui.Loop_status.budget_part state with
  | None ->
    item
      ~cls:"cost"
      ~title:(sprintf "Session cost: $%.4f (/session)" state.cost_usd)
      [ Node.textf "$%.2f" state.cost_usd ]
  | Some (text, level) ->
    chip
      ~cls:("cost budget " ^ level_class level)
      ~title:
        "Session cost and budget: no turn starts once it is spent (/budget)"
      ~command:"/budget"
      ~inject
      [ Node.text text ]
;;

let goal (state : State.t) ~inject =
  match state.goal with
  | None -> Node.none
  | Some g ->
    let level = snd (Prigh_ui.Loop_status.goal_part g) in
    let text =
      match g.status with
      | Active -> sprintf "Goal %d/%d" g.continuations g.max_continuations
      | Done -> "Goal done"
      | Stopped -> "Goal stopped"
    in
    chip
      ~cls:("goal " ^ level_class level)
      ~title:("Goal: " ^ g.text ^ " (/goal)")
      ~command:"/goal"
      ~inject
      [ icon Target; text_ text ]
;;

let reminders (state : State.t) ~inject =
  match state.reminders with
  | [] -> Node.none
  | reminders ->
    chip
      ~cls:"reminders"
      ~title:
        (String.concat
           ~sep:"\n"
           (sprintf
              "Reminders (/remind lists them, /remind rm NAME removes one):"
            :: List.map reminders ~f:Prigh_ui.Remind_command.describe))
      ~command:"/remind"
      ~inject
      [ icon Restart; text_ (sprintf "Remind %d" (List.length reminders)) ]
;;

let resume (m : App.Model.t) (state : State.t) ~inject =
  match state.resume_at with
  | None -> Node.none
  | Some at ->
    chip
      ~cls:"resume warm"
      ~title:
        "Stopped at a usage limit: resumes then (/resume now, /resume cancel)"
      ~command:"/resume"
      ~inject
      [ icon Clock
      ; text_
          (String.capitalize
             (Prigh_ui.Loop_status.resume_part at ~utc_offset:m.utc_offset))
      ]
;;

let background (m : App.Model.t) ~inject =
  match Agents_view.summary m.agents with
  | None -> Node.none
  | Some (text, running) ->
    Node.button
      ~attrs:
        [ classes [ "status-item"; "link"; "background" ] [ "active", running ]
        ; Attr.type_ "button"
        ; Attr.title "Subagents and jobs (/agents)"
        ; Attr.on_click (fun _ -> inject (Action.Open_subagents None))
        ]
      [ (if running then Html.lamp () else icon Bot); text_ text ]
;;

let nix_gc (m : App.Model.t) ~inject =
  match m.nix_gc.collecting with
  | Some p when App.superuser m ->
    let text =
      "Collecting Nix garbage: " ^ Prigh_ui.Nix_gc_command.deleted_so_far p
    in
    Node.button
      ~attrs:
        [ Attr.class_ "status-item link nix-gc active"
        ; Attr.type_ "button"
        ; Attr.title (String.capitalize text ^ " (Settings shows the store)")
        ; Attr.on_click (fun _ -> inject Action.Open_settings)
        ]
      [ Html.lamp (); text_ "Nix GC" ]
  | Some _ | None -> Node.none
;;

let transfers (m : App.Model.t) =
  match Files.transfers m.files with
  | None -> Node.none
  | Some text ->
    item ~cls:"transfers" ~title:text [ Html.lamp (); Node.text text ]
;;

(* Settings that change what you see, and the model's plan (where there is
   room for it); the approval mode is in the composer. *)
let modes (m : App.Model.t) ~plan ~inject =
  let chip ?(cls = "") ~title ~command text =
    Node.button
      ~attrs:
        [ Attr.class_ (String.strip ("status-item link mode " ^ cls))
        ; Attr.type_ "button"
        ; Attr.title title
        ; Attr.on_click (fun _ -> inject (Action.Run command))
        ]
      [ Node.text text ]
  in
  [ (match m.verbosity with
     | Normal -> Node.none
     | v ->
       chip
         ~title:"Transcript verbosity (Ctrl+O cycles)"
         ~command:"/verbosity"
         (Prigh_ui.Verbosity.name v))
  ; (match
       Option.bind m.state ~f:(fun s -> Option.bind s.plan ~f:Plan_view.status)
     with
     | Some text when plan ->
       chip
         ~cls:"plan"
         ~title:"The model's plan (/todos)"
         ~command:"/todos"
         text
     | Some _ | None -> Node.none)
  ]
;;

(* A slot of its own width, whatever it says, so that nothing moves as the
   agent starts and stops: the label gives way, and its title says it all. *)
let activity_item ?title cls children =
  item ~cls:("activity " ^ cls) ?title ~attrs:activity children
;;

let label text = span ~cls:"label" text

let waiting cls text =
  activity_item ~title:text cls [ Html.lamp (); label text ]
;;

let activity (m : App.Model.t) =
  match m.state, m.connection with
  | None, Connected -> waiting "connecting" "Connecting…"
  | _, Reconnecting _ when App.Drops.gave_up m.drops ->
    activity_item
      ~title:"Stopped reconnecting"
      "offline"
      [ label "Stopped reconnecting" ]
  | _, Reconnecting _ -> waiting "connecting offline" "Reconnecting…"
  | Some state, Connected ->
    if m.pending.sending > 0 && not state.running
    then waiting "running" "Sending…"
    else if state.running
    then (
      let doing = Chat.activity m.chat in
      let text =
        String.capitalize
          (Option.value (Prigh_ui.Activity.label doing) ~default:"working")
      in
      activity_item
        (match doing with
         | Retrying _ -> "running retrying"
         | _ -> "running")
        ~title:text
        [ Html.lamp (); label text ])
    else
      activity_item
        "idle"
        [ Node.span ~attrs:[ Attr.class_ "idle-dot" ] []; label "Ready" ]
;;

let counters (m : App.Model.t) ~inject =
  match m.state, m.connection with
  | Some state, Connected ->
    [ context state ~inject
    ; item
        ~cls:"tokens"
        ~title:
          (sprintf
             "Tokens: %d in, %d out, %d cache read"
             state.usage.input
             state.usage.output
             state.usage.cache_read)
        [ Node.textf
            "↑%s ↓%s"
            (tokens state.usage.input)
            (tokens state.usage.output)
        ]
    ; cost state ~inject
    ]
  | _ -> []
;;

let view (m : App.Model.t) ~inject =
  div ~cls:"status" (counters m ~inject @ [ activity m ])
;;

let extras (m : App.Model.t) ~plan ~inject =
  match m.state, m.connection with
  | Some state, Connected ->
    let queued, whose =
      match m.recipient with
      | Main -> Queued.count m.queue, ""
      | Agent id ->
        let q = Agent_queue.find m.agent_queues id in
        List.length q.steer + List.length q.follow_up, " for " ^ id
    in
    div
      ~cls:"status-extras"
      [ goal state ~inject
      ; reminders state ~inject
      ; resume m state ~inject
      ; (if queued > 0
         then
           Node.button
             ~attrs:
               [ Attr.class_ "status-item link queued"
               ; Attr.type_ "button"
               ; Attr.title "Take the last queued message back into the editor"
               ; Attr.on_click (fun _ -> inject Action.Dequeue)
               ]
             [ icon Undo; text_ (sprintf "%d queued%s" queued whose) ]
         else Node.none)
      ; background m ~inject
      ; nix_gc m ~inject
      ; transfers m
      ; Node.fragment (modes m ~plan ~inject)
      ]
  | _ -> div ~cls:"status-extras" []
;;
