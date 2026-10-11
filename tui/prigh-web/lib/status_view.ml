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

let item ?(cls = "") ?title children =
  Node.span
    ~attrs:
      ([ Attr.class_ (String.strip ("status-item " ^ cls)) ]
       @ Option.value_map title ~default:[] ~f:(fun t -> [ Attr.title t ]))
    children
;;

let context (state : State.t) =
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
    item
      ~cls:("context " ^ level)
      ~title:
        (sprintf
           "Context: %s of %s tokens (/compact frees some)"
           (tokens state.context_tokens)
           (tokens state.model.context_window))
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
      ; Node.textf "%.0f%%" percent
      ])
;;

let level_class : Prigh_ui.Loop_status.Level.t -> string = function
  | Normal -> ""
  | Warn -> "warm"
  | Over -> "hot"
;;

let chip ~cls ~title ~command ~inject children =
  Node.button
    ~attrs:
      [ Attr.class_ ("status-item link " ^ cls)
      ; Attr.type_ "button"
      ; Attr.title title
      ; Attr.on_click (fun _ -> inject (Action.Run command))
      ]
    children
;;

(* With a budget: spent / budget, warm from 80%, hot once spent. *)
let cost (state : State.t) ~inject =
  match Prigh_ui.Loop_status.budget_part state with
  | None ->
    item ~cls:"cost" ~title:"Session cost" [ Node.textf "$%.4f" state.cost_usd ]
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
      [ icon Target; Node.text text ]
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
      ; Node.text
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
      [ (if running then Html.lamp () else icon Bot); Node.text text ]
;;

let transfers (m : App.Model.t) =
  match Files.transfers m.files with
  | None -> Node.none
  | Some text ->
    item ~cls:"transfers" ~title:text [ Html.lamp (); Node.text text ]
;;

let host (state : State.t) =
  if String.equal state.active_host Host.backend_id
  then Node.none
  else (
    let name =
      List.find state.hosts ~f:(fun h -> String.equal h.id state.active_host)
      |> Option.value_map ~default:state.active_host ~f:(fun h -> h.name)
    in
    item ~cls:"host" ~title:"Tools run here" [ icon Server; Node.text name ])
;;

let user (m : App.Model.t) ~inject =
  let name =
    match m.hello, m.account with
    | Some ({ user = Some user; _ } as hello), _ ->
      Some
        (match Hello_reply.acting_as hello with
         | Some ns -> sprintf "%s as %s" user ns
         | None -> user)
    | _, Some account when not (List.is_empty m.accounts) ->
      Some (Accounts.Account.name account)
    | _ -> None
  in
  match name with
  | None -> Node.none
  | Some text ->
    Node.button
      ~attrs:
        [ Attr.class_ "status-item link account"
        ; Attr.type_ "button"
        ; Attr.title "Signed in: switch account"
        ; Attr.on_click (fun _ -> inject Action.Open_accounts)
        ]
      [ icon User; Node.text text ]
;;

(* Whether the browser tells you what happens while the tab is hidden; a
   click turns it off, or on (asking the browser if it has to). *)
let notifications (m : App.Model.t) ~inject =
  let n = m.notifications in
  let on = n.enabled && Page_notify.Permission.equal n.permission Granted in
  let cls =
    if on
    then "on"
    else if n.enabled && Page_notify.Permission.equal n.permission Denied
    then "warm"
    else "off"
  in
  let label =
    match n.enabled, n.permission with
    | false, _ -> "Notifications off (/notify on)"
    | true, Granted -> "Notifications on (/notify off)"
    | true, Default -> "Notifications not allowed yet: click to ask the browser"
    | true, Denied ->
      "Notifications blocked by the browser: click for how to allow them"
    | true, Unsupported -> "Notifications need https or localhost"
  in
  Node.button
    ~attrs:
      [ Attr.class_ ("status-item link notify " ^ cls)
      ; Attr.type_ "button"
      ; Attr.title label
      ; Attr.create "aria-label" label
      ; Attr.on_click (fun _ ->
          inject (Action.Run (if on then "/notify off" else "/notify on")))
      ]
    [ icon (if on then Bell else Bell_off) ]
;;

(* Settings that change what you see, and the model's plan; the approval
   mode is in the top bar. *)
let modes (m : App.Model.t) ~inject =
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
     | Some text ->
       chip
         ~cls:"plan"
         ~title:"The model's plan (/todos)"
         ~command:"/todos"
         text
     | None -> Node.none)
  ]
;;

let waiting cls text = item ~cls [ Html.lamp (); Node.text text ]

let view (m : App.Model.t) ~inject =
  match m.state, m.connection with
  | None, Connected -> div ~cls:"status" [ waiting "connecting" "Connecting…" ]
  | _, Reconnecting _ ->
    div ~cls:"status" [ waiting "connecting offline" "Reconnecting…" ]
  | Some state, Connected ->
    let queued = Queued.count m.queue in
    div
      ~cls:"status"
      [ (if m.pending.sending > 0 && not state.running
         then waiting "running" "Sending…"
         else if state.running
         then item ~cls:"running" [ Html.lamp (); Node.text "Working" ]
         else
           item
             ~cls:"idle"
             [ Node.span ~attrs:[ Attr.class_ "idle-dot" ] []
             ; Node.text "Ready"
             ])
      ; context state
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
      ; goal state ~inject
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
             [ icon Undo; Node.text (sprintf "%d queued" queued) ]
         else Node.none)
      ; background m ~inject
      ; transfers m
      ; host state
      ; Node.fragment (modes m ~inject)
      ; div ~cls:"status-spacer" []
      ; notifications m ~inject
      ; user m ~inject
      ]
;;
