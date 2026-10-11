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
    let text, level = Prigh_ui.Loop_status.goal_part g in
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
      ; Node.text (Prigh_ui.Loop_status.resume_part at ~utc_offset:m.utc_offset)
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

(* Modes and settings that change what you see or what happens. *)
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
  ; (match m.state with
     | Some s ->
       (* [auto], where nothing asks first, shows nothing. *)
       let mode = Session_mode.of_state s in
       if (not mode.plan) && Approval_mode.equal mode.approval Auto
       then Node.none
       else
         chip
           ~cls:(if mode.plan then "plan-mode" else "")
           ~title:
             (sprintf
                "Mode: %s (/mode picks, Alt+M cycles)"
                (Session_mode.describe mode))
           ~command:"/mode"
           (Session_mode.label mode)
     | None -> Node.none)
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
    let steer, follow_up = m.queue in
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
      ; (if steer + follow_up > 0
         then
           Node.button
             ~attrs:
               [ Attr.class_ "status-item link queued"
               ; Attr.type_ "button"
               ; Attr.title "Take the last queued message back into the editor"
               ; Attr.on_click (fun _ -> inject Action.Dequeue)
               ]
             [ icon Undo; Node.text (sprintf "%d queued" (steer + follow_up)) ]
         else Node.none)
      ; background m ~inject
      ; host state
      ; Node.fragment (modes m ~inject)
      ; div ~cls:"status-spacer" []
      ; user m ~inject
      ]
;;
