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

let context_percent (state : State.t) =
  if state.model.context_window <= 0
  then None
  else
    Some
      (100.
       *. Float.of_int state.context_tokens
       /. Float.of_int state.model.context_window)
;;

let context_level percent =
  if Float.(percent >= 85.)
  then "hot"
  else if Float.(percent >= 60.)
  then "warm"
  else ""
;;

let busy (m : App.Model.t) =
  match m.state, m.connection with
  | Some state, Connected -> state.running || m.pending.sending > 0
  | None, Connected -> true
  (* Offline: nothing is at work here (the composer says it is offline). *)
  | _, Reconnecting _ -> false
;;

type doing =
  | Idle
  | Working of string
  | Retrying of string
  | Connecting of string
  | Offline of string

let doing (m : App.Model.t) =
  match m.state, m.connection with
  | None, Connected -> Connecting "Connecting…"
  | _, Reconnecting _ when App.Drops.gave_up m.drops ->
    Offline "Stopped reconnecting"
  | _, Reconnecting _ -> Offline "Reconnecting…"
  | Some state, Connected ->
    if m.pending.sending > 0 && not state.running
    then Working "Sending…"
    else if state.running
    then (
      let doing = Chat.activity m.chat in
      let text =
        String.capitalize
          (Option.value (Prigh_ui.Activity.label doing) ~default:"working")
      in
      match doing with
      | Retrying _ -> Retrying text
      | _ -> Working text)
    else Idle
;;

let activity_word m =
  match doing m with
  | Idle -> "Ready"
  | Working w | Retrying w | Connecting w | Offline w -> w
;;

let activity (m : App.Model.t) =
  let word = activity_word m in
  let cls, light =
    match doing m with
    | Idle -> "idle", Node.span ~attrs:[ Attr.class_ "idle-dot" ] []
    | Working _ -> "running", lamp ()
    | Retrying _ -> "running retrying", lamp ()
    | Connecting _ -> "connecting", lamp ()
    | Offline "Stopped reconnecting" -> "offline", Node.none
    | Offline _ -> "connecting offline", lamp ()
  in
  Node.span
    ~attrs:
      [ Attr.class_ ("status-item activity " ^ cls)
      ; Attr.title word
      ; Attr.role "status"
      ; Attr.create "aria-atomic" "true"
      ]
    [ light; span ~cls:"label" word ]
;;

let level_class : Prigh_ui.Loop_status.Level.t -> string = function
  | Normal -> ""
  | Warn -> "warm"
  | Over -> "hot"
;;

let cost (state : State.t) ~inject =
  match Prigh_ui.Loop_status.budget_part state with
  | None ->
    Node.span
      ~attrs:
        [ Attr.class_ "status-item cost"
        ; Attr.title (sprintf "Session cost: $%.4f" state.cost_usd)
        ]
      [ Node.textf "$%.2f" state.cost_usd ]
  | Some (text, level) ->
    Node.button
      ~attrs:
        [ Attr.class_ ("status-item link cost budget " ^ level_class level)
        ; Attr.type_ "button"
        ; Attr.title
            "Session cost and budget: no turn starts once it is spent (/budget)"
        ; Attr.on_click (fun _ -> inject (Action.Run "/budget"))
        ]
      [ Node.text text ]
;;

let fill percent =
  Css_gen.width
    (`Percent
        (Percent.of_percentage (Float.clamp_exn percent ~min:0. ~max:100.)))
;;

let meter percent =
  Node.span
    ~attrs:
      [ Attr.class_ (String.strip ("meter " ^ context_level percent))
      ; Attr.create "aria-hidden" "true"
      ]
    [ Node.span
        ~attrs:[ Attr.class_ "meter-fill"; Attr.style (fill percent) ]
        []
    ]
;;

let ring ?(cls = "") percent children =
  Node.span
    ~attrs:
      [ Attr.class_
          (String.concat
             ~sep:" "
             (List.filter
                [ "ring"; cls; context_level percent ]
                ~f:(Fn.non String.is_empty)))
      ; Attr.css_var
          ~name:"pct"
          (sprintf "%.1f" (Float.clamp_exn percent ~min:0. ~max:100.))
      ]
    children
;;

let details_id = "status-details"

let details (m : App.Model.t) (state : State.t) =
  let line ?(cls = "") k v =
    div
      ~cls:(String.strip ("status-detail " ^ cls))
      [ span ~cls:"status-detail-key" k; span ~cls:"status-detail-value" v ]
  in
  let mode = Session_mode.of_state state in
  Node.div
    ~attrs:
      [ Attr.class_ "status-details"; Attr.role "tooltip"; Attr.id details_id ]
    [ line "Status" (activity_word m)
    ; (match context_percent state with
       | Some percent ->
         line
           ~cls:("context " ^ context_level percent)
           "Context"
           (sprintf
              "%s / %s · %.0f%%"
              (tokens state.context_tokens)
              (tokens state.model.context_window)
              percent)
       | None -> Node.none)
    ; line
        "Tokens"
        (sprintf
           "%s in · %s out · %s cached"
           (tokens state.usage.input)
           (tokens state.usage.output)
           (tokens state.usage.cache_read))
    ; (match Prigh_ui.Loop_status.budget_part state with
       | None -> line "Cost" (sprintf "$%.4f" state.cost_usd)
       | Some (text, level) ->
         line
           ~cls:("cost " ^ level_class level)
           "Cost"
           (sprintf "$%.4f · %s" state.cost_usd text))
    ; line
        "Thinking"
        (if state.model.supports_thinking then state.thinking else "none")
    ; line "Mode" (Session_mode.title mode)
    ; line "Model" state.model.name
    ; (match state.git_branch with
       | Some branch -> line ~cls:"branch-line" "Branch" branch
       | None -> Node.none)
    ; div
        ~cls:"status-detail-hint"
        [ Node.text "Click for the session’s details · /compact frees context" ]
    ]
;;

let context_button ?(cls = "") (state : State.t) ~inject children =
  let label =
    match context_percent state with
    | Some percent ->
      sprintf "Context %.0f%% full: the session's details" percent
    | None -> "The session's details"
  in
  Node.button
    ~attrs:
      [ Attr.class_ (String.strip ("status-item link context " ^ cls))
      ; Attr.type_ "button"
      ; Attr.create "aria-label" label
      ; Attr.create "aria-describedby" details_id
      ; Attr.create "aria-haspopup" "dialog"
      ; Attr.on_click (fun _ -> inject (Action.Run "/session"))
      ]
    children
;;

let gauge ?(cls = "") m state children =
  Node.span
    ~attrs:[ Attr.class_ (String.strip ("status-gauge " ^ cls)) ]
    (children @ [ details m state ])
;;

(* {2 Notes: what applies, shown only while it does} *)

let text_ text = span ~cls:"status-text" text

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
    Node.span
      ~attrs:[ Attr.class_ "status-item transfers"; Attr.title text ]
      [ Html.lamp (); Node.text text ]
;;

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

let notes (m : App.Model.t) ~inject =
  match m.state, m.connection with
  | Some state, Connected ->
    let items =
      [ goal state ~inject
      ; reminders state ~inject
      ; resume m state ~inject
      ; nix_gc m ~inject
      ; transfers m
      ]
      @ modes m ~inject
    in
    div ~cls:"status-notes" items
  | _ -> Node.none
;;
