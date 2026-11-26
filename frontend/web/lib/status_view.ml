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
      let doing = App.Model.activity m in
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
      [ Node.text (Prigh_ui.Loop_status.cost state) ]
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

(* {2 Notes: what applies, shown only while it does} *)

let text_ text = span ~cls:"status-text" text

(* A note is loon's outline Badge, a button: the accent once it is near a
   limit ([warm]), destructive past it ([hot]). *)
let note_classes cls =
  let words = String.split cls ~on:' ' in
  let tone =
    if List.mem words "hot" ~equal:String.equal
    then "text-destructive border-destructive/40"
    else if List.mem words "warm" ~equal:String.equal
    then "text-primary border-primary/40"
    else "text-muted-foreground"
  in
  Loon.Badge.classes
    ~variant:Outline
    ~class_:
      (String.concat
         ~sep:" "
         [ "status-item link loon cursor-pointer max-w-[24ch] hover:bg-muted \
            hover:text-foreground narrow:h-11 narrow:text-sm"
         ; cls
         ; tone
         ])
    ()
;;

let chip ?label ~cls ~title ~command ~inject children =
  Node.button
    ~attrs:
      [ Attr.class_ (note_classes cls)
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
        [ Attr.class_ (note_classes "nix-gc active")
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
      ~attrs:
        [ Attr.class_
            (Loon.Badge.classes
               ~variant:Outline
               ~class_:"status-item transfers max-w-[32ch] text-primary"
               ())
        ; Attr.title text
        ]
      [ Html.lamp (); Node.text text ]
;;

(* The plan's chip: [Plan: current step…], cut at its end (one flex item,
   [truncate]: the badge's centred flex box would clip both ends, with no
   ellipsis); the progress and the whole plan in its title, and a tap opens
   it (/todos). *)
let plan_chip (plan : Plan.t) ~inject =
  let total = List.length plan.steps in
  let completed = Plan.completed plan in
  if completed >= total
  then Node.none
  else (
    let running =
      List.count plan.steps ~f:(fun s -> Plan.Status.equal s.status In_progress)
    in
    let step =
      Option.value_map (Plan.current plan) ~default:"" ~f:(fun s -> s.step)
      ^ if running > 1 then sprintf " (+%d running)" (running - 1) else ""
    in
    let count = sprintf "%d/%d" completed total in
    Node.button
      ~attrs:
        [ Attr.class_
            (note_classes
               "mode plan justify-start max-w-[36ch] narrow:max-w-full")
        ; Attr.type_ "button"
        ; Attr.title
            (String.concat
               ~sep:"\n"
               (sprintf "Plan %s (/todos)" count :: Plan_view.lines plan))
        ; Attr.on_click (fun _ -> inject (Action.Run "/todos"))
        ]
      [ Node.span
          ~attrs:[ Attr.class_ "plan-text min-w-0 truncate" ]
          [ span ~cls:"plan-label font-semibold" "Plan:"
          ; Node.text (" " ^ step)
          ]
      ])
;;

let modes (m : App.Model.t) ~inject =
  [ (match Option.bind m.state ~f:(fun s -> s.plan) with
     | Some plan -> plan_chip plan ~inject
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

(* {2 In the composer's row} *)
