open! Core
open! Import
open Html
module Action = App.Action

let pending_chip ?(attrs = []) ?title ~cls label =
  button
    ~cls:("chip-button awaiting " ^ cls)
    ~title:(Option.value title ~default:(sprintf "Switching to %s…" label))
    ~disabled:true
    ~attrs:(Attr.create "aria-busy" "true" :: attrs)
    ~on_click:Effect.Ignore
    [ Html.lamp (); span ~cls:"chip-label" label ]
;;

let open_chip (m : App.Model.t) =
  match m.dialog with
  | Some (Popover { chip; picker }) -> Some (chip, picker)
  | _ -> None
;;

let menu_open m chips =
  match open_chip m with
  | Some (chip, _) -> List.mem chips chip ~equal:Chip.equal
  | None -> false
;;

let menu_attrs (m : App.Model.t) (chip : Chip.t) =
  let open_ = menu_open m [ chip ] in
  [ Attr.create "aria-haspopup" "dialog"
  ; Attr.create "aria-expanded" (Bool.to_string open_)
  ; (if open_ then Attr.create "aria-controls" "chip-popover" else Attr.empty)
  ]
;;

let anchor (m : App.Model.t) chip button ~inject =
  let menu =
    match open_chip m with
    | Some (shown, picker) when Chip.equal shown chip ->
      Some (Chip_popover_view.view m ~chip picker ~inject)
    | _ -> None
  in
  div
    ~attrs:
      [ classes
          [ "chip-anchor"
          ; (match (chip : Chip.t) with
             | Model -> "model"
             | Thinking -> "thinking"
             | Mode -> "mode"
             | Host -> "host")
          ]
          [ "open", Option.is_some menu ]
      ]
    [ button; Option.value menu ~default:Node.none ]
;;

(* The level's bars and its word; a model without levels says so, muted. *)
let thinking_chip (m : App.Model.t) (state : State.t) ~inject =
  match m.pending.thinking with
  | Some level ->
    pending_chip
      ~cls:"thinking-chip"
      ~title:(sprintf "Switching to thinking %s…" level)
      level
  | None when state.model.supports_thinking ->
    button
      ~cls:"chip-button thinking-chip"
      ~title:
        (sprintf
           "Thinking %s: how much the model reasons before it answers (Alt+T \
            cycles, /thinking)"
           state.thinking)
      ~label:(sprintf "Thinking %s" state.thinking)
      ~attrs:(menu_attrs m Thinking)
      ~on_click:(inject (Action.Open_chip Thinking))
      [ Chip_popover_view.meter state.thinking
      ; span ~cls:"chip-label" state.thinking
      ]
  | None ->
    button
      ~cls:"chip-button thinking-chip unsupported"
      ~title:(sprintf "%s has no thinking levels" state.model.name)
      ~label:"No thinking"
      ~on_click:(inject (Action.Open_chip Thinking))
      [ Chip_popover_view.meter "off"; span ~cls:"chip-label" "no thinking" ]
;;

(* What asks first, by its short name (ask, auto-edit, auto, plan); the
   shield lit where nothing asks. *)
let mode_chip (m : App.Model.t) (state : State.t) ~inject =
  match m.pending.mode with
  | Some mode ->
    pending_chip
      ~cls:"mode-chip"
      ~title:(sprintf "Switching to %s…" (Session_mode.label mode))
      (Session_mode.label mode)
  | None ->
    let mode = Session_mode.of_state state in
    let never_asks =
      (not mode.plan) && Approval_mode.equal mode.approval Auto
    in
    button
      ~cls:
        (String.concat
           ~sep:" "
           ([ "chip-button"; "mode-chip" ]
            @ (if mode.plan then [ "plan-mode" ] else [])
            @ if never_asks then [ "never-asks" ] else []))
      ~title:
        (sprintf
           "%s (Alt+M cycles, /mode picks)"
           (String.capitalize (Session_mode.describe mode)))
      ~label:(Session_mode.title mode)
      ~attrs:(menu_attrs m Mode)
      ~on_click:(inject (Action.Open_chip Mode))
      [ icon Shield; span ~cls:"chip-label" (Session_mode.label mode) ]
;;

let view (m : App.Model.t) ~inject =
  match m.state with
  | None -> Node.none
  | Some state ->
    Node.div
      ~attrs:
        [ classes
            [ "session-chips" ]
            [ "menu-open", menu_open m [ Thinking; Mode ] ]
        ; Attr.role "group"
        ; Attr.create "aria-label" "Thinking and mode"
        ]
      [ anchor m Thinking (thinking_chip m state ~inject) ~inject
      ; anchor m Mode (mode_chip m state ~inject) ~inject
      ]
;;
