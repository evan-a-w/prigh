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
             | Host -> "host"
             | Attach -> "attach-menu")
          ]
          [ "open", Option.is_some menu ]
      ]
    [ button; Option.value menu ~default:Node.none ]
;;
