open! Core
open Bonsai_web
open Bonsai.Let_syntax
open Loon_ui

type t =
  { open_ : string option
  ; set : string option -> unit Effect.t
  }

let component (local_ graph) =
  let open_, set = Bonsai.state None graph in
  let%arr open_ and set in
  { open_; set }
;;

let wrap ?(side = Popper.Side.Bottom) t ~id label trigger =
  Tooltip.view
    ~id:("tip-" ^ id)
    ~open_:([%equal: string option] t.open_ (Some id))
    ~on_open_change:(fun o -> t.set (if o then Some id else None))
    (fun tip ->
      [ Tooltip.trigger tip trigger
      ; Tooltip.content ~side ~side_offset:4. tip [ Vdom.Node.text label ]
      ])
;;
