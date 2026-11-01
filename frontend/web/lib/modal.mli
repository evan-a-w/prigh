open! Core
open! Import

(** The one modal component every dialog uses: a backdrop (clicking it closes),
    a titled box with a close button, the body and a row of buttons. The box
    has [id] (default ["dialog"]) so it can take the focus. With [back]
    (its label, what it does), a back button before the title: a phone's
    page that is a level down. *)
val view
  :  ?cls:string
  -> ?id:string
  -> ?footer:Node.t list
  -> ?back:string * unit Effect.t
  -> title:string
  -> on_close:unit Effect.t
  -> Node.t list
  -> Node.t
