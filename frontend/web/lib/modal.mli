open! Core
open! Import

(** The one modal component every dialog uses: a backdrop (clicking it closes),
    a titled box with a close button, the body and a row of buttons. The box
    has [id] (default ["dialog"]) so it can take the focus. *)
val view
  :  ?cls:string
  -> ?id:string
  -> ?footer:Node.t list
  -> title:string
  -> on_close:unit Effect.t
  -> Node.t list
  -> Node.t
