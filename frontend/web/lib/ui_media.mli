open! Core
open! Import

(** [image] and [gallery] components: a host file shown through a link
    [fs_preview] gives (the page asks for one per path as the call shows,
    see {!Ui_state.unasked}); a click opens the page's image viewer, which
    steps through a gallery. *)

val image
  :  Ui_context.t
  -> Ui_spec.Image.t
  -> width:Ui_spec.Image_width.t
  -> Node.t

val gallery
  :  Ui_context.t
  -> title:string option
  -> Ui_spec.Image.t list
  -> Node.t
