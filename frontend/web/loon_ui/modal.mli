(** What Dialog, Alert_dialog, Sheet and Drawer share: Radix's Dialog
    primitive. The trigger and close buttons, the ids that link the content
    to its title and description, and the content itself: an overlay over
    the page and the content, which takes the focus (its first focusable
    element, or [initial_focus]), keeps it (Tab cycles inside), closes on
    Esc (and a pointer down outside it, with [dismissible]), gives the focus
    back when it closes, stops the page scrolling and ignoring the pointer
    while open, and plays its exit animation. *)

open! Import

type t =
  { id : string
  ; trigger_id : string
  ; open_ : bool
  ; on_open_change : bool -> unit Effect.t
  }

val trigger_id : t -> string
val content_id : t -> string
val title_id : t -> string
val description_id : t -> string
val state_attr : t -> Attr.t

(** Opens and closes it: [Bonsai.state]'s value, made into a [t] by
    [make]. *)
val component
  :  ?default_open:bool
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

val view
  :  ?id:string
  -> ?trigger_id:string
  -> open_:bool
  -> on_open_change:(bool -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** [slot] is the [data-slot], e.g. ["dialog-trigger"]. *)
val trigger : slot:string -> ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** An element that closes it (Radix's [Close asChild]). *)
val close : ?slot:string -> ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** Nothing while closed. [content_attrs] are the content's (its slot and
    classes, the caller's attributes), [role] ["dialog"] or
    ["alertdialog"]. Without [auto_focus] (default true) opening leaves the
    focus where it is, as Vaul's drawer. *)
val content
  :  ?dismissible:bool
  -> ?auto_focus:bool
  -> ?initial_focus:string
  -> ?role:string
  -> overlay:Node.t
  -> content_attrs:Attr.t list
  -> t
  -> Node.t list
  -> Node.t

(** The overlay: a [<div>] with the state, for [content]. *)
val overlay : slot:string -> class_:string -> t -> Node.t

val title
  :  slot:string
  -> class_:string
  -> ?attrs:Attr.t list
  -> t
  -> Node.t list
  -> Node.t

val description
  :  slot:string
  -> class_:string
  -> ?attrs:Attr.t list
  -> t
  -> Node.t list
  -> Node.t
