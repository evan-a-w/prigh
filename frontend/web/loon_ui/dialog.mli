(** A window overlaid on either the primary window or another dialog
    window, rendering the content underneath inert. Port of shadcn/ui's
    [Dialog] (Radix Dialog: focus moves into the content and stays there
    while it is open, Esc or a click outside closes it, focus returns to
    the trigger, the page does not scroll).

    {[
      Dialog.component
        (return (fun dialog ->
          [ Dialog.trigger dialog (Button.view ~variant:Outline [ Node.text "Open" ])
          ; Dialog.content dialog
              [ Dialog.header
                  [ Dialog.title dialog [ Node.text "Edit profile" ]
                  ; Dialog.description dialog [ Node.text "..." ]
                  ]
              ; Dialog.footer
                  [ Dialog.close dialog (Button.view ~variant:Outline [ Node.text "Cancel" ]) ]
              ]
          ]))
        graph
    ]} *)

open! Import

(** The dialog's state, for its parts. *)
type t = Modal.t

(** Controlled. [id] (unique on the page) prefixes the parts' ids. *)
val view
  :  ?id:string
  -> open_:bool
  -> on_open_change:(bool -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps whether it is open ([default_open], [false] by default). *)
val component
  :  ?default_open:bool
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** The element that opens it, with the trigger's attributes (Radix's
    [<DialogTrigger asChild>]). *)
val trigger : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** An element that closes it (Radix's [<DialogClose asChild>]). *)
val close : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** The overlay and the dialog, while open. [show_close_button] (default
    [true]): the X in the corner. [initial_focus]: a selector for the
    element to focus when it opens (default: the first focusable one). *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?show_close_button:bool
  -> ?initial_focus:string
  -> t
  -> Node.t list
  -> Node.t

val header : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** [~show_close_button:dialog] adds an outline "Close" button that closes
    [dialog] (shadcn's [showCloseButton]). *)
val footer
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?show_close_button:t
  -> Node.t list
  -> Node.t

(** The dialog's accessible name. *)
val title : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

(** Its accessible description. *)
val description : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t
