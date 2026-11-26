(** A modal dialog that interrupts the user with important content and
    expects a response. Port of shadcn/ui's [AlertDialog] (Radix
    AlertDialog: as {!Dialog}, but a click outside does not close it, and
    the Cancel button takes the focus when it opens).

    {[
      Alert_dialog.component
        (return (fun d ->
          [ Alert_dialog.trigger d (Button.view ~variant:Outline [ Node.text "Show Dialog" ])
          ; Alert_dialog.content d
              [ Alert_dialog.header
                  [ Alert_dialog.title d [ Node.text "Are you absolutely sure?" ]
                  ; Alert_dialog.description d [ Node.text "..." ]
                  ]
              ; Alert_dialog.footer
                  [ Alert_dialog.cancel d [ Node.text "Cancel" ]
                  ; Alert_dialog.action d [ Node.text "Continue" ]
                  ]
              ]
          ]))
        graph
    ]} *)

open! Import

(** The dialog's state, for its parts. *)
type t = Modal.t

module Size : sig
  type t =
    | Default
    | Sm
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

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

(** The element that opens it (Radix's [<AlertDialogTrigger asChild>]). *)
val trigger : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** The overlay and the dialog ([role="alertdialog"]), while open. *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?size:Size.t
  -> t
  -> Node.t list
  -> Node.t

val header : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val footer : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val title : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t
val description : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

(** An icon beside or above the title. *)
val media : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** A button (default variant) that runs [on_click] and closes it. *)
val action
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Button.Variant.t
  -> ?size:Button.Size.t
  -> ?on_click:unit Effect.t
  -> t
  -> Node.t list
  -> Node.t

(** A button (outline variant) that closes it. *)
val cancel
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Button.Variant.t
  -> ?size:Button.Size.t
  -> t
  -> Node.t list
  -> Node.t
