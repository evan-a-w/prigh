(** Extends the Dialog component to display content that complements the
    main content of the screen. Port of shadcn/ui's [Sheet] (Radix Dialog,
    sliding in from a side of the screen).

    {[
      Sheet.component
        (return (fun sheet ->
          [ Sheet.trigger sheet (Button.view ~variant:Outline [ Node.text "Open" ])
          ; Sheet.content sheet
              [ Sheet.header
                  [ Sheet.title sheet [ Node.text "Edit profile" ]
                  ; Sheet.description sheet [ Node.text "..." ]
                  ]
              ]
          ]))
        graph
    ]} *)

open! Import

(** The sheet's state, for its parts. *)
type t = Modal.t

module Side : sig
  type t =
    | Top
    | Right
    | Bottom
    | Left
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

(** The element that opens it (Radix's [<SheetTrigger asChild>]). *)
val trigger : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** An element that closes it (Radix's [<SheetClose asChild>]). *)
val close : ?attrs:Attr.t list -> t -> Node.t -> Node.t

(** The overlay and the panel on [side] (default [Right]), while open, with
    an X button unless [show_close_button] is [false]. *)
val content
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?side:Side.t
  -> ?show_close_button:bool
  -> t
  -> Node.t list
  -> Node.t

val header : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val footer : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val title : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t
val description : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t
