(** Displays a callout for user attention. Port of shadcn/ui's [Alert].

    {[
      Alert.root
        [ Icon.view Circle_check
        ; Alert.title [ Node.text "Success! Your changes have been saved" ]
        ; Alert.description [ Node.text "This is an alert with icon, title and description." ]
        ]
    ]} *)

open! Import

module Variant : sig
  type t =
    | Default
    | Destructive
  [@@deriving enumerate, sexp_of, compare, equal]

  (** shadcn's name: [default], [destructive]. *)
  val name : t -> string
end

(** shadcn's [alertVariants]. *)
val classes : ?variant:Variant.t -> ?class_:string -> unit -> string

(** A [<div role="alert">]; an icon ([Icon.view]) as its first child sits
    beside the title and description. *)
val root
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Variant.t
  -> Node.t list
  -> Node.t

val title : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val description : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** An action (a button) at the top right of the alert. *)
val action : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
