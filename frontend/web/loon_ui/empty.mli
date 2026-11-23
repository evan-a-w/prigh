(** Use the Empty component to display an empty state. Port of shadcn/ui's
    [Empty].

    {[
      Empty.root
        [ Empty.header
            [ Empty.media ~variant:Icon [ Icon.view Folder_code ]
            ; Empty.title [ Node.text "No Projects Yet" ]
            ; Empty.description [ Node.text "Get started by creating your first project." ]
            ]
        ; Empty.content [ Button.view [ Node.text "Create Project" ] ]
        ]
    ]} *)

open! Import

module Media_variant : sig
  type t =
    | Default
    | Icon
  [@@deriving enumerate, sexp_of, compare, equal]

  (** [default], [icon], as in [data-variant]. *)
  val name : t -> string
end

val root : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val header : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** An icon, avatar or image above the title; [~variant:Icon] puts an icon
    in a muted square. *)
val media
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Media_variant.t
  -> Node.t list
  -> Node.t

val title : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val description : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
val content : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
