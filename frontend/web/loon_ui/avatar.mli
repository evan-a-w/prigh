(** An image element with a fallback for representing the user. Port of
    shadcn/ui's [Avatar] (Radix Avatar: the image shows once it has loaded;
    the fallback until then, and if it fails).

    {[
      Avatar.component
        (return (fun avatar ->
           [ Avatar.image avatar ~src:"https://github.com/shadcn.png" ~alt:"@shadcn" ()
           ; Avatar.fallback avatar [ Node.text "CN" ]
           ]))
        graph
    ]} *)

open! Import

module Size : sig
  type t =
    | Default
    | Sm
    | Lg
  [@@deriving enumerate, sexp_of, compare, equal]

  (** [default], [sm], [lg], as in [data-size]. *)
  val name : t -> string
end

module Status : sig
  (** The image's: Radix's [imageLoadingStatus]. *)
  type t =
    | Loading
    | Loaded
    | Error
  [@@deriving enumerate, sexp_of, compare, equal]
end

(** The avatar's state, for its image and fallback. *)
type t

(** Controlled: [status] is the image's, which it reports with
    [on_status_change] when it loads or fails. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?size:Size.t
  -> status:Status.t
  -> on_status_change:(Status.t -> unit Effect.t)
  -> (t -> Node.t list)
  -> Node.t

(** Keeps the image's status ([Loading] at first). *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?size:Size.t
  -> (t -> Node.t list) Bonsai.t
  -> local_ Bonsai.graph
  -> Node.t Bonsai.t

(** An [<img>]: hidden while it loads, gone if it fails. *)
val image
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?alt:string
  -> t
  -> src:string
  -> unit
  -> Node.t

(** Shown unless the image has loaded (initials, an icon). *)
val fallback : ?attrs:Attr.t list -> ?class_:string -> t -> Node.t list -> Node.t

(** A dot (or a small icon) at the avatar's bottom right, sized by the
    avatar's size: put it among the avatar's children. *)
val badge : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** Overlapping avatars. *)
val group : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t

(** A "+3" at the end of a group, sized as its avatars. *)
val group_count : ?attrs:Attr.t list -> ?class_:string -> Node.t list -> Node.t
