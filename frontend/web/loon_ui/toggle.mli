(** A two-state button that can be either on or off. Port of shadcn/ui's
    [Toggle] (Radix Toggle: a [<button aria-pressed>] with
    [data-state="on|off"]).

    {[
      Toggle.view ~attrs:[ Attr.create "aria-label" "Toggle bold" ] ~pressed
        ~on_change:set [ Icon.view Bold ]
    ]} *)

open! Import

module Variant : sig
  type t =
    | Default
    | Outline
  [@@deriving enumerate, sexp_of, compare, equal]

  (** shadcn's name: [default], [outline]. *)
  val name : t -> string
end

module Size : sig
  type t =
    | Default
    | Sm
    | Lg
  [@@deriving enumerate, sexp_of, compare, equal]

  (** shadcn's name: [default], [sm], [lg]. *)
  val name : t -> string
end

(** shadcn's [toggleVariants]: the classes of a toggle, then [class_]. *)
val classes : ?variant:Variant.t -> ?size:Size.t -> ?class_:string -> unit -> string

(** Controlled. Give an icon-only toggle an [aria-label] in [attrs]. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Variant.t
  -> ?size:Size.t
  -> ?disabled:bool
  -> pressed:bool
  -> on_change:(bool -> unit Effect.t)
  -> Node.t list
  -> Node.t

(** Keeps whether it is pressed, [default_pressed] ([false]) at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?variant:Variant.t
  -> ?size:Size.t
  -> ?disabled:bool
  -> ?default_pressed:bool
  -> Node.t list
  -> local_ Bonsai.graph
  -> bool Control.t Bonsai.t
