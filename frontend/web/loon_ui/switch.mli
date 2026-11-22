(** A control that allows the user to toggle between checked and not
    checked. Port of shadcn/ui's [Switch] (Radix Switch: a
    [<button role="switch">]).

    {[
      Switch.view ~id:"airplane-mode" ~checked ~on_change:set ()
    ]} *)

open! Import

module Size : sig
  type t =
    | Default
    | Sm
  [@@deriving enumerate, sexp_of, compare, equal]

  (** As in [data-size]: [default], [sm]. *)
  val name : t -> string
end

(** Controlled. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?size:Size.t
  -> ?id:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> checked:bool
  -> on_change:(bool -> unit Effect.t)
  -> unit
  -> Node.t

(** Keeps whether it is on, [default_checked] ([false]) at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?size:Size.t
  -> ?id:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?default_checked:bool
  -> local_ Bonsai.graph
  -> bool Control.t Bonsai.t
