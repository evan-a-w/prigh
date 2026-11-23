(** A control that allows the user to toggle between checked and not
    checked. Port of shadcn/ui's [Checkbox] (Radix Checkbox: a
    [<button role="checkbox">]; Space toggles it, Enter does not; a
    [<label for=...>] toggles it too).

    {[
      Checkbox.view ~id:"terms" ~checked:(Checkbox.State.of_bool terms)
        ~on_change:set_terms ()
    ]} *)

open! Import

module State : sig
  type t =
    | Checked
    | Unchecked
    | Indeterminate
  [@@deriving enumerate, sexp_of, compare, equal]

  val of_bool : bool -> t

  (** As in [data-state]: [checked], [unchecked], [indeterminate]. *)
  val name : t -> string
end

(** Controlled. [on_change] gets the new value: [true] from [Unchecked] and
    [Indeterminate]. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> checked:State.t
  -> on_change:(bool -> unit Effect.t)
  -> unit
  -> Node.t

(** Keeps whether it is checked, [default_checked] ([false]) at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?id:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?default_checked:bool
  -> local_ Bonsai.graph
  -> bool Control.t Bonsai.t
