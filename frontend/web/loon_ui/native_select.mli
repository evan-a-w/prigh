(** A styled native HTML select element with consistent design system
    integration. Port of shadcn/ui's [NativeSelect]: the browser's own
    [<select>] (the best choice on phones, whose pickers are native) with a
    chevron.

    {[
      Native_select.view ~value ~on_change:set
        [ Native_select.option ~value:"" [ Node.text "Select status" ]
        ; Native_select.option ~value:"todo" [ Node.text "Todo" ]
        ]
    ]} *)

open! Import

module Size : sig
  type t =
    | Default
    | Sm
  [@@deriving enumerate, sexp_of, compare, equal]

  val name : t -> string
end

(** The [<select>] in its wrapper ([class_] styles the wrapper, [attrs] go
    on the select, as shadcn's). With [value] it is controlled (the option
    of that value is selected; give [on_change] too); without, the browser
    keeps the choice. *)
val view
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?size:Size.t
  -> ?id:string
  -> ?name:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?value:string
  -> ?on_change:(string -> unit Effect.t)
  -> Node.t list
  -> Node.t

(** Keeps the chosen value, [default_value] (or the first option's, [""])
    at first. *)
val component
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?size:Size.t
  -> ?id:string
  -> ?name:string
  -> ?disabled:bool
  -> ?required:bool
  -> ?invalid:bool
  -> ?default_value:string
  -> Node.t list
  -> local_ Bonsai.graph
  -> string Control.t Bonsai.t

(** An [<option>]. *)
val option
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> value:string
  -> Node.t list
  -> Node.t

(** An [<optgroup>] of options under [label]. *)
val opt_group
  :  ?attrs:Attr.t list
  -> ?class_:string
  -> ?disabled:bool
  -> label:string
  -> Node.t list
  -> Node.t
