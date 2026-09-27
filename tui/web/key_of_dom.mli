open! Core

(** Browser [keydown] events (the fields we need, so this stays testable without
    a DOM) mapped to the platform-neutral [Key.t]. *)

module Event : sig
  type t =
    { key : string (** [KeyboardEvent.key] *)
    ; code : string (** [KeyboardEvent.code], e.g. ["KeyB"] *)
    ; ctrl : bool
    ; alt : bool
    ; shift : bool
    ; meta : bool
    }
  [@@deriving sexp_of]
end

(** [None] for modifier-only presses, dead keys and combinations left to the
    browser (paste, reload, ...). *)
val key : Event.t -> Prigh_ui.Key.t option

(** Text committed through the hidden textarea's [input] event, for virtual
    keyboards whose [keydown] carries no key (e.g. Android IMEs): one key per
    scalar value, newlines as Enter. *)
val keys_of_text : string -> Prigh_ui.Key.t list
