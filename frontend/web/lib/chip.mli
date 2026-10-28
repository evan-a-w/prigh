open! Core

(** The composer's chips: each opens its menu over the composer (a popover,
    not the command's dialog). *)

type t =
  | Model
  | Thinking
  | Mode
  | Host
[@@deriving sexp_of, equal, enumerate]

(** The chip [delta] places along, wrapping round; [skip] passes over those
    that open nothing (a model without thinking levels). *)
val step : t -> int -> skip:(t -> bool) -> t
