open! Core

(** The composer's chips: each opens its menu over the composer (a popover,
    not the command's dialog), in the order they sit in its row: the model,
    the host, the thinking level, the mode and the transcript's verbosity.
    [Attach] is the composer's + button. *)

type t =
  | Model
  | Host
  | Thinking
  | Mode
  | Verbosity
  | Attach
[@@deriving sexp_of, equal, enumerate]

(** The chip [delta] places along, wrapping round; [skip] passes over those
    that open nothing (a model without thinking levels). *)
val step : t -> int -> skip:(t -> bool) -> t
