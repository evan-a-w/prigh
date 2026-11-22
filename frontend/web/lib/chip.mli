open! Core

(** The composer's chips: each opens its menu over the composer (a popover,
    not the command's dialog), in the order they sit in its row: the host
    and the model (whose menu has the reasoning level and the mode too).
    [Attach] is the composer's paperclip. (The verbosity has none: Ctrl+O,
    /verbosity, the control centre.) *)

type t =
  | Host
  | Model
  | Attach
[@@deriving sexp_of, equal, enumerate]

(** The chip [delta] places along, wrapping round; [skip] passes over
    those not to go to. *)
val step : t -> int -> skip:(t -> bool) -> t
