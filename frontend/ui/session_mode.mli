open! Core
open! Import

(** What the status lines call the mode: the session's approval mode, or
    [plan] while in plan mode (which keeps the approval mode to return to). *)

type t =
  { approval : P.Approval_mode.t
  ; plan : bool
  }
[@@deriving sexp_of, equal]

val of_state : P.State.t -> t

(** Alt+M: ask → auto-edit → auto → plan → ask. Entering plan mode keeps
    the approval mode; leaving it by cycling goes to [ask]. *)
val cycle : t -> t

(** ["ask"], ["auto-edit"], ["auto"], ["plan"]. *)
val label : t -> string

(** The modes [/mode] offers, in cycle order, as from [current]: the three
    approval modes leave plan mode, [plan] keeps the approval mode. *)
val choices : current:t -> t list

(** What [/mode NAME] means, as from [current]; [None] for an unknown name. *)
val of_name : current:t -> string -> t option

(** What asks first, in a few words for a control: ["Asks: edits &
    commands"], ["Asks: commands"], ["Auto: never asks"], ["Plan mode"]. *)
val title : t -> string

(** How the user leaves plan mode (the model cannot): ["/plan off or
    Alt+M"]. *)
val leave : string

(** One line on what the mode does, for notices and pickers; plan mode's
    says how to leave it ({!leave}). *)
val describe : t -> string

(** [set_mode]'s parameters. *)
val params : t -> (string * P.Json.t) list

val names : string list
