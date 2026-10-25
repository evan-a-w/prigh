open! Core

(** [/pr]'s arguments: [list [open|closed|all]] (the default, also [/pr
    open] etc.), [view N] (also [N], [#N]), [new [title]] and [checkout N]. *)

type t =
  | List of { state : string option } (** [None]: open ones *)
  | View of int
  | New of { title : string option }
  | Checkout of int
[@@deriving sexp_of, equal]

(** [rest] is all that follows [/pr]; errors say the usage. *)
val parse : string -> (t, string) Result.t

(** The subcommands, as [(name, detail)], for completion. *)
val subcommands : (string * string) list
