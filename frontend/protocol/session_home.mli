open! Core

(** Where a worktree switch took a session from ([list_sessions]' and
    [get_state]'s [home]): [/worktree back] goes there. *)

type t =
  { cwd : string
  ; host : string option (** the tool host's id, when recorded *)
  ; host_name : string option
  ; repo : Repo.t option (** where [cwd] is in git, when known *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** The [home] field of [j]; [None] when absent or [null]. *)
val field : Json.t -> t option Or_error.t

(** The branch of its checkout, else [tilde cwd]: ["main"],
    ["~/projects/prigh"]. *)
val label : t -> tilde:(string -> string) -> string
