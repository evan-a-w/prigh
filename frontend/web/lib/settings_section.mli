open! Core

(** The settings page's sections, listed down its side: one shows at a time. *)

type t =
  | Appearance
  | Behaviour
  | Models
  | Git_hosting
  | Nix_store (** superusers only *)
  | Account
[@@deriving sexp_of, equal, enumerate]

(** The sections [superuser] (or not) sees, in order. *)
val available : superuser:bool -> t list

(** [t], or the first section when [superuser] cannot see it. *)
val visible : t -> superuser:bool -> t

(** The section's title. *)
val name : t -> string

(** A line on what is in it and where it is kept. *)
val about : t -> string

(** Its [/settings] argument, e.g. [git]. *)
val arg : t -> string

(** [/settings ARG]: by its argument or name, or a prefix of either. *)
val of_arg : string -> t option

(** Its tab's DOM id. *)
val tab_id : t -> string

module Move : sig
  (** The keys on the section list: ↑ ← [Prev], ↓ → [Next] (both wrap
      around), Home [First], End [Last]. *)
  type t =
    | Prev
    | Next
    | First
    | Last
  [@@deriving sexp_of, equal]
end

val move : t -> Move.t -> superuser:bool -> t
