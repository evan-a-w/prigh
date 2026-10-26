open! Core
open! Import

(** One pull request on show ([/pr view N], or opened from the list): its
    details, checks and diff, and checking it out. *)

module Checkout : sig
  type t =
    | Idle
    | Busy
    | Done of Pr_checkout.t
    | Failed of string
  [@@deriving sexp_of, equal]
end

module Status : sig
  type t =
    | Loading
    | Failed of string
    | Loaded of Pr_detail.t
  [@@deriving sexp_of, equal]
end

type t =
  { number : int
  ; status : Status.t
  ; checkout : Checkout.t
  ; back : Pull_requests.t option
    (** the list it was opened from, as it was: Esc goes back to it *)
  }
[@@deriving sexp_of, equal]

val create : ?back:Pull_requests.t -> int -> t

(** The pull request as far as it is known: in full once loaded, else the
    list's entry. *)
val pull_request : t -> Pull_request.t option

(** [GitHub] or [Forgejo], from the repository once known. *)
val forge_label : t -> string
