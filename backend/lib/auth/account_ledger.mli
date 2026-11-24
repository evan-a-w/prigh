open! Core
open! Import

(** What other users ran on an owner's shared logins, per user and day (UTC),
    in the owner's [<home>/.prigh/account-usage.json] (0600, the last 90
    days): the owner sees each user's, a user only their own. *)

module Totals : sig
  type t =
    { requests : int
    ; input : int
    ; output : int
    ; cache_read : int
    ; cost_usd : float
    }
  [@@deriving sexp_of, equal]

  val zero : t
end

module Row : sig
  type t =
    { user : string
    ; today : Totals.t
    ; last_30_days : Totals.t
    }
  [@@deriving sexp_of]
end

type t

(** [<home>/.prigh/account-usage.json]; read on first use. *)
val create : home:string -> t

(** [login] is the login's ref (["openai-codex/work"]). Written at once. *)
val record
  :  ?now:Time_ns.t
  -> t
  -> login:string
  -> user:string
  -> Usage.t
  -> cost_usd:float
  -> unit

(** By user name; only [user]'s when given. *)
val rows : ?now:Time_ns.t -> ?user:string -> t -> login:string -> Row.t list

(** Forgets a login's usage (it was logged out). *)
val forget : t -> login:string -> unit
