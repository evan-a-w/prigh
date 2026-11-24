open! Core
open! Import

(** What ran on a user's logins, per login, user and day (UTC), in their
    [<home>/.prigh/account-usage.json] (0600, the last 90 days): their own
    requests (under their name; {!single_user} without [-tokens]) and those
    of the users they share a login with. The owner sees each user's, a user
    only their own. Several processes may write it. *)

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

(** Today (UTC), and the last 7 and 30 days, today included. *)
module Periods : sig
  type t =
    { today : Totals.t
    ; last_7_days : Totals.t
    ; last_30_days : Totals.t
    }
  [@@deriving sexp_of, equal]

  val zero : t
  val add : t -> t -> t

  (** [{today, last_7_days, last_30_days}], each [{requests, input, output,
      cache_read, cost_usd}]. *)
  val to_json : t -> Json.t
end

module Row : sig
  type t =
    { user : string
    ; usage : Periods.t
    }
  [@@deriving sexp_of]
end

type t

(** [<home>/.prigh/account-usage.json]; read on first use. *)
val create : home:string -> t

(** A login's key: its ref, ["openai-codex/work"]. *)
val login : Provider_id.t -> Account_id.t -> string

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

(** The rows' sum. *)
val usage : ?now:Time_ns.t -> ?user:string -> t -> login:string -> Periods.t

(** Whose requests a server without users ([prigh serve] without
    [-tokens]) records: [default], as that home's user is called with
    [-tokens]. *)
val single_user : string

(** Forgets a login's usage (it was logged out). *)
val forget : t -> login:string -> unit
