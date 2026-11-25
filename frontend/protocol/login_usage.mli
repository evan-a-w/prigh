open! Core

(** What ran on a login, as [auth_status] gives it per account ([usage]) and
    [account_usage] per user. *)

module Totals : sig
  type t =
    { requests : int
    ; input : int (** tokens, cache reads included *)
    ; output : int
    ; cache_read : int
    ; cost_usd : float (** at API prices, also on a subscription *)
    }
  [@@deriving sexp_of, equal]

  val zero : t
end

type t =
  { today : Totals.t (** UTC *)
  ; last_7_days : Totals.t
  ; last_30_days : Totals.t
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** One user's use of a login: [account_usage]'s rows. *)
module User : sig
  type outer := t

  type t =
    { user : string
    ; you : bool (** the asking user's own *)
    ; usage : outer
    }
  [@@deriving sexp_of, equal]

  val list_of_json : Json.t -> t list Or_error.t
end
