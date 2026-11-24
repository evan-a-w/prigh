open! Core

(** [usage_limits]' reply: how much of their subscription's usage limits the
    user's Claude and ChatGPT logins have used. *)

module Window : sig
  type t =
    { id : string (** the provider's name for it, e.g. [five_hour] *)
    ; label : string (** e.g. ["5-hour session"] *)
    ; used_percent : float (** 0 to 100 *)
    ; resets_at : Time_ns.t option
    }
  [@@deriving sexp_of, equal]
end

module Status : sig
  type t =
    | Not_signed_in
    | Api_key (** an API key: billed per token, no limits *)
    | Limits of
        { plan : string option
        ; windows : Window.t list
        }
    | Failed of string (** says what to do next *)
  [@@deriving sexp_of, equal]
end

(** One stored login's. *)
module Account_usage : sig
  type t =
    { ref_ : string
    ; id : string
    ; label : string
    ; default : bool
    ; status : Status.t
    }
  [@@deriving sexp_of, equal]
end

module Provider_usage : sig
  type t =
    { provider : string (** e.g. [anthropic], for [/login] *)
    ; name : string (** e.g. ["Claude"] *)
    ; status : Status.t (** the default account's *)
    ; accounts : Account_usage.t list (** [[]] from an older backend *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { fetched_at : Time_ns.t
  ; providers : Provider_usage.t list
  }
[@@deriving sexp_of, equal]

(** An unknown [status] is an error naming it. *)
val of_json : Json.t -> t Or_error.t
