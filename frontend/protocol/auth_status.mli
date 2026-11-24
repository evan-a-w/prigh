open! Core

module Method : sig
  type t =
    { method_ : string
    ; label : string
    }
  [@@deriving sexp_of, equal]
end

module Configured : sig
  type t =
    { method_ : string
    ; source : string
    }
  [@@deriving sexp_of, equal]
end

(** A custom OpenAI-compatible provider's endpoint. *)
module Custom : sig
  type t =
    { base_url : string
    ; api : string (** chat, responses or anthropic *)
    ; api_label : string
    }
  [@@deriving sexp_of, equal]
end

(** One of the provider's stored logins. *)
module Account : sig
  type t =
    { ref_ : string (** ["<provider>/<id>"], what requests name it by *)
    ; id : string
    ; label : string
    ; method_ : string
    ; source : string
    ; expires_ms : Int64.t option
    ; default : bool
    ; fallback : bool
      (** takes over when another of the provider's accounts hits its usage
        limit *)
    }
  [@@deriving sexp_of, equal]
end

type t =
  { provider : string
  ; name : string
  ; methods : Method.t list
  ; configured : Configured.t option
  ; expires_ms : Int64.t option
  ; custom : Custom.t option
  ; accounts : Account.t list
    (** in fallback order; [configured] and [expires_ms] are the default's *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
