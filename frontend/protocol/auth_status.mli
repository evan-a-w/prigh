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

(** Another user may use a login (its owner sees them; prigh serve
    -tokens). *)
module Grant : sig
  type t =
    { user : string
    ; since_ms : Int64.t
    }
  [@@deriving sexp_of, equal]
end

(** One of the provider's stored logins, or one another user shares with
    this one ([shared_by]). *)
module Account : sig
  type t =
    { ref_ : string
      (** ["<provider>/<id>"] (["~<owner>/<provider>/<id>"] when shared),
          what requests name it by *)
    ; id : string
    ; label : string
    ; method_ : string
    ; source : string
    ; expires_ms : Int64.t option
    ; default : bool
    ; fallback : bool
      (** takes over when another of the provider's accounts hits its usage
        limit *)
    ; shareable : bool
      (** its owner can share it: there are other users and sharing is on *)
    ; shared_with : Grant.t list (** who its owner shares it with *)
    ; shared_by : string option
      (** the user sharing it with this one: its ref is
          [~OWNER/PROVIDER/ID], its id [~OWNER/ID] *)
    ; hidden : bool (** a shared login the user hid *)
    ; usage : Login_usage.t option
      (** what ran on it lately ([None]: nothing in 30 days, or an older
          backend); on a shared one, the user's own use *)
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
