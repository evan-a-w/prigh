open! Core
open! Import

(** The HTTP the forge code needs, as a value so that tests can answer
    without a network. Errors never contain request headers (tokens).
    Redirects are not followed: a 3xx is the response. *)

module Meth : sig
  type t =
    | Get
    | Post
    | Put
    | Patch
    | Delete
  [@@deriving sexp_of, equal]

  val to_string : t -> string
end

module Request : sig
  type t =
    { meth : Meth.t
    ; url : string
    ; headers : (string * string) list
    ; body : string option
    }
  [@@deriving sexp_of]
end

module Response : sig
  type t =
    { status : int
    ; headers : (string * string) list
    ; body : string
    }
  [@@deriving sexp_of]

  (** The first header named [name], whatever its case. *)
  val header : t -> string -> string option
end

type t = ?cancel:Cancellation.t -> Request.t -> Response.t Or_error.t

(** {!Http_client}, with a 30 second timeout. *)
val real : env:Env.t -> t
