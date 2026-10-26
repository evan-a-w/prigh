open! Core
open! Import

(** The HTTP the forge code needs, as a value so that tests can answer
    without a network. Errors never contain request headers (tokens). *)

module Meth : sig
  type t =
    | Get
    | Post
  [@@deriving sexp_of, equal]
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
    ; body : string
    }
  [@@deriving sexp_of]
end

type t = ?cancel:Cancellation.t -> Request.t -> Response.t Or_error.t

(** {!Http_client}, with a 30 second timeout. *)
val real : env:Env.t -> t
