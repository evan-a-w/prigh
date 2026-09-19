open! Core
open! Import

(** Streaming HTTP POST over cohttp-eio, with TLS via ocaml-tls. *)

module Response : sig
  type t =
    { status : int
    ; headers : (string * string) list
    }
  [@@deriving sexp_of]

  val header : t -> string -> string option
end

module Error : sig
  type t =
    | Connection_failed of string
    | Cancelled
    | Timed_out
  [@@deriving sexp_of]

  val to_string : t -> string
end

(** [on_response] is called once headers arrive, before any [on_chunk]. *)
val post_stream
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?timeout:Time_ns.Span.t
  -> ?on_response:(Response.t -> unit)
  -> url:string
  -> headers:(string * string) list
  -> body:string
  -> on_chunk:(string -> unit)
  -> unit
  -> (Response.t, Error.t) Result.t

val post
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?timeout:Time_ns.Span.t
  -> url:string
  -> headers:(string * string) list
  -> body:string
  -> unit
  -> (Response.t * string, Error.t) Result.t
