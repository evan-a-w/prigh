open! Core
open! Import

(** HTTP over cohttp-eio, with TLS via ocaml-tls. *)

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
    | Connection_lost of string (** after the response started *)
    | Cancelled
    | Timed_out
    | Stalled of Time_ns.Span.t (** nothing arrived for that long *)
  [@@deriving sexp_of]

  val to_string : t -> string
end

(** Adds the scheme's port when the URI has none, so that connecting never
    needs a service-name lookup. *)
val with_default_port : Uri.t -> Uri.t

(** [on_response] is called once headers arrive, before any [on_chunk].
    [idle_timeout] bounds the wait for the response and then for each piece
    of its body ([Stalled]); [timeout], the whole request. *)
val post_stream
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?timeout:Time_ns.Span.t
  -> ?idle_timeout:Time_ns.Span.t
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

(** A GET whose body goes to [on_chunk] as it arrives; an exception raised
    by [on_chunk] ends the request and propagates. *)
val get_stream
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?timeout:Time_ns.Span.t
  -> ?on_response:(Response.t -> unit)
  -> url:string
  -> headers:(string * string) list
  -> on_chunk:(string -> unit)
  -> unit
  -> (Response.t, Error.t) Result.t

val get
  :  env:Env.t
  -> ?cancel:Cancellation.t
  -> ?timeout:Time_ns.Span.t
  -> url:string
  -> headers:(string * string) list
  -> unit
  -> (Response.t * string, Error.t) Result.t
