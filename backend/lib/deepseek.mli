open! Core
open! Import

val default_base_url : string

val create
  :  env:Env.t
  -> ?base_url:string
  -> ?timeout:Time_ns.Span.t
  -> api_key:string
  -> unit
  -> Provider.t

(** Exposed for tests. *)
module For_testing : sig
  val request_body : Provider.Request.t -> Json.t

  module Chunk : sig
    type t =
      { events : Assistant_event.t list
      ; finish_reason : string option
      ; usage : Usage.t option
      }
    [@@deriving sexp_of]
  end

  val parse_chunk : Json.t -> Chunk.t Or_error.t
end
