open! Core
open! Import

(** DeepSeek: {!Openai_chat} with DeepSeek's quirks (its [thinking]
    parameter, [reasoning_content] replayed, no images). *)

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

  module Chunk = Openai_chat.For_testing.Chunk

  val parse_chunk : Json.t -> Chunk.t Or_error.t
end
