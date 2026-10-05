open! Core
open! Import

(** OpenAI-compatible chat completions ([POST .../chat/completions], SSE):
    DeepSeek and custom providers with [api = chat] (aiproxy, LiteLLM,
    OpenRouter, vLLM, Ollama, ...).

    Images go in user messages as [image_url] data-URL parts. A tool
    message's content is text only, so a tool result's images are sent in a
    user message after the run of tool messages, and the result's text says
    so. Models without [supports_images] get a note per image instead.
    Thinking arrives as [reasoning_content] or [reasoning] deltas; tool calls
    are accumulated by [index] (tolerating servers that repeat the id and
    name in every delta, or omit the index or the id). *)

(** How a server takes the thinking level. *)
module Thinking_param : sig
  type t =
    | Deepseek (** [thinking: {type: enabled|disabled}] plus [reasoning_effort] *)
    | Reasoning_effort (** OpenAI's [reasoning_effort] (low/medium/high) *)
  [@@deriving sexp_of]
end

module Quirks : sig
  type t =
    { thinking_param : Thinking_param.t
    ; replay_reasoning : bool
      (** send earlier thinking back as [reasoning_content] (DeepSeek needs
          it for tool-call turns) *)
    }
  [@@deriving sexp_of]

  val deepseek : t
  val generic : t
end

(** [url] is the full chat-completions URL; [headers] carry the
    authorisation and any extra headers. *)
val create
  :  env:Env.t
  -> ?timeout:Time_ns.Span.t
  -> name:string
  -> url:string
  -> headers:(string * string) list
  -> quirks:Quirks.t
  -> unit
  -> Provider.t

module For_testing : sig
  val request_body : quirks:Quirks.t -> Provider.Request.t -> Json.t

  module Chunk : sig
    type t =
      { events : Assistant_event.t list
      ; finish_reason : string option
      ; usage : Usage.t option
      }
    [@@deriving sexp_of]
  end

  val parse_chunk : Json.t -> Chunk.t Or_error.t

  (** Chunks of one stream, sharing the tool-call bookkeeping. *)
  val parse_stream : Json.t list -> Chunk.t Or_error.t list
end
