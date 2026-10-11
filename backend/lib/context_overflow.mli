open! Core

(** Whether a provider error says the request did not fit the model's context
    window (Anthropic's "prompt is too long", OpenAI's
    "context_length_exceeded", and the wordings of OpenAI-compatible
    servers), as opposed to a rate limit or another failure. The agent loop
    compacts the conversation and retries such a request. *)
val matches : string -> bool
