open! Core
open! Import

(** Speech to text for dictation: the audio a frontend recorded goes to the
    [dictation_model]'s OpenAI-compatible [POST {base}/audio/transcriptions]
    as multipart/form-data, and the text comes back. [openai/...] uses the
    OpenAI API key (stored, else [OPENAI_API_KEY]); [NAME/...] a custom
    provider's base URL, headers and key (none: unauthorised, for local
    servers). Other providers cannot transcribe. *)

(** The HTTP this needs, as a value so that tests answer without a network.
    The result is the status and body. *)
module Http : sig
  type t =
    ?cancel:Cancellation.t
    -> url:string
    -> headers:(string * string) list
    -> body:string
    -> unit
    -> (int * string) Or_error.t

  (** {!Http_client.post}, with a two minute timeout. *)
  val real : env:Env.t -> t
end

module Audio : sig
  type t =
    { mime_type : string (** parameters such as [;codecs=opus] are ignored *)
    ; data : string (** the raw bytes *)
    }

  (** 25 MB, the API's limit. *)
  val max_bytes : int

  (** Whether a recording of this many bytes can be sent: not empty, and
      at most {!max_bytes}. *)
  val check_size : int -> unit Or_error.t

  (** The file name the upload is given, whose extension tells the API the
      format: [audio/webm] is [speech.webm], [audio/ogg] [speech.ogg],
      [audio/mp4] [speech.m4a], [audio/mpeg] [speech.mp3], [audio/wav]
      [speech.wav] (and their common aliases). *)
  val filename : mime_type:string -> string Or_error.t
end

(** Where a model's audio goes. *)
module Target : sig
  type t =
    { url : string
    ; headers : (string * string) list
    ; model : string (** the model's id, without the provider *)
    }
  [@@deriving sexp_of]
end

(** Whether [model] ([provider/id]) names a provider that can transcribe,
    without looking at credentials: [openai] or one of [providers] (custom,
    with the chat or responses API). The error says what to pick instead. *)
val check_model : providers:Custom_provider.t list -> string -> unit Or_error.t

(** {!check_model}, then the credentials. Without an OpenAI key the error
    starts with ["No transcription provider:"]. *)
val target
  :  env:Env.t
  -> ?getenv:(string -> string option)
  -> store:Auth_store.t
  -> providers:Custom_provider.t list
  -> string
  -> Target.t Or_error.t

(** The request's body: the fields [model], [response_format=json] and
    [file], separated by [boundary]. *)
val multipart_body
  :  boundary:string
  -> model:string
  -> filename:string
  -> Audio.t
  -> string

(** The audio's text. [boundary] is random by default. *)
val transcribe
  :  env:Env.t
  -> ?getenv:(string -> string option)
  -> ?cancel:Cancellation.t
  -> ?boundary:string
  -> http:Http.t
  -> store:Auth_store.t
  -> providers:Custom_provider.t list
  -> model:string
  -> Audio.t
  -> string Or_error.t
