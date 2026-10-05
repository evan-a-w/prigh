open! Core
open! Import

(** A user-defined OpenAI-compatible endpoint (aiproxy, LiteLLM, OpenRouter,
    vLLM, Ollama, ...), kept in [config.json] under [providers]:

    {v
    "providers": {
      "aiproxy": {
        "base_url": "http://localhost:3000/v1",
        "api": "chat",
        "headers": { "X-Team": "infra" },
        "models": [
          { "id": "gpt-4o", "context_window": 128000, "max_output": 16384,
            "thinking": false, "images": true,
            "cost": { "input": 2.5, "output": 10, "cache_read": 1.25 } }
        ]
      }
    }
    v}

    Its API key lives in [auth.json] under the provider's name. *)

module Api : sig
  type t =
    | Chat (** [POST {base_url}/chat/completions] *)
    | Responses (** [POST {base_url}/responses] *)
    | Anthropic (** [POST {base_url}/messages] *)
  [@@deriving sexp, equal, enumerate]

  (** ["chat"], ["responses"], ["anthropic"]: the config's [api]. *)
  val to_string : t -> string

  val of_string : string -> t option
  val label : t -> string
end

(** What the user knows about a model better than the server's model list. *)
module Model_override : sig
  type t =
    { id : string
    ; name : string option
    ; context_window : int option
    ; max_output : int option
    ; thinking : bool option
    ; images : bool option
    ; cost : Model.Cost.t option
    }
  [@@deriving sexp_of]
end

(** An entry of [GET {base_url}/models]. *)
module Listed_model : sig
  type t =
    { id : string
    ; context_window : int option
      (** [context_length] (OpenRouter), [context_window] or [max_model_len]
          (vLLM), when the server says *)
    }
  [@@deriving sexp_of, equal]

  (** The [data] (or [models]) array of a model list response. *)
  val list_of_json : Json.t -> t list Or_error.t
end

type t =
  { name : string
  ; base_url : string (** without a trailing ['/'] *)
  ; api : Api.t
  ; headers : (string * string) list (** sent with every request *)
  ; models : Model_override.t list
  }
[@@deriving sexp_of]

(** Lowercase letters, digits, ['-'] and ['_'], starting with a letter, and
    not a built-in provider (or ["custom"]). The error says what to type. *)
val validate_name : string -> string Or_error.t

(** An http(s) URL with a host; trailing ['/'] and a pasted endpoint path
    ([/chat/completions], [/models], ...) are removed. *)
val validate_base_url : string -> string Or_error.t

(** [<NAME>_API_KEY], with ['-'] as ['_']. *)
val env_var : string -> string

val provider_id : t -> Provider_id.t

(** The model [id] of this provider: overrides, then what the server said,
    then defaults (128k context, 16k output, no thinking, images, unknown
    cost). *)
val model : t -> ?listed:Listed_model.t -> string -> Model.t

(** The configured models (overrides) followed by the listed ones not among
    them, in the server's order. *)
val models : t -> listed:Listed_model.t list -> Model.t list

val of_json : name:string -> Json.t -> (t * string list) Or_error.t
val to_json : t -> Json.t

(** [config.json]'s providers in file order. Entries that cannot be used are
    skipped, and each problem (skipped entries, ignored fields) is described
    with what to fix. *)
val load : home:string -> t list * string list

(** Adds or replaces the entry, keeping everything else in the file. *)
val save : home:string -> t -> unit Or_error.t

val remove : home:string -> string -> unit Or_error.t
