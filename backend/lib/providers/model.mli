open! Core

(** Per-million-token prices in USD. *)
module Cost : sig
  type t =
    { input : float
    ; output : float
    ; cache_read : float
    }
  [@@deriving sexp_of]
end

(** How a provider is told the thinking setting. *)
module Thinking_style : sig
  type t =
    | Budget of { effort : bool }
    (** Anthropic's extended thinking: [thinking.enabled] with a token
        budget per level, and with [effort] (Claude Opus 4.5) the level in
        [output_config.effort] too *)
    | Adaptive
    (** Anthropic's adaptive thinking with [output_config.effort]; off is
        [thinking.disabled] *)
    | Adaptive_between_tools
    (** adaptive, but off is [thinking.between_tools] (Claude Sonnet 5.5) *)
    | Effort of { off_is_none : bool }
    (** an OpenAI-style reasoning effort (DeepSeek's provider has its own
        parameter); off is the effort ["none"] when [off_is_none], else no
        effort is sent *)
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; provider : Provider_id.t
  ; name : string
  ; context_window : int
  ; max_output : int
  ; thinking_levels : Thinking.Level.t list
    (** the effort levels it takes, lowest first; none: it does not think *)
  ; can_disable_thinking : bool
  ; thinking_default : Thinking.t option
    (** what the provider does when it is sent no effort (the provider's
        default), when known *)
  ; thinking_style : Thinking_style.t
  ; cost : Cost.t
  ; supports_images : bool
    (** otherwise each image is replaced by a note saying it was left out *)
  ; native_computer : Native_computer.t option [@sexp.option]
    (** its built-in computer-use tool, if it has one *)
  }
[@@deriving sexp_of]

(** The built-in models. Custom providers' models come from
    {!Model_registry}. *)
val all : t list

val default : t

(** [None] for custom providers. *)
val default_for : Provider_id.t -> t option

(** ["provider/id"]; the same id can exist under several providers. *)
val key : t -> string

(** The provider name of a [key] (everything before the first ['/']). *)
val key_provider : string -> string option

(** Accepts a [key] or a bare id (the first model that has it). Ids may
    contain ['/']: the part before the first ['/'] is only taken as a
    provider name when a model of that provider has the rest as its id. *)
val find_in : t list -> string -> t option

val find : string -> t option

(** The (at most three) models closest to [query], by key, id or name; only
    the named provider's when [query] starts with one. *)
val closest_in : t list -> string -> t list

(** What a user typed: [find], then case-insensitive display name, then a
    unique case-insensitive prefix of the key, id or name. Failures explain
    themselves ("did you mean: ..." ranked by edit distance, or the
    ambiguous candidates). *)
val resolve_in : t list -> string -> t Or_error.t

val resolve : string -> t Or_error.t

(** Whether an assistant message's [model] (a key, or a bare id in old
    sessions) is the provider's, so that its opaque thinking signatures can
    be replayed to it. *)
val written_by : string -> Provider_id.t -> bool

val cost_usd : t -> Usage.t -> float

(** Whether the provider checks that what came before each thinking block
    sent back is what it was when the block was written, and refuses the
    request (or drops the block) otherwise (Anthropic's preserved thinking,
    Claude 5.1 and later). *)
val binds_thinking_to_prefix : t -> bool

val supports_thinking : t -> bool

(** The thinking settings it takes, in cycling order: off (when it can be
    disabled), the provider's default, then its levels. Just [Off] for a
    model that does not think. *)
val thinking_options : t -> Thinking.t list

(** {!Thinking.clamp} to its options: what a request to it sends, and what a
    session's setting becomes when it switches to it. *)
val clamp_thinking : t -> Thinking.t -> Thinking.t

(** The setting when it is one of its options, else an error listing them. *)
val check_thinking : t -> Thinking.t -> Thinking.t Or_error.t
