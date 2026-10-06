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

(** How a model takes a thinking setting (Anthropic): a token budget, or
    adaptive thinking with an effort level. [can_disable] is false for models
    that cannot turn thinking off. *)
module Thinking_style : sig
  type t =
    | Budget
    | Adaptive of { can_disable : bool }
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; provider : Provider_id.t
  ; name : string
  ; context_window : int
  ; max_output : int
  ; supports_thinking : bool
  ; thinking_style : Thinking_style.t
  ; cost : Cost.t
  ; supports_images : bool
    (** otherwise each image is replaced by a note saying it was left out *)
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
