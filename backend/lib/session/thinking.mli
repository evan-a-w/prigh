open! Core
open! Import

(** How much a model thinks: off, the provider's default, or an effort level.
    Which of these a model takes is {!Model.thinking_options}. *)

module Level : sig
  (** Lowest first; [compare] orders them. *)
  type t =
    | Minimal
    | Low
    | Medium
    | High
    | Xhigh
    | Max
  [@@deriving sexp, jsonaf, equal, compare, enumerate]

  (** ["minimal"], ["low"], ["medium"], ["high"], ["xhigh"], ["max"]: also
      the providers' wire values. *)
  val to_string : t -> string

  val of_string : string -> t option
end

type t =
  | Off
  | On of Level.t option
  (** [On None] is the provider's default: thinking on, no effort sent. *)
[@@deriving sexp, jsonaf, equal]

(** [On None] *)
val default : t

(** The token budget for budget-style thinking (Anthropic's older models),
    before it is capped to fit the request's [max_tokens]. *)
val budget_tokens : t -> int option

(** ["off"], ["default"] or the level: what commands and the RPC take. *)
val to_string : t -> string

(** What {!to_string} gives, and the aliases ["on"] (the name of
    ["default"] before levels were per model) and ["none"] (OpenAI's name for
    off). *)
val of_string : string -> t Or_error.t

(** What a person reads: ["provider's default"] for [On None]. *)
val label : t -> string

(** The setting closest to [t] among [levels] (and [Off] when [can_disable]):
    [Off] when there are no levels; for [Off] that cannot be, the provider's
    default; for a missing level, the nearest one (the lower on a tie). *)
val clamp : t -> levels:Level.t list -> can_disable:bool -> t
