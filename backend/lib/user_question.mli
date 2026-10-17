open! Core
open! Import

(** What the [ask_user] tool asks: one or more questions, each with optional
    choices (one or several to pick) and room for an answer of the user's
    own. *)

module Choice : sig
  type t =
    { label : string
    ; description : string option
    }
  [@@deriving sexp_of, equal]
end

type t =
  { question : string
  ; header : string option (** a short title for the dialog *)
  ; choices : Choice.t list
  ; multiple : bool (** several choices may be picked *)
  }
[@@deriving sexp_of, equal]

(** How a question ends: answered (per question, the picked choices' labels
    and then any text the user typed), declined, cancelled (the run was
    aborted), unanswered within its timeout (of so many seconds) or
    impossible (no one to ask). *)
module Reply : sig
  type t =
    | Answered of string list list
    | Declined
    | Cancelled
    | Timed_out of float
    | Unavailable of string
  [@@deriving sexp_of]
end

val max_questions : int

(** Bounds of a per-call [timeout_seconds]. *)
val min_timeout_seconds : int

val max_timeout_seconds : int

(** How long a question waits, in seconds ([None]: until answered). [config]
    is the config's {!Config.ask_user_timeout}: off, the model cannot turn it
    on. Otherwise the call's [timeout_seconds] ([requested]) replaces it: 0
    waits until answered, anything else is clamped to
    [min_timeout_seconds, max_timeout_seconds]. *)
val timeout : config:float option -> requested:int option -> float option

(** [45s], [9m 30s], [10m], [1h 05m]. *)
val duration_hum : float -> string

(** [ask_user]'s arguments; errors say what to fix. *)
val of_arguments : Json.t -> t list Or_error.t

val to_json : t -> Json.t

(** Checks answers from a frontend against the questions: one list per
    question; a single-choice question takes at most one choice plus text. *)
val check_answers : t list -> string list list -> unit Or_error.t

(** The tool result the model sees. *)
val result_text : t list -> string list list -> string
