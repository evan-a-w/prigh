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
    aborted) or impossible (no one to ask). *)
module Reply : sig
  type t =
    | Answered of string list list
    | Declined
    | Cancelled
    | Unavailable of string
  [@@deriving sexp_of]
end

val max_questions : int

(** [ask_user]'s arguments; errors say what to fix. *)
val of_arguments : Json.t -> t list Or_error.t

val to_json : t -> Json.t

(** Checks answers from a frontend against the questions: one list per
    question; a single-choice question takes at most one choice plus text. *)
val check_answers : t list -> string list list -> unit Or_error.t

(** The tool result the model sees. *)
val result_text : t list -> string list list -> string
