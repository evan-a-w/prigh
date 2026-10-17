open! Core
open! Import

(** Answering an [ask_user] call, one question at a time: the cursor is on
    one of the question's choices or on the last row, the user's own
    answer. Both frontends drive it with keys. *)

type t [@@deriving sexp_of, equal]

(** [deadline]: when the agent stops waiting for the answers. *)
val create : ?deadline:Time_ns.t -> P.Question.t list -> t

val questions : t -> P.Question.t list
val deadline : t -> Time_ns.t option

(** When the agent carries on without the answers, in local time: the
    dialog shows it. *)
val deadline_note : t -> utc_offset:Time_ns.Span.t -> string option

(** The question shown, from 0. *)
val current : t -> int

(** The row of the current question: a choice's index, or the number of
    choices for the own-answer row. *)
val cursor : t -> int

val on_own_answer : t -> bool
val is_picked : t -> question:int -> choice:int -> bool
val own_answer : t -> question:int -> string

(** Moves between rows of the current question. *)
val up : t -> t

val down : t -> t

(** To the previous / next question. *)
val prev : t -> t

val next : t -> t
val go_to : t -> row:int -> t

(** Space: toggles the choice (several may be picked) or picks it (only one);
    on the own-answer row, types a space. *)
val toggle : t -> t

(** Typing goes to the own-answer row. *)
val insert : t -> string -> t

val backspace : t -> t

(** The own answer replaced (a text field's whole value); the cursor goes to
    its row. *)
val set_own : t -> string -> t

(** Enter: a single-choice question takes the choice under the cursor (or,
    on the own-answer row, the text instead); then the next question, or
    [`Done] with the answers after the last one. *)
val accept : t -> [ `Next of t | `Done of string list list ]

(** Per question: the picked choices' labels, then the own answer if any. *)
val answers : t -> string list list

(** Whether a question has an answer yet. *)
val answered : t -> question:int -> bool
