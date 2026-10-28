open! Core
open! Import

(** [ask_user]'s questions for an ACP client: as an elicitation form
    ([elicitation/create], mode [form]) when the client has forms, else, for
    a single question with one choice to pick, as the options of a
    permission request. *)

(** The questions of an [ask_user] event ([to_json]'s form). *)
val of_event : Json.t -> User_question.t list Or_error.t

(** The form's message: the question, or the headers of several. *)
val message : User_question.t list -> string

(** One property per question ([q1], [q2], ...): a string, a string from
    the choices ([oneOf]) or an array of them ([multiple]), plus an optional
    [qN_other] for an answer of the user's own next to choices. *)
val schema : User_question.t list -> Json.t

(** The answers in an accepted form's [content], per question: the picked
    choices, then the user's own text. *)
val answers : User_question.t list -> Json.t -> string list list

(** The choices of a single question that takes one: what a permission
    request can offer. *)
val single_choice : User_question.t list -> User_question.Choice.t list option
