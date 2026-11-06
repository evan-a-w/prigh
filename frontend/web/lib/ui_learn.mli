open! Core
open! Import

(** The components that work in the page alone: a [calculator] (outputs
    recomputed as the inputs change, {!Ui_expr}), [flashcards] (flip,
    previous, next) and a [quiz] (graded as each question is answered, the
    score kept). *)

val calculator : Ui_context.t -> path:string -> Ui_spec.Calculator.t -> Node.t

val flashcards
  :  Ui_context.t
  -> path:string
  -> title:string option
  -> (string * string) list
  -> Node.t

val quiz
  :  Ui_context.t
  -> path:string
  -> title:string option
  -> Ui_spec.Question.t list
  -> Node.t

(** The calculator's outputs for its inputs' current values: each output's
    label and text, or why it has none. *)
val results
  :  Ui_spec.Calculator.t
  -> value:(string -> string option)
  -> (string * (string, string) Result.t) list
