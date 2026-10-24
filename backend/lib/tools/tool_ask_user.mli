open! Core
open! Import

(** [ask_user]: the model asks the user one or more questions
    ({!User_question}) through the context's [ask_user] and gets the
    answers as its result, or, when nobody answers within its timeout
    ({!User_question.timeout}), a reply telling it to carry on. Only the main
    agent has it: subagents cannot ask ({!Tools.for_context} drops it), they
    put open questions in their report for the parent to ask. *)

val tool : Tool.t
val name : string
