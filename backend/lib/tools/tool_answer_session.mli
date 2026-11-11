open! Core
open! Import

(** [answer_session {id, answer}]: the model's answer to a question another
    session asked this one ({!Session_questions.answer}), which arrived as a
    message naming its [id]. Only the main agent has it. It is always
    offered rather than only while a question is open: a question may arrive
    in the middle of a run, whose tools are set, and a tool that comes and
    goes costs the session its prompt cache each time. *)

val name : string

val create
  :  current:(unit -> Session_questions.Party.t option)
  -> questions:Session_questions.t
  -> unit
  -> Tool.t
