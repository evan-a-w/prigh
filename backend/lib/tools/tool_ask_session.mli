open! Core
open! Import

(** [ask_session {session, question}]: the model asks another saved session
    of its namespace a question ({!Session_questions.ask}). The call returns
    at once; that session's own agent gets the question (in a run started
    for it, or at the next step of the run it is in), works with its tools
    and answers with [answer_session] ({!Tool_answer_session}), and the
    answer arrives later as a message. Only the main agent has it: that is
    where answers arrive. *)

val name : string

val create
  :  sessions_dir:(unit -> string)
  -> current:(unit -> Session_questions.Party.t option)
       (** the calling session *)
  -> questions:Session_questions.t
  -> unit
  -> Tool.t
