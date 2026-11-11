open! Core
open! Import

(** The questions the sessions of one server (one namespace) ask each other
    ([ask_session]) and their answers ([answer_session]), in memory only: a
    backend that restarts forgets the questions still open.

    A question and its answer reach a session as messages of the harness's
    ({!take_mail}), which its agent gives the model at the next step of a
    run, or in a run started for them ([wake]). A question stays open until
    the asked session answers it or a run that saw it ends, which answers in
    its place ({!run_ended}). *)

module Party : sig
  (** A session: its id and its name, if it has one. *)
  type t =
    { id : string
    ; name : string option
    }
  [@@deriving sexp_of]
end

module Question : sig
  type t =
    { id : string (** [q1], [q2], ...: unique in [t] *)
    ; asker : Party.t
    ; target : Party.t
    ; question : string
    }
  [@@deriving sexp_of]
end

(** How a run of an asked session ended. *)
module Outcome : sig
  type t =
    | Said of string (** its last message *)
    | Silent
    | Aborted
    | Failed of string
  [@@deriving sexp_of]
end

type t

val create : unit -> t

(** How many questions of one session may be open at once. *)
val max_open : int

(** [wake ~session] makes the session with that id take its mail: it is
    loaded if need be, an idle one starts a run, a running one takes it at
    its next step. The server sets it; without it nothing can be asked. *)
val set_wake : t -> (session:string -> unit Or_error.t) -> unit

(** Asks [target], waking it. Fails, with nothing asked, when it cannot be
    woken or [asker] has [max_open] questions open. *)
val ask
  :  t
  -> asker:Party.t
  -> target:Party.t
  -> question:string
  -> Question.t Or_error.t

(** The open questions asked of [session], oldest first. *)
val asked : t -> session:string -> Question.t list

(** [session]'s answer to the open question [id] asked of it, sent to the
    asker (cut when very long), which is woken. The error names the
    questions that are open. *)
val answer : t -> session:string -> id:string -> string -> unit Or_error.t

val has_mail : t -> session:string -> bool

(** The questions and answers that arrived for [session] since the last
    call, as user messages with their {!Injected.Source}. *)
val take_mail : t -> session:string -> Message.t list

(** A run of [session] ended: the questions it was given ({!take_mail}) and
    did not answer are answered with how it ended. *)
val run_ended : t -> session:string -> Outcome.t -> unit
