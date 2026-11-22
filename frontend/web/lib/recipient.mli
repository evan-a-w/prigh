open! Core
open! Import

(** Who the composer sends to: the main agent, or the background subagent
    whose chat is on show ([steer_agent]: a steer or follow-up while it runs;
    once it has finished, it carries on with the message). *)

type t =
  | Main
  | Agent of string (** its id *)
[@@deriving sexp_of, equal]

(** Why the subagent [id] takes no messages (nested, unknown), saying what
    to do instead; [None] when it does. *)
val refusal : Agents.t -> string -> string option
