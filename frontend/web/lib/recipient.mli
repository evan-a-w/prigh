open! Core
open! Import

(** Who the composer sends to: the main agent, or a running background
    subagent being steered ([steer_agent]). *)

type t =
  | Main
  | Agent of string (** its id *)
[@@deriving sexp_of, equal]

(** Why the subagent [id] takes no messages (finished, nested, unknown),
    saying what to do instead; [None] when it does. *)
val refusal : Agents.t -> string -> string option
