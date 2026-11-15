open! Core
open! Import

(** How the agent's steps read in plain words, in the one-line summary of a
    run of them (prigh-web's quiet transcript): what it is doing now, and
    what a run of them did. *)

module Step : sig
  type t =
    | Call of P.Tool_call.t
    | Thinking
    | Report (** a background job's or subagent's report, a reminder *)
end

(** What [step] is doing, capitalised: ["Running a command"],
    ["Editing src/foo.ml"], ["Thinking"]. An [on_host] call reads as the
    call it runs; a tool without a phrase of its own as ["Using NAME"]. *)
val doing : Step.t -> string

(** What the steps did, capitalised, each kind of step counted once where it
    first appears: ["Ran 5 commands, edited 2 files, read 3 files"]. Files
    count once each however often they were touched. Thinking only shows
    when nothing else happened (["Thought"]). *)
val did : Step.t list -> string

(** The tools with a phrase of their own. *)
val known : string list

(** ["<1s"], ["14s"], ["2m 5s"], ["1h 3m"]. *)
val duration : Time_ns.Span.t -> string
