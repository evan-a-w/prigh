open! Core
open! Import

(** [/agents]' arguments: [/agents] picks a subagent to focus,
    [/agents cancel N], [/agents steer N TEXT] and [/agents follow-up N TEXT]
    (N: the number [/agents] shows, or the id). *)

module Action : sig
  type t =
    | Pick
    | Cancel of string
    | Message of
        { agent : string
        ; kind : Queued_message.Kind.t
        ; text : string
        }
    | Error of string
  [@@deriving sexp_of, equal]
end

val run : string -> Action.t

(** The agent [arg] names: its number in [agents] (from 1) or its id. *)
val find : Agent_view.t list -> string -> Agent_view.t option

(** What to do when [arg] names none of [agents]. *)
val unknown : Agent_view.t list -> string -> string

(** What [rest] (all that follows [/agents ], without leading blanks)
    completes: the offset in [rest] of the word being completed, and the
    candidates (unranked) as [(id, detail)]: the subcommands, then the
    agents' numbers. *)
val complete
  :  agents:Agent_view.t list
  -> string
  -> (int * (string * string) list) option
