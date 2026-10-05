open! Core

(** The backend hands finished background subagents and shell jobs to the main
    agent as a user message of reports, each starting
    [\[subagent <id> finished\] <task>] (or [failed]) or
    [\[job <id> <status>\] <command>] (status [exited N], [killed], ...). *)

module Section : sig
  type t =
    { kind : string (** [subagent] or [job] *)
    ; id : string
    ; ok : bool
    ; status : string
    ; task : string
    ; body : string list
    }
  [@@deriving sexp_of]
end

(** [None] unless [text] is a delivery. *)
val parse : string -> Section.t list option
