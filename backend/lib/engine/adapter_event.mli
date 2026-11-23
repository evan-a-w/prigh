open! Core
open! Import

(** What a composer adapter (Codex, Claude Code, ACP, ...) reports while it
    drives an external harness, in terms that do not depend on the harness.
    Each adapter's codec turns its protocol's messages into these; the
    composer records them in the session and turns them into
    [Agent_event.t]s, so the frontends see what they see of the harness
    engine. Ids ([item], [id], [request_id], [turn_id]) are the harness's
    own, unique within its thread. *)

module Tool_kind : sig
  type t =
    | Command (** a shell command: input [{command}] *)
    | File_change (** edits, creations, deletions: see [Change] *)
    | Read
    | Search
    | Web_search
    | Mcp of
        { server : string
        ; tool : string
        }
    | Todo (** the harness's own plan/todo tool *)
    | Subagent (** a child agent; its own calls name this one as [parent] *)
    | Other of string
  [@@deriving sexp_of, equal]
end

module Change : sig
  module Kind : sig
    type t =
      | Add
      | Delete
      | Update of { move_to : string option }
    [@@deriving sexp_of, equal]
  end

  type t =
    { path : string
    ; kind : Kind.t
    ; diff : string (** unified diff of the file; empty when unknown *)
    }
  [@@deriving sexp_of, equal]
end

module Error_kind : sig
  type t =
    | Usage_limit of { resets_at : float option (** Unix seconds *) }
    | Auth_required of { hint : string (** e.g. "run `codex login` on <host>" *) }
    | Not_installed of { hint : string }
    | Crashed of { stderr_tail : string }
    | Protocol (** the harness said something we could not follow *)
    | Other
  [@@deriving sexp_of, equal]
end

module Stop : sig
  type t =
    | Done
    | Interrupted
    | Failed of
        { kind : Error_kind.t
        ; message : string
        }
  [@@deriving sexp_of, equal]
end

(** How the user answered an [Approval_request]. [Allow_always] only when
    the request [offers_always]. *)
module Approval_reply : sig
  type t =
    | Allow
    | Allow_always
    | Deny of string option (** with the user's reason, told to the model *)
    | Cancel (** stop the turn *)
  [@@deriving sexp_of, equal]
end

module Usage_report : sig
  type t =
    { usage : Usage.t (** this turn's (deltas, not thread totals) *)
    ; cost_usd : float option (** [None]: the harness reports no cost *)
    ; context_tokens : int option (** tokens in the context now *)
    ; context_window : int option
    }
  [@@deriving sexp_of, equal]
end

type t =
  | Thread_started of
      { thread_id : string
      ; model : string option
      }
  | Turn_started of { turn_id : string }
  | Text_delta of
      { item : string
      ; text : string
      }
  | Text_done of
      { item : string
      ; text : string (** the whole text, authoritative over the deltas *)
      }
  | Thinking_delta of
      { item : string
      ; text : string
      }
  | Thinking_done of
      { item : string
      ; text : string
      }
  | Tool_start of
      { id : string
      ; kind : Tool_kind.t
      ; native_name : string (** the harness's name, e.g. "Bash", "commandExecution" *)
      ; title : string (** one line for the UI *)
      ; input : Json.t
      ; parent : string option (** the [Subagent] call this one runs under *)
      }
  | Tool_update of
      { id : string
      ; output : string (** a chunk of streamed output *)
      }
  | Tool_end of
      { id : string
      ; ok : bool
      ; output : string
      ; changes : Change.t list
      ; images : Image.t list
      }
  | Approval_request of
      { request_id : string
      ; tool_id : string option
      ; kind : Tool_kind.t
      ; summary : string (** e.g. the command, or the files touched *)
      ; input : Json.t
      ; offers_always : bool
      }
  | User_question of
      { request_id : string
      ; questions : User_question.t list
      }
  | Request_withdrawn of { request_id : string }
  (** the harness no longer needs the answer (resolved elsewhere, turn over) *)
  | Plan of Plan.t
  | Turn_diff of { unified : string }
  | Usage of Usage_report.t
  | Compacted of
      { tokens_before : int option
      ; tokens_after : int option
      }
  | Retrying of
      { attempt : int option
      ; message : string
      }
  | Notice of string
  | Turn_end of
      { turn_id : string option
      ; stop : Stop.t
      }
[@@deriving sexp_of]
