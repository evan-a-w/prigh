open! Core

module Subagent : sig
  (** A background subagent that is running or whose report has not been
      delivered to the main agent yet. *)
  type t =
    { id : string
    ; task : string
    ; running : bool
    }
  [@@deriving sexp_of, equal]
end

type t =
  { session_id : string
  ; session_path : string
  ; session_name : string option
  ; session_description : string option
  ; cwd : string
  ; git_branch : string option
  ; model : Model.t
  ; thinking : string
  ; running : bool
  ; message_count : int
  ; usage : Usage.t
  ; cost_usd : float
  ; context_tokens : int
  ; active_host : string (** [Host.id]; "backend" or a client id *)
  ; hosts : Host.t list
  (** backend first, then connected tool-capable clients *)
  ; subagents : Subagent.t list
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t
