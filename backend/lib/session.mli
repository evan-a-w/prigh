open! Core
open! Import

(** Append-only JSONL session log forming a tree of entries. The active
    conversation is the path from the root to [head]; rewinding moves [head]
    to an earlier entry so new messages branch from there. *)

module Entry : sig
  type payload =
    | Message of Message.t
    | Model of
        { model : string
        ; thinking : Thinking.t
        }
    | Compaction of
        { summary : string
        ; kept_from : string
          (** id of the first entry retained after the summary *)
        }
  [@@deriving sexp, jsonaf]

  type t =
    { id : string
    ; parent : string option
    ; payload : payload
    }
  [@@deriving sexp, jsonaf]
end

type t

val create : dir:string -> cwd:string -> t
val load : string -> t Or_error.t
val id : t -> string
val path : t -> string
val cwd : t -> string
val head : t -> string option
val entries : t -> Entry.t list

(** Entries on the active path, root first. *)
val active_path : t -> Entry.t list

(** Messages for the next model request: those on the active path, with a
    compaction summary (if any) replacing everything before [kept_from]. *)
val messages : t -> Message.t list

val model : t -> (string * Thinking.t) option
val append_message : t -> Message.t -> Entry.t
val set_model : t -> model:string -> thinking:Thinking.t -> Entry.t
val append_compaction : t -> summary:string -> kept_from:string -> Entry.t
val rewind : t -> to_:string -> unit Or_error.t

(** A new session in [dir] containing a copy of the active path up to and
    including [at] (default: head). *)
val fork : ?at:string -> t -> dir:string -> t Or_error.t

module Summary : sig
  type t =
    { id : string
    ; path : string
    ; cwd : string
    ; created_at : string
    ; first_prompt : string option
    ; message_count : int
    }
  [@@deriving sexp_of]
end

val list : dir:string -> Summary.t list
val default_dir : home:string -> string
