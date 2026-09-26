open! Core
open! Import

(** Append-only JSONL session log forming a tree of entries. The active
    conversation is the path from the root to [head]; rewinding moves [head]
    to an earlier entry so new messages branch from there. *)

module Entry : sig
  module Payload : sig
    type t =
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
      | Name of { name : string }
      | Description of { text : string }
      (** model-written one-line summary, see [Session_description] *)
      | Cwd of { cwd : string }
      | System_prompt of { text : string }
    [@@deriving sexp, jsonaf]
  end

  type t =
    { id : string
    ; parent : string option
    ; payload : Payload.t
    }
  [@@deriving sexp, jsonaf]
end

type t

(** Nothing is written until the session has a message or a name; an
    abandoned empty session leaves no file. *)
val create : dir:string -> cwd:string -> ?parent:string -> unit -> t

val load : string -> t Or_error.t

(** Whether the file at [path] exists yet. *)
val persisted : t -> bool

val id : t -> string
val path : t -> string
val cwd : t -> string
val parent : t -> string option
val head : t -> string option
val entries : t -> Entry.t list
val created_at : t -> string
val updated_at : t -> string

(** Seconds from [created_at] to the last modification (or now, when the file
    has not been written since creation). *)
val duration_seconds : t -> float

(** Last [name] entry written to the file. *)
val name : t -> string option

(** Last [description] entry written to the file. *)
val description : t -> string option

(** Entries on the active path, root first. *)
val active_path : t -> Entry.t list

(** Messages for the next model request: those on the active path, with a
    compaction summary (if any) replacing everything before [kept_from]. *)
val messages : t -> Message.t list

val model : t -> (string * Thinking.t) option

(** The system prompt fixed for this conversation, if one was recorded on the
    active path. *)
val system_prompt : t -> string option

val set_system_prompt : t -> text:string -> Entry.t
val append_message : t -> Message.t -> Entry.t
val set_model : t -> model:string -> thinking:Thinking.t -> Entry.t
val set_name : t -> name:string -> Entry.t
val set_description : t -> text:string -> Entry.t

(** Records a [cwd] entry; a subsequent [load] restores it. *)
val set_cwd : t -> cwd:string -> Entry.t

val append_compaction : t -> summary:string -> kept_from:string -> Entry.t
val rewind : t -> to_:string -> unit Or_error.t

(** Copy [src_path] into [dir] under a fresh stamp. The session id is kept
    unless it already exists in [dir]. *)
val import : dir:string -> string -> t Or_error.t

(** A new session in [dir] containing a copy of the active path up to and
    including [at] (default: head). *)
val fork : ?at:string -> t -> dir:string -> t Or_error.t

module Export_format : sig
  type t =
    | Markdown
    | Jsonl
  [@@deriving sexp_of]

  val of_string : string -> t Or_error.t
  val extension : t -> string
end

(** Markdown transcript of the active path. *)
val to_markdown : t -> string

module Summary : sig
  type t =
    { id : string
    ; path : string
    ; name : string option
    ; description : string option
    ; cwd : string
    ; created_at : string
    ; updated_at : string
    ; first_prompt : string option
    ; message_count : int
    ; parent : string option
    }
  [@@deriving sexp_of]
end

(** Most recently updated first. *)
val list : dir:string -> Summary.t list

val default_dir : home:string -> string
