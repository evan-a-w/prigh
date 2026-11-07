open! Core
open! Import

(** The user's memory: short notes the model saves when the user asks it to
    remember something, and recalls in later sessions (the [memory] tool,
    {!Tool_memory}). [~/.prigh/memory.json] under the world's home, so one
    per namespace with [-tokens]: [{"next": n, "notes": [...]}], oldest
    first.

    Every change re-reads the file and replaces it atomically (a temporary
    file renamed over it) under the file's {!Lock_dir} and this value's
    mutex, so sessions and backends sharing a home do not lose each other's
    notes. Make one per world. *)

type t

(** [~/.prigh/memory.json] under [home]. *)
val path : home:string -> string

val create : path:string -> t

module Note : sig
  type t =
    { id : string (** [m1], [m2], ...: never reused *)
    ; title : string option
    ; text : string
    ; saved : Time_float.t (** shown in UTC *)
    }
  [@@deriving sexp_of]
end

module Saved : sig
  type t =
    | Added of Note.t
    | Replaced of Note.t (** a note with the same title, its id kept *)
  [@@deriving sexp_of]
end

val max_text : int
val max_title : int
val max_notes : int

(** Adds a note, or replaces the one with the same [title] (compared without
    case). Refused when [text] is empty or longer than {!max_text} bytes,
    [title] longer than {!max_title}, or there are {!max_notes} notes. *)
val save
  :  t
  -> ?title:string
  -> now:Time_float.t
  -> string
  -> Saved.t Or_error.t

(** Every note, oldest first. *)
val notes : t -> Note.t list Or_error.t

(** The notes [query] finds, best first: a note's id, or the notes holding
    the most of its words (case-insensitive, as a substring or within one
    typo of one of the note's words), the newest first among equals. *)
val search : Note.t list -> string -> Note.t list

(** Removes the note with this id or title (compared without case). *)
val forget : t -> string -> Note.t Or_error.t
