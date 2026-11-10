open! Core
open! Import

(** The [sessions] tool: the model looks into the other saved sessions of
    its namespace ([sessions_dir]), cheaply: [search] finds sessions by
    words (their title, description, first prompt, directory and
    repository first, then their transcripts, whose files are only parsed
    when their bytes hold the words), [outline] lists one's landmarks
    ({!Outline}) and [read] shows a few of its messages, cut short. Message
    indices are positions in [Session.identified_messages] throughout. It
    reads session files and changes nothing. *)

val name : string

(** The saved session whose id is [key] or the only one whose id starts
    with it; never a path. Errors say what to do next. *)
val find : dir:string -> string -> Session.Summary.t Or_error.t

val create
  :  ?max_scan_bytes:int
       (** how much of the session files one transcript search reads before
           it stops (default 512 MiB) *)
  -> sessions_dir:(unit -> string)
  -> current:(unit -> string option) (** the calling session's id *)
  -> unit
  -> Tool.t
