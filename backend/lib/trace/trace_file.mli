open! Core
open! Import

(** A session's trace: [<session>.trace.jsonl] next to its JSONL, one
    {!Trace.Span.t} per line, appended as each span ends. When it would grow
    past [max_bytes] it becomes [<session>.trace.old.jsonl] (replacing the
    previous one) and a new file starts, so the trace keeps at most twice
    [max_bytes], the newest spans. *)

val default_max_bytes : int

(** The trace files of the session file [session_path], whether they exist
    or not: the older one first. *)
val paths : session_path:string -> string list

(** Whether a file name in the sessions directory is a trace, not a session. *)
val is_trace_file : string -> bool

type t

(** [session_path ()] is where the session lives, [None] while it has no
    file yet: spans wait in memory until it has one. Once that file is gone
    (the session was deleted), spans are dropped. *)
val create : ?max_bytes:int -> session_path:(unit -> string option) -> unit -> t

val write : t -> Trace.Span.t -> unit

(** The spans in the trace files, oldest first, and how many lines could not
    be read (a crash may leave the last one cut short). *)
val load : session_path:string -> Trace.Span.t list * int
