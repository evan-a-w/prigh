open! Core
open! Import

(** What was read from files, by path, kept while the file stays as it was
    (same size and modification time): opening a large session again need
    not parse it again. At most [max_bytes] of files' worth, the least
    recently used dropped first. *)

type 'a t

val create : max_bytes:int -> unit -> 'a t

(** The value for [path] if its file is unchanged since it was added. *)
val find : 'a t -> string -> 'a option

(** [find], else [read path] (added unless it fails). The file is looked at
    before it is read, so a change made while reading counts as a change. *)
val find_or_read
  :  'a t
  -> string
  -> read:(string -> 'a Or_error.t)
  -> 'a Or_error.t

(** Keeps [data] as what [path] holds now. *)
val set : 'a t -> string -> 'a -> unit

val remove : 'a t -> string -> unit

(** Paths cached, least recently used first. *)
val paths : _ t -> string list
