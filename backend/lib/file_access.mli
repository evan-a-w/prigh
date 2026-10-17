open! Core
open! Import

(** How tools read and write the contents of text files: the local disk, or
    something in front of it (an editor connected over ACP, whose unsaved
    buffers win and through which edits go). Only contents go through it:
    whether a file exists, directories, deletes and moves always use the
    disk, as do binary files and images. *)

type t

(** [In_channel.read_all] and [Out_channel.write_all]. *)
val local : t

val create : read:(string -> string) -> write:(string -> data:string -> unit) -> t

val is_local : t -> bool

(** The contents of the file at [path] (absolute); raises when it cannot be
    read. *)
val read : t -> string -> string

(** Replaces the contents of [path] (absolute; its directory exists). *)
val write : t -> string -> data:string -> unit
