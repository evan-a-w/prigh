open! Core
open! Import

(** What a temporary session made that outlives the backend's memory
    ({!Leftover}), one small file per live temporary session in
    [<sessions_dir>/.temporary/<id>.json]: when the backend stops without
    ending it, the next start deletes those things (see
    [Rpc_server.clean_leftovers]). It holds no transcript, only paths. *)

type t =
  { id : string (** the session's *)
  ; leftovers : Leftover.t list
  }
[@@deriving sexp_of, equal]

val dir : sessions_dir:string -> string

(** Replaces the session's file, atomically. *)
val write : dir:string -> t -> unit

val remove : dir:string -> id:string -> unit

(** Every file in [dir]; one that does not parse is removed. *)
val load_all : dir:string -> t list
