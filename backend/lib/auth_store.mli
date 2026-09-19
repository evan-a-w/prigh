open! Core

(** Credential file ([~/.config/prigh/auth.json] by default), one entry per
    provider. Every read goes to disk so changes made by other processes are
    seen; [modify] is the only write path and holds a cross-process lock so
    that two processes cannot both rotate the same OAuth refresh token. *)

type t

val default_path : unit -> string
val create : path:string -> t
val path : t -> string
val read : t -> Provider_id.t -> Credential.t option Or_error.t
val list : t -> (Provider_id.t * Credential.t) list Or_error.t

(** [f] sees the on-disk credential under the lock; the file is rewritten
    only if it returns something different. Entries for unknown providers
    are preserved. *)
val modify
  :  t
  -> Provider_id.t
  -> f:(Credential.t option -> Credential.t option Or_error.t)
  -> Credential.t option Or_error.t

val set : t -> Provider_id.t -> Credential.t -> unit Or_error.t
val remove : t -> Provider_id.t -> unit Or_error.t
