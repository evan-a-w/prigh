open! Core
open! Import

(** A user's preferences and data that frontends keep on the backend, so
    that they follow the user from browser to browser (prigh-web's theme,
    accent, notifications, panel sizes; the prompt history the frontends
    share): [~/.prigh/prefs.json] under the user's home, one per namespace
    with [-tokens]. A JSON object of keys to any JSON values; the backend
    gives the keys no meaning (frontends prefix theirs, e.g. [web.theme]).

    Every change re-reads the file and replaces it atomically (a temporary
    file renamed over it) under a lock, so backends sharing a home (a TUI's
    own and [prigh serve]) do not lose each other's changes. Nothing here
    is a secret: the file is the user's to read. *)

type t

(** [~/.prigh/prefs.json] under [home]. *)
val path : home:string -> string

val create : path:string -> t

(** Change of one key: its new value, [None] when removed. *)
module Change : sig
  type t = string * Json.t option

  val to_json : t list -> Json.t
end

(** The values of [keys] that are set, or of every key. *)
val get : ?keys:string list -> t -> (string * Json.t) list Or_error.t

(** Sets ([Some]) or removes ([None]) these keys, leaving the others; the
    changes that change something. Refused, changing nothing, when a key is
    empty or longer than 200 bytes, a value larger than 256 KiB, or the file
    would grow past 4 MiB. *)
val set : t -> Change.t list -> Change.t list Or_error.t

(** Puts [value] at the front of the list [key] (an absent or non-list value
    is an empty list), unless it is already its first, keeping the first
    [max] (at least 1): a history that clients add to concurrently. Its new
    value. *)
val push : t -> key:string -> Json.t -> max:int -> Json.t Or_error.t
