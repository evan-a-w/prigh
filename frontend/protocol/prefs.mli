open! Core

(** The user's preferences and data kept on the backend ([get_prefs],
    [set_prefs], [push_pref] and the [prefs_changed] event): keys to any JSON
    values, which only the frontends give a meaning. *)

(** One key's new value; [None]: removed (or never set). *)
module Change : sig
  type t =
    { key : string
    ; value : Json.t option
    }
  [@@deriving sexp_of, equal]
end

(** [get_prefs]'s reply ([{prefs: {key: value}}]), as changes from nothing. *)
val of_reply : Json.t -> Change.t list Or_error.t

(** [prefs_changed]'s [prefs]: [null] is a key removed. *)
val changes_of_json : Json.t -> Change.t list Or_error.t

val get_params : keys:string list -> (string * Json.t) list
val set_params : Change.t list -> (string * Json.t) list

(** Puts [value] at the front of the list [key] unless it is its first,
    keeping [max]. *)
val push_params : key:string -> Json.t -> max:int -> (string * Json.t) list
