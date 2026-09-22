open! Core

(** The prompt history shared by every frontend: [~/.prigh/history], one JSON
    string per line. [load] returns the last 500 entries, oldest first, and
    skips lines that are not JSON strings. *)
val load : home:string -> string list

val append : home:string -> string -> unit
