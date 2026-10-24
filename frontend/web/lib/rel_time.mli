open! Core

(** How long ago something happened, for lists: ["just now"], ["5m ago"],
    ["3h ago"], ["2d ago"], ["4mo ago"], ["1y ago"]. *)

(** The backend's timestamps (["2026-10-05 12:34:56.789000Z"]). *)
val parse : string -> Time_ns.t option

val ago : now:Time_ns.t -> Time_ns.t -> string
