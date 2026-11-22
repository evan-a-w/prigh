open! Core

(** How long ago something happened, for lists: ["just now"], ["5m ago"],
    ["3h ago"], ["2d ago"], ["4mo ago"], ["1y ago"]. *)

(** The backend's timestamps (["2026-10-05 12:34:56.789000Z"]). *)
val parse : string -> Time_ns.t option

val ago : now:Time_ns.t -> Time_ns.t -> string

(** The same, as narrow as can be: ["now"], ["5m"], ["3h"], ["2d"], ["4mo"],
    ["1y"]. *)
val short : now:Time_ns.t -> Time_ns.t -> string
