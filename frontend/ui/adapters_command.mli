open! Core
open! Import

(** [/adapters]: whether the composer engine's agent CLIs (Codex, Claude
    Code, ACP agents) are installed and logged in on each connected tool
    host ([list_adapters]); [/adapters refresh] checks again. prigh only
    checks: what is missing says what to run on that host. *)

val usage : string

(** [Ok refresh] *)
val parse : string -> bool Or_error.t

(** A heading per host, then a line per adapter and, under one that cannot
    run, what to do. *)
val lines : P.Adapter_status.t list -> string list
