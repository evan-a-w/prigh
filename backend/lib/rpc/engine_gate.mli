open! Core
open! Import

(** Refuses a request its session's engine cannot honour (see
    [Capabilities]), with an error saying what to do instead, before it is
    dispatched: every way in (the TUI, prigh-web, ACP) gets the same answer. *)
val check : Agent.t -> meth:string -> params:Json.t -> unit Or_error.t
