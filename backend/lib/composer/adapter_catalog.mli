open! Core
open! Import

(** What the composer's harnesses say of their logins without a thread
    ({!Adapter.Catalog}: their models, their usage limits), by tool host and
    login ([login]: the login of prigh's the harness runs on, by a key
    naming it; [None]: its own): asked by running the harness briefly, on
    that login, and kept, so that a session's model picker and usage show
    the harness's own before its first turn. *)

type t

(** [now]: seconds since the epoch. *)
val create : now:(unit -> float) -> unit -> t

(** [adapter]'s ([codex]) on [host], when it said. *)
val find
  :  t
  -> host:string
  -> login:string option
  -> string
  -> Adapter.Catalog.t option

(** How long ago [find]'s answer was asked, in seconds. *)
val age : t -> host:string -> login:string option -> string -> float option

(** Asks [fetch] for [adapter]'s catalog on [host] in a fiber of [sw],
    unless one younger than [max_age] seconds is kept, one is being asked,
    or asking failed less than a minute ago (unless [force]); [on_done] once
    a new one is kept, or asking failed. *)
val refresh
  :  ?force:bool
  -> t
  -> sw:Switch.t
  -> host:string
  -> login:string option
  -> string
  -> max_age:float
  -> fetch:(unit -> Adapter.Catalog.t Or_error.t)
  -> on_done:(unit -> unit)
  -> unit

(** Why asking [adapter]'s catalog on [host] failed last, if it did. *)
val failure
  :  t
  -> host:string
  -> login:string option
  -> string
  -> Error.t option

(** Whether {!refresh} is asking [adapter]'s catalog on [host]. *)
val asking : t -> host:string -> login:string option -> string -> bool

(** Waits for the asking {!refresh} started, if any. *)
val await : t -> host:string -> login:string option -> string -> unit
