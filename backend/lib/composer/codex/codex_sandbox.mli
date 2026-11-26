open! Core
open! Import

(** Whether Codex's own sandbox can run on a tool host, found by running
    [codex sandbox -- true] there through [$probe_programs] ({!Host_probe}):
    on Linux its bubblewrap needs user namespaces, which e.g. Docker's
    default seccomp profile forbids. Where it cannot, Codex runs without it
    ({!Adapter.Start.no_sandbox}), prigh's approvals still deciding. *)

module Status : sig
  type t =
    | Works
    | Unavailable of string (** why, e.g. ["no user namespaces"] *)
    | Unknown of string
    (** the probe could not tell (an older Codex, the host gone): Codex
        keeps its sandbox *)
  [@@deriving sexp_of, equal]
end

(** The [$probe_programs] request. *)
val request : Host_probe.Request.t

(** The status a [$probe_programs] reply says, and the [codex] it ran (none
    when the host has none or could not say). *)
val of_result : Tool.Result.t -> string option * Status.t

(** The latest status by host, probed once per host and [codex] binary. *)
module Cache : sig
  type t

  val create : unit -> t

  (** What was found on [host], probing it ([exec] runs a host operation
      there) unless it is known for [installed], the [codex] path the host
      reported last (any, when [None]). *)
  val status
    :  t
    -> host:string
    -> installed:string option
    -> exec:(name:string -> Json.t -> Tool.Result.t)
    -> Status.t

  (** What is known of [host], without probing. *)
  val find : t -> host:string -> Status.t option

  (** On a host's (re)connection, or after Codex failed to start there. *)
  val forget_host : t -> host:string -> unit
end
