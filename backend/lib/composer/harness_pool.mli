open! Core
open! Import

(** Pool of shared long-lived external harness server processes ({!Harness_mux})
    keyed by isolation tuple [(host, adapter, member, login_key, no_sandbox,
    launch args/env)].

    - Concurrent callers for the same key share a single in-flight boot and
      reuse the warm process once booted.
    - Non-poolable servers (ACP agents not advertising [sessionCapabilities.close],
      agents with [xai_extensions], or remote hosts predating routed MCP)
      automatically fall back to dedicated per-session processes.
    - When a pooled server's token nears expiry, is refused, or changes, it is
      marked draining: new sessions boot a fresh server while existing sessions
      finish on the draining one, which stops once its last session closes.
    - Idle pooled servers close automatically after [idle_timeout].
    - Crashes, host disconnects ({!evict_host}), and {!shutdown} clean up all
      processes cleanly with no orphan leaks. *)

module Key : sig
  type t =
    { host : string
    ; adapter : string
    ; member : string option
    ; login_key : string option
    ; no_sandbox : bool
    ; argv : string list
    ; env_set : (string * string) list
    ; env_unset : string list
    }
  [@@deriving sexp_of, compare, hash, equal]
end

(** Login token metadata for a pooled process that takes its token only at
    process launch (Antigravity). *)
module Token_info : sig
  type t =
    { token : string
    ; expiring : now_ms:int -> bool
    ; secret_digest : string option
    }
end

type t

val create
  :  sw:Switch.t
  -> ?idle_timeout:Time_float.Span.t (** default 10 minutes *)
  -> ?now_ms:(unit -> int)
  -> ?sleep:(float -> unit)
  -> unit
  -> t

(** Acquires a virtual {!Line_transport.t} for [key]. Reuses a warm pooled
    server when available and healthy; otherwise boots one via [spawn]. When
    [poolable] is [false], bypasses the pool and spawns a dedicated process. *)
val acquire
  :  t
  -> protocol:Harness_mux.Protocol.t
  -> key:Key.t
  -> ?poolable:bool
  -> ?token_info:Token_info.t
  -> ?login_refused:string
  -> ?request_timeout:Time_float.Span.t
  -> spawn:(sw:Switch.t -> Line_transport.t Or_error.t)
  -> unit
  -> Line_transport.t Or_error.t

(** Stops and removes all pooled and draining servers running on [host]. *)
val evict_host : t -> host:string -> unit

(** Stops all active, booting, and draining servers. *)
val shutdown : t -> unit

(** Counts of currently active and draining pooled servers (for tests and
    diagnostics). *)
val active_count : t -> int

val draining_count : t -> int
