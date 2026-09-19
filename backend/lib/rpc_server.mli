open! Core
open! Import

(** JSON-lines RPC over a pair of flows. Requests are
    [{"id": ..., "method": ..., "params": {...}}]; responses are
    [{"type": "response", "id": ..., "ok": true, "result": ...}] or
    [{"type": "response", "id": ..., "ok": false, "error": "..."}]. Agent
    events are pushed as [{"type": "event", "event": ..., ...}]. *)

val methods : string list

(** Dispatches one request; exposed for tests. *)
val handle : Agent.t -> Json.t -> Json.t

(** Serves until [input] reaches end of file. *)
val run
  :  env:Env.t
  -> agent:Agent.t
  -> input:_ Eio.Flow.source
  -> output:_ Eio.Flow.sink
  -> unit
