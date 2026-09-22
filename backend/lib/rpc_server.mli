open! Core
open! Import

(** JSON-lines RPC. Requests are [{"id": ..., "method": ..., "params": {...}}];
    responses are [{"type": "response", "id": ..., "ok": true, "result": ...}]
    or [{"type": "response", "id": ..., "ok": false, "error": "..."}]. Events
    are pushed as [{"type": "event", "event": ..., ...}].

    One server holds many sessions (an [Agent.t] each, keyed by session id)
    and many clients. Every client is attached to exactly one session at a
    time; its requests act on that session and it receives that session's
    events. [hello] attaches a client (to a named session or the default
    one) and registers it as a tool host when it advertises [tools]. The
    session methods ([new_session], [switch_session], [fork], [clone],
    [import]) move only the calling client. A session keeps running when its
    clients disconnect; an idle session with no clients is dropped from
    memory (it stays on disk). *)

val methods : string list

type t

module Client : sig
  type t

  val id : t -> string
end

val create
  :  env:Env.t
  -> sw:Switch.t
  -> ?token:string
  -> login:Login_manager.t
  -> sessions_dir:string
  -> new_agent:(?session:Session.t -> cwd:string -> unit -> Agent.t)
  -> default_agent:Agent.t
  -> unit
  -> t

(** Registers a client whose outgoing lines go through [send]; it starts
    attached to the default session. *)
val connect : t -> send:(Json.t -> unit) -> Client.t

(** Dispatches one request from [client] and returns the response. Blocking
    methods block the caller. *)
val handle : t -> Client.t -> Json.t -> Json.t

(** Forgets a client (its sessions keep running). *)
val disconnect : t -> Client.t -> unit

(** Serves one connection until [input] reaches end of file; each request is
    handled in its own fiber. *)
val serve_connection
  :  t
  -> input:_ Eio.Flow.source
  -> output:_ Eio.Flow.sink
  -> unit

(** Aborts every session and waits for them to finish. *)
val shutdown : t -> unit

val agent_of_client : t -> Client.t -> Agent.t
