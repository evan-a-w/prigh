open! Core
open! Import

(** JSON-lines RPC. Requests are [{"id": ..., "method": ..., "params": {...}}];
    responses are [{"type": "response", "id": ..., "ok": true, "result": ...}]
    or [{"type": "response", "id": ..., "ok": false, "error": "..."}]. Events
    are pushed as [{"type": "event", "event": ..., ...}].

    One server holds many sessions (an [Agent.t] each, keyed by session id)
    and many clients. Every client is attached to exactly one session at a
    time; its requests act on that session and it receives that session's
    events. A new client starts in a fresh session (or the [default_agent]
    when one is given); [hello] can attach it to an existing session by id
    or path and registers it as a tool host when it advertises [tools]; hosts
    are visible to every session, so a session can run its tools on a client
    attached elsewhere (replies are routed by exec id). The session methods
    ([new_session], [switch_session], [fork], [clone], [import]) move only
    the calling client. A session keeps running when its
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
  -> ?namespace:string
  -> ?backend_host:bool
       (** whether [new_agent] makes the backend a tool host (default: true) *)
  -> login:Login_manager.t
  -> sessions_dir:string
  -> cwd:string
  -> new_agent:(?session:Session.t -> cwd:string -> unit -> Agent.t)
  -> ?default_agent:Agent.t
  -> unit
  -> t

(** Registers a client whose outgoing lines go through [send]; it starts
    in a new session in [cwd] (or the default one). *)
val connect : t -> send:(Json.t -> unit) -> Client.t

(** Dispatches one request from [client] and returns the response. Blocking
    methods block the caller. *)
val handle : t -> Client.t -> Json.t -> Json.t

(** Forgets a client (its sessions keep running). *)
val disconnect : t -> Client.t -> unit

(** Serves one connection until [read_line] returns [None]; each request is
    handled in its own fiber. *)
val serve_lines
  :  t
  -> read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> unit

(** Aborts every session and waits for them to finish. *)
val shutdown : t -> unit

val agent_of_client : t -> Client.t -> Agent.t

(** Whether [token] is the one [hello] requires (always, without one). *)
val token_ok : t -> string option -> bool

val namespace : t -> string option

(** Where [session]'s terminal should run: on its active tool host, in the
    session's cwd. Without a live session, the backend in the server's cwd
    (if it is a host). *)
val terminal_target
  :  t
  -> session:string option
  -> [ `Backend of string (** cwd *)
     | `Host of string * string (** client id, cwd on that host *)
     | `Unavailable of string (** reason *)
     ]
