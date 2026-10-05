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
    attached elsewhere (replies are routed by exec id). A host is known by its
    host id: [hello]'s [host_id] (the same on every connection from a
    machine, so a reconnecting or restarted host is the same host and its
    sessions resume on it), or its client id without one. A newer connection
    claiming a held host id takes it over and the older one falls back to
    its client id (with a notice), taking the id back if the newer one
    disconnects first. The session methods ([new_session],
    [switch_session], [fork], [clone], [import]) move only the calling
    client. A brand-new session starts on a host chosen from the client's
    context, never on whichever host connected first: on the client itself
    when it is a tool host, else where the client's session is (see
    [ARCHITECTURE.md]). A session keeps running when its clients
    disconnect; an idle session with no clients is dropped from memory (it
    stays on disk, with its tool host, so it is on the same host when loaded
    again). *)

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
    in a new session in [cwd] (or the default one). With [signed_in] (the
    router checked its credentials) it needs no token in [hello], and can
    [list_users] and [set_user] as [signed_in] allows. *)
val connect
  :  ?signed_in:User_access.Signed_in.t
  -> t
  -> send:(Json.t -> unit)
  -> Client.t

(** Dispatches one request from [client] and returns the response. Blocking
    methods block the caller. *)
val handle : t -> Client.t -> Json.t -> Json.t

(** Forgets a client (its sessions keep running). *)
val disconnect : t -> Client.t -> unit

(** Serves one connection until [read_line] returns [None]; each request is
    handled in its own fiber. An allowed [set_user] request ends it too,
    unanswered: the result is its id and the user to serve the rest of the
    connection as. *)
val serve_lines
  :  ?signed_in:User_access.Signed_in.t
  -> t
  -> read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> (Json.t * string) option

(** Aborts every session and waits for them to finish. *)
val shutdown : t -> unit

val agent_of_client : t -> Client.t -> Agent.t

(** The error for any failed authentication. *)
val unauthorised : string

(** Whether [hello] would accept this token (always, without one) and [user]:
    with a namespace, a given [user] must be its name; otherwise [user] is
    ignored. *)
val credentials_ok : t -> ?user:string -> string option -> bool

val namespace : t -> string option

(** Where [session]'s terminal should run: on its active tool host, in that
    host's cwd for the session. Without a live session, the backend in the
    server's cwd (if it is a host). *)
val terminal_target
  :  t
  -> session:string option
  -> [ `Backend of string (** cwd *)
     | `Host of string * string (** host id, cwd on that host *)
     | `Unavailable of string (** reason *)
     ]

(** Relays a terminal channel to the client that is tool host [host] (see
    {!Terminal_relay}) until either side closes. *)
val relay_terminal
  :  t
  -> host:string
  -> key:string
  -> cwd:string
  -> cols:int
  -> rows:int
  -> Terminal_channel.t
  -> unit
