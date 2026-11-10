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
    [ARCHITECTURE.md]). A client without tools can name the tool host on
    its machine with [hello]'s [prefer_host] (prigh-web opened by [prigh
    web]): while that host is connected, the client's sessions move to it
    as they would to the client itself if it were a tool host, and new ones
    start on it. A session keeps running when its clients
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
  -> ?host_grace:float
       (** seconds the in-flight executions of a tool host whose connection
           dropped wait for it to come back with them (default: 60) *)
  -> ?temporary_grace:float
       (** seconds a temporary session without a client waits for one
           before it ends (default: 120) *)
  -> ?sleep:(float -> unit)
       (** how the grace periods are waited for (default: the clock) *)
  -> ?host_timeout:float
       (** seconds after which a tool host that sends heartbeats (its
           [features] have [heartbeat]: [prigh tool-host] pings every 10s)
           and is silent is given up on: its connection is closed and what
           it was running fails at once (default: 30) *)
  -> ?heartbeat_clock:float Eio.Time.clock_ty Eio.Resource.t
       (** what [host_timeout] is measured by (default: [env]'s clock) *)
  -> ?namespace:string
  -> ?backend_host:bool
       (** whether [new_agent] makes the backend a tool host (default: true) *)
  -> ?home:string
       (** whose [config.json] [list_sessions] reads [archive_after_days]
           from (default: the calling client's agent's config) *)
  -> ?forges:Forges.t
       (** git hosts (default: real HTTP, sign-ins in [login]'s store,
           settings in [home]'s config) *)
  -> ?memory:Memory.t
       (** the user's memory, for [get_memory], [save_memory], [forget_memory]
           and [set_memory_enabled] (default: the one under [home]; without
           either they fail). Every client hears [memory_changed] after any
           change, the model's included. *)
  -> ?transcription_http:Transcription.Http.t
       (** where [transcribe] posts the audio (default: real HTTP) *)
  -> ?nix_store:Nix_store.t
       (** for superusers' [nix_store_status] and [nix_store_gc] (without
           it they fail) *)
  -> login:Login_manager.t
  -> sessions_dir:string
  -> cwd:string
  -> new_agent:(?session:Session.t -> cwd:string -> unit -> Agent.t)
  -> ?default_agent:Agent.t
  -> unit
  -> t

(** Lets the sessions of [new_agent] ask each other: a session with mail
    ({!Session_questions}) is woken here, loaded from disk when it is not
    live (with no client, as {!restore_resumes} does). *)
val serve_questions : t -> Session_questions.t -> unit

(** Loads the sessions that recorded a resume after a usage limit still to
    come ([Session.resume_at]), so that they resume on time with nobody
    attached; one that cannot be loaded is skipped with a line on stderr. *)
val restore_resumes : t -> unit

(** Deletes what temporary sessions made that a backend stopped before
    deleting ([Temporary_ledger]), on the hosts connected now (the others'
    when they connect). *)
val clean_leftovers : t -> unit

(** Builds [search_sessions]' index of the saved sessions in a fiber of its
    own that yields after each session, so that the backend serves its
    clients meanwhile; a search meanwhile waits for it. *)
val start_indexing : t -> unit

(** [start_indexing]'s work, returning when it is done (or, if it is under
    way, when it finishes). *)
val index_sessions : t -> unit

(** [search_sessions]' hits: the best [limit] saved sessions for [query],
    from the index. *)
val search_hits : t -> query:string -> limit:int -> Session_search.Hit.t list

(** How many sessions [search_sessions]' index has. *)
val indexed_sessions : t -> int

(** Registers a client whose outgoing lines go through [send]; it starts
    in a new session in [cwd] (or the default one). With [signed_in] (the
    router checked its credentials) it needs no token in [hello], and can
    [list_users] and [set_user] as [signed_in] allows. *)
val connect
  :  ?signed_in:User_access.Signed_in.t
  -> ?close:(string -> unit)
       (** ends the client's connection, with the reason (e.g. a newer
           connection from the same page, [hello]'s [tab], took over) *)
  -> t
  -> send:(Json.t -> unit)
  -> Client.t

(** Dispatches one request from [client] and returns the response. Blocking
    methods block the caller. *)
val handle : t -> Client.t -> Json.t -> Json.t

(** [handle], sending the response to the client, then (when the request
    opened another session) a notice of the lines its file had that could
    not be read. *)
val respond : t -> Client.t -> Json.t -> unit

(** Forgets a client (its sessions keep running). *)
val disconnect : t -> Client.t -> unit

(** Serves one connection until [read_line] returns [None]; each request is
    handled in its own fiber, and the requests still running then are
    cancelled, unless [finish_requests] (stdio, which a script may close
    before the answers come). An allowed [set_user] request ends it too,
    unanswered, once its requests are answered: the result is its id and
    the user to serve the rest of the connection as. What goes out waits in
    an {!Outbox}, so nothing waits for a peer that stopped reading: when
    that outbox fails (too much waiting, or a write failed) or [close] is
    called (a newer connection from the same page), the connection ends
    and its requests are cancelled. *)
val serve_lines
  :  ?signed_in:User_access.Signed_in.t
  -> ?finish_requests:bool
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

(** What [session]'s terminal is keyed by in {!Terminals} and
    {!Terminal_relay}: the session id, prefixed by the namespace. *)
val terminal_key : t -> session:string -> string

(** [f] is called with the terminal key of every deleted session (to kill
    its terminal). *)
val on_session_deleted : t -> f:(key:string -> unit) -> unit

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
