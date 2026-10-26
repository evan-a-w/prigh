open! Core
open! Async

(** The terminal UI's connection to its backend: which backend (one it
    spawns, [prigh serve -listen] at [HOST:PORT], or a backend's prigh-web
    URL, see {!Prigh_ui.Server_address}), the credentials sent
    in [hello], and where the session's tools run. [/connect], [/signin] and
    [/signout] change them while the TUI runs; what works is saved as the
    {!Prigh_client_unix.Saved_connection}, as [-connect], [-token] and
    [-user] are. *)

module Target : sig
  type t =
    | Spawn (** [backend serve], like [prigh-tui -local] *)
    | Remote of string (** [HOST:PORT] or a prigh-web URL *)
  [@@deriving sexp_of, equal]
end

(** What a transport is opened to. *)
module Endpoint : sig
  type t =
    | Process of
        { prog : string
        ; args : string list
        }
    | Tcp of
        { host : string
        ; port : int
        }
    | Websocket of
        { url : string (** as given *)
        ; address : Prigh_ui.Server_address.t
        ; headers : (string * string) list (** sent with the upgrade *)
        }
  [@@deriving sexp_of]

  (** A spawned process, a TCP connection or a WebSocket. *)
  val open_ : t -> Prigh_client.Transport.t Deferred.Or_error.t
end

(** [-tools]: where the session's tools run. Without it, on this machine
    (through [backend tool-host]) when the backend is remote, on the backend
    when we spawned it. *)
module Tools : sig
  type t =
    | Local
    | Remote
  [@@deriving sexp_of]

  val of_string : string -> t Or_error.t
end

type t

(** [hello] is sent on every connection (name, cwd), with the credentials;
    [session] (and [-session] in [spawn_args]) only until the first switch.
    Nothing is opened yet: see {!start}. *)
val create
  :  ?open_transport:
       (Endpoint.t -> Prigh_client.Transport.t Deferred.Or_error.t)
       (** default: {!Endpoint.open_} *)
  -> ?spawn_worker:(unit -> Prigh_client.Transport.t Deferred.Or_error.t)
       (** default: [backend tool-host] *)
  -> ?headers:(string * string) list
       (** sent with a WebSocket's upgrade, e.g. a proxy's credentials *)
  -> home:string
  -> backend:string
  -> spawn_args:string list (** [serve ...] *)
  -> tools:Tools.t option
  -> hello:(string * Prigh_protocol.Json.t) list
  -> session:string option
  -> target:Target.t
  -> token:string option
  -> user:string option
  -> saved:Prigh_client_unix.Saved_connection.t option
  -> unit
  -> t

val client : t -> Prigh_client.Client.t
val info : t -> Prigh_ui.Server_link.t

(** The tool host of the current connection, when tools run here. *)
val tool_host : t -> Prigh_client_unix.Tool_host.t option

(** Notices for the user (a tool host that died, a host id that cannot be
    kept); those sent before this is called are delivered by it. *)
val set_notice : t -> (string -> unit) -> unit

(** Connects and sends [hello]. *)
val start : t -> Prigh_ui.Server_link.Outcome.result Deferred.t

(** After [delay_ms], {!reconnect} with the current [hello], unless a switch
    happened meanwhile. *)
val reconnect
  :  t
  -> delay_ms:int
  -> session:string option
  -> as_user:string option
  -> (Prigh_protocol.Json.t, string) Result.t Deferred.t

(** Opens the new target's transport first (when it fails, nothing changes),
    then drops the current connection (and its tool host), connects and
    sends [hello]. When it is answered, the connection is saved: a remote
    address with the saved token and user if it is the saved address
    ({!Prigh_client_unix.Saved_connection.resolve}'s rules), or the new
    credentials. A spawned backend leaves the saved connection alone. *)
val relink
  :  t
  -> Prigh_ui.Server_link.Change.t
  -> Prigh_ui.Server_link.Outcome.t Deferred.t

(** Forgets the token, here and in the saved connection (keeping its address
    and user), and disconnects. *)
val sign_out : t -> (unit, string) Result.t Deferred.t

(** Feeds the tool host its events; what the UI should see: [None] for the
    end of a connection that a switch has replaced already. *)
val handle_incoming
  :  t
  -> Prigh_client.Client.Incoming.t
  -> Prigh_client.Client.Incoming.t option

val close : t -> unit Deferred.t

(** [hello] plus [tools], [true] with a [host_id] (this machine's, see
    {!Prigh_client_unix.Host_id}), so that the backend knows our tool host
    again when we reconnect or restart (and resumes its sessions on it), and
    the [features] our tool host has ([exec_input]: it forwards
    [Tool_exec_input] to running tools). *)
val connection_hello
  :  (string * Prigh_protocol.Json.t) list
  -> host_id:string option
  -> (string * Prigh_protocol.Json.t) list

(** Connects [client] again after [delay_ms] and sends [hello], with
    [session] and [as_user] replacing those fields; the [hello] reply. With
    [tool_host], [hello] names the executions it still runs
    ([running_execs]), so that the backend keeps them, and once it is
    answered, what the tool host held while offline goes out. *)
val reconnect_client
  :  ?tool_host:Prigh_client_unix.Tool_host.t
  -> Prigh_client.Client.t
  -> hello:(string * Prigh_protocol.Json.t) list
  -> delay_ms:int
  -> session:string option
  -> as_user:string option
  -> (Prigh_protocol.Json.t, string) Result.t Deferred.t
