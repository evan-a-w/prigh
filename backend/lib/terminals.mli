open! Core
open! Import

(** The browser's terminals: one {!Terminal} per key (a session id), shared
    by every channel (a browser's WebSocket, possibly relayed through a tool
    host) that asks for that key and reattached on reconnect.

    The protocol on a terminal channel:
    - server → client binary: terminal output; the first message is the
      replay (the client resets its emulator when the socket opens);
    - client → server binary: input bytes;
    - client → server text: [{"type":"resize","cols":C,"rows":R}] and
      [{"type":"ping"}] (answered with [{"type":"pong"}]);
    - server → client text: [{"type":"exit"}] when the shell has ended and
      [{"type":"error","message":M}] when it could not start; the socket
      closes after either.

    Nothing leaks: a socket that sends nothing for [heartbeat_timeout] is
    closed (the client pings more often than that), a terminal with no
    sockets for [idle_timeout] is killed, and every terminal dies with the
    backend (see {!Terminal}). *)

type t

(** [tmux] defaults to [$PRIGH_TMUX], else [tmux] on the [PATH]. [socket]
    defaults to the server [-L prigh], shared by every backend (session names
    then include the pid). *)
val create
  :  env:Env.t
  -> sw:Switch.t
  -> ?tmux:string
  -> ?socket:Terminal.Socket.t
  -> ?idle_timeout:Time_ns.Span.t (** default: 10 minutes *)
  -> ?heartbeat_timeout:Time_ns.Span.t (** default: 30 seconds *)
  -> ?command:string list (** default: the user's shell *)
  -> unit
  -> t

(** Serves one terminal channel until it closes, creating the terminal for
    [key] in [cwd] (at [cols]x[rows]) unless it is already running. *)
val serve
  :  t
  -> key:string
  -> cwd:string
  -> cols:int
  -> rows:int
  -> Terminal_channel.t
  -> unit

(** Running terminals: key, tmux session name and attached sockets. *)
val live : t -> (string * string * int) list

(** Kills every terminal. *)
val close_all : t -> unit
