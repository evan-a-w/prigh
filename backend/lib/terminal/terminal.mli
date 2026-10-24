open! Core
open! Import

(** An interactive shell in a tmux session, streamed through one control-mode
    client ([tmux -C]) over pipes, so no pty is needed on our side.

    Viewers are terminal emulators: they get the pane's raw output, preceded
    by a replay of the screen and scrollback as tmux renders them (taken
    through the same control client, so nothing is lost or repeated between
    the replay and the live output). Input is typed with [send-keys -H].

    The session has [destroy-unattached] set and the control client is its
    only client, so the session dies with it: if the backend exits or is
    killed, the pipe closes, the client exits and the shell goes away. *)

type t

module Socket : sig
  (** A tmux server: [-L name] or [-S path]. *)
  type t =
    | Name of string
    | Path of string
end

(** Starts a session called [name] on the tmux server at [socket],
    running [command] (default: the user's shell) in [cwd]. *)
val create
  :  env:Env.t
  -> sw:Switch.t
  -> tmux:string
  -> socket:Socket.t
  -> name:string
  -> cwd:string
  -> cols:int
  -> rows:int
  -> ?command:string list
  -> unit
  -> t Or_error.t

val name : t -> string

module Viewer : sig
  type t
end

(** Resizes the terminal to [cols]x[rows], then calls [on_output] with the
    replay and afterwards with every chunk of output. [on_output] runs in the
    terminal's reader and must not block. *)
val attach : t -> cols:int -> rows:int -> on_output:(string -> unit) -> Viewer.t

val detach : t -> Viewer.t -> unit
val viewers : t -> int
val input : t -> string -> unit
val resize : t -> cols:int -> rows:int -> unit
val is_alive : t -> bool

(** Resolved once the session has ended (the shell exited or it was killed). *)
val exited : t -> unit Promise.t

(** Ends the session and waits for it to be gone. *)
val kill : t -> unit

module For_testing : sig
  (** The bytes that recreate a pane: [modes] is the [display-message] output
      for {!mode_format}, [screen] the [capture-pane] lines and [saved] those
      of the normal screen while an application has the alternate one. *)
  val replay : modes:string -> screen:string list -> saved:string list -> string

  val mode_format : string
end
