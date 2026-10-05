open! Core
open! Import

(** The terminal panel: a shell on the session's active tool host, in the
    backend's [/terminal] WebSocket (see backend/lib/terminals.mli), drawn by
    xterm.js in the page. Pure: what the panel shows and what it connects to;
    the page mounts the widget for [Target.t]. *)

module Status : sig
  (** What the widget reports. *)
  type t =
    | Connecting
    | Connected
    | Reconnecting (** the socket dropped: trying again with backoff *)
    | Exited (** the shell ended: a key or "New shell" starts another *)
    | Failed of string (** the backend's error: it does not retry *)
  [@@deriving sexp_of, equal]
end

module Target : sig
  (** Which terminal the widget connects to: a different one is a new
      connection. *)
  type t =
    { session : string
    ; host : string (** the active host's id *)
    ; online : bool (** whether the active host is connected *)
    ; as_user : string option (** a superuser acting as another user *)
    ; generation : int (** bumped by "New shell" and "Retry" *)
    }
  [@@deriving sexp_of, equal]

  val key : t -> string
end

type t =
  { open_ : bool
  ; height : int option (** pixels, once dragged; the CSS default before *)
  ; generation : int
  ; status : (string * Status.t) option
    (** the latest report, for the target with this key *)
  }
[@@deriving sexp_of]

val closed : t
val min_height : int
val target : t -> State.t -> hello:Hello_reply.t option -> Target.t

(** The status of [target]'s connection. *)
val status : t -> Target.t -> Status.t

(** Where the shell runs: the active host's name and directory. *)
val where : State.t -> string * string

(** What to do about a [Failed] message. *)
val advice : string -> string
