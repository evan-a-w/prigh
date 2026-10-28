open! Core
open! Import

(** The session tabs ([Session_tabs]) over the chat, when on and not on a
    phone: each says its session's title and how it stands (a lamp while it
    runs; a mark when it waits for you, failed, or finished out of sight).
    A click opens it; ×, a middle click or Alt+W closes it; dragging moves
    it; [+] starts a new session. [None] when off: nothing in the page. *)
val view
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t option

(** The DOM id of a session's tab. *)
val dom_id : string -> string
