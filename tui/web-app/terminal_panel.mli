open! Core
open Bonsai_web

(** A shell on the backend (its [/terminal] WebSocket, see
    backend/lib/terminals.mli) drawn by xterm.js, which [web-bin/terminal.js]
    wires up. Closing the panel only detaches: the backend keeps the shell for a
    while, so reopening it in the same session finds it again. *)

(** The [/terminal] URL on the same server as the RPC WebSocket [backend]. *)
val url
  :  backend:string
  -> user:string option
  -> token:string option
  -> session:string option
  -> string

val view : url:string -> on_close:unit Effect.t -> Vdom.Node.t
val open_button : on_click:unit Effect.t -> Vdom.Node.t

(** Whether a DOM event happened inside the panel, where keys, pastes and
    scrolling belong to the terminal rather than the app. *)
val contains : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> bool
