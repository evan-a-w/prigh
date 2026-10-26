open! Core

(** The terminal panel's address and extent; [Terminal_widget] draws it. *)

(** The [/terminal] URL (a shell on the backend, see
    backend/lib/terminal/terminals.mli) on the same server as the RPC
    WebSocket [backend], with the credentials as in [hello]; [dir]: a
    shell of its own there. *)
val url
  :  backend:string
  -> user:string option
  -> as_user:string option
  -> token:string option
  -> session:string option
  -> dir:string option
  -> string

(** Whether a DOM event happened inside the panel, where keys, pastes and
    scrolling belong to the terminal rather than the app. *)
val contains : Js_of_ocaml.Dom_html.element Js_of_ocaml.Js.t -> bool
