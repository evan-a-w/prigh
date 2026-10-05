open! Core
open Js_of_ocaml
open Bonsai_web

(** The terminal panel's xterm.js ([web-bin/terminal.js] and the vendored
    xterm.js, loaded on first use) as a widget. A new target's key replaces
    the shell's connection; the old one only detaches (the backend keeps the
    shell). *)

(** Connects to [url] (see [Prigh_ui_web_app.Terminal_panel.url]) and reports
    the connection's state for the target with [key]. *)
val view
  :  url:string
  -> key:string
  -> on_status:(Prigh_web.Terminal.Status.t -> unit Effect.t)
  -> Vdom.Node.t

(** Focuses the shell once it is on the page. *)
val focus : unit -> unit

(** Whether the panel is open, for [install] after a reload. *)
val remember : bool -> unit

(** The panel's top edge resizes it. Restores the height it was given and
    reopens it if it was open before the page reloaded. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit

(** Whether an event happened in the panel, whose keys, pastes and drops are
    the shell's. *)
val contains : Dom_html.element Js.t -> bool

(** Whether a [MutationObserver]'s records are all inside the panel (xterm.js
    redrawing), which other observers can ignore. *)
val only_inside : MutationObserver.mutationRecord Js.t Js.js_array Js.t -> bool
