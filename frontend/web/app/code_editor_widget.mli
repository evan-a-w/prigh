open! Core
open Js_of_ocaml
open Bonsai_web

(** The editor mode's CodeMirror ([bin/code_editor.js] and the vendored
    [codemirror.js], loaded the first time the editor opens) as a widget: it
    shows the input's file, takes its text when its revision changes, and
    reports edits, the selection and scrolling. *)

val view : Prigh_web.Code_editor_view.Input.t -> Vdom.Node.t

(** Done to the mounted editor (once it has loaded). *)
val command : Prigh_web.Code_editor.Command.t -> unit

(** Whether an element is in the editor, whose keys are its own but the
    page's shortcuts. *)
val contains : Dom_html.element Js.t -> bool
