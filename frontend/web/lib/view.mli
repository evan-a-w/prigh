open! Core
open! Import

(** The page, from the model. [terminal] is the page's xterm.js widget for the
    terminal panel, [code_editor] its CodeMirror for the editor mode. *)
val view
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> terminal:(Terminal.Target.t -> Node.t)
  -> code_editor:(Code_editor_view.Input.t -> Node.t)
  -> Node.t
