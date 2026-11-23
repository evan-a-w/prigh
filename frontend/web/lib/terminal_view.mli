open! Core
open! Import

(** The terminal panel: a header saying where the shell runs, with Close and
    (after it exited or failed) New shell or Retry, around [widget], the page's
    xterm.js for the target. Docked under the chat; a sheet on a phone. *)
val view
  :  App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> widget:(Terminal.Target.t -> Node.t)
  -> Node.t
