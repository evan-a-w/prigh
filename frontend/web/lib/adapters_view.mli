open! Core
open! Import

(** Settings → Engines: when the agent CLIs were checked, with Refresh; then
    a card per tool host, a row per adapter (version, logged in or not) and,
    for one that cannot run, what to do there, its command copyable. *)
val groups
  :  App.Model.t
  -> inject:(App.Action.t -> unit Vdom.Effect.t)
  -> Vdom.Node.t list

(** The command a problem says to run (between backquotes), if any. *)
val command_in : string -> string option
