open! Core
open! Import

(** The settings page ([Dialog.Settings], the top bar's gear, [/settings]):
    notifications and the transcript's detail in this browser, tool
    confirmation and the providers on the backend, and the account. Each
    control runs the command that changes it, so the page stays open. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
