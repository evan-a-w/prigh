open! Core
open! Import

(** The open dialog (pickers, help, rename, delete, login, providers,
    background work) and, above it, the oldest tool confirmation (its
    answers, each with its key, or the reason being typed), else the oldest
    [ask_user] question. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
