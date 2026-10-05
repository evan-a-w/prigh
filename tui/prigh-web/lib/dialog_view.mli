open! Core
open! Import

(** The open dialog (pickers, help, rename, delete, login, providers,
    background work) and, above it, the oldest tool confirmation. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
