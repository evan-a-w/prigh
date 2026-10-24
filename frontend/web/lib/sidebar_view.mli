open! Core
open! Import

(** Sessions: search, new, the list (current highlighted, live and running
    marked, age, cwd, size), delete; the signed-in user and sign out. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** [/home/USER/x] as [~/x]. *)
val home_relative : string -> string
