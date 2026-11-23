open! Core
open! Import

(** The session's name, else its description or first prompt, else
    ["New session"]. *)
val of_state : App.Model.t -> State.t -> string
