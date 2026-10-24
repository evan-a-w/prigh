open! Core

(** Two aligned columns for [/help]: the bold thing to type, then what it does. *)
val render : (string * string) list -> Content.t
