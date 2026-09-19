open! Core

(** The single key-binding table. [/help] renders it; a test checks that every
    binding is exercised. *)

module Binding : sig
  type t =
    { keys : Key.t list
    ; intent : Intent.t
    ; help : string
    }
end

val bindings : Binding.t list

(** Printable characters (without Ctrl/Alt) become [Insert]. *)
val lookup : Key.t -> Intent.t option

val help : Content.t
