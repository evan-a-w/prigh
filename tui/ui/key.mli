open! Core

(** Platform-neutral key event. Terminal and browser events both map here. *)

module Code : sig
  type t =
    | Escape
    | Enter
    | Tab
    | Backspace
    | Delete
    | Insert
    | Home
    | End
    | Up
    | Down
    | Left
    | Right
    | Page_up
    | Page_down
    | Function of int
    | Char of string (** one UTF-8 encoded scalar value *)
  [@@deriving sexp_of, equal, compare]
end

type t =
  { code : Code.t
  ; ctrl : bool
  ; alt : bool
  ; shift : bool
  }
[@@deriving sexp_of, equal, compare]

val plain : Code.t -> t
val ctrl : char -> t
val alt : Code.t -> t
val char : char -> t

(** ["Ctrl+C"], ["Alt+Enter"], ["PageUp"], ... *)
val to_string : t -> string
