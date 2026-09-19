open! Core

(** Multi-line text editor state with history. Pure. *)

module Position : sig
  type t =
    { line : int
    ; col : int (** in scalar values, not bytes *)
    }
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of]

val empty : t
val text : t -> string
val lines : t -> string list
val position : t -> Position.t
val is_empty : t -> bool
val set_text : t -> string -> t
val clear : t -> t
val insert : t -> string -> t
val newline : t -> t
val backspace : t -> t
val delete : t -> t
val left : t -> t
val right : t -> t

(** [None] when already on the first/last line, so the caller may use history. *)
val up : t -> t option

val down : t -> t option
val home : t -> t
val end_ : t -> t
val kill_to_end : t -> t
val kill_line : t -> t
val kill_word : t -> t

(** Returns the text and the editor reset; records history unless [secret]. *)
val submit : ?secret:bool -> t -> string * t

val history_prev : t -> t option
val history_next : t -> t option
