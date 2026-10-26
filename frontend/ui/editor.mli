open! Core

(** Multi-line text editor state with history, a kill ring and undo. Pure. *)

module Position : sig
  type t =
    { line : int
    ; col : int (** in scalar values, not bytes *)
    }
  [@@deriving sexp_of, equal]

  val compare : t -> t -> int
end

module Chip : sig
  type t =
    { start : Position.t
    ; stop : Position.t
    }
  [@@deriving sexp_of]

  val lines : t -> int
  val contains : t -> Position.t -> bool
end

type t [@@deriving sexp_of]

val empty : t
val text : t -> string
val lines : t -> string list
val position : t -> Position.t
val is_empty : t -> bool
val kill_ring : t -> string list
val chips : t -> Chip.t list
val set_text : t -> string -> t

(** [set_text] as an edit: {!undo} brings back the text it replaced. *)
val replace_text : t -> string -> t

val set_history : t -> string list -> t
val clear : t -> t
val insert : t -> string -> t

(** Replaces the [len] characters at [at] (on one line) with the string, the
    cursor after it; one undo step. *)
val replace : t -> at:Position.t -> len:int -> string -> t

val newline : t -> t
val backspace : t -> t
val delete : t -> t
val left : t -> t
val right : t -> t
val word_left : t -> t
val word_right : t -> t
val word_start : t -> Position.t
val goto : t -> Position.t -> t

(** [None] when already on the first/last line, so the caller may use history. *)
val up : t -> t option

val down : t -> t option
val home : t -> t
val end_ : t -> t
val kill_to_end : t -> t
val kill_to_start : t -> t
val kill_word : t -> t
val delete_word_forward : t -> t
val yank : t -> t
val yank_pop : t -> t
val undo : t -> t

(** Inserts a bracketed paste and chips blocks longer than three lines. *)
val insert_paste : t -> string -> t

(** Returns the text and the editor reset; records history unless [secret]. *)
val submit : ?secret:bool -> t -> string * t

(** The prompts submitted or set, most recent first. *)
val history : t -> string list

(** Whether Up has gone into the history (until submitted or cleared). *)
val browsing_history : t -> bool

val history_prev : t -> t option
val history_next : t -> t option
