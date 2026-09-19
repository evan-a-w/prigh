open! Core

(** A fully laid-out frame: exactly [height] lines, each at most [width]
    columns, plus the cursor cell. *)
type t =
  { lines : Content.t
  ; cursor : (int * int) option (** row, column *)
  ; width : int
  ; height : int
  }
[@@deriving sexp_of]

(** Plain text with trailing spaces trimmed and the cursor drawn as [▏] when
    [show_cursor]; for tests. *)
val to_plain : ?show_cursor:bool -> t -> string
