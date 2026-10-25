open! Core

(** The fixed zones of the screen, top to bottom: the main area (the
    optional agent sidebar beside the transcript, overlays drawn over the
    transcript's bottom rows), the composer's rule, the editor and the status
    line. Only the editor's height moves the others. Pure. *)

type t =
  { width : int
  ; height : int
  ; sidebar : int
    (** columns of the sidebar, its border included; 0 if hidden *)
  ; main_rows : int
  ; main_width : int (** the transcript's columns *)
  ; rule_row : int
  ; editor_rows : int
  ; status_row : int
  }
[@@deriving sexp_of]

(** The narrowest terminal that shows the sidebar. *)
val sidebar_threshold : int

(** The sidebar's columns, its border included. *)
val sidebar_width : int

(** The most rows the editor takes before it scrolls. *)
val editor_cap : height:int -> int

(** [sidebar] is whether the user wants it; it shows only at
    [sidebar_threshold] columns and more. [editor_rows] is the editor's
    wrapped rows, capped here. *)
val compute : width:int -> height:int -> sidebar:bool -> editor_rows:int -> t
