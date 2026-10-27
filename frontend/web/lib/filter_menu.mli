open! Core
open! Import

(** The transcript filter's menu, under its button ([/filter]): a row per
    kind, checked when on show, then "Show all". What it toggles shows at
    once; Esc puts back the filter it opened with, Enter or a click outside
    keeps it. *)

module Row : sig
  type t =
    | Kind of Transcript_filter.Kind.t
    | Show_all
  [@@deriving sexp_of, equal]

  (** In the menu's order. *)
  val all : t list
end

type t =
  { original : Transcript_filter.t (** what Esc puts back *)
  ; selected : int (** in [Row.all] *)
  }
[@@deriving sexp_of, equal]

val create : Transcript_filter.t -> t
val move : t -> int -> t
val selected : t -> Row.t
val select : t -> Row.t -> t

(** What pressing [row] (a click, Space) makes of the filter. *)
val press : Row.t -> Transcript_filter.t -> Transcript_filter.t

(** The id of the button it opens under. *)
val anchor : string
