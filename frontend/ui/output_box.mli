open! Core

(** Long command output ([/help], [/diff], listings): a scrollable box over
    the transcript's bottom rows instead of transcript content. Pure. *)

type t =
  { title : string
  ; lines : Content.t
  ; top : int (** first body row shown, of the lines wrapped to the box *)
  }
[@@deriving sexp_of, equal]

val create : title:string -> Content.t -> t

(** The body wrapped to [width] (the box's inner width). *)
val wrapped : t -> width:int -> Content.t

(** Moves [by] rows (negative: up), staying within the body when [rows] are
    shown. *)
val scroll : t -> by:int -> width:int -> rows:int -> t

(** The rows shown, and the footer saying where they are when not all fit. *)
val window : t -> width:int -> rows:int -> Content.t * string
