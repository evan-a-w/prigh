open! Core
open! Import

(** A [table] component: formatted cells, sortable by a click on a column's
    header (numbers first descending, text ascending; again reverses).
    Wide tables scroll sideways inside their frame, long ones down under a
    sticky header. *)

val view : Ui_context.t -> path:string -> Ui_spec.Table.t -> Node.t

(** [cell]'s text in [column]'s format ([—] for [null]). *)
val format : Ui_spec.Table.Column.t -> Ui_spec.Cell.t -> string

(** [rows] in the order the header's sort state [sort] ([KEY:asc] or
    [KEY:desc]) says; nulls last either way. *)
val sort
  :  Ui_spec.Table.t
  -> sort:string option
  -> (string * Ui_spec.Cell.t) list list
