open! Core

(** The few browser facilities the platform needs; only callable in a page. *)

val get_item : string -> string option
val set_item : string -> string -> unit
val remove_item : string -> unit
val query_param : string -> string option

(** [ws(s)://<this page's host>/ws]. *)
val same_origin_ws_url : unit -> string

val open_url : string -> unit
val copy_to_clipboard : string -> unit

(** Columns and rows of the monospace grid that fit the window. *)
val grid_size : unit -> int * int

val reload : unit -> unit
val set_app_html : string -> unit
val input_value : string -> string
