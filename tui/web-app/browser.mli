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

(** Columns and rows of the monospace grid that fit the visible viewport (which
    excludes the on-screen keyboard). *)
val grid_size : unit -> int * int

(** Pixel height of one grid row. *)
val cell_height : unit -> float

(** Sizes and positions [#root] to cover exactly the visible viewport. *)
val fit_root : unit -> unit

(** Runs [f] whenever the window or visible viewport resizes or moves. *)
val on_viewport_change : (unit -> unit) -> unit

val href_with_backend
  :  pathname:string
  -> search:string
  -> backend:string
  -> string

val reload_with_backend : string -> unit
val set_app_html : string -> unit
val input_value : string -> string
