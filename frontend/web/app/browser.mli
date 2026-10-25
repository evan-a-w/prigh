open! Core

(** The few browser facilities the platform needs; only callable in a page. *)

val get_item : string -> string option
val set_item : string -> string -> unit
val remove_item : string -> unit
val query_param : string -> string option

(** [ws(s)://<this page's host>/ws]. *)
val same_origin_ws_url : unit -> string

val copy_to_clipboard : string -> unit

(** Runs [f] once the DOM has been patched for the state changes made so far,
    in the same frame, before the browser paints it. *)
val after_render : (unit -> unit) -> unit

(** Runs what [after_render] holds: the page calls it after each render. *)
val rendered : unit -> unit

(** Runs [f] whenever the window or visible viewport resizes or moves. *)
val on_viewport_change : (unit -> unit) -> unit

val href_with_backend
  :  pathname:string
  -> search:string
  -> backend:string
  -> string

(** [search] with [name] set to [value] (replacing any previous value). *)
val with_query_param : search:string -> string -> string -> string

(** [search] without [name] (["?a=1&b=2"], or [""] when nothing is left). *)
val without_query_param : search:string -> string -> string

(** Loads this page again without the query parameter [name]. *)
val reload_without_query_param : string -> unit

(** Sets a query parameter of the page's URL without reloading or adding a
    history entry. *)
val replace_query_param : string -> string -> unit

val set_app_html : string -> unit
val input_value : string -> string
