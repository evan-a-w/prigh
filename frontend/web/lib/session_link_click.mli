open! Core
open! Import

(** A click on a link to one of prigh-web's sessions: a session link
    ({!Session_link_view}), or a link to this page with [?session=] (the
    full URL the model may write, or "Open in new window"'s), which the page
    follows in place rather than loading the page again. *)

type t =
  { id : string
  ; new_window : bool (** Ctrl/⌘/Shift+click or a middle click *)
  }
[@@deriving sexp_of]

(** [session]: the link's {!Session_link_view.id_attribute}; [href]: the
    absolute address its [href] property gives; [own_origin] and
    [own_path]: the page's [location.origin] and [location.pathname]. *)
val decide
  :  own_origin:string
  -> own_path:string
  -> href:string
  -> session:string option
  -> External_link.Click.t
  -> t option
