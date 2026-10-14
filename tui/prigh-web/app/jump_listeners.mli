open! Core
open Js_of_ocaml

(** The page's side of [Jump_view]: the rail's mark of the landmark in view
    ([aria-current], kept as the chat scrolls), and finding and marking for
    a moment the entry a jump went to. *)

val install : unit -> unit

(** The main transcript's entry [n] ([data-entry]), once rendered. *)
val entry : Dom_html.element Js.t -> int -> Dom_html.element Js.t option

(** Marks the entry jumped to for a moment ([.jumped]), and the rail's mark
    for it. *)
val arrived : Dom_html.element Js.t -> unit
