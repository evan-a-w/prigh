open! Core

(** Dragging the panels' resize handles ([Prigh_web.Resize_view]): pointer
    events on the document, the size shown on the panel's element as a CSS
    variable at each animation frame, without the page re-rendering, and
    committed as [Resize (Set _)] when the pointer is released (Esc puts it
    back). Also the sizes kept in local storage and the window's size. *)

(** The listeners; the kept sizes and the window's size are scheduled. *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit

(** The window's size, as [Resize (Viewport _)]. *)
val viewport : unit -> Prigh_web.App.Action.t

val save : Prigh_web.Resize.Save.t -> unit
