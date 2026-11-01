open! Core

(** A phone's drags, by touch only (a mouse and the keyboard do as before),
    each locked to the way it started so that it never fights scrolling:

    - from the left edge, right: the drawer comes out with the finger, and
      opens if it went far enough or fast; dragged left (anywhere in it but a
      session's row), it goes back;
    - in a page of a dialog with a back button (a settings section), from
      the left edge, right: back;
    - a sheet (a chip's menu, the ⋯ menu, a context menu...), down from its
      top while its list is at the top: it follows and closes;
    - a session's row in the drawer, left: its Archive and Delete show
      behind it (right, or a tap elsewhere, hides them);
    - a pending image's tile, up: it is removed.

    The image viewer has its own ({!Viewer_widget}). *)
val install : schedule:(Prigh_web.App.Action.t -> unit) -> unit
