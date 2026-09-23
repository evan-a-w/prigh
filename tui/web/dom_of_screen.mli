open! Core

(** A [Screen.t] as a monospace cell grid: one [div.line] per row, spans with
    style classes ([fg-red], [bold], ...), links as anchors and the cursor cell
    wrapped in [span.cursor]. *)
val screen : Prigh_ui.Screen.t -> Virtual_dom.Vdom.Node.t

(** The class names a style maps to; for the stylesheet and tests. *)
val classes : Prigh_ui.Style.t -> string list
