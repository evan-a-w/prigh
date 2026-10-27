open! Core
open! Import

(** TeX math as KaTeX draws it: [span.math.math-inline], or
    [span.math.math-display] (a block that scrolls sideways when too wide).
    Until KaTeX has loaded, and in tests, the TeX as text; TeX that KaTeX
    rejects stays text, as [.math-error], with KaTeX's message as its
    title. *)
val render : display:bool -> string -> Node.t
