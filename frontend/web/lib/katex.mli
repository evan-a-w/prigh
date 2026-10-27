open! Core

(** KaTeX (the site's katex.min.js, with copy-tex.min.js, which copies
    selected math as its TeX), loaded the first time the page shows math. *)

(** [render ~display tex]: KaTeX's HTML for [tex], or its error message;
    [None] until KaTeX has loaded. The HTML is safe to insert: [trust] is
    off (no [\href], [\url], [\includegraphics] or raw HTML) and macro
    expansion and sizes are bounded. *)
val render : display:bool -> string -> (string, string) Result.t option

val loaded : unit -> bool

(** Calls [k] once KaTeX has loaded; never if it fails to. *)
val when_loaded : (unit -> unit) -> unit
