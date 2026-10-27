open! Core

(** Scripts and stylesheets that the page loads the first time it needs them
    (xterm.js for the terminal, KaTeX for math). *)

type t

(** [scripts] load one after the other, after [css] is linked. *)
val create : css:string list -> scripts:string list -> t

(** Calls [k] once [t] has loaded ([Error src] if [src] failed, after which
    the next [load] tries again); at once if it has. *)
val load : t -> ((unit, string) Result.t -> unit) -> unit

val loaded : t -> bool
