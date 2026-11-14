open! Core
open! Import

(** The idle composer's placeholder when the session has a place to work:
    "Working in [~/projects/prigh] on [main]. Ask prigh anything…", with
    "on HOST" first when its tools run elsewhere than where they would by
    default. *)

type t

(** [None] before the session has a cwd, and for temporary sessions. *)
val of_model : App.Model.t -> t option

(** The whole sentence as plain text, the path in full: the editor's
    placeholder (for screen readers) and tooltip. *)
val sentence : t -> string

(** What the empty editor shows (hidden from screen readers): the sentence on
    one line in a little over half the editor's width (on a phone, as much
    as "Working in …/prigh on main." needs, up to all of it), as long as it
    fits, else with the path cut to its last two,
    then one, components, then without "Ask prigh anything…", then cut with
    an ellipsis. *)
val view : t -> Vdom.Node.t
