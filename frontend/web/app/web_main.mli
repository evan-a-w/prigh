open! Core

(** Connects to the page's backend ([?backend=] or this origin's [/ws]) with the
    saved login, then runs the app in [#app]; shows a sign-in form when [hello]
    fails. *)
val run : unit -> unit
