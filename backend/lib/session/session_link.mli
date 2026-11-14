open! Core

(** Links to sessions that the frontends follow: prigh-web switches to the
    session in place, the TUI shows the id that [/switch] takes. The
    canonical form is relative, [?session=ID], so it works on whatever
    address the user reaches prigh-web at. *)

(** [?session=ID] *)
val href : string -> string

(** [\[title\](?session=ID)]: [title] with its brackets made parentheses
    and on one line. *)
val markdown : id:string -> title:string -> string
