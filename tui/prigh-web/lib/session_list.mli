open! Core
open! Import

(** The sidebar's view of [list_sessions]. *)

(** The name, else the description or first prompt. *)
val title : Session_summary.t -> string

(** The sessions matching [query] (title, first prompt, cwd), best first; all of
    them, in the backend's order, when it is empty. *)
val filter : Session_summary.t list -> query:string -> Session_summary.t list
