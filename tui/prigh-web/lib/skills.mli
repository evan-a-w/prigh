open! Core
open! Import

(** The skills ([list_skills]) where the session's tools run, for [/skill:]'s
    completion: fetched once per place, so a new session, directory, tool host
    or user fetches them again. *)

type t [@@deriving sexp_of]

val empty : t

(** Where skills are found: the session, its directory and its tool host;
    [None] before the state is known. *)
val place : State.t option -> string option

(** The skills listed for [place], once they have arrived. *)
val find : t -> place:string -> Skill.t list option

(** Neither listed nor asked for at [place]. *)
val unknown : t -> place:string -> bool

val requested : place:string -> t
val loaded : place:string -> Skill.t list -> t

(** Where to put skills, when there are none. *)
val none : string

(** [/skills]: name, description and directory. *)
val picker : ?query:string -> Skill.t list -> Dialog.t
