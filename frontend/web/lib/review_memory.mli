open! Core
open! Import

(** What the review page last compared, kept in the user's preferences
    ([Pref_sync.Key.review]) so it follows them from browser to browser:
    per reviewed worktree or branch, the base and whether directly; per
    session worktree, what it last reviewed. Without one, a worktree is
    compared with its default base (see [git_diff]). *)

module Compare : sig
  type t =
    { base : string option
    ; direct : bool
    }
  [@@deriving sexp_of, equal]
end

type t [@@deriving sexp_of, equal]

val empty : t

(** The preference's value; anything else is [empty]. *)
val of_json : Json.t option -> t

val to_json : t -> Json.t

(** What [reviewed] is remembered under, the session's worktree being
    [own] ([None] while unknown: nothing then). *)
val key : own:string option -> Git_diff.Target.t -> string option

val compare : t -> key:string -> Compare.t option

(** What the session worktree [own] reviewed last. *)
val last : t -> own:string -> Git_diff.Target.t option

(** Keeps the choice, the most recent first, at most 50 of each; the
    defaults (the session's own worktree, its default base) are forgotten
    instead. *)
val remember
  :  t
  -> own:string option
  -> Git_diff.Target.t
  -> Compare.t
  -> t
