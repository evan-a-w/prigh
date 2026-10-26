open! Core
open! Import

(** The "New worktree" dialog: a branch (new, or an existing one to check
    out) and the base a new branch starts from (empty: HEAD). *)

module Field : sig
  type t =
    | Branch
    | Base
  [@@deriving sexp_of, equal]

  (** The field's element, for the focus. *)
  val id : t -> string
end

type t =
  { branch : string
  ; base : string
  ; head : string option (** the session's branch: what an empty base is *)
  ; project_dir : string option (** where it goes *)
  ; repo : string option
    (** the repository, from a project's row: a new session starts in the
        worktree; [None]: the session's, which moves into it *)
  ; busy : bool (** created: waiting for the backend *)
  ; error : string option
  }
[@@deriving sexp_of, equal]

val create
  :  ?repo:string
  -> head:string option
  -> project_dir:string option
  -> unit
  -> t

val set : t -> Field.t -> string -> t

(** The branch and base to create, or what to fix first. *)
val request : t -> (string * string option, string) Result.t

(** [/worktree new BRANCH [BASE]], as the request is typed. *)
val command : branch:string -> base:string option -> string
