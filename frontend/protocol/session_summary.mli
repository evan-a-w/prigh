open! Core

type t =
  { id : string
  ; path : string
  ; name : string option
  ; description : string option
  ; cwd : string
  ; created_at : string
  ; updated_at : string option
  ; first_prompt : string option
  ; message_count : int
  ; parent : string option
  ; live : bool (** loaded in the backend *)
  ; running : bool (** its turn, or background subagents or jobs *)
  ; background : int (** running background subagents and jobs *)
  ; clients : int (** frontends attached to it *)
  ; waiting : int (** confirmations and questions waiting for the user *)
  ; failed : bool (** its last reply ended in an error *)
  ; archived : bool
  ; bytes : int (** on disk *)
  ; host : string option (** the tool host its cwd is on, when recorded *)
  ; repo : Repo.t option
    (** where its cwd is in git; [None] outside a repository, or unknown *)
  ; worktrees : Session_worktree.t list
    (** those that matter to it, not removed yet, by when they first did *)
  ; home : Session_home.t option
    (** where a worktree switch took it from, for [/worktree back] *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** One line for lists: the description if there is one, else the first prompt,
    else ["(empty)"]. *)
val blurb : t -> string

(** ["512 B"], ["3.4 KB"], ["12.0 MB"]: for confirmations. *)
val size_string : int -> string

val total_bytes : t list -> int
