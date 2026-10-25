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
  ; archived : bool
  ; bytes : int (** on disk *)
  ; host : string option (** the tool host its cwd is on, when recorded *)
  ; repo : Repo.t option
    (** where its cwd is in git; [None] outside a repository, or unknown *)
  ; agent_worktrees : Agent_worktree.t list
    (** worktrees made for its subagents, not removed yet *)
  }
[@@deriving sexp_of, equal]

val of_json : Json.t -> t Or_error.t

(** One line for lists: the description if there is one, else the first prompt,
    else ["(empty)"]. *)
val blurb : t -> string

(** ["512 B"], ["3.4 KB"], ["12.0 MB"]: for confirmations. *)
val size_string : int -> string

val total_bytes : t list -> int
