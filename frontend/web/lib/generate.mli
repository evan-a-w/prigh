open! Core
open! Import

(** Drafting a field's text with the model ([generate_text]): the ✦
    Generate button in the corner of every description field (a pull
    request's title and body, the review's commit message, a new worktree's
    branch, the session's name). One draft at a time streams into its field;
    Stop (or Esc) keeps what came, Undo puts back the text from before. *)

module Target : sig
  type t =
    | Pr (** the create-pull-request dialog: title and body *)
    | Commit_message (** the review page's commit box *)
    | Worktree_branch (** the new-worktree dialog's branch *)
    | Session_title (** the rename dialog *)
  [@@deriving sexp_of, equal]

  val kind : t -> Generated.Kind.t

  (** What the button's tooltip says it drafts from. *)
  val source : t -> string
end

module Running : sig
  type t =
    { id : string (** the request's [gen_id] *)
    ; target : Target.t
    ; text : string (** streamed so far *)
    ; before : string list (** the field's text before (PR: title, body) *)
    }
  [@@deriving sexp_of, equal]
end

module Undo : sig
  (** What the last draft replaced. *)
  type t =
    { target : Target.t
    ; before : string list
    }
  [@@deriving sexp_of, equal]
end

type t =
  { running : Running.t option
  ; undo : Undo.t option
  ; seq : int
  }
[@@deriving sexp_of, equal]

val empty : t

(** A new draft for [target], whose field holds [before]: its id. A draft
    on its way is replaced (cancel it: [stop] first). *)
val start : t -> Target.t -> before:string list -> t * string

(** A streamed piece: the text so far, when it is the running draft's. *)
val delta : t -> id:string -> string -> t * (Target.t * string) option

(** The draft [id] came back (or failed): Undo can put [before] back. *)
val finish : t -> id:string -> t * Running.t option

(** Stops the running draft, keeping what came: its id, to cancel. *)
val stop : t -> t * string option

val running_for : t -> Target.t -> bool

(** The text [target]'s field had before its last draft. *)
val undo_for : t -> Target.t -> string list option

(** Forgets the undo (the field was closed or used). *)
val forget : t -> Target.t -> t
