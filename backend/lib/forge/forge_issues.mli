open! Core
open! Import

(** Issues and the conversation comments of issues and pull requests (the
    same endpoints on both hosts, which also number pull requests among
    issues). Where the hosts differ: GitHub's issue list also has pull
    requests (left out here), Forgejo takes label ids rather than names
    (looked up), GitHub has a close reason, assignees are added and
    removed on GitHub but set as a whole on Forgejo. *)

module Issue : sig
  type t =
    { number : int
    ; title : string
    ; state : string (** [open] or [closed] *)
    ; state_reason : string option (** GitHub's [completed], [not_planned] *)
    ; author : string
    ; labels : string list
    ; assignees : string list
    ; comments : int
    ; url : string
    ; body : string
    ; created_at : string
    ; updated_at : string
    ; is_pull : bool
    }
  [@@deriving sexp_of]

  val of_json : Json.t -> t Or_error.t
  val to_json : t -> Json.t
end

module Comment : sig
  type t =
    { id : int
    ; author : string
    ; body : string
    ; created_at : string
    ; url : string
    }
  [@@deriving sexp_of]

  val of_json : Json.t -> t Or_error.t
  val to_json : t -> Json.t
end

module Filter : sig
  type t =
    { state : string (** [open], [closed] or [all] *)
    ; labels : string list
    ; assignee : string option
    ; author : string option
    ; search : string option
    ; limit : int
    }

  val default : t
end

(** Newest first, without pull requests. *)
val list : Forge_api.t -> Forge_repo.t -> Filter.t -> Issue.t list Or_error.t

val get : Forge_api.t -> Forge_repo.t -> int -> Issue.t Or_error.t

(** The conversation, oldest first. *)
val comments
  :  ?limit:int
  -> Forge_api.t
  -> Forge_repo.t
  -> int
  -> Comment.t list Or_error.t

val create
  :  Forge_api.t
  -> Forge_repo.t
  -> title:string
  -> body:string
  -> labels:string list
  -> assignees:string list
  -> Issue.t Or_error.t

(** On an issue or a pull request. *)
val comment
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> body:string
  -> Comment.t Or_error.t

val edit
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> title:string option
  -> body:string option
  -> Issue.t Or_error.t

(** [state]: [open] or [closed]; [reason] ([completed], [not_planned]) is
    GitHub's and ignored by Forgejo. *)
val set_state
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> state:string
  -> reason:string option
  -> Issue.t Or_error.t

val add_labels
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> string list
  -> Issue.t Or_error.t

val remove_labels
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> string list
  -> Issue.t Or_error.t

val assign
  :  Forge_api.t
  -> Forge_repo.t
  -> int
  -> add:string list
  -> remove:string list
  -> Issue.t Or_error.t
