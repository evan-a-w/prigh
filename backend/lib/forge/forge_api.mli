open! Core
open! Import

(** The few REST calls prigh makes on GitHub ([api.github.com], or
    [HOST/api/v3]) and Forgejo/Gitea ([HOST/api/v1]). Failures say what
    happened and what to do (sign in again, push the branch). *)

type t =
  { http : Forge_http.t
  ; instance : Forge_instance.t
  ; authorization : string (** the [Authorization] header *)
  ; cancel : Cancellation.t
  }

val create
  :  ?cancel:Cancellation.t
  -> http:Forge_http.t
  -> Forge_instance.t
  -> Forge_credential.t
  -> t

(** The signed-in account's login. *)
val current_user : t -> string Or_error.t

(** [state]: [open], [closed] or [all]; newest first. *)
val list_pulls
  :  t
  -> Forge_repo.t
  -> state:string
  -> Pull_request.t list Or_error.t

val get_pull : t -> Forge_repo.t -> int -> Pull_request.t Or_error.t

(** The whole unified diff, cut at {!max_diff_bytes} (then [true]). *)
val pull_diff : t -> Forge_repo.t -> int -> (string * bool) Or_error.t

val max_diff_bytes : int
val checks : t -> Forge_repo.t -> sha:string -> Pr_checks.t Or_error.t
val default_branch : t -> Forge_repo.t -> string Or_error.t

(** A draft on Forgejo is a [WIP:] title. *)
val create_pull
  :  t
  -> Forge_repo.t
  -> title:string
  -> body:string
  -> head:string
  -> base:string
  -> draft:bool
  -> Pull_request.t Or_error.t
