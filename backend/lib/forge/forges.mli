open! Core
open! Import

(** A user's git hosts: their sign-ins (in the [Auth_store], see
    {!Forge_credential}), settings ({!Forge_instance}), and the pull request
    operations for the repository of a session's directory, whose git runs
    on the session's tool host ({!Git_forge}). *)

module Account : sig
  type t =
    { instance : Forge_instance.t
    ; credential : Forge_credential.t option
    }
  [@@deriving sexp_of]

  (** [forge_status]'s entries: [{host, kind, label, user, method,
      expires_ms, oauth, oauth_note}]. *)
  val to_json : t -> Json.t
end

type t

val create
  :  http:Forge_http.t
  -> sleep:(float -> unit)
  -> ?listen:Forge_login.Listen.t
  -> ?getenv:(string -> string option)
  -> ?home:string (** whose [config.json] has the [forges] settings *)
  -> ?now_ms:(unit -> int)
  -> store:Auth_store.t
  -> unit
  -> t

(** github.com, codeberg.org, the configured hosts and the signed-in ones. *)
val accounts : t -> Account.t list Or_error.t

(** [host] as the user typed it (a URL is fine). *)
val instance : t -> ?kind:Forge_kind.t -> string -> Forge_instance.t Or_error.t

(** Runs the sign-in and stores it. *)
val login
  :  t
  -> Forge_instance.t
  -> method_:Forge_credential.Method.t option
  -> Auth_interaction.t
  -> Forge_credential.t Or_error.t

val logout : t -> string -> unit Or_error.t

(** The API as the signed-in user, refreshing an expiring OAuth token
    under the store's lock. *)
val api
  :  t
  -> ?cancel:Cancellation.t
  -> Forge_instance.t
  -> Forge_api.t Or_error.t

(** Runs a {!Git_forge} request on the session's tool host. *)
type git = Git_forge.Request.t -> Tool_result.t

(** The repository the session is in: the current branch's remote, else
    [origin], else the first remote on a known host. *)
val repo : t -> git:git -> (Forge_repo.t * Git_forge.Info.t) Or_error.t

module Pulls : sig
  type t =
    { repo : Forge_repo.t
    ; prs : Pull_request.t list
    }
  [@@deriving sexp_of]

  (** [pr_list]'s reply: [{repo, prs}]. *)
  val to_json : t -> Json.t
end

module Pull : sig
  type t =
    { repo : Forge_repo.t
    ; pr : Pull_request.t
    ; checks : Pr_checks.t Or_error.t option
      (** [None]: not looked up (no head commit, or a new one) *)
    ; diff : (string * bool) option
      (** the unified diff, and whether it was cut; [None]: not fetched *)
    }
  [@@deriving sexp_of]

  (** [pr_view]'s and [pr_create]'s reply: [{repo, pr, checks?, diff,
      truncated}] ([checks] is [null] when they could not be had). *)
  val to_json : t -> Json.t
end

module Defaults : sig
  (** What a new pull request from the current branch would be. *)
  type t =
    { repo : Forge_repo.t
    ; head : string
    ; base : string
    ; title : string
    ; body : string
    ; commits : string list (** subjects, oldest first *)
    ; pushed : bool (** the remote has the branch with all its commits *)
    }
  [@@deriving sexp_of]

  val to_json : t -> Json.t
end

(** [state]: [open], [closed] or [all]. *)
val list_pulls : t -> git:git -> state:string -> Pulls.t Or_error.t

(** With its checks and (unless [diff] is [false]) its diff. *)
val view_pull : t -> git:git -> ?diff:bool -> int -> Pull.t Or_error.t

val pull_defaults : t -> git:git -> Defaults.t Or_error.t

val create_pull
  :  t
  -> git:git
  -> title:string
  -> body:string
  -> base:string option
  -> head:string option
  -> draft:bool
  -> push:bool
  -> Pull.t Or_error.t

val checkout_pull
  :  t
  -> git:git
  -> worktrees_dir:string
  -> int
  -> Git_forge.Checkout.t Or_error.t
