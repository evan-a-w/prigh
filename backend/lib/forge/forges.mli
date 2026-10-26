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

(** The [pr_*] methods' replies. *)
val list_pulls : t -> git:git -> state:string -> Json.t Or_error.t

val view_pull : t -> git:git -> int -> Json.t Or_error.t
val pull_defaults : t -> git:git -> Json.t Or_error.t

val create_pull
  :  t
  -> git:git
  -> title:string
  -> body:string
  -> base:string option
  -> head:string option
  -> draft:bool
  -> push:bool
  -> Json.t Or_error.t

val checkout_pull
  :  t
  -> git:git
  -> worktrees_dir:string
  -> int
  -> Json.t Or_error.t
