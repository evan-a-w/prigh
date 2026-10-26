open! Core
open! Import

(** One git host as prigh talks to it: its kind, web and API URLs and OAuth
    client. Hosts are configured in [~/.prigh/config.json] under [forges]:
    [{"forges": {"HOST": {"kind": "github"|"forgejo", "client_id": "...",
    "url": "https://HOST"}}}] (all optional), and [$PRIGH_GITHUB_CLIENT_ID]
    gives github.com's client id. *)

type t =
  { host : string (** e.g. [github.com], [codeberg.org], [git.corp:3000] *)
  ; kind : Forge_kind.t
  ; web_url : string (** no trailing slash *)
  ; client_id : string option
  ; default_client : bool
    (** [client_id] is Forgejo's pre-registered git-credential-oauth app,
        not one configured for prigh *)
  }
[@@deriving sexp_of]

(** The [forges] settings, by host. *)
module Settings : sig
  type t [@@deriving sexp_of]

  val empty : t
  val of_config_fields : (string * Json.t) list -> t Or_error.t

  (** {!Config.read_fields} under [home]; a broken file is no settings. *)
  val load : home:string -> t

  (** Configured hosts, in the file's order. *)
  val hosts : t -> string list
end

(** The kind comes from [kind], else the settings, else [stored] (the kind
    of a stored sign-in), else [Github] for github.com and [Forgejo] for
    anything else. *)
val resolve
  :  ?getenv:(string -> string option)
  -> ?kind:Forge_kind.t
  -> ?stored:Forge_kind.t
  -> Settings.t
  -> host:string
  -> t

val api_url : t -> string

(** Forgejo's pre-registered public client for git credential helpers
    (git-credential-oauth), on every stock Forgejo and Gitea instance. *)
val forgejo_default_client_id : string

(** A host as the user may type it: [https://codeberg.org/x] is
    [codeberg.org]. *)
val normalize_host : string -> string Or_error.t

(** How to sign in from the commands: [github] for github.com, else
    [forgejo HOST] or [github HOST]. *)
val login_arg : t -> string
