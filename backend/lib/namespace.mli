open! Core
open! Import

(** A token namespace of [prigh serve -tokens]: a completely separate world
    (sessions, credentials, config, global AGENTS.md, tool hosts) reached by
    presenting [token] in [hello]. *)

type t =
  { name : string
  ; token : string
  }
[@@deriving sexp_of]

(** Comma-separated [name=token] entries; names are [[A-Za-z0-9_-]+] and
    names and tokens are unique. *)
val parse_spec : string -> t list Or_error.t

module World : sig
  (** Everything a server derives from the user's home. *)
  type t =
    { home : string (** for config, sessions, global AGENTS.md *)
    ; sessions_dir : string
    ; store : Auth_store.t
    ; getenv : string -> string option
      (** what provider credentials are looked up with *)
    }

  (** The single-world layout: [home], [auth_file], and the environment. *)
  val legacy : home:string -> auth_file:string -> t
end

(** [<home>/.prigh/namespaces/<name>] (created), except that [default] is
    [home] itself with [legacy_auth_file], so a single-token deployment keeps
    its data. Provider API keys never come from the environment. *)
val world : t -> home:string -> legacy_auth_file:string -> World.t

(** [getenv] without the providers' API key variables. *)
val without_provider_keys : (string -> string option) -> string -> string option
