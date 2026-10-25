open! Core
open! Import

(** The git hosts the user can sign in to (GitHub, Forgejo and Gitea), as
    [forge_status] lists them, and the hosts being signed out of. They belong
    to the user: switching user starts over. *)

module Accounts : sig
  type t =
    | Loading
    | Failed of string (** e.g. a backend without git hosting *)
    | Loaded of Forge_account.t list
  [@@deriving sexp_of, equal]
end

type t =
  { accounts : Accounts.t
  ; signing_out : String.Set.t (** hosts *)
  }
[@@deriving sexp_of, equal]

val empty : t
val list : t -> Forge_account.t list
val find : t -> string -> Forge_account.t option

(** [GitHub (github.com)]. *)
val name : Forge_account.t -> string

(** Who is signed in and how, or that nobody is. *)
val describe : Forge_account.t -> string

module Login : sig
  (** What [/login ARGS] signs in to. *)
  type t =
    | Provider of
        { provider : string
        ; method_ : string option
        }
    | Forge of
        { host : string
        ; kind : string option
        ; method_ : string option (** [token] or [oauth] *)
        ; again : string (** the [/login] arguments that try again *)
        }
    | Ask_host (** [/login forgejo]: which host? *)
    | Bad of string (** the usage *)
  [@@deriving sexp_of, equal]
end

(** [/login github [HOST] [token]], [/login forgejo HOST [token]], [/login
    HOST [token]] for a known host; anything else is a provider. *)
val parse_login : t -> string list -> Login.t

(** The host [/logout ARG] signs out of, if it names a git host ([github]
    for github.com). *)
val logout_host : t -> string -> string option

(** [https://git.example.com/] as [git.example.com]. *)
val normalize_host : string -> string

(** The [/login] picker's items: [forge HOST] ids (and [forge-host] for
    another Forgejo host). *)
val login_items : t -> Picker.Item.t list

(** The signed-in hosts, as [forge HOST] ids. *)
val logout_items : t -> Picker.Item.t list
