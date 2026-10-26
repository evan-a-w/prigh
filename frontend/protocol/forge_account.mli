open! Core

(** One entry of [forge_status]: a git host prigh knows (github.com,
    codeberg.org, configured and signed-in ones) and whether the user is
    signed in to it. *)

type t =
  { host : string (** e.g. [github.com], [codeberg.org] *)
  ; kind : string (** [github] or [forgejo] (Forgejo and Gitea) *)
  ; label : string (** [GitHub], [Forgejo] *)
  ; user : string option (** the account's login, when signed in *)
  ; method_ : string option (** [oauth] or [token]; [None]: signed out *)
  ; expires_ms : Int64.t option (** an OAuth token's expiry, if it has one *)
  ; oauth : bool (** whether [forge_login] can use OAuth for this host *)
  ; oauth_note : string option
    (** why OAuth is unavailable and how to enable it, or what its consent
        page will say *)
  }
[@@deriving sexp_of, equal]

val signed_in : t -> bool

(** The [/login] argument that signs in to this host: [github] for
    github.com, otherwise [forgejo HOST] or [github HOST]. *)
val login_arg : t -> string

val of_json : Json.t -> t Or_error.t

(** [forge_status]'s reply: [{accounts: [...]}]. *)
val list_of_json : Json.t -> t list Or_error.t
