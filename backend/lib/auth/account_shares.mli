open! Core
open! Import

(** Logins a user of [prigh serve -tokens] shares with other users. The
    grants live in the owner's auth.json ({!Account_meta.Grant}); this
    process-wide service holds every user's {!Auth_store.t} (the very ones
    their servers use, so a token refresh takes the owner's lock and writes
    the owner's file) and makes a recipient's request credentials from the
    owner's store, checking the grant on every request. Recipients never get
    the credential; the owner revokes by removing the grant. Only the
    built-in providers' logins can be shared. *)

type t

(** [enabled = false] ([-no-sharing]): nothing can be shared and nothing
    shared is usable. *)
val create : ?enabled:bool -> unit -> t

val enabled : t -> bool

(** A user's world: [store] must be the one their server uses, [home] keeps
    the usage of their shared logins ({!Account_ledger}). *)
val register : t -> user:string -> store:Auth_store.t -> home:string -> unit

(** [f] runs when a login shared with [user] is granted, revoked, renamed or
    logged out. *)
val subscribe : t -> user:string -> f:(unit -> unit) -> unit

(** A user's view of the service: what their server and router use. *)
module Member : sig
  type shares := t

  type t =
    { shares : shares
    ; user : string
    }
end

(** The users [user] may share with: every other one. *)
val targets : t -> user:string -> string list Or_error.t

(** Sets who may use [owner]'s login (none: stops sharing it); returns the
    grants. Tells the users who gained or lost it. *)
val share
  :  t
  -> owner:string
  -> Provider_id.t
  -> Account_id.t
  -> users:string list
  -> Account_meta.Grant.t list Or_error.t

(** [owner]'s logins changed (logged out, renamed): tells the users they are
    or were shared with. *)
val changed : t -> owner:string -> unit

(** A login shared with a user, as they see it: never its credential. *)
module Shared : sig
  type t =
    { owner : string
    ; provider : Provider_id.t
    ; id : Account_id.t
    ; label : string (** the owner's *)
    ; method_ : Login_method.t
    ; pref : Account_meta.Shared_pref.t (** the recipient's *)
    }
  [@@deriving sexp_of]

  val ref_ : t -> Account_ref.t

  (** ["~alice/openai-codex/work"], the key of {!Account_meta.shared_pref}. *)
  val key : t -> string
end

(** Hidden ones included; none when sharing is off. *)
val shared_with : t -> user:string -> Shared.t list

(** ["~OWNER/PROVIDER/ID"]. *)
val key : Provider_id.t -> owner:string -> Account_id.t -> string

(** Whether [owner] still shares the login with [recipient]. *)
val granted
  :  t
  -> recipient:string
  -> Provider_id.t
  -> owner:string
  -> Account_id.t
  -> bool

(** A request's credential for [recipient] from [owner]'s store, refreshing
    it there ({!Provider_auth.resolve}). Errors say the login is no longer
    shared and what to do. *)
val resolve
  :  t
  -> env:Env.t
  -> ?cancel:Cancellation.t
  -> ?refresh:
       (Provider_id.t -> refresh_token:string -> Credential.Oauth.t Or_error.t)
  -> recipient:string
  -> Provider_id.t
  -> owner:string
  -> Account_id.t
  -> Provider_auth.Resolved.t Or_error.t

(** Sets [user]'s preference about a login shared with them. *)
val set_pref
  :  t
  -> user:string
  -> Provider_id.t
  -> owner:string
  -> Account_id.t
  -> f:(Account_meta.Shared_pref.t -> Account_meta.Shared_pref.t)
  -> unit Or_error.t

(** Adds a finished request of [recipient]'s on [owner]'s login to the
    owner's {!Account_ledger}. *)
val record
  :  ?now:Time_ns.t
  -> t
  -> recipient:string
  -> Provider_id.t
  -> owner:string
  -> Account_id.t
  -> Usage.t
  -> cost_usd:float
  -> unit

(** Adds a finished request on [account] to its owner's ledger: [ledger],
    the user's own (none: not recorded), for their own logins (under [member]'s name, else
    {!Account_ledger.single_user}); the owner's for a shared one. *)
val record_request
  :  ?now:Time_ns.t
  -> ?member:Member.t
  -> ?ledger:Account_ledger.t
  -> Provider_id.t
  -> Account_ref.t
  -> Usage.t
  -> cost_usd:float
  -> unit

(** Who used [owner]'s login: everyone for the owner, only themselves for
    another user. *)
val usage
  :  ?now:Time_ns.t
  -> t
  -> user:string
  -> Provider_id.t
  -> owner:string
  -> Account_id.t
  -> Account_ledger.Row.t list Or_error.t
