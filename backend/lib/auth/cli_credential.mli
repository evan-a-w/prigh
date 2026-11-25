open! Core
open! Import

(** One of prigh's logins as an agent CLI's composer session runs on it
    ({!Cli_login}): the access token, fetched from the store whenever the
    CLI needs one, so the refresh token stays with prigh (for a shared
    login, in its owner's store, {!Account_shares.resolve}). Each adapter
    hands it to its CLI in its own way ([Codex_login]). *)

type t [@@deriving sexp_of]

(** [provider]'s login [account], for the user whose [store] it is
    ([member]: also the logins shared with them). Errors when there is no
    such login, listing the ones there are. [ledger]: where {!record} adds
    what ran on the user's own logins. *)
val of_store
  :  env:Env.t
  -> ?member:Account_shares.Member.t
  -> ?ledger:Account_ledger.t
  -> Auth_store.t
  -> Provider_id.t
  -> Account_ref.t
  -> t Or_error.t

(** For tests: a login whose tokens come from [fetch]. *)
val create
  :  provider:Provider_id.t
  -> account:Account_ref.t
  -> label:string
  -> fetch:
       (?refused:string
        -> ?valid_for_ms:int
        -> unit
        -> Provider_auth.Resolved.t Or_error.t)
  -> ?record:(Usage.t -> cost_usd:float -> unit)
  -> unit
  -> t

val provider : t -> Provider_id.t
val account : t -> Account_ref.t

(** The login's label, as the user named it (["work"]). *)
val label : t -> string

(** A token valid for [valid_for_ms] at least (default: the store's five
    minutes), refreshing it when needed; [refused]: the CLI was refused
    with this one, so a newer one is wanted. *)
val fetch
  :  t
  -> ?refused:string
  -> ?valid_for_ms:int
  -> unit
  -> Provider_auth.Resolved.t Or_error.t

(** Adds a turn run on the login to its owner's ledger. *)
val record : t -> Usage.t -> cost_usd:float -> unit
