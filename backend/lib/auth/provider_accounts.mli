open! Core
open! Import

(** A provider's logins (accounts) in an {!Auth_store}: [main] under the
    provider's own key (pi's), the others under
    ["account:<provider>:<id>"], and their labels, default and order in
    {!Account_meta}. With no metadata a provider has its credentials'
    accounts, [main] first, the first being the default and each labelled
    {!Login_method.label} ([main]) or its id: a file from before accounts
    reads as one [main] account, and reading never writes. *)

module Account : sig
  type t =
    { id : Account_id.t
    ; label : string
    ; credential : Credential.t
    ; default : bool
    ; fallback : bool
      (** may take over when another account hits its usage limit (by default
        for subscriptions, never for API keys: no spend the user did not
        choose) *)
    }
  [@@deriving sexp_of]
end

(** The auth.json key of the account's credential. *)
val entry_key : Provider_id.t -> Account_id.t -> string

(** In order (the fallback order). *)
val list : Auth_store.t -> Provider_id.t -> Account.t list Or_error.t

val default : Auth_store.t -> Provider_id.t -> Account.t option Or_error.t

(** By id, or by label (case-insensitive); the error lists the accounts. *)
val find : Auth_store.t -> Provider_id.t -> string -> Account.t Or_error.t

(** As {!Auth_store.modify}, for one account's credential (refreshing a
    token). *)
val modify
  :  Auth_store.t
  -> Provider_id.t
  -> Account_id.t
  -> f:(Credential.t option -> Credential.t option Or_error.t)
  -> Credential.t option Or_error.t

(** Stores a login's credential and returns its account:
    - [account]: logs that one in again (it must exist, or be [main]);
    - [label]: the account with that label, else the one with the same
      OAuth account (a subscription logged in twice), else a new one
      ([main] when the provider has none yet);
    - neither: the default account ([main] when there is none), as when a
      provider had one login. *)
val save_login
  :  ?account:Account_id.t
  -> ?label:string
  -> Auth_store.t
  -> Provider_id.t
  -> Credential.t
  -> Account_id.t Or_error.t

(** Logs the account out (removes its credential and metadata). *)
val remove : Auth_store.t -> Provider_id.t -> Account_id.t -> unit Or_error.t

module Update : sig
  type t =
    { label : string option
    ; make_default : bool
    ; fallback : bool option
    ; move_before : Account_id.t option option
      (** [Some None]: to the end; [Some (Some id)]: before [id] *)
    }

  val empty : t
end

val update
  :  Auth_store.t
  -> Provider_id.t
  -> Account_id.t
  -> Update.t
  -> unit Or_error.t
