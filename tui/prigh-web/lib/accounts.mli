open! Core
open! Import

(** The sign-ins prigh-web remembers, so that the account menu switches between
    them in one click. They are a JSON list in localStorage ([key]); the active
    one is also in [prigh.user]/[prigh.token], where [web-app/]'s [Login] (the
    [-web] page) keeps its single login, so both pages keep working and a
    login saved before the list existed is migrated into it. *)

module Storage : sig
  (** localStorage, or a table in tests. *)
  type t =
    { get : string -> string option
    ; set : string -> string -> unit
    ; remove : string -> unit
    }

  val in_memory : unit -> t
end

module Account : sig
  type t =
    { backend : string (** the WebSocket URL *)
    ; user : string option
    ; token : string option
    ; session : string option (** the last session, rejoined on switching *)
    }
  [@@deriving sexp_of, equal]

  (** The same sign-in: backend and user (the token when there are no users). *)
  val same : t -> t -> bool

  (** The user, or ["token"] / ["anonymous"] on a backend without users. *)
  val name : t -> string

  (** [host:port] of [backend]. *)
  val host : t -> string
end

val key : string

(** The saved accounts, oldest first. *)
val load : Storage.t -> Account.t list

(** Once signed in to [backend]: saves the login in [prigh.user]/[prigh.token]
    as an account (a new sign-in, one from before the list, or a new token for
    a saved user), and returns them all. *)
val remember : Storage.t -> backend:string -> Account.t list

(** The account in [prigh.user]/[prigh.token] on [backend], with its saved
    session. *)
val current : Storage.t -> backend:string -> Account.t option

(** Makes [account] the active one (saved as an account by [remember] once
    the backend accepts it). *)
val activate : Storage.t -> Account.t -> unit

(** Forgets [account]; when it is the active one, [prigh.user]/[prigh.token]
    too. *)
val remove : Storage.t -> Account.t -> unit

(** Remembers [session] as [account]'s last. *)
val set_session : Storage.t -> Account.t -> string -> unit
