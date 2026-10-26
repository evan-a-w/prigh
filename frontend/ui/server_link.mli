open! Core
open! Import

(** The backend a frontend that manages its own connection (the terminal UI)
    talks to: [/connect] changes it, [/signin] and [/signout] its
    credentials. The platform reports it ([App.Action.Server_link]). *)

type t =
  { address : string option
    (** [HOST:PORT] or a prigh-web URL ({!Server_address}); [None]: a
        backend this frontend spawned *)
  ; user : string option (** who we sign in as there *)
  ; saved : string option (** the saved remote address, if any *)
  }
[@@deriving sexp_of, equal]

(** The address, or "a local backend". *)
val describe : t -> string

(** Whether a [hello] error means the backend rejected our user name or token
    (rather than, say, a missing session). *)
val is_rejection : string -> bool

module Change : sig
  (** The token is never printed. *)
  type t =
    | Connect of string option
    (** to an address ({!Server_address.normalize}d), with the saved
        credentials when it is the saved address; [None]: a freshly spawned
        local backend *)
    | Sign_in of
        { user : string option
        ; token : string
        } (** the same server with these credentials *)
  [@@deriving sexp_of, equal]
end

module Outcome : sig
  type link := t

  type result =
    | Unreachable of string
    (** the new transport could not be opened: nothing changed *)
    | Rejected of string
    (** the old connection is gone and the new one's [hello] failed *)
    | Connected of P.Json.t (** the [hello] reply *)
  [@@deriving sexp_of]

  type t =
    { link : link (** after the change *)
    ; result : result
    }
  [@@deriving sexp_of]
end
