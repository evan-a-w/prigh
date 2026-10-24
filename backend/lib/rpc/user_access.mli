open! Core
open! Import

(** Who may sign in as which user (namespace) of [prigh serve -tokens]: each
    user's login token, optional host tokens (sign in as that user, never with
    superuser rights; for tool hosts the deployment starts), and the
    superusers, who may act as any user. *)

type t

(** Validates that [host_tokens] and [superusers] name known users and that
    no token is used twice. *)
val create
  :  Namespace.t list
  -> host_tokens:Namespace.t list
  -> superusers:string list
  -> t Or_error.t

(** Comma-separated user names. *)
val parse_names : string -> string list

val namespaces : t -> Namespace.t list

module Signed_in : sig
  (** A connection's credentials: the user it signed in as, which need not be
      the user it currently acts as. *)
  type access := t

  type t = private
    { access : access
    ; user : string
    ; superuser : bool
    }

  (** The user [as_user] may act as: anyone for a superuser, else only
      [user]. *)
  val switch : t -> string -> string Or_error.t

  (** Every user's name, for superusers. *)
  val users : t -> string list Or_error.t
end

(** Checks a [hello]'s [token] (and [user], when given, against the token's
    user), then [as_user] (when given and not empty) with
    [Signed_in.switch]. Returns the credentials and the user to act as. *)
val authenticate
  :  t
  -> ?user:string
  -> ?as_user:string
  -> string option
  -> (Signed_in.t * string) Or_error.t

(** The error for any failed authentication. *)
val unauthorised : string

(** The error for user switching without [-tokens]. *)
val no_users : string
