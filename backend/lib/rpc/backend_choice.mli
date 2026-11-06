open! Core
open! Import

(** The backend [prigh web] and [prigh tool-host -connect]/[-remote] connect
    to: [-connect], [-token] and [-user], else [$PRIGH_CONNECT],
    [$PRIGH_TOKEN] and [$PRIGH_USER], else the saved connection
    ({!Saved_connection}, by its rules: the saved token and user only go
    with the saved address). Addresses are normalised as they are saved
    ({!Backend_address.normalize}). *)

type t =
  { remote : Backend_address.t
  ; credentials : Web_connect.Credentials.t
  ; source : Web_connect.Check.Source.t (** of the address *)
  ; token_source : Web_connect.Check.Source.t option (** [None]: no token *)
  ; save : Saved_connection.t option
    (** what [prigh web] saves once the backend accepted it *)
  }
[@@deriving sexp_of]

module Error : sig
  type t =
    | Unreadable_saved of
        { file : string
        ; error : Core.Error.t
        }
    | No_backend (** nothing given, in the environment or saved *)
    | Bad_address of
        { error : Core.Error.t
        ; source : Web_connect.Check.Source.t
        }
  [@@deriving sexp_of]

  (** What went wrong and what to do next; [no_backend] says it for
      [No_backend], which depends on the command. *)
  val to_string_hum : t -> no_backend:string -> string
end

(** [$PRIGH_CONNECT], [$PRIGH_TOKEN] and [$PRIGH_USER], empty ones unset. *)
val env : unit -> Saved_connection.Settings.t

val choose
  :  home:string
  -> flags:Saved_connection.Settings.t
  -> env:Saved_connection.Settings.t
  -> (t, Error.t) Result.t

(** What to do when the backend refused the credentials: where they came
    from and how to fix them there. *)
val refused_hint : t -> string

(** After signing in through the browser to [remote]: when it is the saved
    connection's address, saves the grant's proxy cookie with it (and the
    grant's token and user if it has no token). Another address changes
    nothing. *)
val remember_signin
  :  home:string
  -> Backend_address.t
  -> Browser_signin.Grant.t
  -> unit Or_error.t
