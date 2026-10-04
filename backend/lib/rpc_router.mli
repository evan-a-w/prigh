open! Core
open! Import

(** Routes connections to an [Rpc_server]: either a single server, or one
    per user ([prigh serve -tokens]), where a connection belongs to the user
    whose credentials its first request, a [hello], presents, or to the
    user it names as [as_user] (see [User_access]). A [set_user] request
    moves the rest of the connection to another user's server, as if the
    client had sent its first [hello] again (without the session) with
    [set_user]'s id. *)

type t

val single : Rpc_server.t -> t

(** One server per user, created eagerly; [create_server] gets the user's
    world (see [Namespace.world]). *)
val namespaced
  :  User_access.t
  -> home:string
  -> legacy_auth_file:string
  -> create_server:(Namespace.t -> Namespace.World.t -> Rpc_server.t)
  -> t

(** The server a client presenting [token] (and optionally [user] and
    [as_user]) belongs to, with its credentials when there are users: in
    single mode the server if the token is accepted ([user] is ignored, and
    [as_user] is refused), else as [User_access.authenticate] says. *)
val authenticate
  :  t
  -> ?user:string
  -> ?as_user:string
  -> token:string option
  -> unit
  -> (Rpc_server.t * User_access.Signed_in.t option) Or_error.t

val lookup
  :  t
  -> ?user:string
  -> ?as_user:string
  -> token:string option
  -> unit
  -> Rpc_server.t option

(** By user name (the empty name in single mode). *)
val servers : t -> (string * Rpc_server.t) list

(** Like [Rpc_server.serve_lines]. With users the first request must be a
    [hello] whose credentials [authenticate] accepts, or it is answered with
    an error and the connection closed. *)
val serve_lines
  :  t
  -> read_line:(unit -> string option)
  -> write_line:(string -> unit)
  -> unit

val serve_connection
  :  t
  -> input:_ Eio.Flow.source
  -> output:_ Eio.Flow.sink
  -> unit

val shutdown : t -> unit
