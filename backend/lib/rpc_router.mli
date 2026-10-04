open! Core
open! Import

(** Routes connections to an [Rpc_server]: either a single server, or one
    per token namespace ([prigh serve -tokens]), where a connection belongs
    to the namespace whose token its first request, a [hello], presents. *)

type t

val single : Rpc_server.t -> t

(** One server per namespace, created eagerly; [create_server] gets the
    namespace's world (see [Namespace.world]). *)
val namespaced
  :  Namespace.t list
  -> home:string
  -> legacy_auth_file:string
  -> create_server:(Namespace.t -> Namespace.World.t -> Rpc_server.t)
  -> t

(** The server a client presenting [token] belongs to: in single mode the
    server if the token is accepted, else the namespace with that token. *)
val lookup : t -> token:string option -> Rpc_server.t option

(** By namespace name (the empty name in single mode). *)
val servers : t -> (string * Rpc_server.t) list

(** Like [Rpc_server.serve_lines]. With namespaces the first request must be
    a [hello] carrying a known token, or it is answered with an error and the
    connection closed. *)
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
