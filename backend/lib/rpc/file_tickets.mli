open! Core
open! Import

(** Short-lived capabilities for the web server's file transfers
    ({!File_http}): a client asks over its authenticated RPC connection
    ([fs_download], [fs_upload], [fs_preview]) and gets a URL with a random
    ticket, so the token is never in a URL and a ticket only reaches the
    files of the session (and so the namespace and tool host) it was issued
    for. One registry per process. *)

module Kind : sig
  type t =
    | Download of { path : string }
    | Inline of
        { path : string
        ; mime : string
        } (** an image preview: may be fetched again until it expires *)
    | Archive of { path : string } (** the directory as .tar.gz *)
    | Upload of
        { path : string
        ; conflict : File_ops.Conflict.t
        ; size : int
        ; make_dir : bool
        }
  [@@deriving sexp_of]
end

module Ticket : sig
  type t =
    { agent : Agent.t
    ; kind : Kind.t
    ; expires : float (** seconds since the epoch *)
    }
end

(** How long a ticket lasts (5 minutes). *)
val lifetime : float

(** A new ticket; [now] defaults to the clock. *)
val issue : ?now:float -> Agent.t -> Kind.t -> string

(** The ticket, removed unless it is [Inline]; [`Expired] when unknown or
    expired, [`Wrong_endpoint] (keeping it) when it is an upload ticket and
    [upload] is false or the other way round. *)
val take
  :  ?now:float
  -> upload:bool
  -> string
  -> (Ticket.t, [ `Expired | `Wrong_endpoint ]) Result.t

(** The URL path of a ticket: [/files/download?ticket=...] or
    [/files/upload?ticket=...]. *)
val url : Kind.t -> string -> string
