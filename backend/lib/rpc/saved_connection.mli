open! Core
open! Import

(** The remote backend [prigh-tui] and [prigh web] connect to when not told
    otherwise, kept in [~/.prigh/tui-connection.sexp] (mode 0600: it holds
    the token) so that [-connect], [-token] and [-user] need only be given
    once. The TUI has the same module
    ([frontend/client/unix/saved_connection.ml]): keep the two in step. *)

type t =
  { connect : string
  ; token : string option
  ; user : string option
  }
[@@deriving sexp, equal]

(** [<home>/.prigh/tui-connection.sexp] *)
val file : home:string -> string

(** The saved connection, [None] if there is none. *)
val load : home:string -> t option Or_error.t

val save : home:string -> t -> unit Or_error.t

(** Removes the saved connection, if any. *)
val forget : home:string -> unit Or_error.t

module Settings : sig
  type t =
    { connect : string option
    ; token : string option
    ; user : string option
    }
  [@@deriving sexp_of]
end

module Resolved : sig
  type nonrec t =
    { settings : Settings.t (** what to use for this run *)
    ; save : t option (** what to save, if it changed *)
    }
  [@@deriving sexp_of]
end

(** Each setting comes from its flag, else its environment variable, else
    the saved connection (the token and user only along with the saved
    address). Flags are saved: [-connect] replaces the saved connection
    (keeping its token and user if the address is the same), [-token] and
    [-user] update it. With [local] (spawn a backend), the saved connection
    is neither used nor changed. *)
val resolve
  :  saved:t option
  -> flags:Settings.t
  -> env:Settings.t
  -> local:bool
  -> Resolved.t
