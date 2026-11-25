open! Core
open! Import

(** Settings → Engines: whether the composer's agent CLIs are installed and
    logged in on each connected tool host ([list_adapters]), asked for when
    the section opens and on Refresh, and updated by [adapters_changed].
    Only on a backend deploying the composer. They belong to the user
    (switching starts over). *)

type t =
  { statuses : (Adapter_status.t list, string) Result.t option
    (** the last answer; [None] before the first *)
  ; loading : bool (** asked, no answer yet (the last one stays shown) *)
  ; logins : Adapter_login.t list
    (** the login each CLI that can take one of prigh's runs on *)
  }
[@@deriving sexp_of, equal]

val empty : t
val loading : t -> t

val loaded
  :  t
  -> (Adapter_status.t list * Adapter_login.t list, string) Result.t
  -> t

(** [set_adapter_login]'s answer. *)
val logins_changed : t -> Adapter_login.t list -> t

(** [adapters_changed]: the backend's statuses as they are now. *)
val changed : t -> Adapter_status.t list -> t

(** The statuses grouped by host, in the backend's order (the session's
    host first): the host's name and its adapters. *)
val by_host : Adapter_status.t list -> (string * Adapter_status.t list) list
