open! Core
open! Import

(** The environment as the model last knew it: what the session's system
    prompt described, updated by every note delivered since. The session
    records it ([Session.told_environment]), so a change not yet told
    survives the agent being unloaded, a session switch and a backend
    restart: the notes are derived by comparing it with the current
    environment when the next user message is built. *)

type t =
  { host : string (** a [Host.id]; empty: none *)
  ; cwd : string
  ; tools : string list (** sorted *)
  ; plan_mode : bool
  ; client : Client_kind.t option
    (** the kind of client the user prompted from; [None]: none said yet
        (also in sessions from before it) *)
  }
[@@deriving sexp, jsonaf, equal]

(** What a model that knows [told] is to be told about [now] ([host_name]
    names [now.host]), in this order: host, cwd, tools (added and removed),
    plan mode, client (once [now] has one, until told). Only net changes:
    changes that cancel out give no note. *)
val notes : told:t -> t -> host_name:string -> Injected.Note.t list
