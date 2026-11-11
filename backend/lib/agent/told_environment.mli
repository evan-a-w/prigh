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
    (** the kind of client the user prompted from ([Other] until one says);
        [None]: the model was told of none yet (also in sessions from before
        it) *)
  ; user_info : string option
    (** {!User_info}'s text; [None]: none (also in sessions from before it,
        which are so told the text with their next message) *)
  }
[@@deriving sexp, jsonaf, equal]

(** What a model that knows [told] is to be told about [now] ([host_name]
    names [now.host]), in this order: host, cwd, tools (added and removed),
    plan mode, client (whenever [now]'s differs, so the first prompt tells
    it), the user's info (its new text, or that it was removed). Only net changes:
    changes that cancel out give no note. *)
val notes : told:t -> t -> host_name:string -> Injected.Note.t list
