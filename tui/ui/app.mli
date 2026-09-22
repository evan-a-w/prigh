open! Core
module P = Prigh_protocol

(** The whole frontend as a pure state machine. [update] never performs I/O; it
    returns [Command.t]s for the platform to execute, and their results come
    back as [Action.Reply]. *)

module Reply_tag : sig
  type t =
    | Ignore
    | Show_error (** report only failures *)
    | Initial_state
    | Initial_messages
    | Reload_messages
    | Auth_refresh
    | Auth_show
    | Auth_login_picker
    | Auth_logout_picker
    | Models_for_picker of string (** initial filter *)
    | Models_for_switch of string (** the argument to resolve *)
    | Models_after_login of string (** provider *)
    | Sessions_picker
    | Sessions_cache
    | Session_stats
    | Entries_for_fork
    | Entries_for_rewind
    | Entries_for_tree
    | Export_done
    | Deleted_session
    | Paths_for_autocomplete of string
    | Set_model_done
    | Config
    | Config_saved
    | Config_for_confirm of bool
    | Models_catalog
    | Models_for_scoped
    | Compact_done
    | Abort_done
    | Notice_on_success of string
    | History
    | Dequeued
    | Editor_text
    | Reload_messages_notice of string
  [@@deriving sexp_of, equal]
end

module Command : sig
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * P.Json.t) list
        ; tag : Reply_tag.t
        }
    | List_paths of
        { prefix : string
        ; tag : Reply_tag.t
        }
    | Open_browser of string
    | Load_history
    | Append_history of string
    | Copy_to_clipboard of string
    | Suspend
    | Edit_externally of string
    | Quit
  [@@deriving sexp_of, equal]
end

module Action : sig
  type t =
    | Start (** issues the initial requests *)
    | Key of Key.t
    | Intent of Intent.t
    | Event of P.Event.t
    | Protocol_error of string
    | Stderr of string
    | Backend_closed
    | Reply of Reply_tag.t * (P.Json.t, string) Result.t
    | Tick
    | Set_home of string
    | Set_client_id of string (** ours, from the [hello] reply *)
    | Resize of
        { width : int
        ; height : int
        }
  [@@deriving sexp_of]
end

module Model : sig
  type t =
    { state : P.State.t option
    ; models : P.Model.t list
    ; auth : P.Auth_status.t list
    ; transcript : Transcript.t
    ; agents : Agent_view.t list
    ; focus : [ `Main | `Agent of string ]
    ; editor : Editor.t
    ; mode : Mode.t
    ; autocomplete : Autocomplete.t option
    ; sessions : P.Session_summary.t list option
    ; known_paths : String.Set.t
    ; queued : Queue_counts.t
    ; queued_texts : string list
    ; login_lines : string list
    ; viewport : Viewport.t
    ; pending_quit : bool
    ; spinner : int
    ; verbosity : Verbosity.t
    ; config : P.Config.t option
    ; home : string option
    ; client_id : string option
    ; stderr_tail : string list
    ; pending_confirms : (string * string * string) list
    ; backend_gone : bool
    ; width : int
    ; height : int
    ; quitting : bool
    }
  [@@deriving sexp_of]

  val running : t -> bool
end

(** Wrapped transcript lines currently visible, mirroring [Render.screen] for
    [Editing] mode; approximate for dialog modes. *)
val transcript_rows : Model.t -> int

val init : Model.t
val format_tokens : int -> string
val update : Model.t -> Action.t -> Model.t * Command.t list
