open! Core
open! Import

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
    | Sessions_refresh (** the open sessions picker and the cache *)
    | Sessions_deleted
    | Sessions_to_delete of string list (** [/delete]'s arguments *)
    | Archived_to_delete
    | Archive_done of
        { name : string
        ; archived : bool
        }
    | Session_to_archive of
        { arg : string (** [/archive]'s argument *)
        ; archived : bool
        }
    | Paths_for_autocomplete of string
    | Dirs_for_autocomplete of string
    | Set_model_done of string
    | Config
    | Config_saved
    | Default_saved
    | Fallback_shown (** [get_config] for [/fallback] *)
    | Fallback_saved of string
    (** [set_config] of [/fallback]'s text, back in the editor if refused *)
    | Default_dir_shown (** [get_config] for [/default-dir] *)
    | Default_dir_saved of string (** like [Fallback_saved] *)
    | Models_catalog
    | Models_for_scoped
    | Compact_done
    | Git_diff
    | Review_started
    | Abort_done
    | Notice_on_success of string
    | History
    | Dequeued
    | Editor_text
    | Reload_messages_notice of string
    | Reconnect of int (** generation; stale replies are ignored *)
    | Btw of string (** btw id *)
    | Users_list
    | User_switched
    | Jobs_picker
    | Job_output
    | Job_started
    | Skills_picker
    | Snippets_saved of string (** the notice once [set_config] succeeds *)
    | Reminder_added (** acknowledged with the name the backend gave it *)
    | Skills_for_autocomplete of string (** the [Location_cache] key *)
    | Skill_prompt of string
    (** a [/skill:] prompt, steer or follow-up; failing puts it back *)
    | Mcp_picker
    | Mcp_reconnected
    | Mcp_approved of string (** server *)
    | Pending (** [get_pending]: confirmations and questions to show *)
    | Mode_set of string (** the notice *)
    | Refresh_state
    | Dir_changed of P.Dir_change.Target.t
    (** [Missing]: confirm creating it; otherwise acknowledge *)
    | Worktrees_picker (** [list_worktrees] for [/worktree] *)
    | Worktrees_for_autocomplete of string (** the [Location_cache] key *)
    | Worktree_target of
        { command : string (** back in the editor when there is none *)
        ; action : Worktree_command.Action.t (** [Open] or [Remove] *)
        } (** [list_worktrees] to find the worktree [action] names *)
    | Worktree_created (** then [set_cwd] there *)
    | Worktree_entered of string (** [set_cwd] into one; the notice *)
    | Worktree_session_started of
        { path : string
        ; notice : string
        } (** [new_session], then [set_cwd] to [path] *)
    | Worktree_removed of
        { target : string
        ; force : bool
        } (** a refusal over uncommitted changes offers [rm -f] *)
    | Retry_in_editor of
        { command : string
        ; hint : string
        ; then_ : t
        }
    (** on failure the command goes back into the editor, with [hint] after
        the error *)
  [@@deriving sexp_of, equal]
end

module Command : sig
  type t =
    | Rpc of
        { method_ : string
        ; params : (string * P.Json.t) list
        ; tag : Reply_tag.t
        }
    | Open_browser of string
    | Load_history
    | Append_history of string
    | Copy_to_clipboard of string
    | Suspend
    | Edit_externally of string
    | Reconnect of
        { generation : int
        ; delay_ms : int
        ; session : string option
        ; as_user : string option
        }
    (** After [delay_ms], connect again, send [hello] (rejoining [session], as
        [as_user]) and answer [Reply (Reconnect generation, hello reply)]. *)
    | Relink of
        { generation : int
        ; change : Server_link.Change.t
        }
    (** [/connect], [/signin]: switch server or credentials and answer
        [Relinked]. *)
    | Sign_out
    (** Forget the stored credentials (the token) and drop the connection. *)
    | Notify of
        { mode : Notify.Mode.t
        ; notification : Notify.t
        }
    (** Ring the bell or raise a desktop notification: sent only while the
        terminal reports that it does not have the focus. *)
    | Quit
  [@@deriving sexp_of, equal]
end

(** Reconnection is driven from here so the backoff is testable: every
    [Backend_closed] starts a retry loop (250ms doubling, capped at 60s) that
    [/retry-backend-connection] can short-circuit. *)
module Connection : sig
  type t =
    | Connected
    | Reconnecting of
        { attempt : int
        ; generation : int
        ; delay_ms : int
        ; session : string option
        }
    | Switching of
        { generation : int
        ; previous : t (** back to it when the new server cannot be reached *)
        } (** [/connect] or [/signin] under way *)
    | Disconnected of { reason : string }
    (** the server refused us, or we signed out: no retries, until
        [/signin] or [/connect] *)
  [@@deriving sexp_of, equal]

  val delay_ms : attempt:int -> int
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
    | Set_utc_offset of Time_ns.Span.t
    (** the local time zone's, for times shown *)
    | Focus of bool (** the terminal reported gaining or losing the focus *)
    | Hello of P.Hello_reply.t (** the [hello] reply *)
    | Server_link of Server_link.t
    (** the terminal's backend; where it is reported, [/connect], [/signin]
        and [/signout] manage it *)
    | Relinked of
        { generation : int
        ; outcome : Server_link.Outcome.t
        } (** the answer to [Command.Relink] *)
    | Rejected of string
    (** the first [hello] was refused: start disconnected, asking to sign
        in *)
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
    ; skills : P.Skill.t list Location_cache.t
    ; worktrees : P.Worktree.Listing.t Location_cache.t
    ; known_paths : String.Set.t
    ; queued_messages : Queued_message.t list
    ; login_lines : string list
    ; viewport : Viewport.t
    ; pending_quit : bool
    ; spinner : int
    ; activity : Activity.t
      (** the main agent's, from its events; {!activity} squares it with the
          state *)
    ; verbosity : Verbosity.t
    ; config : P.Config.t option
    ; home : string option
    ; client_id : string option
    ; namespace : string option (** the user whose sessions we see *)
    ; user : string option (** the user we signed in as *)
    ; stderr_tail : string list
    ; pending_confirms : P.Pending.Confirm.t list
    ; pending_questions : P.Pending.Ask.t list
    ; connection : Connection.t
    ; reconnect_generation : int
    ; btw : Btw_box.t option
    ; btw_seq : int
    ; width : int
    ; height : int
    ; quitting : bool
    ; utc_offset : Time_ns.Span.t
    ; terminal_focus : Focus.t (** whether the terminal or page has the focus *)
    ; server : Server_link.t option (** [None] until the platform reports it *)
    ; notice : Notice.t option
      (** the latest acknowledgement, until it expires (Esc dismisses it) *)
    ; sidebar : bool
      (** whether the agents sidebar shows on wide terminals (Alt+L) *)
    }
  [@@deriving sexp_of]

  val running : t -> bool

  (** What the main agent is doing: [Idle] unless the state says it runs. *)
  val activity : t -> Activity.t

  (** Whether any subagent is running, including background ones the main agent
      is no longer waiting for. *)
  val agents_running : t -> bool

  (** Background shell jobs of this session still running. *)
  val jobs_running : t -> bool

  val backend_gone : t -> bool

  (** [namespace] when a superuser acts as another user ([/setusr]). *)
  val acting_as : t -> string option
end

(** The screen's zones for the model's size and editor. *)
val layout : Model.t -> Layout.t

(** Wrapped transcript lines currently visible, mirroring [Render.screen]
    (overlays cover some of them). *)
val transcript_rows : Model.t -> int

val init : Model.t
val format_tokens : int -> string

(** [path] with the home directory shown as [~]. *)
val tilde : Model.t -> string -> string

(** Rows a picker lists at once (its page size), capped by the transcript
    area it is drawn over. *)
val picker_rows : Model.t -> int

val update : Model.t -> Action.t -> Model.t * Command.t list
