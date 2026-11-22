open! Core

(** TUI mirror of the backend's persisted configuration. *)

(** [permissions]: the allow/ask/deny rules, as written. *)
module Permissions : sig
  type t =
    { allow : string list
    ; ask : string list
    ; deny : string list
    }
  [@@deriving sexp_of, equal]

  val empty : t
end

type t =
  { scoped_models : string list
  ; approval_mode : Approval_mode.t
    (** the mode new sessions start in ([confirm_tools] from older backends) *)
  ; default_chat_mode : bool
    (** whether new sessions start in chat mode, over [approval_mode] (false
        from older backends) *)
  ; permissions : Permissions.t
  ; default_model : string option
  ; default_thinking : string option
  ; fallback_models : string list
    (** model keys that take over, in order, when a model's usage runs out *)
  ; default_cwd : string option
    (** where new sessions start on [default_host] (the backend's when there
        is none) *)
  ; default_host : string option
    (** the id of the tool host prigh-web's new sessions start on while it is
        connected; [None]: where the session you are on is *)
  ; max_budget_usd : float option
    (** the cost budget of sessions that set none with [/budget] *)
  ; resume_on_limit_reset : bool
    (** a run stopped by a usage limit resumes by itself when the limit
        resets (default true) *)
  ; worktrees_dir : string option
    (** where worktrees are created on the tool host; [None] from backends
        before worktrees *)
  ; archive_after_days : float
    (** sessions inactive this long show as archived; [0.]: never *)
  ; notify : string
    (** how the terminal frontend notifies: [off], [bell] (default) or
        [desktop] *)
  ; snippets : Snippet.t list (** [/snippet]'s, shared by every frontend *)
  ; compact_at_tokens : int
    (** compact once the context exceeds this many tokens (or 80% of the
        model's window, if less); [0]: at 80% only *)
  ; compaction_model : string option
    (** the model that writes compaction summaries; [None]: the
        conversation's *)
  ; mask_observations : bool
    (** replace old tool outputs with placeholders before summarising
        (default true) *)
  ; generate_model : string option
    (** the model that drafts commit messages, pull requests and branch names
        (✦ Generate); [None]: the session's *)
  ; naming_model : string option
    (** the model that names sessions and subagents; [None]:
        [generate_model], else the session's *)
  ; approval_model : string option
    (** the model that judges tool calls in the [smart] approval mode;
        [None]: the session's *)
  }
[@@deriving sexp_of, equal]

val default : t
val of_json : Json.t -> t Or_error.t
val to_json : t -> Json.t
