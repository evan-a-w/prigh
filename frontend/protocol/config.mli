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
  ; permissions : Permissions.t
    (** the mode new sessions start in ([confirm_tools] from older backends) *)
  ; default_model : string option
  ; default_thinking : string option
  ; fallback_models : string list
    (** model keys that take over, in order, when a model's usage runs out *)
  ; default_cwd : string option (** where new sessions start *)
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
  ; naming_model : string option
    (** the model that names sessions and subagents; [None]: the backend's
        [generate_model], else the session's *)
  }
[@@deriving sexp_of, equal]

val default : t
val of_json : Json.t -> t Or_error.t
val to_json : t -> Json.t
