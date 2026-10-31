open! Core
open! Import

(** Whether plan mode refuses a call of [tool] with [args] outright, as
    implementing rather than planning: file edits, creating or removing
    worktrees, creating or checking out pull requests, and subagents in a
    worktree of their own. Other destructive calls ask
    ({!Approval.decide}). *)
val changes_work : tool:string -> Json.t -> bool
