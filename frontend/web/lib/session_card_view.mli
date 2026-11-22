open! Core
open! Import

(** The session card: the session's subagents (each opens its transcript in
    the chat's place) and its repository's worktrees, in two folding
    sections. Beside the chat it sits in the main pane's top-right corner,
    in the chat's right margin, and {!main_attr} moves the chat's column left
    by what that margin lacks; on a phone it is a sheet from the bottom. *)

(** It has subagents or worktrees to show. *)
val something : App.Model.t -> bool

(** Shown beside the chat: open, not a phone, something to show. *)
val inline : App.Model.t -> bool

(** For the main pane ([.main]): with the card {!inline}, a size container
    that sets [--push], the room to keep clear right of the transcript and
    the composer, and pads them by it. *)
val main_attr : App.Model.t -> Attr.t

(** The card, in the main pane, or a phone's sheet. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
