open! Core
open! Import

(** Temporary sessions: the new-session screen's heading and tray (where it
    starts, and the Temporary chip), the pill of a temporary session and
    its menu, and the dialog that confirms leaving one that works. *)

(** The new-session screen's heading: "What are we building?" with a block
    caret, or, temporary, "Temporary session" and what that means. *)
val hero : App.Model.t -> Node.t

(** Under the composer of the new-session screen (over it on a phone): the
    folder and branch it starts in, and the Temporary chip. *)
val tray : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** At the top of a temporary session's chat, opening its menu; on a phone,
    the top strip's title. *)
val pill : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

val menu : inject:(App.Action.t -> unit Effect.t) -> Node.t
val leave : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
