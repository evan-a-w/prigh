open! Core
open! Import

(** A file of the review: its changes (the hunks, or the whole file with
    them inline, as [Code_review.layout] says), highlighted, with line
    numbers to click (Shift+click: a range) to select lines and comment on
    them, and the comments' threads under the lines they are about (found
    again by their text when the file moved: [Prigh_ui.Comment_anchor]):
    replies, the agent's proposed changes to accept or reject, resolving.
    The review page and the file browser's preview of a changed file show
    it. *)
val view
  :  App.Model.t
  -> path:string
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** A comment's thread, as [view] shows it under its lines. *)
val thread
  :  ?at:int * int (** where its lines are now *)
  -> App.Model.t
  -> Review_comment.t
  -> outdated:bool
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
