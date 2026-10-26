open! Core
open! Import

(** A file of the review: its changes (the hunks, or the whole file with
    them inline, as [Code_review.layout] says), highlighted, one line
    number per row (the old one for removed lines). Text selected in it
    (the page's glue reports it: [Code_review.Action.Select_text]) gets a
    floating Comment button by its end, which opens the comment box under
    it, the selection quoted; a line's number selects the line (Shift+click:
    to there). The comments' threads show under their code (found again by
    its text when the file moved: [Prigh_ui.Comment_anchor]), which stays
    marked; pointing at either lights both. A thread has replies, the
    agent's proposed changes to accept or reject, resolving. The review
    page and the file browser's preview of a changed file show it. *)
val view
  :  App.Model.t
  -> path:string
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t

(** A comment's thread, as [view] shows it under its lines. *)
val thread
  :  ?at:Review_comment.Range.t (** where its code is now *)
  -> App.Model.t
  -> Review_comment.t
  -> outdated:bool
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
