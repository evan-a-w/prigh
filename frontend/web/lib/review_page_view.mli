open! Core
open! Import

(** The review page, in the chat's place while open (the chat narrows to a
    column beside it): the changed files with their approvals, the current
    file's diff with its comments ([Review_code_view]) or, in edit mode, its
    text to edit and save, and the controls: approve, comment or edit, the
    hunks or the whole file, next and previous file, send the comments. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The button that opens it, for the file browser and the [/diff] dialog. *)
val open_button
  :  ?path:string
  -> App.Model.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t
