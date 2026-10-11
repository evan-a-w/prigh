open! Core
open! Import

(** The file browser: a panel beside the chat (a sheet on a phone) with the
    directory's breadcrumbs, its entries (sortable by name, size and age),
    the selected entry's actions and a preview, and its dialogs. *)
val view : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** Its dialog (new folder, rename, delete, overwrite), over the page. *)
val dialog : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The top bar's button. *)
val toggle : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The composer's attachment chips: name, size, progress, remove. *)
val chips : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The composer's attach button. *)
val attach_button : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The hidden file pickers the page opens (and Playwright fills):
    [#files-upload-input] (to the panel's directory) and
    [#chat-attach-input] (attachments). *)
val pickers : Node.t
