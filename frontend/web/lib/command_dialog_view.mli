open! Core
open! Import

(** The dialogs that commands open beyond the pickers: [/hotkeys],
    [/scoped-models], a path prompt ([/cd], [/host], [/export], [/import]),
    [/rewind]'s confirmation, [/session], text ([/state]) and an MCP
    server's tools. [None] for the
    other dialogs. *)
val view
  :  App.Model.t
  -> Dialog.t
  -> inject:(App.Action.t -> unit Effect.t)
  -> Node.t option

(** A table of keys (or commands) and what they do. *)
val key_table : (string * string) list -> Node.t
