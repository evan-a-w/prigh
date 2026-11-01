open! Core
open! Import

(** The current session's controls over the chat, which has no header: they
    float over its top. *)

(** Over the top of the chat: the sidebar's toggle at the left (when the
    sidebar is not docked), and at the right what applies
    ({!Status_view.notes}) and the panels' toggles (with the settings when
    the sidebar is not docked). On a phone, a strip: ☰, the session's name
    and a ⋯ whose sheet has the session's thinking level, mode, verbosity
    and usage ({!Chip_popover_view.session_sections}), what applies, the
    panels and the settings. *)
val chrome : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t

(** The session's name, else its description or first prompt, else
    ["New session"]. *)
val session_title : App.Model.t -> State.t -> string
