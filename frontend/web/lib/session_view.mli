open! Core
open! Import

(** The current session's bar over the chat. *)

(** The top bar: the sidebar's toggle at the left (when the sidebar is not
    docked) and the session's project / name, and at the right what applies
    ({!Status_view.notes}) and the toggles of the filter, the files, the
    terminal, the subagents (while there are any), the review and the
    session card
    (with the settings when the sidebar is not docked). On a phone, a strip:
    ☰, the session's name and a ⋯ whose sheet has the session's thinking
    level, mode, verbosity and usage
    ({!Chip_popover_view.session_sections}), what applies, the panels, the
    session card and the settings. *)
val chrome : App.Model.t -> inject:(App.Action.t -> unit Effect.t) -> Node.t
