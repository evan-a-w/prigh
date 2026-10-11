open! Core
open! Import

(** [apply_patch]: edits files with a patch in Codex's format (see
    {!Apply_patch}). It replaces [edit] for the models trained on it (see
    {!Tools.for_model}). Nothing is written unless every operation applies. *)

val tool : Tool.t
