open! Core
open! Import

(** [apply_patch]: edits files with a patch in Codex's format (see
    {!Apply_patch}). It replaces [edit] for the models trained on it (see
    {!Tools.for_model}). Nothing is written unless every operation applies; a write that
    still fails (permissions, a full disk) is reported with what was and was
    not changed. Paths naming the same file ([a.txt], [./a.txt]) are one
    file. *)

val tool : Tool.t
