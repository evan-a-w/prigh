open! Core
open! Import

(** The model names composer sessions record for the external harnesses'
    replies: [ADAPTER/MODEL] ([codex/gpt-5], [claude/default]). Such a
    reply has no price of prigh's: its cost is what the harness reported,
    recorded as spend. *)

val name : adapter:string -> string option -> string

(** Whether a recorded model name is an external harness's. *)
val is_external : string -> bool
