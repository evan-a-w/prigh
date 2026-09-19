open! Core

(** Builds the system prompt: built-in guidance, environment facts, and
    project instructions from [AGENTS.md]/[CLAUDE.md] files found from the
    filesystem root down to [cwd], plus [~/.prigh/AGENTS.md]. *)

val instruction_files : cwd:string -> home:string -> string list

val build
  :  ?date:string
  -> cwd:string
  -> home:string
  -> tools:Tool_spec.t list
  -> unit
  -> string
