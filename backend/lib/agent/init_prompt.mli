open! Core

(** [/init]: the prompt asking the model to explore the project and write a
    concise [AGENTS.md], or improve the existing one. *)
val prompt : cwd:string -> string
