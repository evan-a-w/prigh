open! Core

(** Builds the system prompt: built-in guidance, environment facts, and
    project instructions from [AGENTS.md]/[CLAUDE.md] files found from the
    filesystem root down to [cwd], plus [~/.prigh/AGENTS.md]. *)

val instruction_files : cwd:string -> home:string -> string list

(** The instruction files with their (stripped) contents. *)
val read_instructions : cwd:string -> home:string -> (string * string) list

(** [instructions] (path, text) default to [read_instructions] on this
    machine; [Agent] fetches them from the session's tool host instead. [nix]
    (default false), when the tool host has Nix, adds how to get missing tools
    from nixpkgs. [skills] that the model may use are listed when it has the
    [read] tool. *)
val build
  :  ?date:string
  -> ?instructions:(string * string) list
  -> ?nix:bool
  -> ?skills:Skill.t list
  -> cwd:string
  -> home:string
  -> tools:Tool_spec.t list
  -> unit
  -> string

(** Notes prepended to the next user message when the environment changes
    after the system prompt was fixed, so the prompt prefix stays cacheable. *)
val host_changed_note : host:string -> string

val cwd_changed_note : cwd:string -> string
