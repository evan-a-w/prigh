open! Core
open! Import

(** Agent skills: directories holding a [SKILL.md] whose YAML frontmatter
    names the skill and says when to use it. The system prompt lists them and
    the model reads a skill's file when a task matches it; the user invokes
    one with [/skill:NAME ARGS], which inlines the file into the message. *)

type t =
  { name : string
  ; description : string
  ; path : string (** the [SKILL.md] file *)
  ; model_invocable : bool
    (** [false] with [disable-model-invocation: true]: only the user can
        invoke it, so the system prompt leaves it out *)
  }
[@@deriving sexp_of, jsonaf]

(** Where skills are looked for, highest precedence first: [.prigh/skills],
    [.claude/skills] and [.agents/skills] in [cwd] and each of its ancestors,
    then the same under [home]. *)
val roots : cwd:string -> home:string -> string list

(** The skills under {!roots}, by name; when two share a name the one from
    the earlier root wins. A skill directory may be nested a few levels deep
    in a root. Files without a description are skipped. *)
val discover : cwd:string -> home:string -> t list

(** Parses a [SKILL.md] at [path]: the skill and the body after the
    frontmatter. The name defaults to the directory's. *)
val parse : path:string -> string -> (t * string) Or_error.t

(** [/skill:NAME ARGS] as (NAME, ARGS). *)
val invocation : string -> (string * string) option

(** The user message for invoking [t] with [args]. *)
val expand : t -> body:string -> args:string -> string

(** An {!expand}ed message as it was typed ([/skill:NAME ARGS]); other
    texts unchanged. *)
val as_typed : string -> string

(** The system prompt's skills section, if any skill is [model_invocable]. *)
val prompt_section : t list -> string option

(** The error for an unknown skill: the closest names and how to list them. *)
val unknown : t list -> string -> Error.t
