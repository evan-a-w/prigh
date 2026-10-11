open! Core

(** The fixed set of built-in tools. *)

val all : Tool.t list

(** The main agent's tools for the user and the session: [update_plan]
    ({!Plan}), [ask_user] and [exit_plan_mode]. Not in [all]: tool hosts do
    not run them. *)
val interaction : Tool.t list

(** Built-in tools that stand in for one of [all] for some models
    ([apply_patch]). *)
val variants : Tool.t list

(** [all @ variants]: what a tool host can run. *)
val builtin : Tool.t list

val find : string -> Tool.t option

(** Whether [model] edits with [apply_patch] rather than [edit]: OpenAI's
    GPT-5 and later, which are trained on Codex's tools. *)
val uses_apply_patch : Model.t -> bool

(** [tools] with the file editor ([edit] or [apply_patch]) that suits
    [model], where there is one, and without [web_search] unless [model]'s
    provider runs it. *)
val for_model : Model.t -> Tool.t list -> Tool.t list

(** The file editor among [tools]. *)
val editor : Tool.t list -> Tool.t option

val specs : Tool.t list -> Tool_spec.t list

(** The tools available to an agent at [depth]: the parent's [parent] tools,
    restricted to [only] if given. The [subagent] tool is dropped at depth 2
    and beyond so that delegation cannot recurse further, and the background
    controls ([subagent_wait], [job_wait], ...), {!interaction} and [goal_complete] at depth 1
    and beyond, where subagents and [bash] run synchronously ([bash] loses
    [background]) and nobody can be asked. *)
val for_context
  :  parent:Tool.t list
  -> depth:int
  -> ?only:string list
  -> unit
  -> Tool.t list Or_error.t
