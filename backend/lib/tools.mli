open! Core

(** The fixed set of built-in tools. *)

val all : Tool.t list
val find : string -> Tool.t option
val specs : Tool.t list -> Tool_spec.t list

(** The tools available to an agent at [depth]: the parent's [parent] tools,
    restricted to [only] if given. The [subagent] tool is dropped at depth 2
    and beyond so that delegation cannot recurse further. *)
val for_context
  :  parent:Tool.t list
  -> depth:int
  -> ?only:string list
  -> unit
  -> Tool.t list Or_error.t
