open! Core
open! Import

(** [goal_complete {summary}] ({!Goal.tool_name}): [complete] records it, and
    fails when no goal is active. Only the main agent has it, and only while
    a goal is active. *)
val create : complete:(summary:string -> unit Or_error.t) -> Tool.t
