open! Core
open! Import

(** A subagent's name: a few words saying what it does, shown wherever the
    frontends list subagents, in place of its task's first line. The calling
    model gives it ([subagent]'s [name]); otherwise the naming model writes
    one ({!ask}), or it is made from the task ({!of_task}). *)

val max_length : int

(** A name given by the calling model, {!Naming.clean}ed. *)
val of_given : string -> string option

(** A name from the task's text alone: its first sentence, without a leading
    clause that only sets the scene (["Implement on a branch `x`: fix
    y"] is ["Fix y"]), cut short. *)
val of_task : string -> string

(** What the naming model is asked to write. *)
val instructions : string

(** The naming model's input: [task], cut short, and the names of the
    caller's other subagents, which the name should differ from. *)
val prompt : task:string -> siblings:string list -> string

(** Asks [model] for a name. *)
val ask
  :  provider:Provider.t
  -> model:Model.t
  -> ?cancel:Cancellation.t
  -> task:string
  -> siblings:string list
  -> unit
  -> string Or_error.t
