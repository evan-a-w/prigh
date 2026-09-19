open! Core
open! Import

(** A tool that delegates a task to a nested agent loop with its own context,
    tool set, model and turn budget, returning its final reply. *)

val max_turns : int

val create
  :  provider:Provider.t
  -> current_model:(unit -> Model.t)
  -> current_thinking:(unit -> Thinking.t)
  -> home:string
  -> Tool.t
