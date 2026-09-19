open! Core
open! Import

(** A tool that runs a nested agent loop on a delegated task with a
    restricted tool set and turn budget, returning its final reply. *)

module Role : sig
  type t =
    | Explore (** read-only investigation *)
    | Worker (** may edit files and run commands *)
  [@@deriving sexp_of, enumerate]

  val of_string : string -> t option
  val to_string : t -> string
  val tools : t -> Tool.t list
end

val max_turns : int

val create
  :  provider:Provider.t
  -> current_model:(unit -> Model.t)
  -> current_thinking:(unit -> Thinking.t)
  -> home:string
  -> Tool.t
