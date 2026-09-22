open! Core
open! Import

type t =
  { spec : Tool_spec.t
  ; run : context -> Json.t -> Tool_result.t
  }

and context =
  { env : Env.t
  ; cwd : string
  ; cancel : Cancellation.t
  ; on_output : string -> unit (** streamed partial output, e.g. from bash *)
  ; depth : int (** 0 for the main agent *)
  ; agent_id : string option
  ; call_id : string
  ; tools : t list (** tools available to this agent *)
  ; emit : Agent_event.t -> unit
  ; execute : executor
    (** how tool calls of this agent (and of its subagents) are run; the
        default runs them in this process *)
  }

(** Runs a tool call. [Agent] installs one that forwards [on_host] tools to
    the session's active tool host. *)
and executor = context -> t -> Json.t -> Tool_result.t

module Context : sig
  val create
    :  ?cancel:Cancellation.t
    -> ?execute:executor
    -> ?on_output:(string -> unit)
    -> ?depth:int
    -> ?agent_id:string
    -> ?call_id:string
    -> ?tools:t list
    -> ?emit:(Agent_event.t -> unit)
    -> env:Env.t
    -> cwd:string
    -> unit
    -> context

  type nonrec t = context
end

module Result = Tool_result

val name : t -> string

(** Runs the tool in this process, turning [Tool_args.Invalid] and other
    exceptions into error results. *)
val execute : t -> Context.t -> Json.t -> Result.t

(** [execute] through the context's executor. *)
val execute_via : Context.t -> t -> Json.t -> Result.t

(** Resolves a user-supplied path against [cwd], expanding [~]. *)
val resolve : cwd:string -> string -> string

(** Resolves a user-supplied path against the context cwd, expanding [~]. *)
val resolve_path : Context.t -> string -> string

val expand_home : string -> string
