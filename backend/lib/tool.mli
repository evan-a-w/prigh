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
  ; background : Background_tasks.t option
    (** where [subagent] and [bash] with [background] start background work;
        [None] runs them synchronously *)
  ; execute : executor
    (** how tool calls of this agent (and of its subagents) are run; the
        default runs them in this process *)
  ; cache_key : string option
    (** the agent's [Provider.Request.cache_key], from which a subagent's
        is derived *)
  ; halt : unit -> string option
    (** why no further turn may start (the session's budget is spent), polled
        by subagents before each of their turns *)
  ; stdin : Stdin_feed.t option
    (** input for the process this call starts (a background job's [bash]);
        without it the process's stdin is empty *)
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
    -> ?background:Background_tasks.t
    -> ?cache_key:string
    -> ?halt:(unit -> string option)
    -> ?stdin:Stdin_feed.t
    -> env:Env.t
    -> cwd:string
    -> unit
    -> context

  type nonrec t = context
end

module Result = Tool_result

val name : t -> string

(** Runs the tool in this process, turning [Tool_args.Invalid] and other
    exceptions into error results. Output is repaired to valid UTF-8. *)
val execute : t -> Context.t -> Json.t -> Result.t

(** [execute] through the context's executor. *)
val execute_via : Context.t -> t -> Json.t -> Result.t

(** Resolves a user-supplied path against [cwd], expanding [~]. *)
val resolve : cwd:string -> string -> string

(** Resolves a user-supplied path against the context cwd, expanding [~]. *)
val resolve_path : Context.t -> string -> string

val expand_home : string -> string
