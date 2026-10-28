open! Core
open! Import

type t =
  { spec : Tool_spec.t
  ; run : context -> Json.t -> Tool_result.t
  }

and context =
  { env : Env.t
  ; cwd : string
  ; home : unit -> string option
    (** the home directory of the tool host [cwd] is on, for [~] in the
        arguments of tools run in this process on that host's behalf (the
        backend's [subagent]); [None]: this process's [$HOME]. Only called
        for a path starting with [~] (it may ask the host). *)
  ; cancel : Cancellation.t
  ; on_output : string -> unit (** streamed partial output, e.g. from bash *)
  ; depth : int (** 0 for the main agent *)
  ; agent_id : string option
  ; call_id : string
  ; tools : t list (** tools available to this agent *)
  ; conversation : Conversation.t option
    (** the calling agent's conversation up to this call ([Agent_loop]
        fills it in), for [subagent]'s [fork] *)
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
  ; files : File_access.t
    (** where [read], [edit], [write] and [apply_patch] read and write file
        contents (default: the disk) *)
  ; approve : approver
    (** checks a model's tool call before it runs (the session's approval
        mode and rules, see [Approval]); subagents inherit it *)
  ; ask_user :
      call_id:string
      -> timeout_seconds:int option
      -> User_question.t list
      -> User_question.Reply.t
    (** asks the user (the [ask_user] tool, which passes its
        [timeout_seconds]); only the main agent can *)
  }

(** Runs a tool call. [Agent] installs one that forwards [on_host] tools to
    the session's active tool host. *)
and executor = context -> t -> Json.t -> Tool_result.t

(** [Some result] refuses the call with that result (denied, cancelled);
    [None] lets it run. *)
and approver = context -> t -> Json.t -> Tool_result.t option

module Context : sig
  val create
    :  ?cancel:Cancellation.t
    -> ?execute:executor
    -> ?on_output:(string -> unit)
    -> ?depth:int
    -> ?agent_id:string
    -> ?call_id:string
    -> ?tools:t list
    -> ?conversation:Conversation.t
    -> ?emit:(Agent_event.t -> unit)
    -> ?background:Background_tasks.t
    -> ?cache_key:string
    -> ?halt:(unit -> string option)
    -> ?stdin:Stdin_feed.t
    -> ?files:File_access.t
    -> ?approve:approver
    -> ?home:(unit -> string option)
    -> ?ask_user:
         (call_id:string
          -> timeout_seconds:int option
          -> User_question.t list
          -> User_question.Reply.t)
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

(** Resolves a user-supplied path against [cwd], expanding [~] to [home]
    (default: [$HOME]). *)
val resolve : ?home:string -> cwd:string -> string -> string

(** Resolves a user-supplied path against the context cwd, expanding [~] to
    the context's [home]. *)
val resolve_path : Context.t -> string -> string

(** [~] and [~/...] with [home] (default: [$HOME]). *)
val expand_home : ?home:string -> string -> string
