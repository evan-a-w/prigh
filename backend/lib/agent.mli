open! Core
open! Import

(** Orchestrates one interactive conversation: owns the session, model
    settings, the running loop (at most one at a time), the steer/follow-up
    queues, and broadcasts events to subscribers. *)

module Host : sig
  (** Where a session's [on_host] tools run: the backend itself (id
      [backend_id]) or a connected client that advertised tool support. *)
  type t =
    { id : string
    ; name : string
    ; cwd : string
    ; session_id : string option (** the session the client is attached to *)
    ; session_name : string option
    }
  [@@deriving sexp_of, equal]

  val backend_id : string
end

module State : sig
  type t =
    { session_id : string
    ; session_path : string
    ; session_name : string option
    ; session_description : string option
    ; cwd : string
    ; git_branch : string option
    ; model : Model.t
    ; thinking : Thinking.t
    ; running : bool
    ; message_count : int
    ; usage : Usage.t
      (** assistant messages on the active path, plus subagents and [btw] calls since the agent loaded the session *)
    ; cost_usd : float
    ; context_tokens : int (** input tokens of the last request, if any *)
    ; active_host : string
      (** [Host.id]; may be absent from [hosts], or [""] when the backend
              host is disabled and no client host was ever adopted *)
    ; hosts : Host.t list
      (** the backend first (unless disabled), then connected clients *)
    ; subagents : Background_tasks.Summary.t list
      (** background subagents still running or not yet delivered *)
    ; jobs : Background_tasks.Summary.t list
      (** background shell jobs still running or not yet delivered *)
    }
  [@@deriving sexp_of]
end

module Session_stats : sig
  type t =
    { message_count : int
    ; turns : int (** assistant messages *)
    ; tool_calls : (string * int) list (** by tool name *)
    ; usage : Usage.t
    ; cost_usd : float
    ; context_percent : float
    ; model_changes : int
    ; compactions : int
    ; duration_seconds : float
    }
  [@@deriving sexp_of]
end

module Event : sig
  type t =
    | Loop of Agent_event.t
    | State_changed of State.t
    | Compacted of { summary : string }
    | Notice of string
    | Config_changed of Config.t
    | Queue_update of
        { steer : string list (** the queued texts, in order *)
        ; follow_up : string list
        }
    | Tool_exec of
        { host : string (** only delivered to this host *)
        ; exec_id : string
        ; call_id : string
        ; name : string
        ; arguments : Json.t
        ; cwd : string
        }
    | Tool_exec_cancel of
        { host : string
        ; exec_id : string
        }
  [@@deriving sexp_of]
end

module Queued : sig
  type t =
    { text : string (** as typed, e.g. [/skill:NAME ARGS] *)
    ; skill : string option (** the expanded skill invocation *)
    ; attachments : string list
    ; images : Image.t list
    }
  [@@deriving sexp_of]
end

(** A tool call waiting for [respond_confirm]. *)
module Pending_confirm : sig
  type t =
    { call_id : string
    ; name : string
    ; summary : string
    }
  [@@deriving sexp_of]
end

type t

val create
  :  env:Env.t
  -> sw:Switch.t
  -> provider:Provider.t
  -> tools:Tool.t list
  -> sessions_dir:string
  -> home:string
  -> ?session:Session.t
  -> ?model:Model.t
       (** default: the config's [default_model], else [fallback_model] *)
  -> ?thinking:Thinking.t
       (** default: the config's [default_thinking], else [Off] *)
  -> ?fallback_model:Model.t (** default: [Model.default] *)
  -> ?models:Model_registry.t
       (** resolves the session's and the config's model keys *)
  -> ?auto_describe:bool
       (** write a [Session_description] after a turn once the conversation
           is long enough (default: false) *)
  -> ?backend_host:bool
       (** whether the backend itself is a tool host (default: true). Without
           it, [on_host] tools, instructions, path listings and the git branch
           never touch the backend's filesystem, and a session adopts the
           first connected client host when its own is missing. *)
  -> ?mcp:Mcp_hub.t
  -> ?use_default_cwd:bool
       (** start a new session in the config's [default_cwd] rather than
           [cwd] (default: true; false when [cwd] was given explicitly) *)
       (** the backend's MCP servers, when it is the tool host; without it
           sessions use no MCP servers on any host *)
  -> cwd:string
  -> unit
  -> t

val subscribe : t -> f:(Event.t -> unit) -> unit
val state : t -> State.t
val session : t -> Session.t
val env : t -> Env.t
val messages : t -> Message.t list

(** Attachments are paths (relative to the agent cwd or absolute) whose
    contents are appended to the user message as [<file>] blocks, read with
    [Tool_read] limits (image files are attached as images, like [images]).

    A text [/skill:NAME ARGS] invokes the skill [NAME] from the tool host
    ({!Skill.expand}); an unknown name is an error. *)

(** Starts a run. Fails if one is already running. *)
val prompt
  :  ?attachments:string list
  -> ?images:Image.t list
  -> t
  -> string
  -> unit Or_error.t

(** Queued and injected after the current turn's tool results, or starts a
    run when idle. *)
val steer
  :  ?attachments:string list
  -> ?images:Image.t list
  -> t
  -> string
  -> unit Or_error.t

(** Queued to run after the current run finishes, or starts a run when idle. *)
val follow_up
  :  ?attachments:string list
  -> ?images:Image.t list
  -> t
  -> string
  -> unit Or_error.t

(** The skills on the active tool host. *)
val skills : t -> Skill.t list Or_error.t

(** The MCP servers for the session on the active tool host, starting those
    not running yet; [reconnect] restarts failed ones. Their tools are
    offered to the model from the next run on (each run starts by asking the
    host again), and problems are reported once each as [Notice]s. *)
val mcp_servers : ?reconnect:bool -> t -> Mcp_tools.Listing.t Or_error.t

(** Approves a project's MCP server on the host and starts it. *)
val approve_mcp
  :  t
  -> source:string
  -> server:string
  -> Mcp_tools.Listing.t Or_error.t

(** Cancels the active run (if any) and clears the queues. Returns the texts
    that were queued and are therefore restored to the caller: steer messages
    in order, then follow-ups. Emits [Queue_update]. *)
val abort : t -> string list

(** Pops the most recently queued message: follow-ups take priority over steer
    messages. Emits [Queue_update] when a message was removed. *)
val dequeue : t -> Queued.t option

(** The texts of the queued steer and follow-up messages, in order (what the
    last [Queue_update] said). *)
val queued_texts : t -> string list * string list

(** Confirmations still unanswered, by call id. *)
val pending_confirms : t -> Pending_confirm.t list

(** Runs [command] through the bash machinery, emitting [Tool_start],
    [Tool_output] and [Tool_end] events under a synthetic [shell-<n>] call id.
    When [add_to_context] is set, appends [\$ <command>\n<output>] to the
    session as a user message. Fails while a run is in progress. *)
val shell
  :  t
  -> command:string
  -> add_to_context:bool
  -> Tool_result.t Or_error.t

(** Whether the main loop is running (background subagents do not count). *)
val is_running : t -> bool

(** Blocks until no run is active, the queues are empty and no background
    subagent or job is running (their deliveries included). *)
val wait_idle : t -> unit

(** {2 Background subagents and jobs}

    The [subagent] tool and [bash] with [background] start them and return at
    once. A finished one's report becomes a user message (several finishing
    together share one): an idle agent starts a turn for it, a running one
    injects it at the next turn boundary (with steering), unless
    [subagent_wait]/[job_wait]/... already returned it. [abort] leaves them
    running; after an abort, reports that are ready wait for the next run. *)

val has_running_subagents : t -> bool

(** Subagents or jobs. *)
val has_running_background : t -> bool

(** Every subagent run since the agent was created (or its session was
    replaced), with status and activity, oldest first. *)
val subagents : t -> Subagent_log.Summary.t list

(** One of [subagents] by agent id or starting tool call id, with its
    transcript. *)
val subagent
  :  t
  -> string
  -> (Subagent_log.Summary.t * Timed_message.t list) option

(** Cancels one; its partial report is still delivered. *)
val cancel_subagent : t -> agent_id:string -> unit Or_error.t

(** Cancels every running subagent and job; with [discard] their reports are
    dropped. *)
val cancel_background : ?discard:bool -> t -> unit

(** Every job of this agent, oldest first. *)
val jobs : t -> Background_tasks.Task.t list

(** Kills a running job (on its host); its report is still delivered. *)
val kill_job : t -> job_id:string -> unit Or_error.t

(** The last [lines] lines of a job's output, under a header line. *)
val job_output : t -> job_id:string -> lines:int -> string Or_error.t

(** Starts [command] as a background job on the active host (for [!&cmd]);
    returns its id. *)
val start_job : t -> command:string -> string

val set_model : t -> Model.t -> unit
val set_thinking : t -> Thinking.t -> unit

(** The agent's persistent configuration (loaded from [~/.prigh/config.json] at
    creation). *)
val config : t -> Config.t

(** Writes the configuration and emits [Event.Config_changed]. *)
val set_config : t -> Config.t -> unit Or_error.t

(** Saves the current model and thinking level as the defaults for new
    sessions. Rereads the config file first, so settings changed by other
    sessions are kept. *)
val save_as_default : t -> unit Or_error.t

(** Answers a pending [Tool_confirm] request. Fails if no confirmation is
    outstanding for [call_id]. *)
val respond_confirm : t -> call_id:string -> allow:bool -> unit Or_error.t

(** [instructions]: what the summary should focus on (the user's). *)
val compact : ?instructions:string -> t -> string Or_error.t

(** Answers a side question with one tool-less model call over a snapshot of
    the conversation (see {!Btw}), concurrently with any run and without
    touching the session; [on_delta] receives the streamed text. The usage
    counts towards [State.usage]/[cost_usd]. Returns the reply and its cost. *)
val btw
  :  t
  -> question:string
  -> cancel:Cancellation.t
  -> on_delta:(string -> unit)
  -> (Message.Assistant.t * float) Or_error.t

(** In-place session replacement for a single-agent embedding (the CLI and
    tests); background subagents are cancelled and their reports dropped.
    [Rpc_server] instead keeps one agent per session and moves clients
    between them, so they keep running and deliver to their own session. *)
val new_session : t -> unit

val switch_session : t -> path:string -> unit Or_error.t
val fork : t -> ?at:string -> unit -> unit Or_error.t
val rewind : t -> to_:string -> unit Or_error.t
val set_session_name : t -> string -> unit

(** Changes the working directory used by tools and the system prompt, and
    records it in the session. Fails while a run is in progress. *)
val set_cwd : t -> path:string -> unit Or_error.t

(** Paths under the session cwd matching [prefix], listed on the active tool
    host (for [@] completion). *)
val list_paths : t -> prefix:string -> Json.t Or_error.t

(** Directory completions for [prefix] (absolute, [~/] or relative to the
    session cwd on that host), listed on [host] (default: the active one),
    in the notation the prefix used. *)
val list_dirs : ?host:string -> t -> prefix:string -> Json.t Or_error.t

(** Refuses to delete the active session. *)
val delete_session : t -> path:string -> unit Or_error.t

(** Writes the session to [path] (default: [<sessions_dir>/exports/...]) and
    returns the path written. Without the backend host it is written on the
    active tool host instead (default: a file named like the session in the
    cwd). *)
val export
  :  t
  -> format:Session.Export_format.t
  -> ?path:string
  -> unit
  -> string Or_error.t

(** Copies a session file into the sessions directory and switches to it;
    returns the new path. *)
val import_session : t -> path:string -> string Or_error.t

val session_stats : t -> Session_stats.t

(** {2 Tool hosts} *)

(** Replaces the client hosts (every connected client able to run tools,
    whichever session it is attached to). In-flight executions on hosts that
    are gone fail. Hosts keep the cwd this session last used on them. *)
val set_hosts : t -> Host.t list -> unit

(** Makes [id] the active host (with its own cwd) unless the user pinned a
    host that is still connected; called when a client attaches. *)
val prefer_host : t -> string -> unit

(** The backend first (unless disabled), then the client hosts. *)
val hosts : t -> Host.t list

val active_host : t -> string

(** Switches where tools run from the next call on. The session cwd becomes
    [cwd], which must be a directory on that host (resolved there; [~/] is
    expanded), or the host's own cwd when omitted. Fails for an unknown host or
    a missing directory without switching. *)
val set_active_host : t -> string -> cwd:string option -> unit Or_error.t

(** Streamed output of a remote execution, relayed by the host. *)
val tool_exec_output : t -> exec_id:string -> chunk:string -> unit Or_error.t

(** Completes a remote execution. *)
val tool_exec_result : t -> exec_id:string -> Tool_result.t -> unit Or_error.t
