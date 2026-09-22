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
    }
  [@@deriving sexp_of]

  val backend_id : string
end

module State : sig
  type t =
    { session_id : string
    ; session_path : string
    ; session_name : string option
    ; cwd : string
    ; git_branch : string option
    ; model : Model.t
    ; thinking : Thinking.t
    ; running : bool
    ; message_count : int
    ; usage : Usage.t (** summed over assistant messages on the active path *)
    ; cost_usd : float
    ; context_tokens : int (** input tokens of the last request, if any *)
    ; active_host : string (** [Host.id]; may be absent from [hosts] *)
    ; hosts : Host.t list (** the backend first, then connected clients *)
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
        { steer : int
        ; follow_up : int
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
    { text : string
    ; attachments : string list
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
  -> ?thinking:Thinking.t
  -> cwd:string
  -> unit
  -> t

val subscribe : t -> f:(Event.t -> unit) -> unit
val state : t -> State.t
val session : t -> Session.t
val messages : t -> Message.t list

(** Attachments are paths (relative to the agent cwd or absolute) whose
    contents are appended to the user message as [<file>] blocks, read with
    [Tool_read] limits. *)

(** Starts a run. Fails if one is already running. *)
val prompt : ?attachments:string list -> t -> string -> unit Or_error.t

(** Queued and injected after the current turn's tool results, or starts a
    run when idle. *)
val steer : ?attachments:string list -> t -> string -> unit

(** Queued to run after the current run finishes, or starts a run when idle. *)
val follow_up : ?attachments:string list -> t -> string -> unit

(** Cancels the active run (if any) and clears the queues. Returns the texts
    that were queued and are therefore restored to the caller: steer messages
    in order, then follow-ups. Emits [Queue_update]. *)
val abort : t -> string list

(** Pops the most recently queued message: follow-ups take priority over steer
    messages. Emits [Queue_update] when a message was removed. *)
val dequeue : t -> Queued.t option

(** Runs [command] through the bash machinery, emitting [Tool_start],
    [Tool_output] and [Tool_end] events under a synthetic [shell-<n>] call id.
    When [add_to_context] is set, appends [\$ <command>\n<output>] to the
    session as a user message. Fails while a run is in progress. *)
val shell
  :  t
  -> command:string
  -> add_to_context:bool
  -> Tool_result.t Or_error.t

val is_running : t -> bool

(** Blocks until no run is active and the queues are empty. *)
val wait_idle : t -> unit

val set_model : t -> Model.t -> unit
val set_thinking : t -> Thinking.t -> unit

(** The agent's persistent configuration (loaded from [~/.prigh/config.json] at
    creation). *)
val config : t -> Config.t

(** Writes the configuration and emits [Event.Config_changed]. *)
val set_config : t -> Config.t -> unit Or_error.t

(** Answers a pending [Tool_confirm] request. Fails if no confirmation is
    outstanding for [call_id]. *)
val respond_confirm : t -> call_id:string -> allow:bool -> unit Or_error.t

val compact : t -> string Or_error.t

(** In-place session replacement for a single-agent embedding (the CLI and
    tests). [Rpc_server] instead keeps one agent per session and moves
    clients between them. *)
val new_session : t -> unit

val switch_session : t -> path:string -> unit Or_error.t
val fork : t -> ?at:string -> unit -> unit Or_error.t
val rewind : t -> to_:string -> unit Or_error.t
val set_session_name : t -> string -> unit

(** Changes the working directory used by tools and the system prompt, and
    records it in the session. Fails while a run is in progress. *)
val set_cwd : t -> path:string -> unit Or_error.t

(** Refuses to delete the active session. *)
val delete_session : t -> path:string -> unit Or_error.t

(** Writes the session to [path] (default: [<sessions_dir>/exports/...]) and
    returns the path written. *)
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

(** Registers a client as a possible tool host. If the current active host is
    not connected, the new host becomes active (and the session cwd becomes
    its cwd). *)
val add_host : t -> Host.t -> unit

(** Forgets a host; its in-flight tool executions fail. *)
val remove_host : t -> string -> unit

val hosts : t -> Host.t list
val active_host : t -> string

(** Switches where tools run from the next call on; the session cwd becomes
    the host's. Fails for an unknown host. *)
val set_active_host : t -> string -> unit Or_error.t

(** Streamed output of a remote execution, relayed by the host. *)
val tool_exec_output : t -> exec_id:string -> chunk:string -> unit Or_error.t

(** Completes a remote execution. *)
val tool_exec_result
  :  t
  -> exec_id:string
  -> text:string
  -> is_error:bool
  -> unit Or_error.t
