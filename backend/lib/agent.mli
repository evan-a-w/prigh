open! Core
open! Import

(** Orchestrates one interactive conversation: owns the session, model
    settings, the running loop (at most one at a time), the steer/follow-up
    queues, and broadcasts events to subscribers. *)

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
