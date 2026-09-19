open! Core
open! Import

(** Orchestrates one interactive conversation: owns the session, model
    settings, the running loop (at most one at a time), the steer/follow-up
    queues, and broadcasts events to subscribers. *)

module State : sig
  type t =
    { session_id : string
    ; session_path : string
    ; cwd : string
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

module Event : sig
  type t =
    | Loop of Agent_event.t
    | State_changed of State.t
    | Compacted of { summary : string }
    | Notice of string
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

(** Starts a run. Fails if one is already running. *)
val prompt : t -> string -> unit Or_error.t

(** Queued and injected after the current turn's tool results, or starts a
    run when idle. *)
val steer : t -> string -> unit

(** Queued to run after the current run finishes, or starts a run when idle. *)
val follow_up : t -> string -> unit

(** Cancels the active run (if any) and clears the queues. Returns the texts
    that were queued and are therefore restored to the caller: steer messages
    in order, then follow-ups. Emits [Queue_update]. *)
val abort : t -> string list

val is_running : t -> bool

(** Blocks until no run is active and the queues are empty. *)
val wait_idle : t -> unit

val set_model : t -> Model.t -> unit
val set_thinking : t -> Thinking.t -> unit
val compact : t -> string Or_error.t
val new_session : t -> unit
val switch_session : t -> path:string -> unit Or_error.t
val fork : t -> ?at:string -> unit -> unit Or_error.t
val rewind : t -> to_:string -> unit Or_error.t
