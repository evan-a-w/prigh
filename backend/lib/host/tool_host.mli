open! Core
open! Import

(** Runs tools and terminals on this machine for a backend.

    As the stdio worker ([prigh tool-host]), the frontend spawns it and
    forwards the backend's events as JSON lines:
    - [{"type":"exec","exec_id","name","arguments","cwd"}]
    - [{"type":"cancel","exec_id"}]
    - [{"type":"input","exec_id","data","close"}] (stdin for an exec that
      had ["stdin": true])
    - [{"type":"terminal_open","term_id","key","cwd","cols","rows"}]
    - [{"type":"terminal_frame","term_id","kind","data"}]
    - [{"type":"terminal_close","term_id"}]

    and relays its replies to the backend as the requests of the same names
    ([output] as [tool_exec_output], [result] as [tool_exec_result]):
    - [{"type":"output","exec_id","chunk"}]
    - [{"type":"result","exec_id","text","is_error"}]
    - [{"type":"terminal_frame","term_id","kind","data"}]
    - [{"type":"terminal_closed","term_id"}] (not sent when the frontend
      closed the terminal)

    Frames carry [kind] ["binary"] (base64 [data]) or ["text"]; their
    content is the {!Terminals} protocol. Each exec and terminal runs in its
    own fiber. *)

(** What a backend sends a tool host and what it answers with, for a client
    that is a tool host besides other things (the ACP adapter). *)
module Reply : sig
  type t

  (** [tool_exec_output], [tool_exec_result], [terminal_frame] or
      [terminal_closed]: the RPC method relaying it to the backend. *)
  val method_ : t -> string

  val fields : t -> (string * Json.t) list
end

module Worker : sig
  type t

  (** [files] is where an exec's file tools read and write contents, given
      the session ([tool_exec]'s [session_id]) and the tool call it runs
      for (default: the disk). *)
  val create
    :  env:Env.t
    -> sw:Switch.t
    -> terminals:Terminals.t Lazy.t
    -> mcp:Mcp_hub.t
    -> default_cwd:string
    -> ?files:
         (session_id:string option -> call_id:string option -> File_access.t)
    -> send:(Reply.t -> unit)
    -> unit
    -> t

  (** Handles a backend event meant for tool hosts ([tool_exec],
      [tool_exec_cancel], [tool_exec_input], [terminal_*]); ignores the
      others. *)
  val handle_event : t -> Json.t -> unit

  (** Cancels the running execs and closes the terminal viewers. *)
  val stop : t -> unit
end

(** What a tool host announces in [hello]'s [features]. *)
val features : string list

(** Serves JSON lines until [input] ends. [terminals] defaults to
    {!Terminals.create}'s defaults, created on first use. *)
val run
  :  env:Env.t
  -> ?terminals:Terminals.t Lazy.t
  -> input:_ Eio.Flow.source
  -> output:_ Eio.Flow.sink
  -> unit
  -> unit

(** Connects to a backend ({!Backend_socket}: its JSON-lines port, or its
    WebSocket for a URL) as an RPC client, says [hello]
    as a tool host (with the feature [exec_input]) and serves its
    [tool_exec], [tool_exec_cancel], [tool_exec_input] and [terminal_*]
    events, reconnecting with exponential backoff (from
    [initial_backoff], default 0.5s, up to [max_backoff], default 10s) when
    the connection fails or drops. Every [hello] carries the same [host_id]
    (default: a random {!Host_id.generate}; [prigh tool-host] passes
    {!Host_id.load_or_create}'s), so the backend sees a reconnect as the
    same host and its sessions resume on it. Returns only when the backend
    refuses the credentials (retrying cannot help then); [log] defaults to
    stderr. *)
val connect
  :  env:Env.t
  -> ?terminals:Terminals.t Lazy.t
  -> ?log:(string -> unit)
  -> ?initial_backoff:Time_ns.Span.t
  -> ?max_backoff:Time_ns.Span.t
  -> ?on_connect:(unit -> unit) (** after each accepted [hello] *)
  -> backend:Backend_address.t
  -> token:string option
  -> ?user:string (** the namespace's name, sent in [hello] *)
  -> ?host_id:string
  -> name:string
  -> cwd:string
  -> unit
  -> Error.t
