open! Core
open! Import

(** Runs tools and terminals on this machine for a backend.

    As the stdio worker ([prigh tool-host]), the frontend spawns it and
    forwards the backend's events as JSON lines:
    - [{"type":"exec","exec_id","name","arguments","cwd"}]
    - [{"type":"cancel","exec_id"}]
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

(** Serves JSON lines until [input] ends. [terminals] defaults to
    {!Terminals.create}'s defaults, created on first use. *)
val run
  :  env:Env.t
  -> ?terminals:Terminals.t Lazy.t
  -> input:_ Eio.Flow.source
  -> output:_ Eio.Flow.sink
  -> unit
  -> unit

(** Connects to a backend's JSON-lines port as an RPC client, says [hello]
    as a tool host and serves its [tool_exec], [tool_exec_cancel] and
    [terminal_*] events, reconnecting with exponential backoff (from
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
  -> host:string
  -> port:int
  -> token:string option
  -> ?user:string (** the namespace's name, sent in [hello] *)
  -> ?host_id:string
  -> name:string
  -> cwd:string
  -> unit
  -> Error.t
