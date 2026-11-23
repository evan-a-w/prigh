open! Core
open! Import

(** The backend's end of a {!Host_process} ([$process]) on a tool host: the
    program's stdout as lines to read and its stdin as lines to write. It
    knows nothing of sessions or agents: it runs the exec through
    {!Exec.t}, which may be {!Host_ops.execute} in this process or an
    exec forwarded to a remote tool host.

    {!read_line} never needs a host round trip per line: lines are acked
    in batches, half a window at a time. The [on_output] the stream gives
    {!Exec.t} never blocks, as an exec's output may be delivered by the
    fiber serving the host's connection. *)

module Exec : sig
  (** Runs a host op with an open stdin, like [Agent.host_call]. *)
  type t =
    stdin:Stdin_feed.t
    -> cancel:Cancellation.t
    -> on_output:(string -> unit)
    -> name:string
    -> Json.t
    -> Tool.Result.t
end

module Ended : sig
  type t =
    | Exited of
        { program : string
        ; exit : Process.Exit.t
        ; stderr_tail : string
        }
    | Not_installed of
        { program : string
        ; host : string
        ; login_shell_path : string option
        }
    | Old_host of string (** the host's name; its prigh has no [$process] *)
    | Host_gone of string (** why: the host disconnected, restarted, ... *)
    | Broken of string (** a protocol error: lost lines, a bad frame *)
    | Stopped (** {!stop} *)
  [@@deriving sexp_of]

  (** What to tell the user, with what to do about it. *)
  val to_message : t -> string
end

module Started : sig
  type t =
    { pid : int
    ; prigh : string (** the host's prigh executable *)
    ; mcp_socket : string option (** with [mcp]: the side channel's socket *)
    ; mcp_token : string option
      (** with [mcp]: what the bridge presents first ({!Mcp_bridge}) *)
    }
  [@@deriving sexp_of]
end

module Mcp_event : sig
  type t =
    | Line of
        { conn : int
        ; line : string
        }
    | Closed of int
  [@@deriving sexp_of]
end

type t

(** Starts [arguments] on the host [host] (its name, for messages), in a
    fiber in [sw]. [on_stderr] gets the program's forwarded stderr and
    [on_mcp] what comes from its MCP side channel (with [mcp]: see
    {!Host_process}); neither may block. *)
val start
  :  sw:Switch.t
  -> exec:Exec.t
  -> host:string
  -> ?on_stderr:(string -> unit)
  -> ?on_mcp:(Mcp_event.t -> unit)
  -> Host_process.Arguments.t
  -> t

(** The program on its host once it started, or why it did not. *)
val started : t -> (Started.t, Ended.t) Result.t Promise.t

(** The next line of the program's stdout, blocking; [None] once it ended
    and every line was read. *)
val read_line : t -> string option

(** Writes [line] and a newline to the program's stdin. Fails once the
    stream ended. *)
val write_line : t -> string -> unit Or_error.t

(** The end of the program's stdin. *)
val close_stdin : t -> unit

(** Writes [line] and a newline to the MCP connection [conn]. *)
val mcp_write : t -> conn:int -> string -> unit Or_error.t

(** The end of what [conn] is sent. *)
val mcp_close : t -> conn:int -> unit

(** SIGINT to the program's process group. *)
val interrupt : t -> unit

(** Stops the program: the host closes its stdin, sends SIGTERM and after a
    grace period SIGKILL. *)
val stop : t -> unit

val ended : t -> Ended.t Promise.t
