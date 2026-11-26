open! Core
open! Import

(** [$process]: a long-lived program on a tool host whose stdout lines
    stream to the backend and whose stdin takes the backend's lines, over an
    ordinary exec ([tool_exec] with [stdin], [tool_exec_output],
    [tool_exec_input], [tool_exec_result], [tool_exec_cancel]). Coding-agent
    CLIs speaking JSON lines on stdio run this way on the session's host.

    Each output chunk and input write is one or more [\n]-terminated JSON
    frames ({!Up}, {!Down}). Stdout lines are numbered from 1; the host
    sends at most [window] lines beyond the last [Ack], after which it stops
    reading the program's stdout, so a slow consumer blocks the program
    rather than filling memory. Stderr is never mixed into the lines: a
    rate-capped copy goes up as [Err] and its tail ends up in the result.

    Arguments: [argv] (the program and its arguments), [env] (variables
    added to the host's environment), [window] (default 64), [max_line]
    (bytes, default 32 MiB; a longer line kills the program),
    [stderr_rate] (bytes of stderr forwarded per minute, default 64 KiB),
    [term_grace_ms] (how long a cancelled program has after SIGTERM,
    default 3000), [path] (the program's path, instead of looking
    [argv]'s first element up on [PATH]), [mcp] and [secret]. It runs in the
    exec's cwd.

    With [secret], the program gets [secret.value] and a newline on a
    socket as its descriptor {!secret_fd}, whose number is in its
    environment as [secret.fd_env]: not the secret itself, which every
    command it runs would inherit. The host closes its end once it wrote
    it. [Started] says whether it got it: an older host ignores [secret].

    With [mcp], the host also listens on a Unix socket in a private (0700)
    directory, for as long as the program runs (its path is
    {!Placeholder.mcp_socket}, for the bridge's MCP config only: it is not in
    the program's environment, which every command it runs inherits): each connection
    (a [prigh mcp-bridge] the program started as an MCP server) is relayed
    line by line as [Mcp] frames up and [Mcp_in] frames down, numbered by
    [conn]. These are not counted in the window. In [argv]'s arguments and
    [env]'s values, {!Placeholder.prigh} becomes the host's own prigh
    executable and {!Placeholder.mcp_socket} the socket, so that the
    program's MCP configuration can name the bridge. [Started] reports
    both, and the token a connection must send as its first line (else it is
    closed): the backend gives it only to the bridge, in its MCP config. *)

val op : string

(** The longest line, in bytes, a program's stdout or stdin may carry: twice
    JSON-escaped, it stays under the RPC's frame limit. *)
val max_line : int

module Up : sig
  type t =
    | Started of
        { pid : int
        ; path : string
        ; prigh : string (** this host's prigh executable *)
        ; mcp_socket : string option
        ; mcp_token : string option
          (** what a bridge presents first, else the socket closes *)
        ; secret : bool
          (** it got [secret] on its descriptor (a host predating that, or
              unable to, does not say so) *)
        }
    | Out of
        { seq : int
        ; line : string
        }
    | Err of string
    | Err_dropped of int (** bytes of stderr not forwarded *)
    | Mcp of
        { conn : int
        ; line : string
        } (** a line from an MCP connection *)
    | Mcp_closed of int (** the connection's bridge closed it *)
  [@@deriving sexp_of, equal]

  val to_line : t -> string
  val of_line : string -> t Or_error.t
end

module Down : sig
  type t =
    | In of string (** a line for the program's stdin, without its [\n] *)
    | Ack of int (** the consumer took every line up to this one *)
    | Close (** the end of the program's stdin *)
    | Signal of Signal.t (** to the program's process group *)
    | Mcp_in of
        { conn : int
        ; line : string
        } (** a line for an MCP connection, without its [\n] *)
    | Mcp_close of int (** the end of what the connection is sent *)
  [@@deriving sexp_of, equal]

  val to_line : t -> string
  val of_line : string -> t Or_error.t
end

module Placeholder : sig
  (** ["{{prigh}}"] *)
  val prigh : string

  (** ["{{mcp_socket}}"] *)
  val mcp_socket : string
end

module Outcome : sig
  type t =
    | Ran of
        { exit : Process.Exit.t
        ; stderr_tail : string
        }
    | Not_found of
        { program : string
        ; login_shell_path : string option
          (** where a login shell finds it, when the host's [PATH] does
              not have it *)
        }
    | Failed of string
  [@@deriving sexp_of]

  val to_result : t -> Tool.Result.t

  (** [None] for a result that is not a [$process] one (the backend's own,
      e.g. for a cancelled call or a host that went away). *)
  val of_result : Tool.Result.t -> t option
end

(** The program's descriptor carrying [secret]. *)
val secret_fd : int

module Secret : sig
  type t =
    { fd_env : string (** the variable naming the descriptor *)
    ; value : string (** one line, at most 16 KiB *)
    }
end

module Arguments : sig
  type t =
    { argv : string list
    ; env : (string * string) list
    ; env_unset : string list (** removed from the host's environment *)
    ; window : int
    ; max_line : int
    ; stderr_rate : int
    ; term_grace : Time_ns.Span.t
    ; path : string option
    ; mcp : bool
    ; secret : Secret.t option
    }

  (** The defaults for everything but [argv]. *)
  val create : argv:string list -> t

  val to_json : t -> Json.t
end

(** Runs [$process] on this machine; [stdin] is the exec's input, carrying
    {!Down} frames. [self] is this host's prigh executable (default: this
    process's). *)
val execute
  :  env:Env.t
  -> ?self:string
  -> stdin:Stdin_feed.t option
  -> cancel:Cancellation.t
  -> on_output:(string -> unit)
  -> cwd:string
  -> arguments:Json.t
  -> unit
  -> Tool.Result.t
