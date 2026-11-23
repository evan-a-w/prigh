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
    default 3000) and [path] (the program's path, instead of looking
    [argv]'s first element up on [PATH]). It runs in the exec's cwd. *)

val op : string

module Up : sig
  type t =
    | Started of
        { pid : int
        ; path : string
        }
    | Out of
        { seq : int
        ; line : string
        }
    | Err of string
    | Err_dropped of int (** bytes of stderr not forwarded *)
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
  [@@deriving sexp_of, equal]

  val to_line : t -> string
  val of_line : string -> t Or_error.t
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

module Arguments : sig
  type t =
    { argv : string list
    ; env : (string * string) list
    ; window : int
    ; max_line : int
    ; stderr_rate : int
    ; term_grace : Time_ns.Span.t
    ; path : string option
    }

  (** The defaults for everything but [argv]. *)
  val create : argv:string list -> t

  val to_json : t -> Json.t
end

(** Runs [$process] on this machine; [stdin] is the exec's input, carrying
    {!Down} frames. *)
val execute
  :  env:Env.t
  -> stdin:Stdin_feed.t option
  -> cancel:Cancellation.t
  -> on_output:(string -> unit)
  -> cwd:string
  -> arguments:Json.t
  -> unit
  -> Tool.Result.t
