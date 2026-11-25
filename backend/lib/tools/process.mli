open! Core
open! Import

(** Subprocess execution with streamed output, timeout and cancellation.
    Killing the process on cancellation/timeout uses SIGKILL. *)

module Exit : sig
  type t =
    | Exited of int
    | Signaled of Signal.t
    | Timed_out
    | Cancelled
  [@@deriving sexp_of]

  val is_success : t -> bool
end

(** [stdin] is written to the process's stdin, which is then closed; with
    [input] instead, stdin stays open and gets [input]'s items until its
    [Close] (or the process exits, which [finish]es [input]).

    With [merge_stderr], the process's stderr is its stdout's pipe:
    [on_stdout] gets both in the order the process wrote them, and
    [on_stderr] nothing.

    On [cancel], with [term_grace] the process first gets the end of
    [input] and SIGTERM, and is killed only if it has not exited within
    [term_grace]; without it, it is killed straight away. [on_spawn] gets
    its pid once it started. [unset_env] are removed from the environment
    the child inherits. [extra_fds] are the child's besides its stdio: each
    descriptor as the number given (3 or more), and still the caller's to
    close. *)
val run
  :  env:Env.t
  -> ?cwd:string
  -> ?extra_env:(string * string) list
  -> ?unset_env:string list
  -> ?stdin:string
  -> ?input:Stdin_feed.t
  -> ?timeout:Time_ns.Span.t
  -> ?cancel:Cancellation.t
  -> ?on_stdout:(string -> unit)
  -> ?on_stderr:(string -> unit)
  -> ?merge_stderr:bool
  -> ?term_grace:Time_ns.Span.t
  -> ?on_spawn:(Pid.t -> unit)
  -> ?extra_fds:(int * Eio_unix.Fd.t) list
  -> prog:string
  -> args:string list
  -> unit
  -> Exit.t

module Output : sig
  type t =
    { exit : Exit.t
    ; stdout : string
    ; stderr : string
    }
  [@@deriving sexp_of]
end

val run_collect
  :  env:Env.t
  -> ?cwd:string
  -> ?extra_env:(string * string) list
  -> ?unset_env:string list
  -> ?stdin:string
  -> ?timeout:Time_ns.Span.t
  -> ?cancel:Cancellation.t
  -> prog:string
  -> args:string list
  -> unit
  -> Output.t
