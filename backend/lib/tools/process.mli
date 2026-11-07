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
    [on_stderr] nothing. *)
val run
  :  env:Env.t
  -> ?cwd:string
  -> ?extra_env:(string * string) list
  -> ?stdin:string
  -> ?input:Stdin_feed.t
  -> ?timeout:Time_ns.Span.t
  -> ?cancel:Cancellation.t
  -> ?on_stdout:(string -> unit)
  -> ?on_stderr:(string -> unit)
  -> ?merge_stderr:bool
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
  -> ?stdin:string
  -> ?timeout:Time_ns.Span.t
  -> ?cancel:Cancellation.t
  -> prog:string
  -> args:string list
  -> unit
  -> Output.t
