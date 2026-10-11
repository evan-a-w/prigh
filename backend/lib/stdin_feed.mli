open! Core
open! Import

(** Input for a running process's stdin, written by [job_input] and taken by
    whoever feeds the process: [Process.run] on this machine, or the
    backend's forwarding to the tool host that runs it ([tool_exec_input]).
    Writes before the consumer starts are kept in order. *)

type t

val create : unit -> t

module Item : sig
  type t =
    | Data of string
    | Close
  [@@deriving sexp_of]
end

(** Queues [data] (when not empty), then the end of input with [close]. Fails
    when stdin was closed, the process has finished, or the consumer cannot
    pass input on ([refuse]). *)
val write : t -> string -> close:bool -> unit Or_error.t

(** Blocks until there is an item. *)
val take : t -> Item.t

(** Later writes fail with [reason]. *)
val refuse : t -> string -> unit

(** The process has finished: later writes fail. *)
val finish : t -> unit
