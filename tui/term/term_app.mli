open! Core
open! Async
open Bonsai_term

(** The Bonsai value the driver hands back: the rendered view, the event handler
    and the action injector, plus the current model for tests. *)
module Result_ : sig
  type t =
    { model : Prigh_ui.App.Model.t
    ; view : View.t
    ; handler : Event.t -> unit Effect.t
    ; inject : Prigh_ui.App.Action.t -> unit Effect.t
    }
end

(** The reusable Bonsai app construction. [run] mounts it against a spawned
    backend; tests mount it against an in-memory transport. *)
val app
  :  Prigh_client.Client.t
  -> exit:(unit -> unit Bonsai.Effect.t)
  -> quit_requested:unit Ivar.t
  -> dimensions:Dimensions.t Bonsai.t
  -> local_ Bonsai.graph
  -> Result_.t Bonsai.t

(** The most recent [Result_.t] seen by the driver, so tests can render the pure
    screen from the model alongside the terminal frame. Reset by
    [with_test_driver]. *)
val latest_result : Result_.t option ref

(** Runs [f] with the real Bonsai_term driver on an in-memory tty (no fds are
    touched) and no background frame loop: the caller drives
    [Driver.compute_frame]. The driver is released afterwards. *)
module Test_terminal : sig
  type t

  val create : width:int -> height:int -> t

  (** Changes the reported size and wakes Notty's window-change listener, as a
      SIGWINCH would. *)
  val resize : t -> width:int -> height:int -> unit
end

val with_test_driver
  :  client:Prigh_client.Client.t
  -> terminal:Test_terminal.t
  -> writer:Async.Writer.t
  -> reader:Async.Reader.t
  -> ((Result_.t, unit, Prigh_ui.App.Action.t) Bonsai_term.Driver.t
      -> 'a Async.Deferred.Or_error.t)
  -> 'a Async.Deferred.Or_error.t

(** Runs the TUI over [transport] until the user quits. [hello] is sent first
    (name, cwd, session, token, ...); with [local_tools] (the path of the prigh
    binary) the session's tools run on this machine through [prigh tool-host]
    and [hello] advertises [tools: true]. *)
val run
  :  transport:Prigh_client.Transport.t
  -> hello:(string * Prigh_protocol.Json.t) list
  -> local_tools:string option
  -> unit Deferred.Or_error.t
