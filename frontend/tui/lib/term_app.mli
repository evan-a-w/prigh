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

(** The reusable Bonsai app construction. [run] mounts it against the real
    backend; tests mount it against an in-memory transport. The platform's
    connection effects ([reconnect], [/connect], [/signin], [/signout]) act
    on [link]. *)
val app
  :  ?start_on_activate:bool
  -> Backend_link.t
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
  :  link:Backend_link.t
  -> terminal:Test_terminal.t
  -> writer:Async.Writer.t
  -> reader:Async.Reader.t
  -> ((Result_.t, unit, Prigh_ui.App.Action.t) Bonsai_term.Driver.t
      -> 'a Async.Deferred.Or_error.t)
  -> 'a Async.Deferred.Or_error.t

(** Connects ({!Backend_link.start}) and runs the TUI until the user quits.
    A remote backend that rejects the credentials does not stop it: it starts
    disconnected, with the sign-in dialog open. *)
val run : Backend_link.t -> unit Deferred.Or_error.t
