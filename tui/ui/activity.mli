open! Core
open! Import

(** What a running agent is doing, from its events: both frontends show it
    (the status line, the reply being streamed) so that a slow model, a
    thinking one and a failing one are told apart. *)

type t =
  | Idle
  | Working (** running, between steps (or joined part way through) *)
  | Waiting of P.Retry.t option
  (** asked the model, nothing back yet; the retry this attempt is *)
  | Retrying of P.Retry.t (** waiting to ask again after a transient error *)
  | Thinking
  | Writing
  | Running_tools
[@@deriving sexp_of, equal]

(** For an agent the backend says is running or not. *)
val of_running : bool -> t

(** The top level events of one agent (not those of its subagents). *)
val apply : t -> P.Event.t -> t

(** [t] as the backend's state has it: [Idle] when not running, at least
    [Working] when it is. *)
val set_running : t -> bool -> t

(** ["thinking"], ["retrying in 4s (attempt 2/6): HTTP 529: Overloaded"]...;
    [None] when idle. Lower case, one line. *)
val label : t -> string option

(** ["800ms"], ["4s"]. *)
val seconds : int -> string

(** The retry the agent waits for, or the attempt it waits on. *)
val retry : t -> P.Retry.t option
