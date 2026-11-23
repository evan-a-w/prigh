open! Core
open! Import

(** What a composer session tells an external agent that takes it over: on
    a switch of adapter (Codex, Claude Code, an ACP agent) or tool host, or
    when its thread cannot be resumed. Not a summary written by a model but
    a selection of the session's own messages, verbatim, within a token
    budget, under a fixed header that names where the session came from,
    what was left out and how to read it (the [sessions] tool prigh offers
    the agent). Historical material is marked as context, not a request.

    The target gets everything ([Full]) when it has no thread on this host
    yet, else only what came after its last turn ([Since]). *)

module Strategy : sig
  type t =
    | Full
    | Since of { entry : string (** the last entry the target saw *) }
  [@@deriving sexp_of, equal]
end

type t =
  { text : string
  ; strategy : Strategy.t
  ; covered : (int * int) option
    (** the first and last message index (as the [sessions] tool's [read]
        counts them) considered *)
  ; included : int list (** the message indices quoted, oldest first *)
  ; omitted : int list (** considered but left out, oldest first *)
  ; tokens : int (** estimated size of [text] *)
  }
[@@deriving sexp_of]

(** 24k tokens. *)
val default_budget : int

(** Whether [adapter] needs a handoff at all: prigh's own harness reads the
    session's messages itself. *)
val needed : Adapter_id.t -> bool

(** The handoff for [to_] on [host] in [cwd]; [None] when there is nothing
    it has not seen. [from] is the engine the session ran on until now
    ([None]: none yet); [from_host] and [from_cwd] default to those of the
    session's latest external thread. *)
val build
  :  session:Session.t
  -> from:Engine_choice.t option
  -> ?from_host:string
  -> ?from_cwd:string
  -> to_:Adapter_id.t
  -> host:string
  -> cwd:string
  -> ?budget:int
  -> unit
  -> t option
