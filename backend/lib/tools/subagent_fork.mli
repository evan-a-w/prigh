open! Core
open! Import

(** [subagent]'s [fork]: a subagent that starts from its caller's
    conversation rather than a blank one.

    The call that starts it is pending in that conversation (as are any
    calls made alongside it): the fork gets a result for each, and then its
    task, framed so that it knows it is the fork. When it uses its caller's
    model and all its tools, it is sent its caller's system prompt and tool
    specs too, so that the provider's prompt cache covers the whole shared
    prefix ({!shared_tools}). *)

(** The conversation and then a result for each tool call of its last
    (assistant) message: for [call_id], that this call started the fork;
    for the others, that the caller runs them. *)
val context : Conversation.t -> call_id:string -> Message.t list

(** The caller's [tools], in order and with their specs, for a fork at
    [depth]: those it may not use there ({!Tools.unavailable}) refuse when
    called, saying why. Also returns their names. *)
val shared_tools : Tool.t list -> depth:int -> Tool.t list * string list

module Framing : sig
  type t =
    { task : string
    ; extra_context : string option (** [subagent]'s [context] *)
    ; parent_cwd : string
    ; cwd : string
    ; worktree : Agent_worktree.t option
    ; refused : string list (** tools it sees but may not use *)
    ; sync_subagents : bool
      (** it sees its caller's [subagent] tool, which runs synchronously
          for it *)
    }
end

(** The fork's first message: who it is, where it works, and its task. *)
val prompt : Framing.t -> Message.t
