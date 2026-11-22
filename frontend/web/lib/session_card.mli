open! Core
open! Import

(** The session card's own state (in the chat's top-right corner; a sheet
    from the bottom on a phone): which of its sections are unfolded, and
    whether the phone's sheet is open. Whether the card shows beside the chat
    is [Model.session_card_open]. *)

module Section : sig
  type t =
    | Subagents
    | Worktrees
  [@@deriving sexp_of, equal]
end

module Action : sig
  type t =
    | Fold of Section.t (** fold or unfold it *)
    | Sheet of bool (** a phone's sheet opens or closes *)
  [@@deriving sexp_of]
end

type t =
  { subagents_open : bool
  ; worktrees_open : bool
  ; sheet : bool
  }
[@@deriving sexp_of, equal]

(** The subagents unfolded, the worktrees folded, the sheet closed. *)
val default : t

val is_open : t -> Section.t -> bool
val apply : t -> Action.t -> t

(** What a subagent is doing, as its row says it: ["thinking…"] (between
    tools), ["working…"] (in a tool), or how long ago it finished
    (["now"], ["2m"], ["3h"]), ["failed 2m"] when it failed. *)
module Activity : sig
  type t =
    | Thinking
    | Working
    | Finished of
        { failed : bool
        ; ago : string
        }
  [@@deriving sexp_of, equal]

  val of_agent : Agents.Agent.t -> now:Time_ns.t -> t
  val to_string : t -> string
  val running : t -> bool
end
