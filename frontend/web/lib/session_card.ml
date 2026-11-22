open! Core
open! Import

module Section = struct
  type t =
    | Parents
    | Subagents
    | Worktrees
  [@@deriving sexp_of, equal]
end

module Action = struct
  type t =
    | Fold of Section.t
    | Sheet of bool
  [@@deriving sexp_of]
end

type t =
  { subagents_open : bool
  ; worktrees_open : bool
  ; sheet : bool
  }
[@@deriving sexp_of, equal]

let default = { subagents_open = true; worktrees_open = false; sheet = false }

let is_open t (section : Section.t) =
  match section with
  | Parents -> true
  | Subagents -> t.subagents_open
  | Worktrees -> t.worktrees_open
;;

let apply t (action : Action.t) =
  match action with
  | Fold Parents -> t
  | Fold Subagents -> { t with subagents_open = not t.subagents_open }
  | Fold Worktrees -> { t with worktrees_open = not t.worktrees_open }
  | Sheet sheet -> { t with sheet }
;;

module Activity = struct
  type t =
    | Thinking
    | Working
    | Finished of
        { failed : bool
        ; ago : string
        }
  [@@deriving sexp_of, equal]

  let short_ago ~now at =
    match Rel_time.ago ~now at with
    | "just now" -> "now"
    | ago -> String.chop_suffix_if_exists ago ~suffix:" ago"
  ;;

  let of_agent (a : Agents.Agent.t) ~now =
    match a.status with
    | Running -> if Option.is_some a.current_tool then Working else Thinking
    | Complete | Failed ->
      Finished
        { failed = Agents.Status.equal a.status Failed
        ; ago = short_ago ~now (Option.value a.ended_at ~default:a.started_at)
        }
  ;;

  let to_string = function
    | Thinking -> "thinking…"
    | Working -> "working…"
    | Finished { failed = false; ago } -> ago
    | Finished { failed = true; ago } -> "failed " ^ ago
  ;;

  let running = function
    | Thinking | Working -> true
    | Finished _ -> false
  ;;
end
