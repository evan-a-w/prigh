open! Core
open! Import

module Section = struct
  type t =
    | Parents
    | Subagents
    | Ended (** the finished and failed subagents, in one row *)
    | Jobs
    | Ended_jobs (** the jobs that exited or were killed, in one row *)
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
  ; ended_open : bool
  ; jobs_open : bool
  ; ended_jobs_open : bool
  ; worktrees_open : bool
  ; sheet : bool
  }
[@@deriving sexp_of, equal]

let default =
  { subagents_open = true
  ; ended_open = false
  ; jobs_open = true
  ; ended_jobs_open = false
  ; worktrees_open = false
  ; sheet = false
  }
;;

let for_session t =
  { t with ended_open = false; ended_jobs_open = false; sheet = false }
;;

let is_open t (section : Section.t) =
  match section with
  | Parents -> true
  | Subagents -> t.subagents_open
  | Ended -> t.ended_open
  | Jobs -> t.jobs_open
  | Ended_jobs -> t.ended_jobs_open
  | Worktrees -> t.worktrees_open
;;

let apply t (action : Action.t) =
  match action with
  | Fold Parents -> t
  | Fold Subagents -> { t with subagents_open = not t.subagents_open }
  | Fold Ended -> { t with ended_open = not t.ended_open }
  | Fold Jobs -> { t with jobs_open = not t.jobs_open }
  | Fold Ended_jobs -> { t with ended_jobs_open = not t.ended_jobs_open }
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

module Job_state = struct
  type t =
    | Running
    | Done
    | Killed
    | Failed of string
  [@@deriving sexp_of, equal]

  let of_job (j : Agents.Job.t) =
    match j.info.running, j.info.exit with
    | true, _ -> Running
    | false, (None | Some "exited 0") -> Done
    | false, Some "killed" -> Killed
    | false, Some exit ->
      (match String.chop_prefix exit ~prefix:"exited " with
       | Some code -> Failed ("exit " ^ code)
       | None -> Failed "failed")
  ;;

  let to_string t ~elapsed =
    let ran =
      Option.some_if
        Time_ns.Span.(elapsed >= second)
        (Agents.format_span elapsed)
    in
    let with_ran word =
      Option.value_map ran ~default:word ~f:(fun ran -> word ^ " " ^ ran)
    in
    match t with
    | Running -> Agents.format_span elapsed
    | Done -> with_ran "done"
    | Killed -> with_ran "killed"
    | Failed word -> with_ran word
  ;;
end
