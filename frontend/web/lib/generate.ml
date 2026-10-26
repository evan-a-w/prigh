open! Core
open! Import

module Target = struct
  type t =
    | Pr
    | Commit_message
    | Worktree_branch
    | Session_title
  [@@deriving sexp_of, equal]

  let kind : t -> Generated.Kind.t = function
    | Pr -> Pr
    | Commit_message -> Commit_message
    | Worktree_branch -> Branch_name
    | Session_title -> Session_title
  ;;

  let source = function
    | Pr ->
      "from the branch's commits, its diff and the repository's PR guidance"
    | Commit_message ->
      "from the staged changes, the session's goal and recent commits"
    | Worktree_branch -> "from the task typed here (or the session's goal)"
    | Session_title -> "from the conversation"
  ;;
end

module Running = struct
  type t =
    { id : string
    ; target : Target.t
    ; text : string
    ; before : string list
    }
  [@@deriving sexp_of, equal]
end

module Undo = struct
  type t =
    { target : Target.t
    ; before : string list
    }
  [@@deriving sexp_of, equal]
end

type t =
  { running : Running.t option
  ; undo : Undo.t option
  ; seq : int
  }
[@@deriving sexp_of, equal]

let empty = { running = None; undo = None; seq = 0 }

let start t target ~before =
  let seq = t.seq + 1 in
  let id = sprintf "gen-%d" seq in
  { running = Some { id; target; text = ""; before }; undo = None; seq }, id
;;

let delta t ~id piece =
  match t.running with
  | Some r when String.equal r.id id ->
    let r = { r with text = r.text ^ piece } in
    { t with running = Some r }, Some (r.target, r.text)
  | Some _ | None -> t, None
;;

let finish t ~id =
  match t.running with
  | Some r when String.equal r.id id ->
    ( { t with
        running = None
      ; undo = Some { target = r.target; before = r.before }
      }
    , Some r )
  | Some _ | None -> t, None
;;

let stop t =
  match t.running with
  | None -> t, None
  | Some r -> fst (finish t ~id:r.id), Some r.id
;;

let running_for t target =
  Option.exists t.running ~f:(fun r -> Target.equal r.target target)
;;

let undo_for t target =
  match t.undo with
  | Some u when Target.equal u.target target -> Some u.before
  | Some _ | None -> None
;;

let forget t target =
  match t.undo with
  | Some u when Target.equal u.target target -> { t with undo = None }
  | Some _ | None -> t
;;
