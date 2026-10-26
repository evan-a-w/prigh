open! Core
open! Import

module Source = struct
  type t =
    | Created
    | Entered
    | Pull_request of { number : int }
    | Agent of
        { agent : string
        ; task : string
        }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { path : string
  ; branch : string option [@jsonaf.option]
  ; root : string
  ; source : Source.t
  }
[@@deriving sexp, jsonaf, equal]

let of_agent (w : Agent_worktree.t) =
  { path = w.path
  ; branch = Some w.branch
  ; root = w.root
  ; source = Agent { agent = w.agent; task = w.task }
  }
;;

let of_location (l : Git_location.t) =
  Option.some_if
    (not l.main)
    { path = l.worktree; branch = l.branch; root = l.root; source = Entered }
;;

let contains ~path ~dir =
  String.equal path dir || String.is_prefix dir ~prefix:(path ^ "/")
;;
