open! Core
open! Import

type t =
  { agent : string
  ; task : string
  ; path : string
  ; branch : string
  ; root : string
  }
[@@deriving sexp, jsonaf, equal]

let report_line t =
  sprintf
    "[worktree: the work is on branch %s, checked out in %s (a worktree of \
     %s); review it there, then merge the branch or remove the worktree \
     (/worktree rm %s)]"
    t.branch
    t.path
    t.root
    t.path
;;
