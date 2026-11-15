open! Core
open! Import

type t =
  { agent : string
  ; task : string
  ; path : string
  ; branch : string
  ; root : string
  ; host : string option [@sexp.option] [@jsonaf.option]
  }
[@@deriving sexp, jsonaf, equal]

let report_line t =
  sprintf
    "[worktree: the work is on branch %s, checked out in %s (a worktree of \
     %s)%s; review it there, then merge the branch or remove the worktree \
     (%s)]"
    t.branch
    t.path
    t.root
    (Option.value_map t.host ~default:"" ~f:(fun host ->
       sprintf " on the tool host %s" host))
    (match t.host with
     | None -> "/worktree rm " ^ t.path
     | Some host ->
       sprintf "the worktree tool with host %s and action remove" host)
;;
