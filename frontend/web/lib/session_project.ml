open! Core
open! Import

let name ?host_home (s : Session_summary.t) ~cwd =
  match s.repo with
  | Some r -> Prigh_ui.Session_worktrees.repo_name r
  | None ->
    let cwd = String.chop_suffix_if_exists cwd ~suffix:"/" in
    if
      Option.exists host_home ~f:(fun home ->
        String.equal cwd (String.chop_suffix_if_exists home ~suffix:"/"))
    then "~"
    else (
      match Filename.basename cwd with
      | "" | "." -> "/"
      | name -> name)
;;
