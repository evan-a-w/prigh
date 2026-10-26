open! Core
open! Import

module Node = struct
  type t =
    { worktree : P.Session_worktree.t
    ; listed : P.Worktree.t option
    ; here : bool
    }
  [@@deriving sexp_of, equal]

  let label t = P.Session_worktree.label t.worktree

  let agent t =
    match t.worktree.source with
    | Agent _ -> true
    | Created | Entered | Pull_request _ -> false
  ;;
end

let repo_name (r : P.Repo.t) =
  match Filename.basename r.root with
  | "" | "/" | "." -> r.root
  | name -> String.chop_suffix_if_exists name ~suffix:".git"
;;

let place (s : P.Session_summary.t) =
  Option.map s.repo ~f:(fun r ->
    match r.branch with
    | Some branch -> sprintf "%s ⎇ %s" (repo_name r) branch
    | None -> repo_name r)
;;

let of_session ?(listings = []) ?cwd (s : P.Session_summary.t) =
  let cwd = Option.value cwd ~default:s.cwd in
  List.filter_map s.worktrees ~f:(fun w ->
    let listing =
      List.find listings ~f:(fun (l : P.Worktree.Listing.t) ->
        String.equal l.root w.root)
    in
    let listed =
      Option.bind listing ~f:(fun l ->
        List.find l.worktrees ~f:(fun (lw : P.Worktree.t) ->
          String.equal lw.path w.path))
    in
    match listing, listed with
    | Some _, None -> None
    | _ ->
      let worktree =
        match listed with
        | Some l when Option.is_some l.branch -> { w with branch = l.branch }
        | _ -> w
      in
      Some
        { Node.worktree
        ; listed
        ; here = P.Session_worktree.contains w ~dir:cwd
        })
;;

let roots sessions =
  List.concat_map sessions ~f:(fun (s : P.Session_summary.t) ->
    List.map s.worktrees ~f:(fun w -> w.root))
  |> List.dedup_and_sort ~compare:String.compare
;;

let summary nodes =
  List.map nodes ~f:(fun (n : Node.t) ->
    sprintf "⎇ %s%s" (Node.label n) (if Node.agent n then " (subagent)" else ""))
  |> String.concat ~sep:" · "
;;
