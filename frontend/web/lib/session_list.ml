open! Core
open! Import

let title (s : Session_summary.t) =
  match s.name with
  | Some name when not (String.is_empty (String.strip name)) -> name
  | _ -> Session_summary.blurb s
;;

let filter sessions ~query =
  Prigh_ui.Fuzzy.rank ~query sessions ~key:(fun (s : Session_summary.t) ->
    String.concat
      ~sep:" "
      ([ title s; Option.value s.first_prompt ~default:""; s.cwd ]
       @ Option.to_list (Prigh_ui.Session_worktrees.place s)
       @ List.map s.worktrees ~f:Session_worktree.label))
;;

let dom_id id = "session-" ^ id

module Sections = struct
  type t =
    { pinned : Session_summary.t list
    ; active : Session_summary.t list
    ; archived : Session_summary.t list
    }
end

let is_pinned (s : Session_summary.t) =
  (not s.archived) && Option.value s.pinned ~default:false
;;

let sections sessions ~query =
  let pinned, rest = List.partition_tf (filter sessions ~query) ~f:is_pinned in
  let active, archived =
    List.partition_tf rest ~f:(fun (s : Session_summary.t) -> not s.archived)
  in
  { Sections.pinned; active; archived }
;;

let archived_shown ~query ~archived_open =
  archived_open || not (String.is_empty (String.strip query))
;;

let shown sessions ~query ~archived_open =
  let { Sections.pinned; active; archived } = sections sessions ~query in
  pinned
  @ active
  @ if archived_shown ~query ~archived_open then archived else []
;;
