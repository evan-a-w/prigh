open! Core
open! Import
module Project_tree = Prigh_ui.Project_tree

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
       @
       match s.repo with
       | None -> []
       | Some r -> Filename.basename r.root :: Option.to_list r.branch))
;;

let dom_id id = "session-" ^ id

let sections sessions ~query =
  List.partition_tf (filter sessions ~query) ~f:(fun (s : Session_summary.t) ->
    not s.archived)
;;

let searching ~query = not (String.is_empty (String.strip query))
let archived_shown ~query ~archived_open = archived_open || searching ~query

let tree sessions ~query ~listing =
  let active, archived = sections sessions ~query in
  if searching ~query
  then Project_tree.build active
  else Project_tree.build ?listing ~owners:archived active
;;

let shown sessions ~query ~archived_open ~folded ~listing =
  let _, archived = sections sessions ~query in
  let folded = if searching ~query then String.Set.empty else folded in
  Project_tree.sessions ~collapsed:folded (tree sessions ~query ~listing)
  @ if archived_shown ~query ~archived_open then archived else []
;;
