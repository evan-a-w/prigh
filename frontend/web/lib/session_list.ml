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
      [ title s; Option.value s.first_prompt ~default:""; s.cwd ])
;;

let dom_id id = "session-" ^ id

let sections sessions ~query =
  List.partition_tf (filter sessions ~query) ~f:(fun (s : Session_summary.t) ->
    not s.archived)
;;

let archived_shown ~query ~archived_open =
  archived_open || not (String.is_empty (String.strip query))
;;

let shown sessions ~query ~archived_open =
  let active, archived = sections sessions ~query in
  if archived_shown ~query ~archived_open then active @ archived else active
;;
