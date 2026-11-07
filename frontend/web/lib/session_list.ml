open! Core
open! Import
module Session_search = Prigh_ui.Session_search

let title = Session_search.title

let results ?content sessions ~query =
  Session_search.sessions ?content ~query sessions
;;

let filter ?content sessions ~query =
  List.map (results ?content sessions ~query) ~f:(fun r -> r.session)
;;

let dom_id id = "session-" ^ id

let sections ?content sessions ~query =
  List.partition_tf
    (results ?content sessions ~query)
    ~f:(fun (r : Session_search.Result.t) -> not r.session.archived)
;;

let archived_shown ~query ~archived_open =
  archived_open || not (String.is_empty (String.strip query))
;;

let shown ?content sessions ~query ~archived_open =
  let active, archived = sections ?content sessions ~query in
  List.map
    (if archived_shown ~query ~archived_open then active @ archived else active)
    ~f:(fun r -> r.session)
;;
