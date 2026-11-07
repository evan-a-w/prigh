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

module Sections = struct
  type t =
    { pinned : Session_search.Result.t list
    ; active : Session_search.Result.t list
    ; archived : Session_search.Result.t list
    }
end

let is_pinned (s : Session_summary.t) =
  (not s.archived) && Option.value s.pinned ~default:false
;;

let sections ?content sessions ~query =
  let pinned, rest =
    List.partition_tf
      (results ?content sessions ~query)
      ~f:(fun (r : Session_search.Result.t) -> is_pinned r.session)
  in
  let active, archived =
    List.partition_tf rest ~f:(fun (r : Session_search.Result.t) ->
      not r.session.archived)
  in
  { Sections.pinned; active; archived }
;;

let archived_shown ~query ~archived_open =
  archived_open || not (String.is_empty (String.strip query))
;;

let shown ?content sessions ~query ~archived_open =
  let { Sections.pinned; active; archived } = sections ?content sessions ~query in
  List.map
    (pinned @ active @ if archived_shown ~query ~archived_open then archived else [])
    ~f:(fun (r : Session_search.Result.t) -> r.session)
;;
