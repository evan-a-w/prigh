open! Core
open! Import
module Session_search = Prigh_ui.Session_search

let title = Session_search.title
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

let sections sessions =
  let pinned, rest =
    List.partition_tf
      (Session_search.sessions ~query:"" sessions)
      ~f:(fun (r : Session_search.Result.t) -> is_pinned r.session)
  in
  let active, archived =
    List.partition_tf rest ~f:(fun (r : Session_search.Result.t) ->
      not r.session.archived)
  in
  { Sections.pinned; active; archived }
;;

let shown sessions ~archived_open =
  let { Sections.pinned; active; archived } = sections sessions in
  List.map
    (pinned @ active @ if archived_open then archived else [])
    ~f:(fun (r : Session_search.Result.t) -> r.session)
;;
