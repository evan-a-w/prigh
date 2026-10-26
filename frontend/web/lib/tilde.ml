open! Core
open! Import

let of_path ?host_home cwd =
  match host_home with
  | Some home when String.equal cwd home -> "~"
  | Some home ->
    (match String.chop_prefix cwd ~prefix:(home ^ "/") with
     | Some rest when not (String.equal home "/") -> "~/" ^ rest
     | Some _ | None -> cwd)
  | None ->
    (match String.chop_prefix cwd ~prefix:"/home/" with
     | Some rest ->
       (match String.lsplit2 rest ~on:'/' with
        | Some (_, path) -> "~/" ^ path
        | None -> "~")
     | None -> cwd)
;;

let host_home (state : State.t) ~host =
  List.find_map state.hosts ~f:(fun (h : Host.t) ->
    if String.equal h.id host then h.home else None)
;;

let active_host_home (state : State.t) = host_home state ~host:state.active_host

let session_host_home state (s : Session_summary.t) =
  Option.bind state ~f:(fun (state : State.t) ->
    if String.equal s.id state.session_id
    then active_host_home state
    else Option.bind s.host ~f:(fun host -> host_home state ~host))
;;
