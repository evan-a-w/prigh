open! Core
open! Import

type t = P.Agent_queue.t list [@@deriving sexp_of]

let messages t ~agent_id =
  let q = P.Agent_queue.find t agent_id in
  Queued_message.of_queues ~steer:q.steer ~follow_up:q.follow_up
;;

let update t ~agent_id (kind : Queued_message.Kind.t) ~f =
  let change (q : P.Agent_queue.t) =
    match kind with
    | Steer -> { q with steer = f q.steer }
    | Follow_up -> { q with follow_up = f q.follow_up }
  in
  let t =
    if
      List.exists t ~f:(fun (q : P.Agent_queue.t) ->
        String.equal q.agent_id agent_id)
    then t
    else t @ [ P.Agent_queue.find [] agent_id ]
  in
  List.filter_map t ~f:(fun (q : P.Agent_queue.t) ->
    let q = if String.equal q.agent_id agent_id then change q else q in
    Option.some_if (not (List.is_empty q.steer && List.is_empty q.follow_up)) q)
;;

let add t ~agent_id kind text =
  update t ~agent_id kind ~f:(fun texts -> texts @ [ text ])
;;

let remove t ~agent_id kind text =
  let rec drop = function
    | [] -> []
    | x :: rest when String.equal x text -> rest
    | x :: rest -> x :: drop rest
  in
  update t ~agent_id kind ~f:(fun texts -> List.rev (drop (List.rev texts)))
;;
