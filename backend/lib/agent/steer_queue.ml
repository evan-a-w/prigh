open! Core
open! Import

module Kind = struct
  type t =
    | Steer
    | Follow_up
  [@@deriving sexp_of, equal]

  let to_string = function
    | Steer -> "steer"
    | Follow_up -> "follow_up"
  ;;

  let of_string = function
    | "steer" -> Ok Steer
    | "follow_up" | "follow-up" -> Ok Follow_up
    | other -> Or_error.errorf "%S is not a queue: use \"steer\" or \"follow_up\"" other
  ;;
end

type 'a t =
  { steer : 'a Queue.t
  ; follow_up : 'a Queue.t
  }

let create () = { steer = Queue.create (); follow_up = Queue.create () }

let queue t : Kind.t -> _ = function
  | Steer -> t.steer
  | Follow_up -> t.follow_up
;;

let push t kind item = Queue.enqueue (queue t kind) item
let is_empty t = Queue.is_empty t.steer && Queue.is_empty t.follow_up
let items t kind = Queue.to_list (queue t kind)

let take_steers t =
  let items = Queue.to_list t.steer in
  Queue.clear t.steer;
  items
;;

let take_follow_up t = Queue.dequeue t.follow_up

let take_all t =
  let steers = take_steers t in
  let follow_ups = Queue.to_list t.follow_up in
  Queue.clear t.follow_up;
  steers @ follow_ups
;;

let steers_first t =
  if Queue.is_empty t.steer
  then false
  else (
    Queue.blit_transfer ~src:t.follow_up ~dst:t.steer ();
    Queue.blit_transfer ~src:t.steer ~dst:t.follow_up ();
    true)
;;

let replace q items =
  Queue.clear q;
  List.iter items ~f:(Queue.enqueue q)
;;

let pop_last t =
  let pop q =
    match List.rev (Queue.to_list q) with
    | [] -> None
    | last :: rest ->
      replace q (List.rev rest);
      Some last
  in
  match pop t.follow_up with
  | Some _ as item -> item
  | None -> pop t.steer
;;

let remove t kind ~index ~matches =
  let q = queue t kind in
  let items = Queue.to_list q in
  match List.nth items index with
  | Some item when matches item ->
    replace q (List.filteri items ~f:(fun i _ -> i <> index));
    Ok item
  | _ ->
    Or_error.error_string
      "that message is no longer queued: it was delivered or removed already"
;;
