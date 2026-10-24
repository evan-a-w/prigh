open! Core
open! Import
module Message = Prigh_ui.Queued_message

type t =
  { items : Message.t list
  ; delivering : Message.t list
  }
[@@deriving sexp_of]

let empty = { items = []; delivering = [] }
let items t = t.items
let count t = List.length t.items

let texts items kind =
  List.filter_map items ~f:(fun (m : Message.t) ->
    Option.some_if (Message.Kind.equal m.kind kind) m.text)
;;

(* Each of [items] that [others] lacks, counting repeated texts. *)
let missing items ~from:others =
  let rest = ref others in
  List.filter items ~f:(fun (m : Message.t) ->
    match List.findi !rest ~f:(fun _ text -> String.equal text m.text) with
    | Some (i, _) ->
      rest := List.filteri !rest ~f:(fun j _ -> j <> i);
      false
    | None -> true)
;;

(* Some never show (an abort from another frontend): the newest only. *)
let max_delivering = 10

let update t ~steer ~follow_up =
  let gone = missing t.items ~from:(steer @ follow_up) in
  let delivering = t.delivering @ gone in
  { items = Message.of_queues ~steer ~follow_up
  ; delivering = List.drop delivering (List.length delivering - max_delivering)
  }
;;

let remove t (m : Message.t) =
  let items = List.filter t.items ~f:(Fn.non (Message.equal m)) in
  { t with
    items =
      Message.of_queues
        ~steer:(texts items Steer)
        ~follow_up:(texts items Follow_up)
  }
;;

let delivered t ~text =
  match
    List.findi t.delivering ~f:(fun _ (m : Message.t) ->
      String.equal m.text text)
  with
  | Some (i, m) ->
    ( { t with delivering = List.filteri t.delivering ~f:(fun j _ -> j <> i) }
    , Some m )
  | None -> t, None
;;

let forget_delivering t = { t with delivering = [] }
