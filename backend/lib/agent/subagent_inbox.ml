open! Core
open! Import

module From = struct
  type t =
    | User
    | Caller
  [@@deriving sexp_of, equal]
end

module Listing = struct
  type t =
    { agent_id : string
    ; steer : string list
    ; follow_up : string list
    }
  [@@deriving sexp_of]
end

module Item = struct
  type t =
    { text : string
    ; from : From.t
    ; kind : Steer_queue.Kind.t
    }

  let message t =
    let note =
      Injected.Note.steer
        ~from_user:(From.equal t.from User)
        ~follow_up:(Steer_queue.Kind.equal t.kind Follow_up)
    in
    Message.user
      ~origin:{ Injected.empty with notes = [ note ] }
      (Injected.prefix_notes [ note ] t.text)
  ;;
end

type t =
  { queue : Item.t Steer_queue.t
  ; changed : Eio.Condition.t
  ; mutable closed : bool
  ; mutable dropped : string list
  ; mutable on_change : unit -> unit
  }

let create () =
  { queue = Steer_queue.create ()
  ; changed = Eio.Condition.create ()
  ; closed = false
  ; dropped = []
  ; on_change = ignore
  }
;;

let set_on_change t f = t.on_change <- f
let is_open t = not t.closed

let changed t =
  Eio.Condition.broadcast t.changed;
  t.on_change ()
;;

let push t kind ~from text =
  if t.closed
  then Or_error.error_string "the subagent has stopped"
  else (
    Steer_queue.push t.queue kind { Item.text; from; kind };
    changed t;
    Ok ())
;;

let texts t =
  let texts kind =
    List.map (Steer_queue.items t.queue kind) ~f:(fun (i : Item.t) -> i.text)
  in
  texts Steer, texts Follow_up
;;

let pop_last t =
  Option.map (Steer_queue.pop_last t.queue) ~f:(fun (item : Item.t) ->
    changed t;
    item.text)
;;

let remove t kind ~index ~text =
  Steer_queue.remove t.queue kind ~index ~matches:(fun (item : Item.t) ->
    Option.for_all text ~f:(String.equal item.text))
  |> Or_error.map ~f:(fun (item : Item.t) ->
    changed t;
    item.text)
;;

let messages t items =
  if not (List.is_empty items) then changed t;
  List.map items ~f:Item.message
;;

let take_steers t = messages t (Steer_queue.take_steers t.queue)
let take_all t = messages t (Steer_queue.take_all t.queue)

let await t =
  while Steer_queue.is_empty t.queue do
    Eio.Condition.await_no_mutex t.changed
  done
;;

let close_if_empty t =
  if Steer_queue.is_empty t.queue then t.closed <- true;
  t.closed
;;

let close t =
  t.closed <- true;
  let left =
    List.map (Steer_queue.take_all t.queue) ~f:(fun (i : Item.t) -> i.text)
  in
  if not (List.is_empty left)
  then (
    t.dropped <- t.dropped @ left;
    changed t);
  left
;;

let take_dropped t =
  let dropped = t.dropped in
  t.dropped <- [];
  dropped
;;
