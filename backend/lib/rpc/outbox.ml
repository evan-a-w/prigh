open! Core
open! Import

module State = struct
  type t =
    | Open
    | Finishing
    | Failed
end

module Entry = struct
  type t =
    | Line of string
    | Rendered_late of (unit -> string)
end

type t =
  { queue : Entry.t Queue.t
  ; mutable bytes : int
  ; max_lines : int
  ; max_bytes : int
  ; mutable state : State.t
  ; ready : Eio.Condition.t
  ; failed : string Promise.t
  ; resolve_failed : string Promise.u
  }

let create ?(max_lines = 10_000) ?(max_bytes = 256 * 1024 * 1024) () =
  let failed, resolve_failed = Promise.create () in
  { queue = Queue.create ()
  ; bytes = 0
  ; max_lines
  ; max_bytes
  ; state = Open
  ; ready = Eio.Condition.create ()
  ; failed
  ; resolve_failed
  }
;;

let fail t reason =
  match t.state with
  | Failed -> ()
  | Open | Finishing ->
    t.state <- Failed;
    Queue.clear t.queue;
    t.bytes <- 0;
    Promise.resolve t.resolve_failed reason;
    Eio.Condition.broadcast t.ready
;;

let enqueue t (entry : Entry.t) =
  match t.state with
  | Failed | Finishing -> ()
  | Open ->
    Queue.enqueue t.queue entry;
    (match entry with
     | Line line -> t.bytes <- t.bytes + String.length line
     | Rendered_late _ -> ());
    let lines = Queue.length t.queue in
    if lines > t.max_lines
    then
      fail
        t
        (sprintf
           "the peer is not reading what is sent to it (%d messages waiting)"
           lines)
    else if t.bytes > t.max_bytes && lines > 1
    then
      fail
        t
        (sprintf
           "the peer is not reading what is sent to it (%d bytes waiting)"
           t.bytes)
    else Eio.Condition.broadcast t.ready
;;

let send t line = enqueue t (Line line)
let send_late t render = enqueue t (Rendered_late render)

let finish t =
  match t.state with
  | Open ->
    t.state <- Finishing;
    Eio.Condition.broadcast t.ready
  | Finishing | Failed -> ()
;;

let rec run t ~write =
  match t.state, Queue.dequeue t.queue with
  | Failed, _ -> ()
  | (Open | Finishing), Some entry ->
    let line =
      match entry with
      | Line line ->
        t.bytes <- t.bytes - String.length line;
        line
      | Rendered_late render -> render ()
    in
    (match write line with
     | () -> run t ~write
     | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
     | exception exn -> fail t ("cannot write: " ^ Exn.to_string exn))
  | Finishing, None -> ()
  | Open, None ->
    Eio.Condition.await_no_mutex t.ready;
    run t ~write
;;

let failed t = t.failed
let waiting t = Queue.length t.queue
