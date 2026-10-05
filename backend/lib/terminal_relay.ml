open! Core
open! Import

module Outgoing = struct
  type t =
    | Frame of Terminal_channel.Frame.t
    | Closed
    | Failed of string
    | Overflow
end

module Relay = struct
  type t =
    { host : string
    ; outgoing : Outgoing.t Eio.Stream.t
    ; mutable overflowed : bool
    }
end

type t =
  { relays : Relay.t String.Table.t
  ; mutable seq : int
  }

let create () = { relays = String.Table.create (); seq = 0 }

(* A browser that cannot keep up is dropped; it reconnects and gets a fresh
   replay from the host. *)
let queue_capacity = 4096

let push (relay : Relay.t) (item : Outgoing.t) =
  if not relay.overflowed
  then
    if Eio.Stream.length relay.outgoing >= queue_capacity - 1
    then (
      relay.overflowed <- true;
      Eio.Stream.add relay.outgoing Overflow)
    else Eio.Stream.add relay.outgoing item
;;

let event name ~host ~term_id fields =
  `Object
    ([ "type", `String "event"
     ; "event", `String name
     ; "host", `String host
     ; "term_id", `String term_id
     ]
     @ fields)
;;

let number n = `Number (Int.to_string n)

let serve t ~host ~send_event ~key ~cwd ~cols ~rows channel =
  t.seq <- t.seq + 1;
  let term_id = sprintf "term-%d" t.seq in
  let relay =
    { Relay.host
    ; outgoing = Eio.Stream.create Int.max_value
    ; overflowed = false
    }
  in
  Hashtbl.set t.relays ~key:term_id ~data:relay;
  send_event
    (event
       "terminal_open"
       ~host
       ~term_id
       [ "key", `String key
       ; "cwd", `String cwd
       ; "cols", number cols
       ; "rows", number rows
       ]);
  let rec write () =
    match Eio.Stream.take relay.outgoing with
    | Frame frame ->
      Terminal_channel.send channel frame;
      if Terminal_channel.is_closed channel then `Browser_gone else write ()
    | Overflow -> `Browser_gone
    | Closed -> `Host_done
    | Failed message ->
      Terminal_channel.send_text
        channel
        (Json.to_string
           (`Object [ "type", `String "error"; "message", `String message ]));
      `Host_done
  in
  let rec read () =
    match Terminal_channel.read channel with
    | None -> `Browser_gone
    | Some frame ->
      send_event
        (event
           "terminal_frame"
           ~host
           ~term_id
           (Terminal_channel.Frame.to_fields frame));
      read ()
  in
  Exn.protect
    ~f:(fun () ->
      match Fiber.first write read with
      | `Host_done -> ()
      | `Browser_gone -> send_event (event "terminal_close" ~host ~term_id []))
    ~finally:(fun () -> Hashtbl.remove t.relays term_id)
;;

let find t ~client params =
  match Json.member "term_id" params with
  | Some (`String term_id) ->
    (match Hashtbl.find t.relays term_id with
     | Some relay when String.equal relay.host client -> Ok relay
     | Some _ | None -> Or_error.errorf "no terminal %S" term_id)
  | _ -> Or_error.error_string "missing string param \"term_id\""
;;

let frame t ~client params =
  Or_error.bind (find t ~client params) ~f:(fun relay ->
    Or_error.map (Terminal_channel.Frame.of_json params) ~f:(fun frame ->
      push relay (Frame frame)))
;;

let closed t ~client params =
  Or_error.map (find t ~client params) ~f:(fun relay -> push relay Closed)
;;

let host_gone t ~host =
  Hashtbl.iter t.relays ~f:(fun relay ->
    if String.equal relay.host host
    then push relay (Failed "the tool host disconnected"))
;;
