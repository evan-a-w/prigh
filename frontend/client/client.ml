open! Core
open! Async_kernel
open Prigh_protocol

module Incoming = struct
  type t =
    | Event of Event.t
    | Protocol_error of string
    | Stderr of string
    | Closed
  [@@deriving sexp_of]
end

type t =
  { connect : unit -> Transport.t Deferred.Or_error.t
  ; mutable transport : Transport.t option
  ; mutable connecting : unit Deferred.Or_error.t option
  ; mutable attempts : int (** so that an abandoned attempt can tell *)
  ; mutable detached : unit Ivar.t (** filled once [transport] is gone *)
  ; pending : Json.t Or_error.t Ivar.t Int.Table.t
  ; mutable next_id : int
  ; mutable received : int
  ; incoming_w : Incoming.t Pipe.Writer.t
  ; incoming_r : Incoming.t Pipe.Reader.t
  }

let fail_pending t message =
  Hashtbl.iteri t.pending ~f:(fun ~key:_ ~data ->
    Ivar.fill_if_empty data (Or_error.error_string message));
  Hashtbl.clear t.pending
;;

let push t incoming = Pipe.write_without_pushback_if_open t.incoming_w incoming

let handle_line t line =
  t.received <- t.received + 1;
  if String.is_empty (String.strip line)
  then ()
  else (
    match Server_message.of_line line with
    | Error e -> push t (Protocol_error (Error.to_string_hum e))
    | Ok (Event event) -> push t (Event event)
    | Ok (Response { id = Some id; result }) ->
      (match Hashtbl.find_and_remove t.pending id with
       | Some ivar ->
         Ivar.fill_if_empty ivar (Result.map_error result ~f:Error.of_string)
       | None ->
         push
           t
           (Protocol_error (sprintf "response for unknown request id %d" id)))
    | Ok (Response { id = None; result }) ->
      let text =
        match result with
        | Ok _ -> "response without id"
        | Error e -> e
      in
      push t (Protocol_error text))
;;

let attach t (transport : Transport.t) =
  t.transport <- Some transport;
  let detached = Ivar.create () in
  t.detached <- detached;
  don't_wait_for
    (let%bind () =
       Pipe.iter_without_pushback transport.lines ~f:(fun line ->
         if Ivar.is_empty detached then handle_line t line)
     in
     (* Skip if [disconnect] or [close] already detached this transport, so a
        newer transport attached in the meantime is untouched. *)
     if Ivar.is_empty detached
     then (
       (match t.transport with
        | Some current when phys_equal current transport -> t.transport <- None
        | _ -> ());
       fail_pending t "backend closed";
       push t Closed;
       Ivar.fill_if_empty detached ());
     return ());
  don't_wait_for
    (Pipe.iter_without_pushback transport.stderr_lines ~f:(fun line ->
       if Ivar.is_empty detached then push t (Stderr line)))
;;

let connect t =
  match t.transport, t.connecting with
  | Some _, _ -> Deferred.Or_error.ok_unit
  | None, Some in_flight -> in_flight
  | None, None ->
    t.attempts <- t.attempts + 1;
    let attempt = t.attempts in
    let result =
      match%map t.connect () with
      | result when attempt <> t.attempts ->
        Result.iter result ~f:(fun (transport : Transport.t) ->
          transport.close ());
        Or_error.error_string "abandoned"
      | Error _ as e ->
        t.connecting <- None;
        e
      | Ok transport ->
        t.connecting <- None;
        attach t transport;
        Ok ()
    in
    if not (Deferred.is_determined result) then t.connecting <- Some result;
    result
;;

let create ~connect =
  let incoming_r, incoming_w = Pipe.create () in
  { connect
  ; transport = None
  ; connecting = None
  ; attempts = 0
  ; detached = Ivar.create_full ()
  ; pending = Int.Table.create ()
  ; next_id = 1
  ; received = 0
  ; incoming_w
  ; incoming_r
  }
;;

let call t method_ params =
  match t.transport with
  | None -> Deferred.Or_error.error_string "not connected"
  | Some transport ->
    let id = t.next_id in
    t.next_id <- id + 1;
    let ivar = Ivar.create () in
    Hashtbl.set t.pending ~key:id ~data:ivar;
    (match
       Or_error.try_with_join (fun () ->
         transport.send_line (Request.to_line { id; method_; params }))
     with
     | Ok () -> Ivar.read ivar
     | Error _ as err ->
       Hashtbl.remove t.pending id;
       Deferred.return err)
;;

let is_connected t = Option.is_some t.transport
let received t = t.received
let waiting t = Hashtbl.length t.pending

let abandon_connecting t =
  if Option.is_some t.connecting
  then (
    t.connecting <- None;
    t.attempts <- t.attempts + 1)
;;

let disconnect t =
  abandon_connecting t;
  match t.transport with
  | None -> ()
  | Some transport ->
    t.transport <- None;
    fail_pending t "backend closed";
    push t Closed;
    Ivar.fill_if_empty t.detached ();
    transport.close ()
;;

let disconnected t =
  match t.transport with
  | None -> return ()
  | Some _ -> Ivar.read t.detached
;;

let incoming t = t.incoming_r

let close t =
  abandon_connecting t;
  Pipe.close t.incoming_w;
  match t.transport with
  | None -> return ()
  | Some transport ->
    t.transport <- None;
    fail_pending t "backend closed";
    Ivar.fill_if_empty t.detached ();
    transport.close ();
    transport.closed
;;

let decode_with call ~f =
  match%map call with
  | Error _ as e -> e
  | Ok json -> f json
;;

let decode_list json ~f =
  match json with
  | `Array items -> Or_error.all (List.map items ~f)
  | other -> Or_error.errorf "expected array, got %s" (Json.to_string other)
;;

let list_models t =
  decode_with (call t "list_models" []) ~f:(decode_list ~f:Model.of_json)
;;

let list_sessions t =
  decode_with
    (call t "list_sessions" [])
    ~f:(decode_list ~f:Session_summary.of_json)
;;
