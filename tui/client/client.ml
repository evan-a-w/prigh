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
  ; pending : Json.t Or_error.t Ivar.t Int.Table.t
  ; mutable next_id : int
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
  don't_wait_for
    (let%bind () =
       Pipe.iter_without_pushback transport.lines ~f:(handle_line t)
     in
     (* A newer transport may already be in place after a reconnect. *)
     (match t.transport with
      | Some current when phys_equal current transport -> t.transport <- None
      | _ -> ());
     fail_pending t "backend closed";
     push t Closed;
     return ());
  don't_wait_for
    (Pipe.iter_without_pushback transport.stderr_lines ~f:(fun line ->
       push t (Stderr line)))
;;

let connect t =
  match t.transport, t.connecting with
  | Some _, _ -> Deferred.Or_error.ok_unit
  | None, Some in_flight -> in_flight
  | None, None ->
    let result =
      match%map t.connect () with
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
  ; pending = Int.Table.create ()
  ; next_id = 1
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
    transport.send_line (Request.to_line { id; method_; params });
    Ivar.read ivar
;;

let is_connected t = Option.is_some t.transport
let incoming t = t.incoming_r

let close t =
  Pipe.close t.incoming_w;
  match t.transport with
  | None -> return ()
  | Some transport ->
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
