open! Core
open! Async
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
  { transport : Transport.t
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

let handle_line t line =
  if String.is_empty (String.strip line)
  then ()
  else (
    match Server_message.of_line line with
    | Error e ->
      Pipe.write_without_pushback_if_open
        t.incoming_w
        (Protocol_error (Error.to_string_hum e))
    | Ok (Event event) ->
      Pipe.write_without_pushback_if_open t.incoming_w (Event event)
    | Ok (Response { id = Some id; result }) ->
      (match Hashtbl.find_and_remove t.pending id with
       | Some ivar ->
         Ivar.fill_if_empty ivar (Result.map_error result ~f:Error.of_string)
       | None ->
         Pipe.write_without_pushback_if_open
           t.incoming_w
           (Protocol_error (sprintf "response for unknown request id %d" id)))
    | Ok (Response { id = None; result }) ->
      let text =
        match result with
        | Ok _ -> "response without id"
        | Error e -> e
      in
      Pipe.write_without_pushback_if_open t.incoming_w (Protocol_error text))
;;

let create transport =
  let incoming_r, incoming_w = Pipe.create () in
  let t =
    { transport
    ; pending = Int.Table.create ()
    ; next_id = 1
    ; incoming_w
    ; incoming_r
    }
  in
  don't_wait_for
    (let%bind () =
       Pipe.iter_without_pushback transport.lines ~f:(handle_line t)
     in
     fail_pending t "backend closed";
     Pipe.write_without_pushback_if_open incoming_w Closed;
     Pipe.close incoming_w;
     return ());
  don't_wait_for
    (Pipe.iter_without_pushback transport.stderr_lines ~f:(fun line ->
       Pipe.write_without_pushback_if_open incoming_w (Stderr line)));
  t
;;

let call t method_ params =
  let id = t.next_id in
  t.next_id <- id + 1;
  let ivar = Ivar.create () in
  Hashtbl.set t.pending ~key:id ~data:ivar;
  t.transport.send_line (Request.to_line { id; method_; params });
  Ivar.read ivar
;;

let incoming t = t.incoming_r
let close t = t.transport.close ()
let closed t = t.transport.closed

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

let get_state t = decode_with (call t "get_state" []) ~f:State.of_json

let get_messages t =
  decode_with (call t "get_messages" []) ~f:(decode_list ~f:Message.of_json)
;;

let list_models t =
  decode_with (call t "list_models" []) ~f:(decode_list ~f:Model.of_json)
;;

let list_sessions t =
  decode_with
    (call t "list_sessions" [])
    ~f:(decode_list ~f:Session_summary.of_json)
;;

let auth_status t =
  decode_with (call t "auth_status" []) ~f:(decode_list ~f:Auth_status.of_json)
;;

let prompt t text =
  decode_with (call t "prompt" [ "text", Json.str text ]) ~f:(fun _ -> Ok ())
;;
