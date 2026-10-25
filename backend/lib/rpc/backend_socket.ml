open! Core
open! Import

type t =
  | Lines of
      { reader : Eio.Buf_read.t
      ; flow : Eio.Flow.two_way_ty Eio.Resource.t
      }
  | Websocket of Websocket.t

let connect ~env ~sw (address : Backend_address.t) =
  if address.lines
  then (
    let flow = Backend_address.connect ~env ~sw address in
    Lines
      { reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024); flow })
  else
    Websocket (Backend_address.websocket ~env ~sw address ~path:"/ws" ~query:[])
;;

let read_line = function
  | Lines { reader; _ } ->
    (match Eio.Buf_read.line reader with
     | exception (End_of_file | Eio.Io _ | Eio.Buf_read.Buffer_limit_exceeded)
       -> None
     | line -> Some line)
  | Websocket ws -> Websocket.read_text ws
;;

let write_line t line =
  match t with
  | Lines { flow; _ } -> Eio.Flow.copy_string (line ^ "\n") flow
  | Websocket ws ->
    Websocket.send_text ws line;
    if Websocket.is_closed ws then failwith "the WebSocket is closed"
;;

let close = function
  | Lines { flow; _ } ->
    (try Eio.Flow.shutdown flow `All with
     | _ -> ())
  | Websocket ws -> Websocket.close ws
;;
