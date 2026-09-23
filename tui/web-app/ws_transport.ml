open! Core
open! Async_kernel
open Js_of_ocaml

let connect ~url =
  let ws = new%js WebSockets.webSocket (Js.string url) in
  let opened = Ivar.create () in
  let lines_r, lines_w = Pipe.create () in
  let stderr_r, _stderr_w = Pipe.create () in
  let closed = Ivar.create () in
  let finish () =
    Ivar.fill_if_empty opened (Or_error.errorf "cannot connect to %s" url);
    Pipe.close lines_w;
    Ivar.fill_if_empty closed ()
  in
  ws##.onopen
  := Dom.handler (fun _ ->
       Ivar.fill_if_empty opened (Ok ());
       Js._true);
  ws##.onmessage
  := Dom.handler (fun ev ->
       Pipe.write_without_pushback_if_open lines_w (Js.to_string ev##.data);
       Js._true);
  ws##.onerror
  := Dom.handler (fun _ ->
       finish ();
       Js._true);
  ws##.onclose
  := Dom.handler (fun _ ->
       finish ();
       Js._true);
  match%map Ivar.read opened with
  | Error _ as e -> e
  | Ok () ->
    Ok
      { Prigh_client.Transport.send_line =
          (fun line ->
            match ws##.readyState with
            | OPEN -> ws##send (Js.string line)
            | CONNECTING | CLOSING | CLOSED -> ())
      ; lines = lines_r
      ; stderr_lines = stderr_r
      ; close = (fun () -> ws##close)
      ; closed = Ivar.read closed
      }
;;
