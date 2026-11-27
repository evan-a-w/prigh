open! Core
open! Async_kernel
open Js_of_ocaml

let connect ?timeout ~url () =
  let ws = new%js WebSockets.webSocket (Js.string url) in
  let opened = Ivar.create () in
  let lines_r, lines_w = Pipe.create () in
  let stderr_r, _stderr_w = Pipe.create () in
  let closed = Ivar.create () in
  let finish ?(error = sprintf "cannot connect to %s" url) () =
    Ivar.fill_if_empty opened (Or_error.error_string error);
    Pipe.close lines_w;
    Ivar.fill_if_empty closed ()
  in
  let close_ws ?error () =
    (try ws##close with
     | _ -> ());
    finish ?error ()
  in
  ws##.onopen
  := Dom.handler (fun _ ->
       if Ivar.is_empty opened
       then Ivar.fill_if_empty opened (Ok ())
       else (
         try ws##close with
         | _ -> ());
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
  Option.iter timeout ~f:(fun span ->
    don't_wait_for
      (let%map () = Clock_ns.after span in
       if Ivar.is_empty opened then close_ws ~error:"timed out" ()));
  match%map Ivar.read opened with
  | Error _ as e -> e
  | Ok () ->
    Ok
      { Prigh_client.Transport.send_line =
          (fun line ->
            match ws##.readyState with
            | OPEN ->
              (try
                 ws##send (Js.string line);
                 Ok ()
               with
               | exn ->
                 finish ();
                 Or_error.errorf "send failed: %s" (Exn.to_string exn))
            | CONNECTING | CLOSING | CLOSED ->
              finish ();
              Or_error.error_string "websocket not open")
      ; lines = lines_r
      ; stderr_lines = stderr_r
      ; close =
          (fun () ->
            (* A dead network may not finish the closing handshake for
               minutes: the transport is closed now. *)
            close_ws ())
      ; closed = Ivar.read closed
      }
;;
