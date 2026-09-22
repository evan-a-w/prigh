open! Core
open! Async

let connect ~host ~port =
  match%map
    Monitor.try_with_or_error (fun () ->
      Tcp.connect (Tcp.Where_to_connect.of_host_and_port { host; port }))
  with
  | Error _ as e -> e
  | Ok (_socket, reader, writer) ->
    let _stderr_r, _stderr_w = Pipe.create () in
    Ok
      { Transport.send_line =
          (fun line ->
            if not (Writer.is_closed writer) then Writer.write_line writer line)
      ; lines = Reader.lines reader
      ; stderr_lines = _stderr_r
      ; close = (fun () -> don't_wait_for (Writer.close writer))
      ; closed = Reader.close_finished reader
      }
;;
