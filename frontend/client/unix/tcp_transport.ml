open! Core
open! Async
open Prigh_client

let connect ~host ~port =
  let%map.Deferred.Or_error (channel : Channel.t) = Channel.tcp ~host ~port in
  let stderr_r, stderr_w = Pipe.create () in
  Pipe.close stderr_w;
  { Transport.send_line = (fun line -> channel.write (line ^ "\n"))
  ; lines = Reader.lines channel.reader
  ; stderr_lines = stderr_r
  ; close = channel.close
  ; closed = Reader.close_finished channel.reader
  }
;;
