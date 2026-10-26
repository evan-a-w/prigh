open! Core
open! Async

type t =
  { reader : Reader.t
  ; write : string -> unit
  ; close : unit -> unit
  }

let of_reader_writer reader writer =
  { reader
  ; write =
      (fun data -> if not (Writer.is_closed writer) then Writer.write writer data)
  ; close =
      (fun () ->
        don't_wait_for (Writer.close writer);
        don't_wait_for (Reader.close reader))
  }
;;

let tcp ~host ~port =
  Monitor.try_with_or_error (fun () ->
    let%map socket, reader, writer =
      Tcp.connect (Tcp.Where_to_connect.of_host_and_port { host; port })
    in
    Socket.setopt socket Socket.Opt.keepalive true;
    of_reader_writer reader writer)
;;
