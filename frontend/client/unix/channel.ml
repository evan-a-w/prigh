open! Core
open! Async

type t =
  { reader : Reader.t
  ; write : string -> unit
  ; close : unit -> unit
  }

let of_reader_writer reader writer =
  (* A peer gone shows as the reader's end, not as a writer's exception. *)
  Writer.set_raise_when_consumer_leaves writer false;
  Monitor.detach_and_iter_errors (Writer.monitor writer) ~f:ignore;
  { reader
  ; write =
      (fun data ->
        if not (Writer.is_closed writer) then Writer.write writer data)
  ; close =
      (fun () ->
        (* The reader shares the descriptor: closing it first would lose
           what the writer has not flushed yet. *)
        don't_wait_for
          (let%bind () = Writer.close writer in
           Reader.close reader))
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
