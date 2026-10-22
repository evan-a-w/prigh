open! Core
open! Import

(** Fibers on a long-lived switch that cannot fail it. An exception escaping
    a fiber fails its whole switch: on the server's, that ends [prigh serve]
    (every connection drops, every session's background subagents and jobs
    are lost) for what is one session's or one connection's bug. *)

(** [Fiber.fork ~sw f], except that an exception from [f] (other than a
    cancellation) goes to [on_error], by default a line on stderr naming
    [what]. *)
val fork
  :  sw:Switch.t
  -> what:string
  -> ?on_error:(exn -> unit)
  -> (unit -> unit)
  -> unit

(** Calls [f], reporting an exception from it (other than a cancellation)
    as {!fork} does instead of raising it. *)
val run : what:string -> ?on_error:(exn -> unit) -> (unit -> unit) -> unit

(** Accepts connections on [socket] and handles each in its own fiber until
    [sw] ends, like a loop of [Eio.Net.accept_fork], except that a failed
    [accept] (e.g. too many open files) is reported and retried after
    [retry_after] seconds instead of ending the loop. [on_error] reports
    both a failed handler and a failed [accept]. *)
val accept_loop
  :  sw:Switch.t
  -> clock:_ Eio.Time.clock
  -> ?retry_after:float
  -> on_error:(exn -> unit)
  -> [> 'a Eio.Net.listening_socket_ty ] Eio.Resource.t
  -> 'a Eio.Net.stream_socket_ty Eio.Net.connection_handler
  -> 'b
