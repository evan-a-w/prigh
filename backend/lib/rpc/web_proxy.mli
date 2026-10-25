open! Core
open! Import

(** prigh-web for a backend elsewhere ([prigh web -connect]): the page's
    files are served locally, what it asks of the backend goes to [remote],
    so the page talks only to its own origin (no mixed content or
    cross-origin requests, also for a [wss://] backend):

    - the RPC WebSocket ([/ws]) is relayed line by line over a
      {!Backend_socket} (so a [HOST:PORT] may be either of the backend's
      ports);
    - the terminal WebSocket ([/terminal]) message by message to the
      remote's;
    - file transfers ({!File_http}'s paths) are forwarded as they are,
      under the remote's prefix and with its [Host].

    The page's credentials pass through: the remote checks them. [log]
    reports a remote that cannot be reached. *)

val websockets
  :  env:Env.t
  -> log:(string -> unit)
  -> Backend_address.t
  -> (string * Web_server.on_websocket) list

val routes
  :  env:Env.t
  -> log:(string -> unit)
  -> Backend_address.t
  -> Web_server.Request.t
  -> Web_server.on_http option
