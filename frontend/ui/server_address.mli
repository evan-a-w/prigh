open! Core

(** Where a remote backend is, as [-connect] and [/connect] name it (the
    backend's [Backend_address] parses the same):

    - [HOST:PORT]: [prigh serve -listen] (or [-web]); JSON lines over TCP.
    - a prigh-web URL, [ws://], [wss://], [http://] or [https://], optionally
      with the path the site is under ([wss://host/ws],
      [https://host/prigh/]): the same JSON lines as WebSocket text messages
      on its [/ws], over TLS for [wss] and [https] (e.g. through a reverse
      proxy). *)

type t =
  { tls : bool
  ; host : string
  ; port : int
  ; prefix : string
    (** the site's path without a trailing slash: [""] or e.g. ["/prigh"] *)
  ; lines : bool (** given as [HOST:PORT] *)
  }
[@@deriving sexp_of, equal]

(** The error says what is accepted. *)
val parse : string -> (t, string) Result.t

(** [HOST:PORT], or the WebSocket URL of the RPC ([wss://host/prigh/ws]). *)
val to_string : t -> string

(** How the status line shows it: [HOST:PORT], or [wss://host/prigh]. *)
val display : t -> string

(** The [Host] header for it. *)
val authority : t -> string

(** The path of the RPC's WebSocket ([/prigh/ws]). *)
val websocket_path : t -> string

(** A URL in the form {!to_string} gives (so that [https://host/] and
    [wss://host/ws] are saved and matched as one), anything else as given
    (stripped). *)
val normalize : string -> string

(** {!display} of an address string, or the string itself when it does not
    parse. *)
val display_string : string -> string
