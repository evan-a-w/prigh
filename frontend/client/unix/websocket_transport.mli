open! Core
open! Async
open Prigh_client

(** The JSON-lines RPC as WebSocket text messages, as prigh-web speaks it:
    a client of the [/ws] of a backend's [-web] port, directly ([ws://]) or
    through a TLS-terminating reverse proxy ([wss://]). *)

(** What a server answered instead of opening the WebSocket. *)
module Refusal : sig
  type t =
    { status : string (** the status line after the HTTP version *)
    ; headers : (string * string) list
    }
  [@@deriving sexp_of]

  (** Why, and what to do: a redirect, 401 or HTML page is a proxy's
      sign-in (exempt [/ws] and [/terminal] from it, or tunnel over SSH), a
      redirect to [https] wants [wss://], and so on. [url] is the address as
      the user gave it, [host] its host name. *)
  val explain : url:string -> host:string -> tls:bool -> t -> string
end

(** Parses [Name: value] (for [-header] and [$PRIGH_CONNECT_HEADERS]). *)
val parse_header : string -> (string * string, string) Result.t

(** Connects to [host]:[port] (over TLS when [tls], validated with
    [authenticator], default the system's CAs) and opens the WebSocket at
    [path], sending [headers] with the upgrade (e.g. a proxy's credentials).
    The transport answers the server's pings, pings it when nothing came for
    [ping_after] (default 20s) and closes when nothing came for [timeout]
    (default 60s), so that the client reconnects. The error explains a
    refused upgrade ({!Refusal.explain}) or certificate. *)
val connect
  :  ?authenticator:X509.Authenticator.t
  -> ?headers:(string * string) list
  -> ?ping_after:Time_float.Span.t
  -> ?timeout:Time_float.Span.t
  -> url:string
  -> tls:bool
  -> host:string
  -> port:int
  -> authority:string (** the [Host] header *)
  -> path:string
  -> unit
  -> Transport.t Deferred.Or_error.t
