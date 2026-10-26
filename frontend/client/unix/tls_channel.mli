open! Core
open! Async

(** TLS (the pure [tls] engine) over a {!Channel.t}. *)

(** The system's CAs ([$SSL_CERT_FILE] or [$NIX_SSL_CERT_FILE] if set, else
    the usual locations). *)
val system_authenticator : unit -> X509.Authenticator.t Or_error.t

(** The client's handshake with [host] (its name is checked against the
    certificate; also sent as SNI), validating the certificate with
    [authenticator] (default: {!system_authenticator}). The error says why
    the certificate was refused. Takes over [raw]. *)
val client
  :  ?authenticator:X509.Authenticator.t
  -> host:string
  -> Channel.t
  -> Channel.t Deferred.Or_error.t

(** The server's side, for tests. *)
val server : Tls.Config.server -> Channel.t -> Channel.t Deferred.Or_error.t
