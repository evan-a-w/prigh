open! Core

(** A throwaway certificate authority and a certificate it issued for
    [localhost] and [127.0.0.1], made afresh for each test run: what a test
    trusts in place of the system's CAs. *)

type t =
  { authenticator : X509.Authenticator.t (** trusts only this CA *)
  ; server : Tls.Config.server (** presents the issued certificate *)
  }

val create : unit -> t

(** Trusts another, unrelated CA: the issued certificate fails it. *)
val stranger : unit -> X509.Authenticator.t
