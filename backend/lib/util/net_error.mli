open! Core
open! Import

(** A short human description of a failed network operation, e.g. "address
    already in use" or "connection refused", instead of Eio's nested
    exception dump; other exceptions are printed as usual. *)
val to_string_hum : exn -> string

(** The system error behind an Eio or Unix exception, if any. *)
val unix_error : exn -> Core_unix.Error.t option
