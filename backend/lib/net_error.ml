open! Core
open! Import

let of_backend = function
  | Eio_unix.Unix_error (error, _, _) -> Some error
  | _ -> None
;;

let unix_error = function
  | Eio.Io (Eio.Net.E (Connection_failure (Refused backend)), _)
  | Eio.Io (Eio.Net.E (Connection_reset backend), _)
  | Eio.Io (Eio.Fs.E (Permission_denied backend), _)
  | Eio.Io (Eio.Fs.E (Not_found backend), _)
  | Eio.Io (Eio.Fs.E (Already_exists backend), _)
  | Eio.Io (Eio.Exn.X backend, _) -> of_backend backend
  | Core_unix.Unix_error (error, _, _) -> Some error
  | _ -> None
;;

let to_string_hum exn =
  match unix_error exn with
  | Some error -> String.lowercase (Core_unix.Error.message error)
  | None ->
    (match exn with
     | Eio.Io (Eio.Net.E (Connection_failure (Refused _)), _) ->
       "connection refused"
     | Eio.Io (Eio.Net.E (Connection_failure Timeout), _) ->
       "connection timed out"
     | Eio.Io (Eio.Net.E (Connection_reset _), _) -> "connection reset"
     | Eio.Io (Eio.Net.E (Address_lookup_failed e), _) ->
       String.lowercase (Eio.Net.Getaddrinfo_error.to_message e)
     | Eio.Io (Eio.Fs.E (Permission_denied _), _) -> "permission denied"
     | exn -> Exn.to_string exn)
;;
