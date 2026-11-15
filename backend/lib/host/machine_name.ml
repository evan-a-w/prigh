open! Core

let get () =
  match Sys.getenv "PRIGH_HOST_NAME" with
  | Some name when not (String.is_empty (String.strip name)) -> name
  | Some _ | None -> Core_unix.gethostname ()
;;
