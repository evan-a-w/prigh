open! Core
open! Import

type t =
  | Connected of P.Host.t
  | Disconnected of
      { id : string
      ; name : string
      }
  | Unset
[@@deriving sexp_of, equal]

let of_state (s : P.State.t) =
  match List.find s.hosts ~f:(fun h -> String.equal h.id s.active_host) with
  | Some h -> Connected h
  | None
    when String.is_empty s.active_host
         || String.equal s.active_host P.Host.backend_id -> Unset
  | None ->
    let name =
      match s.active_host_name with
      | Some name when not (String.is_empty name) -> name
      | Some _ | None -> s.active_host
    in
    Disconnected { id = s.active_host; name }
;;

let reconnect_hint ~name =
  sprintf
    "Tools wait for %s to reconnect: run prigh tool-host -connect (or a TUI) \
     there, or /host picks another"
    name
;;
