open! Core
open! Import

let adapters = [ "codex"; "claude"; "acp" ]

let name ~adapter model =
  sprintf "%s/%s" adapter (Option.value model ~default:"default")
;;

let is_external name =
  match String.lsplit2 name ~on:'/' with
  | Some (adapter, _) -> List.mem adapters adapter ~equal:String.equal
  | None -> false
;;
