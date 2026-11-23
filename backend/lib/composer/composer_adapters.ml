open! Core
open! Import

let find engines : Adapter_id.t -> Adapter.t Or_error.t = function
  | Codex -> Ok (module Codex_adapter)
  | Claude -> Ok (module Claude_adapter)
  | Acp name ->
    (match Engines.acp_agent engines name with
     | Some agent -> Ok (Acp_adapter.create agent)
     | None -> Or_error.error_string (Engines.unknown_acp name))
  | Prigh ->
    Or_error.error_string
      "prigh's own harness runs in the backend, not as an external process"
;;
