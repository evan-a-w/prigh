open! Core
open! Import

let find ?autonomous_quiet engines : Adapter_id.t -> Adapter.t Or_error.t
  = function
  | Codex -> Ok (module Codex_adapter)
  | Claude -> Ok (module Claude_adapter)
  | Acp name ->
    (match Engines.acp_agent engines name with
     | Some agent -> Ok (Acp_adapter.create ?autonomous_quiet agent)
     | None -> Or_error.error_string (Engines.unknown_acp name))
  | Backend (Acp "antigravity") ->
    (match Engines.acp_agent engines "antigravity" with
     | Some agent ->
       Ok (Acp_adapter.create ~backend_hosted:true ?autonomous_quiet agent)
     | None -> Or_error.error_string (Engines.unknown_acp "antigravity"))
  | Backend inner ->
    Or_error.errorf
      "the composer can't run %s on the backend yet (only \
       backend:acp:antigravity is supported)"
      (Adapter_id.label inner)
  | Prigh ->
    Or_error.error_string
      "prigh's own harness runs in the backend, not as an external process"
;;
