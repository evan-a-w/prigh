open! Core
open! Import

let find : Adapter_id.t -> Adapter.t Or_error.t = function
  | Codex -> Ok (module Codex_adapter)
  | Claude -> Ok (module Claude_adapter)
  | (Acp _ | Prigh) as id ->
    Or_error.errorf
      "the composer can't drive %s in this build of prigh yet: start a Codex \
       or Claude Code session instead"
      (Adapter_id.label id)
;;
