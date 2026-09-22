open! Core
open! Import

type t =
  { name : string
  ; description : string
  ; parameters : Json.t
  ; parallel_safe : bool
  ; destructive : bool
  ; on_host : bool
    (** Runs on the session's active tool host (filesystem and process
          tools) rather than always in the backend (e.g. subagent). *)
  }
[@@deriving sexp_of]
