open! Core
open! Import

type t =
  | Text_delta of string
  | Thinking_delta of string
  | Thinking_signature of string
  (** Attaches to the current thinking block (creating an empty one if
      needed); providers emit it once the block is complete. *)
  | Tool_call_start of
      { index : int
      ; id : string
      ; name : string
      }
  | Tool_call_delta of
      { index : int
      ; arguments : string
      }
[@@deriving sexp_of]
