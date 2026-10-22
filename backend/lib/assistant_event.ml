open! Core
open! Import

type t =
  | Text_delta of string
  | Thinking_delta of string
  (** Extends the current thinking block, or starts one (also when the
      current one is signed, so complete); an empty one starts a block whose
      text may never come. *)
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
  | Server_tool_use of
      { id : string
      ; name : string
      ; input : string
      }
  (** A provider-side tool (web search) started, or its input is complete:
      adds the [Server_tool] block [id] or updates its input. *)
  | Server_tool_result of
      { id : string
      ; output : string
      ; is_error : bool
      ; raw : string option [@sexp.option]
      }
  (** The result of the server tool [id]; [raw] is what to echo back to the
      provider. *)
[@@deriving sexp_of]
