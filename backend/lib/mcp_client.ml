open! Core
open! Import

module Tool = struct
  type t =
    { name : string
    ; description : string
    ; input_schema : Json.t
    ; read_only : bool
    }
  [@@deriving sexp_of]
end

type t = unit

let connect ~env:_ ~sw:_ ?timeout:_ _ = Or_error.error_string "stub"
let tools () = Ok []
let call () ~cancel:_ ~tool:_ ~arguments:_ = Tool_result.error "stub"
let failure () = None
let close () = ()
