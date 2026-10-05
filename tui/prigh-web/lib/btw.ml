open! Core

module Status = struct
  type t =
    | Streaming
    | Done
    | Failed of string
  [@@deriving sexp_of, equal]
end

type t =
  { id : string
  ; question : string
  ; answer : string
  ; status : Status.t
  }
[@@deriving sexp_of, equal]

let create ~id ~question = { id; question; answer = ""; status = Streaming }
let append t delta = { t with answer = t.answer ^ delta }

let finish t ~answer =
  { t with
    answer = (if String.is_empty answer then t.answer else answer)
  ; status = Done
  }
;;

let fail t error = { t with status = Failed error }
