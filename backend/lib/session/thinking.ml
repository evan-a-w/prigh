open! Core
open! Import

module Level = struct
  type t =
    | Low
    | High
    | Max
  [@@deriving sexp, jsonaf, equal, enumerate]

  let to_string t = String.lowercase (Sexp.to_string [%sexp (t : t)])
  let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)
end

type t =
  | Off
  | On of Level.t option
[@@deriving sexp, jsonaf, equal]

let default = On None

(* The token budget for budget-style thinking (Anthropic's older models),
   before it is capped to fit the request's [max_tokens]. *)
let budget_tokens = function
  | Off -> None
  | On None -> Some 8_192
  | On (Some Low) -> Some 2_048
  | On (Some High) -> Some 16_384
  | On (Some Max) -> Some 32_000
;;

let to_string = function
  | Off -> "off"
  | On None -> "on"
  | On (Some level) -> Level.to_string level
;;

let of_string s =
  match String.lowercase (String.strip s) with
  | "off" -> Ok Off
  | "on" -> Ok (On None)
  | s ->
    (match Level.of_string s with
     | Some level -> Ok (On (Some level))
     | None ->
       Or_error.error_string "thinking must be one of: off, on, low, high, max")
;;
