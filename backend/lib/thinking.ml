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
