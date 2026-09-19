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
