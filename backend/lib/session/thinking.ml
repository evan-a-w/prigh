open! Core
open! Import

module Level = struct
  type t =
    | Minimal
    | Low
    | Medium
    | High
    | Xhigh
    | Max
  [@@deriving sexp, jsonaf, equal, compare, enumerate]

  let to_string t = String.lowercase (Sexp.to_string [%sexp (t : t)])
  let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)
end

type t =
  | Off
  | On of Level.t option
[@@deriving sexp, jsonaf, equal]

let default = On None

let budget_tokens = function
  | Off -> None
  | On None | On (Some Medium) -> Some 8_192
  | On (Some Minimal) -> Some 1_024
  | On (Some Low) -> Some 2_048
  | On (Some High) -> Some 16_384
  | On (Some Xhigh) -> Some 32_000
  | On (Some Max) -> Some 64_000
;;

let to_string = function
  | Off -> "off"
  | On None -> "default"
  | On (Some level) -> Level.to_string level
;;

let label = function
  | On None -> "provider's default"
  | (Off | On (Some _)) as t -> to_string t
;;

let of_string s =
  match String.lowercase (String.strip s) with
  | "off" | "none" -> Ok Off
  | "default" | "on" -> Ok (On None)
  | s ->
    (match Level.of_string s with
     | Some level -> Ok (On (Some level))
     | None ->
       Or_error.error_string
         "thinking must be one of: off, default, minimal, low, medium, high, \
          xhigh, max")
;;

let rank level =
  fst (List.findi_exn Level.all ~f:(fun _ l -> Level.equal l level))
;;

(* The lower one on a tie: [compare] on [(distance, level)]. *)
let nearest level levels =
  List.min_elt levels ~compare:(fun a b ->
    Comparable.lift
      [%compare: int * Level.t]
      ~f:(fun l -> abs (rank l - rank level), l)
      a
      b)
;;

let clamp t ~levels ~can_disable =
  match levels, t with
  | [], _ -> Off
  | _, Off when can_disable -> Off
  | _, Off -> On None
  | _, On None -> On None
  | _, On (Some level) when List.mem levels level ~equal:Level.equal -> t
  | _, On (Some level) -> On (nearest level levels)
;;
