open! Core
open! Import

module Target = struct
  type t =
    | Pending of int
    | New
  [@@deriving sexp_of, equal]

  let to_string = function
    | Pending i -> sprintf "pending:%d" i
    | New -> "new"
  ;;

  let of_string s =
    match String.chop_prefix s ~prefix:"pending:" with
    | Some n -> Option.map (Int.of_string_opt n) ~f:(fun i -> Pending i)
    | None -> if String.equal s "new" then Some New else None
  ;;
end

type t =
  { image : Image.t
  ; target : Target.t
  ; color : string
  ; size : int
  ; strokes : int
  }
[@@deriving sexp_of, equal]

let colors =
  [ "Red", "#ff453a"
  ; "Yellow", "#ffd60a"
  ; "Green", "#32d74b"
  ; "Blue", "#0a84ff"
  ; "White", "#ffffff"
  ; "Black", "#111111"
  ]
;;

let sizes = [ "Fine", 3; "Medium", 6; "Bold", 12 ]

let create image target =
  { image; target; color = snd (List.hd_exn colors); size = 6; strokes = 0 }
;;
