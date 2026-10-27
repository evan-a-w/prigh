open! Core
open! Import
module Filter = Transcript_filter

module Row = struct
  type t =
    | Kind of Filter.Kind.t
    | Show_all
  [@@deriving sexp_of, equal]

  let all = List.map Filter.Kind.all ~f:(fun k -> Kind k) @ [ Show_all ]
end

type t =
  { original : Filter.t
  ; selected : int
  }
[@@deriving sexp_of, equal]

let rows = List.length Row.all
let create original = { original; selected = 0 }

let move t delta =
  { t with
    selected = Int.clamp_exn (t.selected + delta) ~min:0 ~max:(rows - 1)
  }
;;

let selected t = List.nth_exn Row.all t.selected

let select t row =
  match List.findi Row.all ~f:(fun _ r -> Row.equal r row) with
  | Some (i, _) -> { t with selected = i }
  | None -> t
;;

let press (row : Row.t) filter =
  match row with
  | Kind kind -> Filter.toggle filter kind
  | Show_all -> Filter.all
;;

let anchor = "filter-toggle"
