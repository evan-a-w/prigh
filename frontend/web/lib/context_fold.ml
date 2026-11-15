open! Core
open! Import
module Row = Diff_view.Row
module Gap = Diff_view.Gap

module How = struct
  type t =
    | Down
    | Up
    | All
  [@@deriving sexp_of, equal]
end

module Shown = struct
  type t =
    { top : int
    ; bottom : int
    }
  [@@deriving sexp_of, equal]

  let none = { top = 0; bottom = 0 }
end

type t = Shown.t Int.Map.t [@@deriving sexp_of, equal]

let empty = Int.Map.empty
let context = 3
let step = 20
let min_gap = 3

let changed (row : Row.t) =
  match row with
  | Line { kind = `Added | `Removed; _ } -> true
  | Line { kind = `Same; _ } | Hunk _ | Gap _ -> false
;;

let new_line (row : Row.t) =
  match row with
  | Line { new_line; _ } -> new_line
  | Hunk _ | Gap _ -> None
;;

let fold t rows =
  let rows = Array.of_list rows in
  let n = Array.length rows in
  if not (Array.exists rows ~f:changed)
  then Array.to_list rows
  else (
    (* How far each row is from the nearest change, looking each way. *)
    let far = Array.create ~len:n Int.max_value in
    let last = ref None in
    for i = 0 to n - 1 do
      if changed rows.(i) then last := Some i;
      Option.iter !last ~f:(fun j -> far.(i) <- i - j)
    done;
    last := None;
    for i = n - 1 downto 0 do
      if changed rows.(i) then last := Some i;
      Option.iter !last ~f:(fun j -> far.(i) <- Int.min far.(i) (j - i))
    done;
    let hideable i =
      far.(i) > context
      &&
      match rows.(i) with
      | Line { kind = `Same; _ } -> true
      | Line _ | Hunk _ | Gap _ -> false
    in
    let out = ref [] in
    let emit i = out := rows.(i) :: !out in
    let rec go i =
      if i < n
      then
        if not (hideable i)
        then (
          emit i;
          go (i + 1))
        else (
          let rec run_end j =
            if j < n && hideable j then run_end (j + 1) else j
          in
          let e = run_end i in
          let len = e - i in
          let id = Option.value (new_line rows.(i)) ~default:i in
          let shown = Option.value (Map.find t id) ~default:Shown.none in
          let top = Int.min len shown.top in
          let bottom = Int.min (len - top) shown.bottom in
          let hidden = len - top - bottom in
          if hidden < min_gap
          then
            for k = i to e - 1 do
              emit k
            done
          else (
            for k = i to i + top - 1 do
              emit k
            done;
            out
            := Row.Gap
                 { id
                 ; first =
                     Option.value
                       (new_line rows.(i + top))
                       ~default:(i + top + 1)
                 ; hidden
                 ; before = i + top > 0
                 ; after = e - bottom < n
                 }
               :: !out;
            for k = e - bottom to e - 1 do
              emit k
            done);
          go e)
    in
    go 0;
    List.rev !out)
;;

let reveal t (g : Gap.t) (how : How.t) =
  Map.update t g.id ~f:(fun shown ->
    let shown = Option.value shown ~default:Shown.none in
    match how with
    | Down -> { shown with top = shown.top + step }
    | Up -> { shown with bottom = shown.bottom + step }
    | All -> { shown with top = shown.top + g.hidden })
;;
