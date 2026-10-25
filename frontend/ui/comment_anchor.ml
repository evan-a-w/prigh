open! Core
open! Import

module Located = struct
  type t =
    | At of
        { first : int
        ; last : int
        }
    | Outdated
  [@@deriving sexp_of, equal]
end

let locate (c : P.Review_comment.t) ~lines : Located.t =
  let anchor = Array.of_list (String.split_lines c.anchor) in
  let k = Array.length anchor in
  let n = Array.length lines in
  let matches_at i =
    i >= 0
    && i + k <= n
    && Array.for_alli anchor ~f:(fun j line -> String.equal lines.(i + j) line)
  in
  if k = 0
  then
    if c.first <= n
    then At { first = c.first; last = Int.min c.last n }
    else Outdated
  else (
    let was = c.first - 1 in
    if matches_at was
    then At { first = c.first; last = c.first + k - 1 }
    else (
      let best =
        Sequence.range 0 (n - k + 1)
        |> Sequence.filter ~f:matches_at
        |> Sequence.min_elt ~compare:(fun a b ->
          Int.compare (abs (a - was)) (abs (b - was)))
      in
      match best with
      | Some i -> At { first = i + 1; last = i + k }
      | None -> Outdated))
;;
