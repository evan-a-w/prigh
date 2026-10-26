open! Core
open! Import
module Range = P.Review_comment.Range

module Located = struct
  type t =
    | At of Range.t
    | Outdated
  [@@deriving sexp_of, equal]
end

let nearest ~was candidates =
  Sequence.min_elt candidates ~compare:(fun (a, _) (b, _) ->
    Int.compare (abs (a - was)) (abs (b - was)))
;;

(* Every place the selected text [sel] (its lines) occurs, as the 0-based
   index of its first line and its columns. *)
let selection_matches ~line ~count sel ~start_col =
  let m = Array.length sel in
  let line_at i = line (i + 1) in
  Sequence.range 0 (count - m + 1)
  |> Sequence.concat_map ~f:(fun i ->
    match m with
    | 1 ->
      (match line_at i with
       | None -> Sequence.empty
       | Some text ->
         String.substr_index_all text ~may_overlap:true ~pattern:sel.(0)
         |> List.sort ~compare:(fun a b ->
           Int.compare (abs (a - start_col)) (abs (b - start_col)))
         |> List.hd
         |> Option.value_map ~default:Sequence.empty ~f:(fun pos ->
           Sequence.singleton
             ( i
             , { P.Review_comment.Columns.start_col = pos
               ; end_col = pos + String.length sel.(0)
               } )))
    | _ ->
      let all =
        Option.both (line_at i) (line_at (i + m - 1))
        |> Option.bind ~f:(fun (head, tail) ->
          if
            String.is_suffix head ~suffix:sel.(0)
            && String.is_prefix tail ~prefix:sel.(m - 1)
            && List.for_all
                 (List.range 1 (m - 1))
                 ~f:(fun j ->
                   Option.exists (line_at (i + j)) ~f:(String.equal sel.(j)))
          then
            Some
              { P.Review_comment.Columns.start_col =
                  String.length head - String.length sel.(0)
              ; end_col = String.length sel.(m - 1)
              }
          else None)
      in
      Option.value_map all ~default:Sequence.empty ~f:(fun cols ->
        Sequence.singleton (i, cols)))
;;

let locate (c : P.Review_comment.t) ~line ~count : Located.t =
  let r = c.range in
  let anchor = Array.of_list (String.split_lines c.anchor) in
  let k = Array.length anchor in
  let matches_at i =
    i >= 0
    && i + k <= count
    && Array.for_alli anchor ~f:(fun j text ->
      Option.exists (line (i + j + 1)) ~f:(String.equal text))
  in
  let was = r.first - 1 in
  if k = 0
  then
    if r.first <= count
    then At { r with last = Int.min r.last count }
    else Outdated
  else if matches_at was
  then At r
  else (
    match
      nearest
        ~was
        (Sequence.range 0 (count - k + 1)
         |> Sequence.filter ~f:matches_at
         |> Sequence.map ~f:(fun i -> i, ()))
    with
    | Some (i, ()) -> At { r with first = i + 1; last = i + k }
    | None ->
      (match r.columns with
       | None -> Outdated
       | Some { start_col; _ } ->
         let sel =
           Array.of_list (String.split ~on:'\n' (P.Review_comment.selected c))
         in
         (match
            nearest ~was (selection_matches ~line ~count sel ~start_col)
          with
          | Some (i, columns) ->
            At
              { r with
                first = i + 1
              ; last = i + Array.length sel
              ; columns = Some columns
              }
          | None -> Outdated)))
;;
