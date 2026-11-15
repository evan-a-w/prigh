open! Core
open! Import

module Range = struct
  type t =
    { start : int
    ; end_ : int
    }
  [@@deriving sexp_of, equal]
end

let min_kept = 0.4
let max_cells = 40_000
let word_char c = Char.is_alphanum c || Char.equal c '_' || Char.to_int c >= 128
let operator_char c = String.mem "<>=!&|+-*/%^~:.?@$" c

(* The words' ranges, in order. *)
let words s =
  let n = String.length s in
  let rec go i acc =
    if i >= n
    then List.rev acc
    else (
      let c = s.[i] in
      let run f =
        let j = ref (i + 1) in
        while !j < n && f s.[!j] do
          incr j
        done;
        !j
      in
      let j =
        if word_char c
        then run word_char
        else if Char.is_whitespace c
        then run Char.is_whitespace
        else if operator_char c
        then run operator_char
        else i + 1
      in
      go j ({ Range.start = i; end_ = j } :: acc))
  in
  Array.of_list (go 0 [])
;;

let text s (r : Range.t) = String.sub s ~pos:r.start ~len:(r.end_ - r.start)
let blank s (r : Range.t) = String.for_all (text s r) ~f:Char.is_whitespace
let solid s (r : Range.t) = if blank s r then 0 else r.end_ - r.start

(* The changed words of each side, joined where only whitespace (or
   nothing) is between them. *)
let joined s (changed : Range.t list) =
  List.fold changed ~init:[] ~f:(fun acc (r : Range.t) ->
    match acc with
    | (last : Range.t) :: rest
      when String.for_all
             (String.sub s ~pos:last.end_ ~len:(r.start - last.end_))
             ~f:Char.is_whitespace -> { last with end_ = r.end_ } :: rest
    | _ -> r :: acc)
  |> List.rev
;;

let ranges ~old ~new_ =
  let a = words old in
  let b = words new_ in
  let n = Array.length a in
  let m = Array.length b in
  if String.equal old new_ || n * m > max_cells
  then None
  else (
    let lcs = Array.make_matrix ~dimx:(n + 1) ~dimy:(m + 1) 0 in
    let same i j = String.equal (text old a.(i)) (text new_ b.(j)) in
    for i = n - 1 downto 0 do
      for j = m - 1 downto 0 do
        lcs.(i).(j)
        <- (if same i j
            then lcs.(i + 1).(j + 1) + 1
            else Int.max lcs.(i + 1).(j) lcs.(i).(j + 1))
      done
    done;
    let kept_a = Array.create ~len:n false in
    let kept_b = Array.create ~len:m false in
    let rec walk i j =
      if i < n && j < m
      then
        if same i j
        then (
          kept_a.(i) <- true;
          kept_b.(j) <- true;
          walk (i + 1) (j + 1))
        else if lcs.(i + 1).(j) >= lcs.(i).(j + 1)
        then walk (i + 1) j
        else walk i (j + 1)
    in
    walk 0 0;
    let total s words = Array.sum (module Int) words ~f:(solid s) in
    let kept s words kept =
      Array.foldi words ~init:0 ~f:(fun i acc r ->
        if kept.(i) then acc + solid s r else acc)
    in
    let common = kept old a kept_a in
    let most = Int.max (total old a) (total new_ b) in
    if most = 0 || Float.(of_int common /. of_int most < min_kept)
    then None
    else (
      let changed s words kept =
        Array.filteri words ~f:(fun i _ -> not kept.(i))
        |> Array.to_list
        |> joined s
      in
      Some (changed old a kept_a, changed new_ b kept_b)))
;;
