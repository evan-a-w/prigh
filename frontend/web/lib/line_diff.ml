open! Core

module Line = struct
  type t =
    | Same of string
    | Removed of string
    | Added of string
  [@@deriving sexp_of, equal]
end

let max_cells = 250_000

let lines text =
  if String.is_empty text then [||] else Array.of_list (String.split_lines text)
;;

let diff ~old ~new_ =
  let a = lines old in
  let b = lines new_ in
  let n = Array.length a in
  let m = Array.length b in
  if n * m > max_cells
  then
    Array.to_list (Array.map a ~f:(fun l -> Line.Removed l))
    @ Array.to_list (Array.map b ~f:(fun l -> Line.Added l))
  else (
    (* [lcs.(i).(j)]: the LCS length of [a.(i..)] and [b.(j..)]. *)
    let lcs = Array.make_matrix ~dimx:(n + 1) ~dimy:(m + 1) 0 in
    for i = n - 1 downto 0 do
      for j = m - 1 downto 0 do
        lcs.(i).(j)
        <- (if String.equal a.(i) b.(j)
            then lcs.(i + 1).(j + 1) + 1
            else Int.max lcs.(i + 1).(j) lcs.(i).(j + 1))
      done
    done;
    let rec walk i j acc =
      if i < n && j < m && String.equal a.(i) b.(j)
      then walk (i + 1) (j + 1) (Line.Same a.(i) :: acc)
      else if i < n && (j >= m || lcs.(i + 1).(j) >= lcs.(i).(j + 1))
      then walk (i + 1) j (Line.Removed a.(i) :: acc)
      else if j < m
      then walk i (j + 1) (Line.Added b.(j) :: acc)
      else List.rev acc
    in
    walk 0 0 [])
;;
