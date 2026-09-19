open! Core
open! Import

type op =
  | Keep of string
  | Del of string
  | Add of string

type positioned =
  { op : op
  ; old_before : int
  ; new_before : int
  }

let context = 3

let lines s =
  if String.is_empty s then [||] else Array.of_list (String.split_lines s)
;;

let ops a b =
  let n = Array.length a in
  let m = Array.length b in
  let dp = Array.make_matrix ~dimx:(n + 1) ~dimy:(m + 1) 0 in
  for i = n - 1 downto 0 do
    for j = m - 1 downto 0 do
      dp.(i).(j)
      <- (if String.equal a.(i) b.(j)
          then dp.(i + 1).(j + 1) + 1
          else Int.max dp.(i + 1).(j) dp.(i).(j + 1))
    done
  done;
  let rec walk i j acc =
    if i = n && j = m
    then List.rev acc
    else if i < n && j < m && String.equal a.(i) b.(j)
    then walk (i + 1) (j + 1) (Keep a.(i) :: acc)
    else if i < n && (j = m || dp.(i + 1).(j) >= dp.(i).(j + 1))
    then walk (i + 1) j (Del a.(i) :: acc)
    else walk i (j + 1) (Add b.(j) :: acc)
  in
  walk 0 0 []
;;

let position ops =
  let old = ref 1 in
  let new_ = ref 1 in
  List.map ops ~f:(fun op ->
    let p = { op; old_before = !old; new_before = !new_ } in
    (match op with
     | Keep _ ->
       incr old;
       incr new_
     | Del _ -> incr old
     | Add _ -> incr new_);
    p)
  |> Array.of_list
;;

let is_change (p : positioned) =
  match p.op with
  | Keep _ -> false
  | Del _ | Add _ -> true
;;

(* Change ops within [2 * context] keeps of each other belong to one hunk;
   otherwise they get separate hunks with a gap between them. *)
let clusters ops =
  let changes =
    List.filter_map
      (List.range 0 (Array.length ops))
      ~f:(fun i -> if is_change ops.(i) then Some i else None)
  in
  let rec go acc current = function
    | [] -> List.rev (List.rev current :: acc)
    | i :: rest ->
      (match current with
       | prev :: _ when i - prev > (2 * context) + 1 ->
         go (List.rev current :: acc) [ i ] rest
       | _ -> go acc (i :: current) rest)
  in
  match changes with
  | [] -> []
  | first :: rest -> go [] [ first ] rest
;;

let render_hunk ops ~start ~stop =
  let old_count = ref 0 in
  let new_count = ref 0 in
  for i = start to stop - 1 do
    match ops.(i).op with
    | Keep _ ->
      incr old_count;
      incr new_count
    | Del _ -> incr old_count
    | Add _ -> incr new_count
  done;
  let old_start = ops.(start).old_before in
  let old_start = if !old_count = 0 then old_start - 1 else old_start in
  let new_start = ops.(start).new_before in
  let new_start = if !new_count = 0 then new_start - 1 else new_start in
  let buf = Buffer.create 256 in
  Buffer.add_string
    buf
    (sprintf "@@ -%d,%d +%d,%d @@\n" old_start !old_count new_start !new_count);
  for i = start to stop - 1 do
    (match ops.(i).op with
     | Keep s ->
       Buffer.add_char buf ' ';
       Buffer.add_string buf s
     | Del s ->
       Buffer.add_char buf '-';
       Buffer.add_string buf s
     | Add s ->
       Buffer.add_char buf '+';
       Buffer.add_string buf s);
    Buffer.add_char buf '\n'
  done;
  Buffer.contents buf
;;

let hunks ~path ~before ~after =
  if String.equal before after
  then ""
  else (
    let a = lines before in
    let b = lines after in
    let ops = position (ops a b) in
    let hunks =
      List.map (clusters ops) ~f:(fun cluster ->
        let first = List.hd_exn cluster in
        let last = List.last_exn cluster in
        let start = Int.max 0 (first - context) in
        let stop = Int.min (Array.length ops) (last + context + 1) in
        render_hunk ops ~start ~stop)
    in
    match hunks with
    | [] -> ""
    | _ ->
      sprintf "--- a/%s\n+++ b/%s\n" path path ^ String.concat hunks ~sep:"")
;;
