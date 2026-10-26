open! Core
open! Import

(* The diff's files, each from its [diff --git] line. *)
let files diff =
  let lines = String.split_lines diff in
  let flush acc = function
    | [] -> acc
    | file -> List.rev file :: acc
  in
  let acc, current =
    List.fold lines ~init:([], []) ~f:(fun (acc, current) line ->
      if String.is_prefix line ~prefix:"diff --git "
      then flush acc current, [ line ]
      else acc, line :: current)
  in
  List.rev (flush acc current)
;;

let counts lines =
  List.fold lines ~init:(0, 0) ~f:(fun (added, removed) line ->
    if
      String.is_prefix line ~prefix:"+++ "
      || String.is_prefix line ~prefix:"--- "
    then added, removed
    else if String.is_prefix line ~prefix:"+"
    then added + 1, removed
    else if String.is_prefix line ~prefix:"-"
    then added, removed + 1
    else added, removed)
;;

(* The lines that fit in [budget] bytes, and how many did not. *)
let take_bytes lines ~budget =
  let rec go acc used = function
    | [] -> List.rev acc, 0
    | line :: rest ->
      let used = used + String.length line + 1 in
      if used > budget
      then List.rev acc, 1 + List.length rest
      else go (line :: acc) used rest
  in
  go [] 0 lines
;;

let digest ~max_bytes diff =
  if String.length diff <= max_bytes
  then diff
  else (
    let files = files diff in
    let share = Int.max 600 (max_bytes / Int.max 1 (List.length files)) in
    let buf = Buffer.create max_bytes in
    let left_out = ref [] in
    List.iter files ~f:(fun lines ->
      let header = Option.value (List.hd lines) ~default:"" in
      let added, removed = counts lines in
      let room = max_bytes - Buffer.length buf in
      if room < 200
      then left_out := sprintf "%s (+%d -%d)" header added removed :: !left_out
      else (
        let kept, dropped = take_bytes lines ~budget:(Int.min share room) in
        List.iter kept ~f:(fun line ->
          Buffer.add_string buf line;
          Buffer.add_char buf '\n');
        if dropped > 0
        then
          Buffer.add_string
            buf
            (sprintf
               "[... %d more lines of this file left out; it is +%d -%d in all]\n"
               dropped
               added
               removed)));
    (match List.rev !left_out with
     | [] -> ()
     | left_out ->
       Buffer.add_string
         buf
         (sprintf
            "[%d more files left out:]\n%s\n"
            (List.length left_out)
            (String.concat ~sep:"\n" left_out)));
    Buffer.contents buf)
;;
