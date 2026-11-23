open! Import

let continue_match = 1.
let space_word_jump = 0.9
let non_space_word_jump = 0.8
let character_jump = 0.17
let transposition = 0.1
let penalty_skipped = 0.999
let penalty_case_mismatch = 0.9999
let penalty_not_complete = 0.99

let is_gap = function
  | '\\' | '/' | '_' | '+' | '.' | '#' | '"' | '@' | '[' | '(' | '{' | '&' -> true
  | _ -> false
;;

let is_space c = Char.is_whitespace c || Char.equal c '-'

let count s ~pos ~len ~f =
  if len <= 0 then 0 else String.count (String.sub s ~pos ~len) ~f
;;

let format s = String.map (String.lowercase s) ~f:(fun c -> if is_space c then ' ' else c)

let score ?(keywords = []) value ~search =
  let s = if List.is_empty keywords then value else String.concat ~sep:" " (value :: keywords) in
  let abbr = search in
  let ls = format s in
  let la = format abbr in
  let n = String.length s in
  let m = String.length abbr in
  let memo = Hashtbl.create (module struct
      type t = int * int [@@deriving sexp_of, compare, hash]
    end)
  in
  let char_at str i = if i >= 0 && i < String.length str then Some str.[i] else None in
  let index_from i c = if i > n then None else String.index_from ls i c in
  let rec inner si ai =
    if ai = m
    then if si = n then continue_match else penalty_not_complete
    else (
      match Hashtbl.find memo (si, ai) with
      | Some r -> r
      | None ->
        let ac = la.[ai] in
        let high = ref 0. in
        let rec loop index =
          match index with
          | None -> ()
          | Some index ->
            let score = ref (inner (index + 1) (ai + 1)) in
            if Float.(!score > !high)
            then (
              if index = si
              then score := !score *. continue_match
              else if is_gap s.[index - 1]
              then (
                score := !score *. non_space_word_jump;
                let breaks = count s ~pos:si ~len:(index - 1 - si) ~f:is_gap in
                if breaks > 0 && si > 0
                then score := !score *. (penalty_skipped ** Float.of_int breaks))
              else if is_space s.[index - 1]
              then (
                score := !score *. space_word_jump;
                let breaks = count s ~pos:si ~len:(index - 1 - si) ~f:is_space in
                if breaks > 0 && si > 0
                then score := !score *. (penalty_skipped ** Float.of_int breaks))
              else (
                score := !score *. character_jump;
                if si > 0
                then score := !score *. (penalty_skipped ** Float.of_int (index - si)));
              if not (Char.equal s.[index] abbr.[ai])
              then score := !score *. penalty_case_mismatch);
            let next_a = char_at la (ai + 1) in
            let prev_s = char_at ls (index - 1) in
            if (Float.(!score < transposition)
                && Option.equal Char.equal prev_s next_a
                && Option.is_some next_a)
               || (Option.equal Char.equal next_a (Some ac)
                   && not (Option.equal Char.equal prev_s (Some ac)))
            then (
              let t = inner (index + 1) (ai + 2) in
              if Float.(t *. transposition > !score) then score := t *. transposition);
            if Float.(!score > !high) then high := !score;
            loop (index_from (index + 1) ac)
        in
        loop (index_from si ac);
        Hashtbl.set memo ~key:(si, ai) ~data:!high;
        !high)
  in
  inner 0 0
;;
