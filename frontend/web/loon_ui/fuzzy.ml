open! Import

let is_word_start text i =
  i = 0
  ||
  let prev = text.[i - 1] in
  (not (Char.is_alphanum prev))
  || (Char.is_lowercase prev && Char.is_uppercase text.[i])
;;

let score ~query text =
  let query = String.strip query |> String.lowercase in
  let lower = String.lowercase text in
  let n = String.length lower in
  if String.is_empty query
  then Some 0
  else (
    let rec go qi ti prev_match score =
      if qi = String.length query
      then Some score
      else if ti >= n
      then None
      else if Char.equal query.[qi] ' '
      then go (qi + 1) ti prev_match score
      else if Char.equal query.[qi] lower.[ti]
      then (
        let bonus =
          1
          + (if prev_match = ti - 1 then 5 else 0)
          + (if is_word_start text ti then 8 else 0)
          + if ti = 0 then 10 else 0
        in
        go (qi + 1) (ti + 1) ti (score + bonus))
      else go qi (ti + 1) prev_match score
    in
    go 0 0 (-2) 0 |> Option.map ~f:(fun s -> (s * 4) - (n / 4)))
;;

let best ~query texts =
  List.filter_map texts ~f:(score ~query) |> List.max_elt ~compare:Int.compare
;;
