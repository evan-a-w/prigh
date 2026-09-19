open! Core

let is_subsequence ~query s =
  let n = String.length query in
  let rec go qi si =
    if qi >= n
    then true
    else if si >= String.length s
    then false
    else if Char.equal query.[qi] s.[si]
    then go (qi + 1) (si + 1)
    else go qi (si + 1)
  in
  go 0 0
;;

let word_start s i =
  i = 0
  ||
  let p = s.[i - 1] in
  Char.equal p ' '
  || Char.equal p '-'
  || Char.equal p '_'
  || Char.equal p '/'
  || Char.equal p '.'
;;

let score ~query candidate =
  let query = String.lowercase (String.strip query) in
  let s = String.lowercase candidate in
  if String.is_empty query
  then Some 0
  else (
    let length_penalty = String.length s in
    if String.is_prefix s ~prefix:query
    then Some (4000 - length_penalty)
    else (
      match String.substr_index s ~pattern:query with
      | Some i when word_start s i -> Some (3000 - length_penalty)
      | Some _ -> Some (2000 - length_penalty)
      | None ->
        (* Also try matching every whitespace-separated word as a subsequence so
           "fable 5.1" finds "Claude Fable 5.1". *)
        let words =
          String.split query ~on:' ' |> List.filter ~f:(Fn.non String.is_empty)
        in
        if List.for_all words ~f:(fun w -> is_subsequence ~query:w s)
        then Some (1000 - length_penalty)
        else None))
;;

let rank ~query items ~key =
  List.filter_map items ~f:(fun item ->
    Option.map (score ~query (key item)) ~f:(fun s -> s, item))
  |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Int.compare b a)
  |> List.map ~f:snd
;;
