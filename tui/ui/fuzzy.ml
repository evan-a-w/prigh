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

(* How well [word] matches [s] as a subsequence, if it does: the best over
   where its first letter is found, counting letters that follow each other
   or start a word, less the gaps between them. *)
let subsequence_quality ~word s =
  let n = String.length word in
  let from start =
    let rec go qi si prev quality =
      if qi >= n
      then Some quality
      else if si >= String.length s
      then None
      else (
        match String.index_from s si word.[qi] with
        | None -> None
        | Some p ->
          let quality =
            quality
            + (if p = prev + 1 then 16 else -Int.min 8 (p - prev - 1))
            + if word_start s p then 12 else 0
          in
          go (qi + 1) (p + 1) p quality)
    in
    go 1 (start + 1) start (if word_start s start then 12 else 0)
  in
  if n = 0
  then Some 0
  else
    List.filter_mapi (String.to_list s) ~f:(fun i c ->
      if Char.equal c word.[0] then from i else None)
    |> List.max_elt ~compare:Int.compare
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
        then (
          let quality =
            List.sum
              (module Int)
              words
              ~f:(fun word ->
                Option.value ~default:0 (subsequence_quality ~word s))
          in
          Some (1000 + Int.clamp_exn quality ~min:0 ~max:900 - length_penalty))
        else None))
;;

let rank ~query items ~key =
  List.filter_map items ~f:(fun item ->
    Option.map (score ~query (key item)) ~f:(fun s -> s, item))
  |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Int.compare b a)
  |> List.map ~f:snd
;;
