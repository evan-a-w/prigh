open! Core

let enable = "\027[?1004h"
let disable = "\027[?1004l"

let split input =
  if not (String.mem input '\027')
  then input, []
  else (
    let kept = Buffer.create (String.length input) in
    let reports = ref [] in
    let n = String.length input in
    let rec go i =
      if i >= n
      then ()
      else if
        i + 2 < n
        && Char.equal input.[i] '\027'
        && Char.equal input.[i + 1] '['
        && (Char.equal input.[i + 2] 'I' || Char.equal input.[i + 2] 'O')
      then (
        reports := Char.equal input.[i + 2] 'I' :: !reports;
        go (i + 3))
      else (
        Buffer.add_char kept input.[i];
        go (i + 1))
    in
    go 0;
    Buffer.contents kept, List.rev !reports)
;;

type t = { mutable pending : string }

let create () = { pending = "" }
let has_pending t = not (String.is_empty t.pending)

let flush t =
  let out = t.pending in
  t.pending <- "";
  out
;;

let feed t chunk =
  let input = if String.is_empty t.pending then chunk else t.pending ^ chunk in
  t.pending <- "";
  let n = String.length input in
  let split_at =
    if n >= 2 && Char.equal input.[n - 2] '\027' && Char.equal input.[n - 1] '['
    then n - 2
    else if n >= 1 && Char.equal input.[n - 1] '\027'
    then n - 1
    else n
  in
  let complete =
    if split_at = n
    then input
    else (
      t.pending <- String.sub input ~pos:split_at ~len:(n - split_at);
      String.sub input ~pos:0 ~len:split_at)
  in
  split complete
;;
