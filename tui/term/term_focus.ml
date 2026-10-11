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
      else if i + 2 < n
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
