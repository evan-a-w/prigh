open! Core

let distance a b =
  let a = String.lowercase a
  and b = String.lowercase b in
  let n = String.length a
  and m = String.length b in
  let prev = Array.init (m + 1) ~f:Fn.id in
  let cur = Array.create ~len:(m + 1) 0 in
  for i = 1 to n do
    cur.(0) <- i;
    for j = 1 to m do
      let cost = if Char.equal a.[i - 1] b.[j - 1] then 0 else 1 in
      cur.(j)
      <- Int.min (Int.min (prev.(j) + 1) (cur.(j - 1) + 1)) (prev.(j - 1) + cost)
    done;
    Array.blit ~src:cur ~src_pos:0 ~dst:prev ~dst_pos:0 ~len:(m + 1)
  done;
  prev.(m)
;;
