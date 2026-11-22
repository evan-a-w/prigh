open! Core

module Match = struct
  type t =
    { path : string
    ; score : int
    ; positions : int array
    }
  [@@deriving sexp_of]
end

let letters q =
  String.filter (String.lowercase q) ~f:(fun c -> not (Char.is_whitespace c))
;;

let narrows ~query ~query' =
  String.is_prefix (letters query') ~prefix:(letters query)
;;

let is_sep = function
  | '/' | '_' | '-' | '.' | ' ' -> true
  | _ -> false
;;

(* The query's letters as late in [s] as they go: scanning back from the end
   gives the start of the rightmost match, and scanning forward from there
   the tightest letters after it. [q] is lowercase. *)
let positions q s ~from =
  let n = String.length s in
  let m = String.length q in
  let j = ref (m - 1) in
  let i = ref (n - 1) in
  while !j >= 0 && !i >= from do
    if
      Char.equal
        (Char.lowercase (String.unsafe_get s !i))
        (String.unsafe_get q !j)
    then decr j;
    decr i
  done;
  if !j >= 0
  then None
  else (
    let start = !i + 1 in
    let out = Array.create ~len:m 0 in
    let j = ref 0 in
    let i = ref start in
    while !j < m do
      if
        Char.equal
          (Char.lowercase (String.unsafe_get s !i))
          (String.unsafe_get q !j)
      then (
        out.(!j) <- !i;
        incr j);
      incr i
    done;
    Some out)
;;

let base_start s =
  match String.rindex s '/' with
  | Some i -> i + 1
  | None -> 0
;;

let rate ~query s (ps : int array) ~base =
  let score = ref 0 in
  let prev = ref (-2) in
  Array.iteri ps ~f:(fun k p ->
    let c = String.unsafe_get s p in
    let before = if p = 0 then '/' else String.unsafe_get s (p - 1) in
    score := !score + 16;
    if p = base
    then score := !score + 18
    else if Char.equal before '/'
    then score := !score + 12
    else if is_sep before
    then score := !score + 10
    else if Char.is_uppercase c && Char.is_lowercase before
    then score := !score + 10;
    if p = !prev + 1
    then score := !score + 12
    else if k > 0
    then score := !score - Int.min 12 (2 + (p - !prev - 1));
    if k < String.length query && Char.equal c (String.unsafe_get query k)
    then incr score;
    prev := p);
  let n = String.length s in
  let in_base = Array.length ps > 0 && ps.(0) >= base in
  if in_base then score := !score + 40;
  (* The whole of the name before its extension. *)
  if in_base && ps.(0) = base
  then (
    let last = ps.(Array.length ps - 1) in
    if last + 1 = n || Char.equal (String.get s (last + 1)) '.'
    then score := !score + 30);
  let depth = String.count s ~f:(Char.equal '/') in
  !score - (n / 6) - (depth * 2)
;;

let score ~query s =
  let q = letters query in
  if String.is_empty q
  then Some { Match.path = s; score = 0; positions = [||] }
  else (
    let raw = String.filter query ~f:(fun c -> not (Char.is_whitespace c)) in
    let base = base_start s in
    let ps =
      match positions q s ~from:base with
      | Some ps -> Some ps
      | None -> positions q s ~from:0
    in
    Option.map ps ~f:(fun ps ->
      { Match.path = s; score = rate ~query:raw s ps ~base; positions = ps }))
;;

let rank ~query paths =
  let found = Array.filter_map paths ~f:(fun p -> score ~query p) in
  Array.stable_sort found ~compare:(fun (a : Match.t) (b : Match.t) ->
    match Int.compare b.score a.score with
    | 0 ->
      (match Int.compare (String.length a.path) (String.length b.path) with
       | 0 -> String.compare a.path b.path
       | c -> c)
    | c -> c);
  found
;;
