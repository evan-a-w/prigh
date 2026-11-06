open! Core

let group digits =
  let n = String.length digits in
  let b = Buffer.create (n + (n / 3)) in
  String.iteri digits ~f:(fun i c ->
    if i > 0 && (n - i) mod 3 = 0 then Buffer.add_char b ',';
    Buffer.add_char b c);
  Buffer.contents b
;;

let fixed ~decimals f =
  let s = sprintf "%.*f" decimals (Float.abs f) in
  let int_part, frac =
    match String.lsplit2 s ~on:'.' with
    | Some (i, f) -> i, "." ^ f
    | None -> s, ""
  in
  let negative = Float.(f < 0.) && not (String.for_all s ~f:(fun c -> Char.equal c '0' || Char.equal c '.')) in
  (if negative then "−" else "") ^ group int_part ^ frac
;;

let strip_zeros s =
  if String.mem s '.'
  then String.rstrip s ~drop:(Char.equal '0') |> String.rstrip ~drop:(Char.equal '.')
  else s
;;

let number ?decimals f =
  if Float.is_nan f
  then "—"
  else if Float.is_inf f
  then if Float.(f > 0.) then "∞" else "−∞"
  else (
    match decimals with
    | Some decimals -> fixed ~decimals:(Int.clamp_exn decimals ~min:0 ~max:10) f
    | None ->
      let decimals = if Float.(abs f < 1.) then 4 else 2 in
      strip_zeros (fixed ~decimals f))
;;

let percent ?decimals f =
  let v = f *. 100. in
  (match decimals with
   | Some _ -> number ?decimals v
   | None -> strip_zeros (fixed ~decimals:1 v))
  ^ "%"
;;

let currency ?decimals ~code f =
  let code = String.uppercase code in
  let symbol, default =
    match code with
    | "USD" -> "$", 2
    | "EUR" -> "€", 2
    | "GBP" -> "£", 2
    | "JPY" -> "¥", 0
    | "KRW" -> "₩", 0
    | "INR" -> "₹", 2
    | "CNY" -> "¥", 2
    | "AUD" -> "A$", 2
    | "CAD" -> "C$", 2
    | code -> code ^ " ", 2
  in
  let decimals = Option.value decimals ~default in
  let s = fixed ~decimals f in
  match String.chop_prefix s ~prefix:"−" with
  | Some rest -> "−" ^ symbol ^ rest
  | None -> symbol ^ s
;;

let compact f =
  let a = Float.abs f in
  let scaled d suffix = strip_zeros (fixed ~decimals:1 (f /. d)) ^ suffix in
  if Float.(a >= 1e12)
  then scaled 1e12 "T"
  else if Float.(a >= 1e9)
  then scaled 1e9 "B"
  else if Float.(a >= 1e6)
  then scaled 1e6 "M"
  else if Float.(a >= 1e4)
  then scaled 1e3 "k"
  else number f
;;

let nice_step raw =
  let e = Float.round_down (Float.log10 raw) in
  let base = 10. **. e in
  let f = raw /. base in
  let nice =
    if Float.(f <= 1.) then 1. else if Float.(f <= 2.) then 2. else if Float.(f <= 5.) then 5. else 10.
  in
  nice *. base
;;

let ticks ?(count = 5) lo hi =
  let lo, hi = Float.min lo hi, Float.max lo hi in
  let lo, hi =
    if Float.(hi -. lo < 1e-12)
    then if Float.(lo = 0.) then 0., 1. else Float.min lo 0., Float.max hi 0.
    else lo, hi
  in
  let lo, hi = if Float.(hi -. lo < 1e-12) then lo -. 1., hi +. 1. else lo, hi in
  let step = nice_step ((hi -. lo) /. Float.of_int (Int.max 1 (count - 1))) in
  (* Within a tick of zero: from zero. *)
  let lo = if Float.(lo > 0. && lo <= step) then 0. else lo in
  let hi = if Float.(hi < 0. && hi >= -.step) then 0. else hi in
  let first = Float.round_down (lo /. step) *. step in
  let last = Float.round_up (hi /. step) *. step in
  let n = Float.iround_nearest_exn ((last -. first) /. step) in
  List.init (n + 1) ~f:(fun i ->
    let v = first +. (Float.of_int i *. step) in
    (* No [-0.] or [0.30000000000000004]. *)
    let v = Float.round_decimal v ~decimal_digits:10 in
    if Float.(v = 0.) then 0. else v)
;;
