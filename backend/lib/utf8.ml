open! Core

let replacement = "\xEF\xBF\xBD"

(* Expected sequence length for a lead byte and the allowed range of its
   first continuation byte, per the Unicode well-formed byte table (which
   excludes overlongs, surrogates and code points above U+10FFFF). *)
let lead b =
  if b < 0x80
  then Some (1, (0, 0))
  else if b < 0xC2
  then None
  else if b < 0xE0
  then Some (2, (0x80, 0xBF))
  else if b < 0xF0
  then (
    match b with
    | 0xE0 -> Some (3, (0xA0, 0xBF))
    | 0xED -> Some (3, (0x80, 0x9F))
    | _ -> Some (3, (0x80, 0xBF)))
  else if b < 0xF5
  then (
    match b with
    | 0xF0 -> Some (4, (0x90, 0xBF))
    | 0xF4 -> Some (4, (0x80, 0x8F))
    | _ -> Some (4, (0x80, 0xBF)))
  else None
;;

(* Number of bytes of the sequence starting at [i] that are present and
   consistent, and the expected total length. *)
let scan s i =
  let n = String.length s in
  let byte j = Char.to_int s.[j] in
  match lead (byte i) with
  | None -> None
  | Some (len, (lo, hi)) ->
    let ok j =
      j < n
      &&
      let b = byte j in
      let lo, hi = if j = i + 1 then lo, hi else 0x80, 0xBF in
      b >= lo && b <= hi
    in
    let rec count j = if j < i + len && ok j then count (j + 1) else j - i in
    Some (count (i + 1), len)
;;

let sequence_length s i =
  match scan s i with
  | Some (present, len) when present = len -> Some len
  | _ -> None
;;

let is_valid s =
  let n = String.length s in
  let rec go i =
    i >= n
    ||
    match sequence_length s i with
    | Some len -> go (i + len)
    | None -> false
  in
  go 0
;;

let sanitize s =
  if is_valid s
  then s
  else (
    let n = String.length s in
    let buf = Buffer.create (n + 8) in
    let rec go i =
      if i < n
      then (
        match sequence_length s i with
        | Some len ->
          Buffer.add_substring buf s ~pos:i ~len;
          go (i + len)
        | None ->
          Buffer.add_string buf replacement;
          go (i + 1))
    in
    go 0;
    Buffer.contents buf)
;;

let split_incomplete_suffix s =
  let n = String.length s in
  let rec try_lead i back =
    if i < 0 || back > 3
    then s, ""
    else (
      match scan s i with
      | Some (present, len) when present = n - i && present < len ->
        String.prefix s i, String.drop_prefix s i
      | Some _ -> s, ""
      | None -> try_lead (i - 1) (back + 1))
  in
  try_lead (n - 1) 0
;;
