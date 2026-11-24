open! Core
open! Import

type t = string [@@deriving sexp, equal, compare]

let main = "main"
let is_main = String.equal main
let to_string t = t
let max_length = 32
let valid_char c = Char.is_lowercase c || Char.is_digit c || Char.equal c '-'

let of_string s =
  if
    String.is_empty s
    || String.length s > max_length
    || not (String.for_all s ~f:valid_char)
  then
    Or_error.errorf
      "%S is not an account id: use 1 to %d lowercase letters, digits and -"
      s
      max_length
  else Ok s
;;

let slug label =
  let buf = Buffer.create (String.length label) in
  String.iter (String.lowercase label) ~f:(fun c ->
    if Char.is_alphanum c
    then Buffer.add_char buf c
    else if
      Buffer.length buf > 0
      && not (Char.equal (Buffer.nth buf (Buffer.length buf - 1)) '-')
    then Buffer.add_char buf '-');
  let s = String.rstrip (Buffer.contents buf) ~drop:(Char.equal '-') in
  let s = String.prefix s (max_length - 3) in
  let s = String.rstrip s ~drop:(Char.equal '-') in
  if String.is_empty s then "account" else s
;;

let of_label label ~taken =
  let base = slug label in
  let free id = (not (is_main id)) && not (List.mem taken id ~equal) in
  if free base
  then base
  else
    Sequence.find_exn
      (Sequence.map
         (Sequence.unfold ~init:2 ~f:(fun n -> Some (n, n + 1)))
         ~f:(fun n -> sprintf "%s-%d" base n))
      ~f:free
;;

let same written account =
  String.equal
    (Option.value written ~default:main)
    (Option.value account ~default:main)
;;
