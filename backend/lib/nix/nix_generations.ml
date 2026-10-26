open! Core
open! Import

type t =
  | Older_than_days of int
  | All
[@@deriving sexp_of, equal]

let max_days = 36500

let of_days days =
  if days = 0
  then Ok All
  else if days > 0 && days <= max_days
  then Ok (Older_than_days days)
  else Or_error.errorf "%d days is out of range (0 to %d)" days max_days
;;

let of_json json =
  let bad () =
    Or_error.error_string
      "param \"delete_generations\" must be a number of days (0 for all old \
       generations) or \"all\""
  in
  match json with
  | None | Some `Null -> Ok None
  | Some (`String "all") -> Ok (Some All)
  | Some (`Number n) ->
    (match Int.of_string_opt n with
     | Some days ->
       (match of_days days with
        | Ok t -> Ok (Some t)
        | Error _ -> bad ())
     | None -> bad ())
  | Some _ -> bad ()
;;

let to_json = function
  | None -> `Null
  | Some All -> `String "all"
  | Some (Older_than_days days) -> `Number (Int.to_string days)
;;

let to_arg = function
  | None -> "keep"
  | Some All -> "all"
  | Some (Older_than_days days) -> sprintf "%dd" days
;;

let of_arg = function
  | "keep" -> Ok None
  | "all" -> Ok (Some All)
  | s ->
    (match String.chop_suffix s ~suffix:"d" with
     | Some n
       when (not (String.is_empty n)) && String.for_all n ~f:Char.is_digit ->
       Or_error.map (of_days (Int.of_string n)) ~f:Option.some
     | _ -> Or_error.errorf "bad generations %S (keep, all or Nd)" s)
;;
