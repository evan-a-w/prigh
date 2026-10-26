open! Core

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

let of_json = function
  | None | Some `Null -> Ok None
  | Some (`String "all") -> Ok (Some All)
  | Some (`Number n) ->
    (match Int.of_string_opt n with
     | Some days -> Or_error.map (of_days days) ~f:Option.some
     | None -> Or_error.errorf "bad generations %s" n)
  | Some json ->
    Or_error.errorf "bad generations %s" (Jsonaf.to_string json)
;;

let to_json = function
  | None -> `Null
  | Some All -> Json.str "all"
  | Some (Older_than_days days) -> Json.int days
;;
