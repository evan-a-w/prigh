open! Core

type t =
  { mime_type : string
  ; bytes : int
  }
[@@deriving sexp_of, equal]

let decoded_size data =
  let data = String.rstrip data ~drop:(Char.equal '=') in
  String.length data * 3 / 4
;;

let of_json j =
  let open Or_error.Let_syntax in
  let%bind mime_type = Json.string_field j "mime_type" in
  let%map data = Json.string_field j "data" in
  { mime_type; bytes = decoded_size data }
;;

let size_to_string bytes =
  if bytes < 1024
  then sprintf "%d B" bytes
  else if bytes < 1024 * 1024
  then sprintf "%.1f KB" (Float.of_int bytes /. 1024.)
  else sprintf "%.1f MB" (Float.of_int bytes /. (1024. *. 1024.))
;;

let to_string_hum t =
  sprintf "[image: %s, %s]" t.mime_type (size_to_string t.bytes)
;;
