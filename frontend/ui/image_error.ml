open! Core

let too_large error =
  let error = String.lowercase error in
  match String.substr_index error ~pattern:"image" with
  | None -> false
  | Some at ->
    let after = String.drop_prefix error at in
    List.exists
      [ "exceed"
      ; "too large"
      ; "too big"
      ; "maximum size"
      ; "maximum allowed size"
      ; "size limit"
      ]
      ~f:(fun pattern -> String.is_substring after ~substring:pattern)
;;
