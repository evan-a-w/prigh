open! Core
open! Import

let attr fields =
  Attr.create
    "style"
    (String.concat
       ~sep:" "
       (List.map fields ~f:(fun (k, v) -> sprintf "%s: %s;" k v)))
;;
