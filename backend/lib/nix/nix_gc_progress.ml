open! Core
open! Import

type t =
  { paths_deleted : int
  ; generations_deleted : int
  ; message : string option
  }
[@@deriving sexp_of]

let to_json t =
  `Object
    [ "type", `String "event"
    ; "event", `String "nix_gc_progress"
    ; "paths_deleted", `Number (Int.to_string t.paths_deleted)
    ; "generations_deleted", `Number (Int.to_string t.generations_deleted)
    ; ( "message"
      , Option.value_map t.message ~default:`Null ~f:(fun m -> `String m) )
    ]
;;
