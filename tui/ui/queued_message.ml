open! Core
open! Import

module Kind = struct
  type t =
    | Steer
    | Follow_up
  [@@deriving sexp_of, equal]

  let name = function
    | Steer -> "steer"
    | Follow_up -> "follow-up"
  ;;

  let describe = function
    | Steer -> "steers the current turn"
    | Follow_up -> "follows up after the turn"
  ;;

  let wire = function
    | Steer -> "steer"
    | Follow_up -> "follow_up"
  ;;
end

type t =
  { kind : Kind.t
  ; index : int
  ; text : string
  }
[@@deriving sexp_of, equal]

let of_queues ~steer ~follow_up =
  let of_list kind = List.mapi ~f:(fun index text -> { kind; index; text }) in
  of_list Steer steer @ of_list Follow_up follow_up
;;

let id t = sprintf "%s:%d" (Kind.wire t.kind) t.index
let find ts ~id:wanted = List.find ts ~f:(fun t -> String.equal (id t) wanted)

let remove_params t =
  [ "queue", P.Json.str (Kind.wire t.kind)
  ; "index", P.Json.int t.index
  ; "text", P.Json.str t.text
  ]
;;

let one_line t =
  String.split_lines t.text
  |> List.map ~f:String.strip
  |> List.filter ~f:(Fn.non String.is_empty)
  |> String.concat ~sep:" "
;;
