open! Core

module Item = struct
  type t =
    { name : string
    ; state : string
    ; url : string option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind name = Json.string_field j "name" in
    let%bind state = Json.string_field j "state" in
    let%map url = Json.string_opt_field j "url" in
    { name; state; url }
  ;;
end

type t =
  { state : string
  ; items : Item.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind state = Json.string_field j "state" in
  let%map items = Json.optional_list_field j "items" ~f:Item.of_json in
  { state; items }
;;
