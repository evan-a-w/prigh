open! Core

module Cost = struct
  type t =
    { input : float
    ; output : float
    ; cache_read : float
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind input = Json.float_field j "input" in
    let%bind output = Json.float_field j "output" in
    let%map cache_read = Json.float_field j "cache_read" in
    { input; output; cache_read }
  ;;
end

type t =
  { id : string
  ; provider : string
  ; key : string
  ; name : string
  ; context_window : int
  ; max_output : int
  ; supports_thinking : bool
  ; cost : Cost.t
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind provider = Json.string_field j "provider" in
  let%bind key = Json.string_field j "key" in
  let%bind name = Json.string_field j "name" in
  let%bind context_window = Json.int_field j "context_window" in
  let%bind max_output = Json.int_field j "max_output" in
  let%bind supports_thinking = Json.bool_field j "supports_thinking" in
  let%map cost = Json.object_field j "cost" >>= Cost.of_json in
  { id
  ; provider
  ; key
  ; name
  ; context_window
  ; max_output
  ; supports_thinking
  ; cost
  }
;;
