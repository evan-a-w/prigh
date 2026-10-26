open! Core

module State = struct
  type t =
    | Open
    | Closed
    | Merged
  [@@deriving sexp_of, equal]

  let to_string = function
    | Open -> "open"
    | Closed -> "closed"
    | Merged -> "merged"
  ;;

  let of_string = function
    | "open" -> Ok Open
    | "closed" -> Ok Closed
    | "merged" -> Ok Merged
    | other -> Or_error.errorf "unknown pull request state %S" other
  ;;
end

type t =
  { number : int
  ; title : string
  ; state : State.t
  ; draft : bool
  ; author : string
  ; head : string
  ; base : string
  ; url : string
  ; body : string
  ; created_at : string
  ; updated_at : string
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind number = Json.int_field j "number" in
  let%bind title = Json.string_field j "title" in
  let%bind state = Json.string_field j "state" >>= State.of_string in
  let%bind draft = Json.bool_default_field j "draft" ~default:false in
  let%bind author = Json.string_field j "author" in
  let%bind head = Json.string_field j "head" in
  let%bind base = Json.string_field j "base" in
  let%bind url = Json.string_field j "url" in
  let%bind body = Json.string_opt_field j "body" >>| Option.value ~default:"" in
  let%bind created_at = Json.string_field j "created_at" in
  let%map updated_at = Json.string_field j "updated_at" in
  { number
  ; title
  ; state
  ; draft
  ; author
  ; head
  ; base
  ; url
  ; body
  ; created_at
  ; updated_at
  }
;;
