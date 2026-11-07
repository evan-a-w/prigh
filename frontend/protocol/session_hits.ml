open! Core

module Snippet = struct
  type t =
    { text : string
    ; ranges : (int * int) list
    }
  [@@deriving sexp_of, equal]

  let range = function
    | `Array [ start; length ] ->
      let int j = Json.int_field (Json.obj [ "n", j ]) "n" in
      Or_error.both (int start) (int length)
    | other -> Or_error.errorf "expected [start, length], got %s" (Json.to_string other)
  ;;

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind text = Json.string_field j "text" in
    let%map ranges = Json.list_field j "ranges" ~f:range in
    { text; ranges }
  ;;
end

module Hit = struct
  type t =
    { id : string
    ; path : string
    ; score : int
    ; snippet : Snippet.t option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind id = Json.string_field j "id" in
    let%bind path = Json.string_field j "path" in
    let%bind score = Json.int_field j "score" in
    let%map snippet =
      match Json.field j "snippet" with
      | None | Some `Null -> Ok None
      | Some snippet -> Snippet.of_json snippet |> Or_error.map ~f:Option.some
    in
    { id; path; score; snippet }
  ;;
end

type t =
  { query : string
  ; hits : Hit.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind query = Json.string_field j "query" in
  let%map hits = Json.list_field j "hits" ~f:Hit.of_json in
  { query; hits }
;;

let params ~query ~limit = Json.obj [ "query", Json.str query; "limit", Json.int limit ]
