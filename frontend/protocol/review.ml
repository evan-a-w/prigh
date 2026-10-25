open! Core

type t =
  { comments : Review_comment.t list
  ; approvals : (string * string) list
  }
[@@deriving sexp_of, equal]

let empty = { comments = []; approvals = [] }

let of_json j =
  let open Or_error.Let_syntax in
  let%bind comments = Json.list_field j "comments" ~f:Review_comment.of_json in
  let%map approvals =
    Json.optional_list_field j "approvals" ~f:(fun a ->
      let%bind path = Json.string_field a "path" in
      let%map hash = Json.string_field a "hash" in
      path, hash)
  in
  { comments; approvals }
;;

let on t path =
  List.filter t.comments ~f:(fun c -> String.equal c.Review_comment.path path)
;;

let unsent t = List.count t.comments ~f:Review_comment.unsent
