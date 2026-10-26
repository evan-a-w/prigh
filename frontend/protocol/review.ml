open! Core

module Approval = struct
  type t =
    { root : string option
    ; path : string
    ; hash : string
    }
  [@@deriving sexp_of, equal]
end

type t =
  { comments : Review_comment.t list
  ; approvals : Approval.t list
  }
[@@deriving sexp_of, equal]

let empty = { comments = []; approvals = [] }

let of_json j =
  let open Or_error.Let_syntax in
  let%bind comments = Json.list_field j "comments" ~f:Review_comment.of_json in
  let%map approvals =
    Json.optional_list_field j "approvals" ~f:(fun a ->
      let%bind root = Json.string_opt_field a "root" in
      let%bind path = Json.string_field a "path" in
      let%map hash = Json.string_field a "hash" in
      { Approval.root; path; hash })
  in
  { comments; approvals }
;;

let scope t ~root ~own =
  let mine (a : Approval.t) = Option.exists a.root ~f:(String.equal root) in
  { comments =
      List.filter t.comments ~f:(fun c ->
        String.equal c.Review_comment.root root)
  ; approvals =
      List.filter t.approvals ~f:mine
      @
      if own
      then List.filter t.approvals ~f:(fun a -> Option.is_none a.root)
      else []
  }
;;

let approval t path =
  List.find t.approvals ~f:(fun (a : Approval.t) -> String.equal a.path path)
;;

let on t path =
  List.filter t.comments ~f:(fun c -> String.equal c.Review_comment.path path)
;;

let unsent t = List.count t.comments ~f:Review_comment.unsent
