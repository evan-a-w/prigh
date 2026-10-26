open! Core

module Approval = struct
  type t =
    { root : string option
    ; path : string
    ; hash : string
    }
  [@@deriving sexp_of, equal]
end

module Snapshot = struct
  module Content = struct
    type t =
      | Text of { md5 : string }
      | Absent
      | Binary of { md5 : string }
      | Too_large of { bytes : int }
    [@@deriving sexp_of, equal]

    let of_json j =
      let open Or_error.Let_syntax in
      match%bind Json.string_field j "kind" with
      | "text" ->
        let%map md5 = Json.string_field j "md5" in
        Text { md5 }
      | "absent" -> Ok Absent
      | "binary" ->
        let%map md5 = Json.string_field j "md5" in
        Binary { md5 }
      | "too_large" ->
        let%map bytes = Json.int_field j "bytes" in
        Too_large { bytes }
      | kind -> Or_error.errorf "unknown snapshot kind %S" kind
    ;;
  end

  type t =
    { root : string
    ; path : string
    ; action : string
    ; by : string option
    ; at : Time_ns.t option
    ; content : Content.t
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind root = Json.string_field j "root" in
    let%bind path = Json.string_field j "path" in
    let%bind action = Json.string_field j "action" in
    let%bind by = Json.string_opt_field j "by" in
    let%bind at = Json.time_ms_opt_field j "at_ms" in
    let%map content =
      Or_error.bind (Json.object_field j "content") ~f:Content.of_json
    in
    { root; path; action; by; at; content }
  ;;
end

type t =
  { comments : Review_comment.t list
  ; approvals : Approval.t list
  ; snapshots : Snapshot.t list
  }
[@@deriving sexp_of, equal]

let empty = { comments = []; approvals = []; snapshots = [] }

let of_json j =
  let open Or_error.Let_syntax in
  let%bind comments = Json.list_field j "comments" ~f:Review_comment.of_json in
  let%bind approvals =
    Json.optional_list_field j "approvals" ~f:(fun a ->
      let%bind root = Json.string_opt_field a "root" in
      let%bind path = Json.string_field a "path" in
      let%map hash = Json.string_field a "hash" in
      { Approval.root; path; hash })
  in
  let%map snapshots =
    Json.optional_list_field j "snapshots" ~f:Snapshot.of_json
  in
  { comments; approvals; snapshots }
;;

let scope t ~root ~own =
  let mine (a : Approval.t) = Option.exists a.root ~f:(String.equal root) in
  { comments =
      List.filter t.comments ~f:(fun c ->
        String.equal c.Review_comment.root root)
  ; approvals =
      (List.filter t.approvals ~f:mine
       @
       if own
       then List.filter t.approvals ~f:(fun a -> Option.is_none a.root)
       else [])
  ; snapshots =
      List.filter t.snapshots ~f:(fun s -> String.equal s.Snapshot.root root)
  }
;;

let approval t path =
  List.find t.approvals ~f:(fun (a : Approval.t) -> String.equal a.path path)
;;

let on t path =
  List.filter t.comments ~f:(fun c -> String.equal c.Review_comment.path path)
;;

let unsent t = List.count t.comments ~f:Review_comment.unsent

let history t path =
  List.filter t.snapshots ~f:(fun s -> String.equal s.Snapshot.path path)
;;
