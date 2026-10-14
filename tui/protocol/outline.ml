open! Core

module Kind = struct
  type t =
    | User
    | Compaction
    | Error
  [@@deriving sexp_of, equal]
end

module Point = struct
  type t =
    { index : int
    ; kind : Kind.t
    ; text : string
    ; at : Time_ns.Alternate_sexp.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]

  (* [None]: a kind from a newer backend, left out. *)
  let of_json j =
    let open Or_error.Let_syntax in
    let%bind kind =
      match%map Json.string_field j "kind" with
      | "user" -> Some Kind.User
      | "compaction" -> Some Kind.Compaction
      | "error" -> Some Kind.Error
      | _ -> None
    in
    match kind with
    | None -> Ok None
    | Some kind ->
      let%bind index = Json.int_field j "index" in
      let%bind text = Json.string_field j "text" in
      let%map at = Json.time_ms_opt_field j "at" in
      Some { index; kind; text; at }
  ;;
end

type t = Point.t list [@@deriving sexp_of, equal]

let of_json j =
  Json.list_field j "points" ~f:Point.of_json |> Or_error.map ~f:List.filter_opt
;;
