open! Core

module Kind = struct
  type t =
    | File
    | Dir
    | Link of [ `File | `Dir | `Missing ]
    | Other
  [@@deriving sexp_of, equal]

  let is_dir = function
    | Dir | Link `Dir -> true
    | File | Link (`File | `Missing) | Other -> false
  ;;

  let of_json j =
    let open Or_error.Let_syntax in
    match%bind Json.string_field j "kind" with
    | "file" -> return File
    | "dir" -> return Dir
    | "link" ->
      (match%map Json.string_opt_field j "target" with
       | Some "file" -> Link `File
       | Some "dir" -> Link `Dir
       | Some _ | None -> Link `Missing)
    | _ -> return Other
  ;;
end

module Entry = struct
  type t =
    { name : string
    ; kind : Kind.t
    ; size : int option
    ; mtime : Time_ns.t option
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind name = Json.string_field j "name" in
    let%bind kind = Kind.of_json j in
    let%bind size =
      match Json.field j "size" with
      | None -> return None
      | Some _ -> Json.int_field j "size" >>| Option.some
    in
    let%map mtime = Json.time_ms_opt_field j "mtime" in
    { name; kind; size; mtime }
  ;;
end

type t =
  { path : string
  ; parent : string option
  ; home : string
  ; entries : Entry.t list
  ; total : int
  ; hidden : int
  ; select : string option
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind path = Json.string_field j "path" in
  let%bind parent = Json.string_opt_field j "parent" in
  let%bind home = Json.string_field j "home" in
  let%bind entries = Json.list_field j "entries" ~f:Entry.of_json in
  let%bind total = Json.int_field j "total" in
  let%bind hidden = Json.int_field j "hidden" in
  let%map select = Json.string_opt_field j "select" in
  { path; parent; home; entries; total; hidden; select }
;;
