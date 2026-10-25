open! Core

module Content = struct
  type t =
    | Text of
        { text : string
        ; truncated : bool
        ; md5 : string option
        }
    | Image of
        { mime : string
        ; url : string option
        }
    | Binary
  [@@deriving sexp_of, equal]
end

type t =
  { path : string
  ; name : string
  ; size : int
  ; mtime : Time_ns.t option
  ; content : Content.t
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind path = Json.string_field j "path" in
  let%bind name = Json.string_field j "name" in
  let%bind size = Json.int_field j "size" in
  let%bind mtime = Json.time_ms_opt_field j "mtime" in
  let%map content =
    match%bind Json.string_field j "kind" with
    | "text" ->
      let%bind text = Json.string_field j "text" in
      let%bind truncated =
        Json.bool_default_field j "truncated" ~default:false
      in
      let%map md5 = Json.string_opt_field j "md5" in
      Content.Text { text; truncated; md5 }
    | "image" ->
      let%bind mime = Json.string_field j "mime" in
      let%map url = Json.string_opt_field j "url" in
      Content.Image { mime; url }
    | _ -> return Content.Binary
  in
  { path; name; size; mtime; content }
;;
