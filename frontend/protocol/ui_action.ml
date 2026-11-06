open! Core

type t =
  { call_id : string
  ; component : string
  ; title : string
  ; values : Json.t
  }
[@@deriving sexp_of, equal]

let to_json t =
  Json.obj
    [ "call_id", Json.str t.call_id
    ; "component", Json.str t.component
    ; "title", Json.str t.title
    ; "values", t.values
    ]
;;

let reference ~call_id ~component =
  sprintf "render_ui call %s, component %s" call_id component
;;

let message_text t =
  sprintf
    "The user responded through \"%s\" (%s):\n%s"
    t.title
    (reference ~call_id:t.call_id ~component:t.component)
    (Json.to_string t.values)
;;

module Sent = struct
  type t =
    { call_id : string
    ; component : string
    ; values : Json.t option
    }
  [@@deriving sexp_of]

  let marker = "(render_ui call "

  let of_text text =
    let open Option.Let_syntax in
    let first, rest =
      match String.lsplit2 text ~on:'\n' with
      | Some (first, rest) -> first, rest
      | None -> text, ""
    in
    let%bind () =
      Option.some_if (String.is_prefix first ~prefix:"The user responded through ") ()
    in
    (* The title may hold anything: the reference is the last one. *)
    let%bind i = List.last (String.substr_index_all first ~may_overlap:false ~pattern:marker) in
    let inside = String.drop_prefix first (i + String.length marker) in
    let%bind inside = String.chop_suffix inside ~suffix:"):" in
    let%bind call_id, component = String.lsplit2 inside ~on:',' in
    let%map component =
      String.chop_prefix (String.lstrip component) ~prefix:"component "
    in
    { call_id = String.strip call_id
    ; component
    ; values = Result.ok (Json.parse (String.strip rest))
    }
  ;;
end
