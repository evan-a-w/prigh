open! Core
open! Import

type t =
  { instance : Forge_instance.t
  ; owner : string
  ; name : string
  ; remote : string
  }
[@@deriving sexp_of]

let web_url t = sprintf "%s/%s/%s" t.instance.web_url t.owner t.name

let api_url t =
  sprintf
    "%s/repos/%s/%s"
    (Forge_instance.api_url t.instance)
    (Uri.pct_encode t.owner)
    (Uri.pct_encode t.name)
;;

let to_json t =
  `Object
    [ "host", `String t.instance.host
    ; "kind", `String (Forge_kind.to_string t.instance.kind)
    ; "owner", `String t.owner
    ; "name", `String t.name
    ; "remote", `String t.remote
    ; "url", `String (web_url t)
    ]
;;
