open! Core
open! Import

module Method = struct
  type t =
    | Oauth
    | Token
  [@@deriving sexp, equal]

  let to_string = function
    | Oauth -> "oauth"
    | Token -> "token"
  ;;

  let of_string = function
    | "oauth" -> Ok Oauth
    | "token" -> Ok Token
    | other ->
      Or_error.errorf "unknown sign-in method %S (oauth or token)" other
  ;;
end

type t =
  { kind : Forge_kind.t
  ; method_ : Method.t
  ; token : string
  ; refresh : string option
  ; expires_ms : int option
  ; client_id : string option
  ; user : string option
  }
[@@deriving sexp, equal]

let key_prefix = "forge:"
let key ~host = key_prefix ^ host

let to_json t =
  let opt name = Option.map ~f:(fun v -> name, `String v) in
  `Object
    (List.filter_opt
       [ Some ("type", `String "forge")
       ; Some ("kind", `String (Forge_kind.to_string t.kind))
       ; Some ("method", `String (Method.to_string t.method_))
       ; Some ("token", `String t.token)
       ; opt "refresh" t.refresh
       ; Option.map t.expires_ms ~f:(fun ms ->
           "expires", `Number (Int.to_string ms))
       ; opt "client_id" t.client_id
       ; opt "user" t.user
       ])
;;

let of_json json =
  let str name =
    match Json.member name json with
    | Some (`String s) -> Ok s
    | _ -> Or_error.errorf "missing %S" name
  in
  let str_opt name =
    match Json.member name json with
    | Some (`String s) -> Some s
    | _ -> None
  in
  let open Or_error.Let_syntax in
  let%bind () =
    match str "type" with
    | Ok "forge" -> Ok ()
    | _ -> Or_error.error_string "not a git host credential"
  in
  let%bind kind = str "kind" >>= Forge_kind.of_string in
  let%bind method_ = str "method" >>= Method.of_string in
  let%map token = str "token" in
  { kind
  ; method_
  ; token
  ; refresh = str_opt "refresh"
  ; expires_ms = Option.bind (Json.member "expires" json) ~f:Json.int
  ; client_id = str_opt "client_id"
  ; user = str_opt "user"
  }
;;

let needs_refresh ?(now_ms = Credential.now_ms ()) t =
  match t.expires_ms with
  | None -> false
  | Some expires -> now_ms + Credential.refresh_margin_ms >= expires
;;

let authorization t =
  match t.kind, t.method_ with
  | Forgejo, Token -> "token " ^ t.token
  | Github, _ | Forgejo, Oauth -> "Bearer " ^ t.token
;;
