open! Core
open! Import

module Oauth = struct
  type t =
    { access : string
    ; refresh : string
    ; expires_ms : int
    ; account_id : string option
    }
  [@@deriving sexp, equal]
end

type t =
  | Api_key of string
  | Oauth of Oauth.t
[@@deriving sexp, equal]

let kind = function
  | Api_key _ -> "api_key"
  | Oauth _ -> "oauth"
;;

let to_json = function
  | Api_key key -> `Object [ "type", `String "api_key"; "key", `String key ]
  | Oauth { access; refresh; expires_ms; account_id } ->
    `Object
      (List.concat
         [ [ "type", `String "oauth"
           ; "access", `String access
           ; "refresh", `String refresh
           ; "expires", `Number (Int.to_string expires_ms)
           ]
         ; Option.value_map account_id ~default:[] ~f:(fun id ->
             [ "accountId", `String id ])
         ])
;;

let string_member name json =
  match Json.member name json with
  | Some (`String s) -> Ok s
  | _ -> Or_error.errorf "credential: missing string %S" name
;;

let of_json (json : Json.t) =
  match json with
  | `String key -> Ok (Api_key key)
  | `Object _ ->
    let open Or_error.Let_syntax in
    (match%bind string_member "type" json with
     | "api_key" ->
       let%map key = string_member "key" json in
       Api_key key
     | "oauth" ->
       let%bind access = string_member "access" json in
       let%bind refresh = string_member "refresh" json in
       let%map expires_ms =
         match Option.bind (Json.member "expires" json) ~f:Json.int with
         | Some ms -> Ok ms
         | None ->
           (match Json.member "expires" json with
            | Some (`Number s) ->
              (try Ok (Float.to_int (Float.of_string s)) with
               | _ -> Or_error.error_string "credential: bad expires")
            | _ -> Or_error.error_string "credential: missing expires")
       in
       let account_id = Result.ok (string_member "accountId" json) in
       Oauth { access; refresh; expires_ms; account_id }
     | other -> Or_error.errorf "credential: unknown type %S" other)
  | _ -> Or_error.error_string "credential must be an object"
;;

let now_ms () = Time_ns.to_int_ns_since_epoch (Time_ns.now ()) / 1_000_000

(* Refresh ahead of expiry so that in-flight requests never hit a dead token. *)
let refresh_margin_ms = 5 * 60 * 1000

let oauth_needs_refresh ?(now_ms = now_ms ()) (o : Oauth.t) =
  now_ms + refresh_margin_ms >= o.expires_ms
;;
