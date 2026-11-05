open! Core
open! Import

let key_of_hash hash =
  let hash = String.chop_prefix_if_exists hash ~prefix:"#" in
  List.find_map (String.split hash ~on:'&') ~f:(fun pair ->
    match String.lsplit2 pair ~on:'=' with
    | Some ("desktop", key) when not (String.is_empty key) -> Some key
    | _ -> None)
;;

let storage_key = "prigh.desktop_key"
let status_path = "/desktop/status"
let connect_path = "/desktop/connect"
let quit_path = "/desktop/quit"
let status_body ~key = Json.to_string (Json.obj [ "key", Json.str key ])
let quit_body = status_body

let connect_body ~key ~address ~user ~token =
  let opt = Option.value_map ~default:`Null ~f:Json.str in
  Json.to_string
    (Json.obj
       [ "key", Json.str key
       ; "address", Json.str address
       ; "user", opt user
       ; "token", opt token
       ])
;;

let parse text =
  match Json.parse text with
  | Ok (`Object _ as json) -> Some json
  | Ok _ | Error _ -> None
;;

let string json name =
  match Json.field json name with
  | Some (`String s) when not (String.is_empty s) -> Some s
  | _ -> None
;;

module Status = struct
  type t =
    | Connect of
        { address : string option
        ; user : string option
        ; error : string option
        }
    | Connected of { code : string option }
  [@@deriving sexp_of]

  let of_json text =
    Option.bind (parse text) ~f:(fun json ->
      match string json "state" with
      | Some "connect" ->
        Some
          (Connect
             { address = string json "address"
             ; user = string json "user"
             ; error = string json "error"
             })
      | Some "connected" -> Some (Connected { code = string json "code" })
      | _ -> None)
  ;;
end

module Connect_reply = struct
  type t = (string option, string) Result.t [@@deriving sexp_of]

  let of_json text =
    Option.bind (parse text) ~f:(fun json ->
      match Json.field json "ok" with
      | Some `True -> Some (Ok (string json "code"))
      | Some `False ->
        Some
          (Error
             (Option.value
                (string json "error")
                ~default:"The backend did not accept the connection."))
      | _ -> None)
  ;;
end
