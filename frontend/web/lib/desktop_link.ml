open! Core
open! Import

let key_of_hash hash =
  let hash = String.chop_prefix_if_exists hash ~prefix:"#" in
  List.find_map (String.split hash ~on:'&') ~f:(fun pair ->
    match String.lsplit2 pair ~on:'=' with
    | Some ("desktop", key) when not (String.is_empty key) -> Some key
    | _ -> None)
;;

(* [%XX] escapes; a malformed one is kept as it is. *)
let pct_decode s =
  let b = Buffer.create (String.length s) in
  let n = String.length s in
  let rec go i =
    if i < n
    then (
      match s.[i] with
      | '%' when i + 2 < n -> go_escape i
      | c ->
        Buffer.add_char b c;
        go (i + 1))
  and go_escape i =
    match Int.of_string_opt ("0x" ^ String.sub s ~pos:(i + 1) ~len:2) with
    | Some code ->
      Buffer.add_char b (Char.of_int_exn code);
      go (i + 3)
    | None ->
      Buffer.add_char b '%';
      go (i + 1)
  in
  go 0;
  Buffer.contents b
;;

module Hint = struct
  type t = { host : string option } [@@deriving sexp_of]

  let of_hash hash =
    let pairs =
      List.filter_map
        (String.split (String.chop_prefix_if_exists hash ~prefix:"#") ~on:'&')
        ~f:(fun pair ->
          Option.map (String.lsplit2 pair ~on:'=') ~f:(fun (k, v) ->
            k, pct_decode v))
    in
    match List.Assoc.find pairs ~equal:String.equal "app" with
    | Some "desktop" ->
      Some
        { host =
            List.Assoc.find pairs ~equal:String.equal "host"
            |> Option.filter ~f:(Fn.non String.is_empty)
        }
    | _ -> None
  ;;
end

let storage_key = "prigh.desktop_key"
let hint_storage_key = "prigh.desktop_app"

let browser_shell_of_hash hash =
  List.exists
    (String.split (String.chop_prefix_if_exists hash ~prefix:"#") ~on:'&')
    ~f:(String.equal "shell=browser")
;;

let shell_storage_key = "prigh.desktop_shell"

let follow ~browser url =
  if not browser
  then url
  else (
    match String.lsplit2 url ~on:'#' with
    | Some (base, "") -> base ^ "#shell=browser"
    | Some (base, fragment) -> base ^ "#" ^ fragment ^ "&shell=browser"
    | None -> url ^ "#shell=browser")
;;

let quit_url = "about:blank#prigh-desktop-quit"
let status_path = "/desktop/status"
let connect_path = "/desktop/connect"
let quit_path = "/desktop/quit"
let status_body ~key = Json.to_string (Json.obj [ "key", Json.str key ])
let quit_body = status_body

let connect_body ~key ~address ~user ~token ~plaintext =
  let opt = Option.value_map ~default:`Null ~f:Json.str in
  Json.to_string
    (Json.obj
       ([ "key", Json.str key
        ; "address", Json.str address
        ; "user", opt user
        ; "token", opt token
        ]
        @ if plaintext then [ "plaintext", `True ] else []))
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
    | Remote of string
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
      | Some "remote" ->
        Option.map (string json "url") ~f:(fun url -> Remote url)
      | _ -> None)
  ;;
end

module Connect_reply = struct
  type t =
    | Connected of string option
    | Refused of string
    | Plaintext of string
    | Remote of string
  [@@deriving sexp_of]

  let of_json text =
    Option.bind (parse text) ~f:(fun json ->
      match Json.field json "ok" with
      | Some `True ->
        (match string json "remote" with
         | Some url -> Some (Remote url)
         | None -> Some (Connected (string json "code")))
      | Some `False ->
        let error =
          Option.value
            (string json "error")
            ~default:"The backend did not accept the connection."
        in
        (match Json.field json "plaintext" with
         | Some `True -> Some (Plaintext error)
         | _ -> Some (Refused error))
      | _ -> None)
  ;;
end
