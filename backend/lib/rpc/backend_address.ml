open! Core
open! Import

type t =
  { tls : bool
  ; host : string
  ; port : int
  ; prefix : string
  ; lines : bool
  }
[@@deriving sexp_of, equal]

let expected =
  "HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a \
   prigh-web URL (e.g. wss://server/ws or https://server/prigh/)"
;;

let unbracket host =
  String.chop_prefix host ~prefix:"["
  |> Option.bind ~f:(String.chop_suffix ~suffix:"]")
  |> Option.value ~default:host
;;

let bracket host = if String.mem host ':' then "[" ^ host ^ "]" else host

let parse_url text =
  let uri = Uri.of_string text in
  let scheme =
    match Option.map (Uri.scheme uri) ~f:String.lowercase with
    | Some ("ws" | "http") -> Ok false
    | Some ("wss" | "https") -> Ok true
    | Some _ | None ->
      Or_error.errorf
        "%S: the scheme must be ws, wss, http or https; expected %s"
        text
        expected
  in
  Or_error.bind scheme ~f:(fun tls ->
    match Uri.host uri with
    | None | Some "" ->
      Or_error.errorf "%S has no host; expected %s" text expected
    | Some host ->
      let path =
        String.rstrip (Uri.path uri) ~drop:(Char.equal '/')
        |> String.chop_suffix_if_exists ~suffix:"/ws"
        |> String.rstrip ~drop:(Char.equal '/')
      in
      Ok
        { tls
        ; host = unbracket host
        ; port = Option.value (Uri.port uri) ~default:(if tls then 443 else 80)
        ; prefix = path
        ; lines = false
        })
;;

let parse text =
  let text = String.strip text in
  if String.is_substring text ~substring:"://"
  then parse_url text
  else (
    match String.rsplit2 text ~on:':' with
    | Some (host, port)
      when (not (String.exists host ~f:Char.is_whitespace))
           && not (String.mem host '/') ->
      (match Int.of_string_opt port with
       | Some port when port > 0 && port < 65536 ->
         Ok
           { tls = false
           ; host =
               (if String.is_empty host then "127.0.0.1" else unbracket host)
           ; port
           ; prefix = ""
           ; lines = true
           }
       | _ -> Or_error.errorf "%S: bad port; expected %s" text expected)
    | _ -> Or_error.errorf "%S is not %s" text expected)
;;

let authority t =
  let default = if t.tls then 443 else 80 in
  if (not t.lines) && t.port = default
  then bracket t.host
  else sprintf "%s:%d" (bracket t.host) t.port
;;

let to_string t =
  if t.lines
  then authority t
  else
    sprintf
      "%s://%s%s/ws"
      (if t.tls then "wss" else "ws")
      (authority t)
      t.prefix
;;

let normalize text =
  match parse text with
  | Ok t when not t.lines -> to_string t
  | Ok _ | Error _ -> String.strip text
;;

let connect ~env ~sw t =
  let net = Eio.Stdenv.net env in
  match
    Eio.Net.getaddrinfo_stream net t.host ~service:(Int.to_string t.port)
  with
  | [] -> failwithf "cannot resolve %s" t.host ()
  | addr :: _ ->
    let socket = Eio.Net.connect ~sw net addr in
    Web_server.keepalive socket;
    if t.tls
    then
      (Http_client.tls_client ~host:t.host socket
        :> Eio.Flow.two_way_ty Eio.Resource.t)
    else (socket :> Eio.Flow.two_way_ty Eio.Resource.t)
;;

exception Not_upgraded of string

let websocket ~env ~sw t ~path ~query =
  let flow = connect ~env ~sw t in
  let reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) in
  let target =
    Uri.to_string
      (Uri.make
         ~path:(t.prefix ^ path)
         ~query:(List.map query ~f:(fun (k, v) -> k, [ v ]))
         ())
  in
  match Websocket.connect ~reader ~flow ~host:(authority t) ~target with
  | Ok ws -> ws
  | Error answer ->
    raise
      (Not_upgraded
         (sprintf
            "%s did not open the WebSocket at %s: it answered %s"
            (authority t)
            (t.prefix ^ path)
            answer))
;;
