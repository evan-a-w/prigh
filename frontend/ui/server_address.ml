open! Core

type t =
  { tls : bool
  ; host : string
  ; port : int
  ; prefix : string
  ; lines : bool
  }
[@@deriving sexp_of, equal]

let expected =
  "HOST:PORT (prigh serve -listen, e.g. 127.0.0.1:7777) or a prigh-web URL \
   (e.g. wss://server/ws or https://server/prigh/)"
;;

let unbracket host =
  String.chop_prefix host ~prefix:"["
  |> Option.bind ~f:(String.chop_suffix ~suffix:"]")
  |> Option.value ~default:host
;;

let bracket host = if String.mem host ':' then "[" ^ host ^ "]" else host

let parse_port text port =
  match Int.of_string_opt port with
  | Some port when port > 0 && port < 65536 -> Ok port
  | _ -> Error (sprintf "%S: bad port; expected %s" text expected)
;;

(* [host], [host:port], [[v6]] or [[v6]:port], after any [user@]. *)
let split_authority text authority =
  let authority =
    match String.rsplit2 authority ~on:'@' with
    | Some (_, rest) -> rest
    | None -> authority
  in
  let host, port =
    if String.is_prefix authority ~prefix:"["
    then (
      match String.lsplit2 authority ~on:']' with
      | Some (v6, "") -> v6 ^ "]", None
      | Some (v6, rest) -> v6 ^ "]", String.chop_prefix rest ~prefix:":"
      | None -> authority, None)
    else (
      match String.rsplit2 authority ~on:':' with
      | Some (host, port) -> host, Some port
      | None -> authority, None)
  in
  let host = unbracket host in
  if String.is_empty host
  then Error (sprintf "%S has no host; expected %s" text expected)
  else (
    match port with
    | None -> Ok (host, None)
    | Some port ->
      Result.map (parse_port text port) ~f:(fun port -> host, Some port))
;;

let parse_url text ~scheme ~rest =
  let tls =
    match String.lowercase scheme with
    | "ws" | "http" -> Ok false
    | "wss" | "https" -> Ok true
    | _ ->
      Error
        (sprintf
           "%S: the scheme must be ws, wss, http or https; expected %s"
           text
           expected)
  in
  Result.bind tls ~f:(fun tls ->
    let rest =
      String.take_while rest ~f:(fun c ->
        not (Char.equal c '?' || Char.equal c '#'))
    in
    let authority, path =
      match String.lsplit2 rest ~on:'/' with
      | Some (authority, path) -> authority, "/" ^ path
      | None -> rest, ""
    in
    Result.map (split_authority text authority) ~f:(fun (host, port) ->
      let prefix =
        String.rstrip path ~drop:(Char.equal '/')
        |> String.chop_suffix_if_exists ~suffix:"/ws"
        |> String.rstrip ~drop:(Char.equal '/')
      in
      { tls
      ; host
      ; port = Option.value port ~default:(if tls then 443 else 80)
      ; prefix
      ; lines = false
      }))
;;

let parse text =
  let text = String.strip text in
  match String.substr_index text ~pattern:"://" with
  | Some i ->
    parse_url
      text
      ~scheme:(String.prefix text i)
      ~rest:(String.drop_prefix text (i + 3))
  | None ->
    (match String.rsplit2 text ~on:':' with
     | Some (host, port)
       when (not (String.exists host ~f:Char.is_whitespace))
            && not (String.mem host '/') ->
       Result.map (parse_port text port) ~f:(fun port ->
         { tls = false
         ; host = (if String.is_empty host then "127.0.0.1" else unbracket host)
         ; port
         ; prefix = ""
         ; lines = true
         })
     | _ -> Error (sprintf "%S is not %s" text expected))
;;

let authority t =
  let default = if t.tls then 443 else 80 in
  if (not t.lines) && t.port = default
  then bracket t.host
  else sprintf "%s:%d" (bracket t.host) t.port
;;

let scheme t = if t.tls then "wss" else "ws"
let websocket_path t = t.prefix ^ "/ws"

let to_string t =
  if t.lines
  then authority t
  else sprintf "%s://%s%s" (scheme t) (authority t) (websocket_path t)
;;

let display t =
  if t.lines
  then authority t
  else sprintf "%s://%s%s" (scheme t) (authority t) t.prefix
;;

let normalize text =
  match parse text with
  | Ok t when not t.lines -> to_string t
  | Ok _ | Error _ -> String.strip text
;;

let display_string text =
  match parse text with
  | Ok t -> display t
  | Error _ -> text
;;
