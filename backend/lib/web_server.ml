open! Core
open! Import

module Request = struct
  type t =
    { meth : string
    ; path : string
    ; headers : (string * string) list
    }
  [@@deriving sexp_of]

  let header t name =
    List.Assoc.find t.headers ~equal:String.Caseless.equal name
  ;;

  let parse reader =
    match Eio.Buf_read.line reader with
    | exception (End_of_file | Eio.Io _) -> None
    | request_line ->
      (match String.split request_line ~on:' ' with
       | [ meth; target; _version ] ->
         let path =
           match String.lsplit2 target ~on:'?' with
           | Some (path, _query) -> path
           | None -> target
         in
         let rec headers acc =
           match Eio.Buf_read.line reader with
           | exception (End_of_file | Eio.Io _) -> List.rev acc
           | "" -> List.rev acc
           | line ->
             (match String.lsplit2 line ~on:':' with
              | Some (name, value) ->
                headers ((String.strip name, String.strip value) :: acc)
              | None -> headers acc)
         in
         Some { meth; path; headers = headers [] }
       | _ -> None)
  ;;
end

let content_type path =
  match
    String.lowercase
      (Filename.split_extension path |> snd |> Option.value ~default:"")
  with
  | "html" -> "text/html; charset=utf-8"
  | "js" -> "text/javascript; charset=utf-8"
  | "css" -> "text/css; charset=utf-8"
  | "json" -> "application/json"
  | "svg" -> "image/svg+xml"
  | "png" -> "image/png"
  | "ico" -> "image/x-icon"
  | "map" -> "application/json"
  | _ -> "application/octet-stream"
;;

let response ~status ~reason ?(headers = []) body =
  let headers =
    [ "Content-Length", Int.to_string (String.length body)
    ; "Connection", "close"
    ; "Cache-Control", "no-cache"
    ]
    @ headers
  in
  sprintf
    "HTTP/1.1 %d %s\r\n%s\r\n%s"
    status
    reason
    (String.concat
       (List.map headers ~f:(fun (k, v) -> sprintf "%s: %s\r\n" k v)))
    body
;;

(* Only plain relative components: no [..], no absolute paths, no hidden
   files. *)
let safe_relative path =
  let parts =
    List.filter (String.split path ~on:'/') ~f:(Fn.non String.is_empty)
  in
  if
    List.exists parts ~f:(fun part ->
      String.equal part ".." || String.is_prefix part ~prefix:".")
  then None
  else Some (String.concat parts ~sep:"/")
;;

let static ~root (request : Request.t) =
  if not (String.equal request.meth "GET" || String.equal request.meth "HEAD")
  then response ~status:405 ~reason:"Method Not Allowed" "method not allowed\n"
  else (
    let path =
      if String.equal request.path "/" then "/index.html" else request.path
    in
    match safe_relative path with
    | None -> response ~status:404 ~reason:"Not Found" "not found\n"
    | Some rel ->
      let file = Filename.concat root rel in
      (match Sys_unix.is_file file with
       | `Yes ->
         let body = In_channel.read_all file in
         response
           ~status:200
           ~reason:"OK"
           ~headers:[ "Content-Type", content_type file ]
           (if String.equal request.meth "HEAD" then "" else body)
       | `No | `Unknown ->
         response ~status:404 ~reason:"Not Found" "not found\n"))
;;

let ws_path = "/ws"

let percent_encode s =
  let unreserved = function
    | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '.' | '_' | '~' -> true
    | _ -> false
  in
  String.concat_map s ~f:(fun c ->
    if unreserved c then String.of_char c else sprintf "%%%02X" (Char.to_int c))
;;

let browser_url ~host ~port ~token =
  let host =
    if
      String.is_substring host ~substring:":"
      && not (String.is_prefix host ~prefix:"[")
    then "[" ^ host ^ "]"
    else host
  in
  let base = sprintf "http://%s:%d/" host port in
  Option.filter token ~f:(Fn.non String.is_empty)
  |> Option.value_map ~default:base ~f:(fun token ->
    base ^ "?token=" ^ percent_encode token)
;;

let wants_upgrade (request : Request.t) =
  String.equal request.path ws_path
  && Option.value_map
       (Request.header request "Upgrade")
       ~default:false
       ~f:(fun v -> String.Caseless.equal (String.strip v) "websocket")
;;

type on_lines =
  read_line:(unit -> string option) -> write_line:(string -> unit) -> unit

let serve_json_lines ~(on_lines : on_lines) ~reader flow =
  on_lines
    ~read_line:(fun () ->
      match Eio.Buf_read.line reader with
      | exception (End_of_file | Eio.Io _) -> None
      | line -> Some line)
    ~write_line:(fun line -> Eio.Flow.copy_string (line ^ "\n") flow)
;;

(* A terminal frontend (`-connect`) starts with a JSON request; a browser
   starts with an HTTP request line. *)
let is_json_lines reader =
  match Eio.Buf_read.peek_char reader with
  | Some '{' -> true
  | Some _ | None -> false
  | exception (End_of_file | Eio.Io _) -> false
;;

let handle ~root ~on_websocket ~on_lines flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) in
  if is_json_lines reader
  then serve_json_lines ~on_lines ~reader flow
  else (
    match Request.parse reader with
    | None -> ()
    | Some request when wants_upgrade request ->
      (match Request.header request "Sec-WebSocket-Key" with
       | None ->
         Eio.Flow.copy_string
           (response
              ~status:400
              ~reason:"Bad Request"
              "missing Sec-WebSocket-Key\n")
           flow
       | Some key ->
         Eio.Flow.copy_string
           (sprintf
              "HTTP/1.1 101 Switching Protocols\r\n\
               Upgrade: websocket\r\n\
               Connection: Upgrade\r\n\
               Sec-WebSocket-Accept: %s\r\n\
               \r\n"
              (Websocket.accept_key (String.strip key)))
           flow;
         let ws = Websocket.create ~reader ~flow () in
         on_websocket ws;
         Websocket.close ws)
    | Some request ->
      (match root with
       | None ->
         Eio.Flow.copy_string
           (response
              ~status:404
              ~reason:"Not Found"
              "no web root configured; only /ws is served\n")
           flow
       | Some root -> Eio.Flow.copy_string (static ~root request) flow))
;;

let serve_rpc server ws =
  Rpc_server.serve_lines
    server
    ~read_line:(fun () -> Websocket.read_text ws)
    ~write_line:(Websocket.send_text ws)
;;

let listen ~env ~sw ~addr ~port ~root ~on_websocket ~on_lines =
  let socket =
    Eio.Net.listen
      ~sw
      ~backlog:16
      ~reuse_addr:true
      (Eio.Stdenv.net env)
      (`Tcp (addr, port))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> port
  in
  Fiber.fork_daemon ~sw (fun () ->
    while true do
      Eio.Net.accept_fork
        ~sw
        socket
        ~on_error:(fun exn ->
          eprintf "prigh: web connection failed: %s\n%!" (Exn.to_string exn))
        (fun flow _addr -> handle ~root ~on_websocket ~on_lines flow)
    done);
  port
;;

module For_testing = struct
  let safe_relative = safe_relative
  let content_type = content_type
end
