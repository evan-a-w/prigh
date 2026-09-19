open! Core
open! Import

module Result_ = struct
  type t =
    { code : string
    ; state : string
    }
  [@@deriving sexp_of]
end

type t =
  { port : int
  ; result : Result_.t Promise.t
  }

let port t = t.port
let wait t = Promise.await t.result

let escape_html s =
  String.concat_map s ~f:(function
    | '&' -> "&amp;"
    | '<' -> "&lt;"
    | '>' -> "&gt;"
    | '"' -> "&quot;"
    | '\'' -> "&#39;"
    | c -> String.of_char c)
;;

let page ~heading ~message =
  sprintf
    {|<!doctype html>
<html lang="en"><head><meta charset="utf-8"><title>%s</title>
<style>body{margin:0;min-height:100vh;display:flex;align-items:center;justify-content:center;background:#09090b;color:#fafafa;font-family:system-ui,sans-serif;text-align:center}p{color:#a1a1aa}</style>
</head><body><main><h1>%s</h1><p>%s</p></main></body></html>
|}
    (escape_html heading)
    (escape_html heading)
    (escape_html message)
;;

let respond flow ~status ~body =
  let head =
    sprintf
      "HTTP/1.1 %d %s\r\n\
       Content-Type: text/html; charset=utf-8\r\n\
       Content-Length: %d\r\n\
       Connection: close\r\n\
       \r\n"
      status
      (if status = 200 then "OK" else "Bad Request")
      (String.length body)
  in
  try Eio.Flow.copy_string (head ^ body) flow with
  | Eio.Io _ -> ()
;;

let read_request_target flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024) in
  match String.split (Eio.Buf_read.line reader) ~on:' ' with
  | _meth :: target :: _ -> Some target
  | _ -> None
;;

module Outcome = struct
  type t =
    | Success of Result_.t
    | Failure of
        { status : int
        ; message : string
        }
end

let classify ~path ~expected_state target : Outcome.t =
  let uri = Uri.of_string target in
  let query name = Uri.get_query_param uri name in
  if not (String.equal (Uri.path uri) path)
  then Failure { status = 404; message = "Callback route not found." }
  else (
    match query "error", query "code", query "state" with
    | Some error, _, _ ->
      Failure
        { status = 400
        ; message = "Authentication did not complete. Error: " ^ error
        }
    | None, (None | Some ""), _ | None, _, (None | Some "") ->
      Failure { status = 400; message = "Missing code or state parameter." }
    | None, Some code, Some state ->
      if String.equal state expected_state
      then Success { code; state }
      else Failure { status = 400; message = "State mismatch." })
;;

let handle ~path ~expected_state ~resolve flow =
  match read_request_target flow with
  | None ->
    respond
      flow
      ~status:400
      ~body:
        (page ~heading:"Authentication failed" ~message:"Malformed request.")
  | Some target ->
    (match classify ~path ~expected_state target with
     | Failure { status; message } ->
       respond
         flow
         ~status
         ~body:(page ~heading:"Authentication failed" ~message)
     | Success result ->
       respond
         flow
         ~status:200
         ~body:
           (page
              ~heading:"Authentication successful"
              ~message:"You can close this window and return to prigh.");
       resolve result)
;;

let start
      ~sw
      ~(env : Env.t)
      ?(host = "127.0.0.1")
      ~port
      ~path
      ~expected_state
      ()
  =
  let net = Eio.Stdenv.net env in
  match
    let addr =
      Eio_unix.Net.sockaddr_of_unix_stream
        (Core_unix.ADDR_INET (Core_unix.Inet_addr.of_string host, port))
    in
    Eio.Net.listen ~sw ~reuse_addr:true ~backlog:8 net addr
  with
  | exception exn ->
    Or_error.error_s
      [%message
        "cannot listen for the OAuth callback"
          (host : string)
          (port : int)
          ~error:(Exn.to_string exn : string)]
  | socket ->
    let port =
      match Eio.Net.listening_addr socket with
      | `Tcp (_, port) -> port
      | `Unix _ -> port
    in
    let result, resolver = Promise.create () in
    let resolve r =
      if not (Promise.is_resolved result) then Promise.resolve resolver r
    in
    Fiber.fork_daemon ~sw (fun () ->
      while true do
        Eio.Net.accept_fork
          ~sw
          socket
          ~on_error:(fun _ -> ())
          (fun flow _addr -> handle ~path ~expected_state ~resolve flow)
      done;
      `Stop_daemon);
    Ok { port; result }
;;

module For_testing = struct
  module Outcome = Outcome

  let classify = classify
end
