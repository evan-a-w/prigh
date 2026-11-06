open! Core
open! Prigh

let session = "s3cret-session"
let cookie = "proxy_session=" ^ session

let read_head reader =
  let line = Eio.Buf_read.line reader in
  let rec headers acc =
    match Eio.Buf_read.line reader with
    | "" -> List.rev acc
    | line ->
      (match String.lsplit2 line ~on:':' with
       | Some (name, value) ->
         headers ((String.strip name, String.strip value) :: acc)
       | None -> headers acc)
  in
  line, headers []
;;

let signed_in headers =
  List.exists headers ~f:(fun (name, value) ->
    String.Caseless.equal name "Cookie"
    && List.exists (String.split value ~on:';') ~f:(fun c ->
      String.equal (String.strip c) cookie))
;;

let reply flow ~status ~headers body =
  Eio.Flow.copy_string
    (sprintf
       "HTTP/1.1 %s\r\n%sContent-Length: %d\r\nConnection: close\r\n\r\n%s"
       status
       (String.concat
          (List.map headers ~f:(fun (name, value) -> sprintf "%s: %s\r\n" name value)))
       (String.length body)
       body)
    flow
;;

let forward ~env ~upstream ~reader ~line ~headers flow =
  Eio.Switch.run
  @@ fun sw ->
  let up =
    Eio.Net.connect
      ~sw
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, upstream))
  in
  Eio.Flow.copy_string
    (sprintf
       "%s\r\n%s\r\n"
       line
       (String.concat
          (List.map headers ~f:(fun (name, value) -> sprintf "%s: %s\r\n" name value))))
    up;
  let copy src dst =
    try Eio.Flow.copy src dst with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ -> ()
  in
  Eio.Fiber.first
    (fun () -> copy (Eio.Buf_read.as_flow reader) up)
    (fun () -> copy up flow)
;;

let serve ~env ~upstream flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) in
  match read_head reader with
  | exception (End_of_file | Eio.Io _) -> ()
  | line, headers ->
    let path =
      match String.split line ~on:' ' with
      | [ _; target; _ ] -> target
      | _ -> "/"
    in
    if String.equal path "/login"
    then
      reply
        flow
        ~status:"302 Found"
        ~headers:
          [ "Location", "/"
          ; "Set-Cookie", cookie ^ "; Path=/; HttpOnly; SameSite=Lax"
          ]
        ""
    else if signed_in headers
    then forward ~env ~upstream ~reader ~line ~headers flow
    else
      reply
        flow
        ~status:"302 Found"
        ~headers:
          [ "Location", "https://login.example.com/outpost.goauthentik.io/start"
          ; "Content-Type", "text/html"
          ]
        "<html>sign in</html>"
;;

let start ~env ~sw ~upstream =
  let socket =
    Eio.Net.listen
      ~sw
      ~backlog:16
      ~reuse_addr:true
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  Eio.Fiber.fork_daemon ~sw (fun () ->
    let rec loop () =
      Eio.Net.accept_fork
        ~sw
        socket
        ~on_error:(fun _ -> ())
        (fun flow _ -> serve ~env ~upstream flow);
      loop ()
    in
    loop ());
  match Eio.Net.listening_addr socket with
  | `Tcp (_, port) -> port
  | `Unix _ -> assert false
;;

let request ~env ~port ?(headers = []) ?(body = "") ~meth target =
  Eio.Switch.run
  @@ fun sw ->
  let flow =
    Eio.Net.connect
      ~sw
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Eio.Flow.copy_string
    (sprintf
       "%s %s HTTP/1.1\r\nHost: 127.0.0.1:%d\r\nConnection: close\r\n%sContent-Length: %d\r\n\r\n%s"
       meth
       target
       port
       (String.concat
          (List.map headers ~f:(fun (name, value) -> sprintf "%s: %s\r\n" name value)))
       (String.length body)
       body)
    flow;
  Eio.Buf_read.(parse_exn take_all) flow ~max_size:(1024 * 1024)
;;
