open! Core
open! Import

let reason = function
  | Failure message | Backend_address.Not_upgraded message -> message
  | exn -> Net_error.to_string_hum exn
;;

let unreachable ~log remote exn =
  let message =
    sprintf
      "cannot reach %s: %s"
      (Backend_address.to_string remote)
      (reason exn)
  in
  log message;
  message
;;

let rpc ~env ~log remote ~query:_ ws =
  Switch.run
  @@ fun sw ->
  match Backend_socket.connect ~env ~sw remote with
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
    ignore (unreachable ~log remote exn : string);
    Websocket.close ~code:1011 ws
  | socket ->
    let rec up () =
      match Websocket.read_text ws with
      | None -> ()
      | Some line ->
        (match Backend_socket.write_line socket line with
         | () -> up ()
         | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
         | exception _ -> ())
    in
    let rec down () =
      match Backend_socket.read_line socket with
      | None -> ()
      | Some line ->
        Websocket.send_text ws line;
        if not (Websocket.is_closed ws) then down ()
    in
    Fiber.first up down;
    Backend_socket.close socket
;;

let terminal ~env ~log remote ~query ws =
  Switch.run
  @@ fun sw ->
  match Backend_address.websocket ~env ~sw remote ~path:"/terminal" ~query with
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
    let message = unreachable ~log remote exn in
    Websocket.send_text
      ws
      (Json.to_string
         (`Object [ "type", `String "error"; "message", `String message ]))
  | upstream ->
    let rec pump from into =
      match Websocket.read from with
      | None -> ()
      | Some message ->
        (match message with
         | `Text text -> Websocket.send_text into text
         | `Binary data -> Websocket.send_binary into data);
        if not (Websocket.is_closed into) then pump from into
    in
    Fiber.first (fun () -> pump ws upstream) (fun () -> pump upstream ws);
    Websocket.close upstream
;;

let websockets ~env ~log remote =
  [ "/ws", rpc ~env ~log remote; "/terminal", terminal ~env ~log remote ]
;;

let hop_by_hop = [ "host"; "connection"; "keep-alive"; "upgrade" ]

let forward
      ~env
      ~log
      (remote : Backend_address.t)
      (request : Web_server.Request.t)
      ~reader
      flow
  =
  Switch.run
  @@ fun sw ->
  match Backend_address.connect ~env ~sw remote with
  | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
  | exception exn ->
    Web_server.respond
      flow
      (Static_site.Response.text
         ~status:502
         ~reason:"Bad Gateway"
         (unreachable ~log remote exn ^ "\n"))
  | upstream ->
    let headers =
      List.filter request.headers ~f:(fun (name, _) ->
        not (List.mem hop_by_hop name ~equal:String.Caseless.equal))
    in
    Eio.Flow.copy_string
      (sprintf
         "%s %s%s HTTP/1.1\r\nHost: %s\r\nConnection: close\r\n%s\r\n"
         request.meth
         remote.prefix
         request.target
         (Backend_address.authority remote)
         (String.concat
            (List.map headers ~f:(fun (name, value) ->
               sprintf "%s: %s\r\n" name value))))
      upstream;
    let ignore_errors f =
      try f () with
      | Eio.Cancel.Cancelled _ as exn -> raise exn
      | _ -> ()
    in
    (* The body (an upload) goes up as it comes; the answer ends it all. *)
    Fiber.first
      (fun () ->
         ignore_errors (fun () ->
           Eio.Flow.copy (Eio.Buf_read.as_flow reader) upstream);
         Fiber.await_cancel ())
      (fun () -> ignore_errors (fun () -> Eio.Flow.copy upstream flow))
;;

let routes ~env ~log remote (request : Web_server.Request.t) =
  Option.some_if
    (File_http.handles ~path:request.path)
    (forward ~env ~log remote)
;;
