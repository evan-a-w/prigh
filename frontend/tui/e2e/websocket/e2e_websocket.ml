open! Core
open! Async
module Client = Prigh_client.Client
module Link = Prigh_tui.Backend_link
module Cert = Prigh_client_unix_testing.Test_cert
module Server = Prigh_client_unix_testing.Test_server

(* The real backend's prigh-web port through the TUI's WebSocket transport:
   directly (ws://), and through a TLS-terminating proxy (wss://) whose
   certificate a throwaway CA, trusted in place of the system's, issued. *)

let backend () =
  match Sys.getenv "PRIGH_BACKEND" with
  | Some p -> p
  | None ->
    let rec find_root dir =
      if Sys_unix.file_exists_exn (Filename.concat dir "backend/dune-project")
      then Some dir
      else if String.equal dir "/"
      then None
      else find_root (Filename.dirname dir)
    in
    (match find_root (Core_unix.getcwd ()) with
     | Some root -> Filename.concat root "backend/_build/default/bin/main.exe"
     | None -> "prigh")
;;

(* Starts [prigh serve -web 127.0.0.1:0] and reads its port off stderr. *)
let start_backend ~tmp =
  let%bind process =
    Process.create_exn
      ~env:(`Extend [ "HOME", tmp ])
      ~prog:(backend ())
      ~args:
        [ "serve"
        ; "-faux"
        ; "-auth-file"
        ; Filename.concat tmp "auth.json"
        ; "-cwd"
        ; tmp
        ; "-model"
        ; "deepseek/deepseek-flash"
        ; "-token"
        ; "sesame"
        ; "-web"
        ; "127.0.0.1:0"
        ]
      ()
  in
  let stderr = Process.stderr process in
  let rec port () =
    match%bind Reader.read_line stderr with
    | `Eof -> return None
    | `Ok line ->
      (match
         String.chop_prefix line ~prefix:"prigh: prigh-web on http://127.0.0.1:"
       with
       | Some rest ->
         return (Int.of_string_opt (String.rstrip rest ~drop:(Char.equal '/')))
       | None -> port ())
  in
  let%map port = Clock.with_timeout (Time_float.Span.of_sec 30.) (port ()) in
  match port with
  | `Result (Some port) ->
    don't_wait_for (Reader.drain stderr);
    Some (process, port)
  | _ ->
    Signal_unix.send_i Signal.kill (`Pid (Process.pid process));
    None
;;

let endpoint ?(headers = []) url =
  match Prigh_ui.Server_address.parse url with
  | Ok address -> Link.Endpoint.Websocket { url; address; headers }
  | Error e -> failwith e
;;

let session ?authenticator label url =
  printf "== %s\n" label;
  let open_ () =
    match authenticator with
    | None -> Link.Endpoint.open_ (endpoint url)
    | Some authenticator ->
      (* What [Endpoint.open_] does, with the test's CA. *)
      (match Prigh_ui.Server_address.parse url with
       | Error e -> Deferred.Or_error.error_string e
       | Ok a ->
         Prigh_client_unix.Websocket_transport.connect
           ~authenticator
           ~url
           ~tls:a.tls
           ~host:a.host
           ~port:a.port
           ~authority:(Prigh_ui.Server_address.authority a)
           ~path:(Prigh_ui.Server_address.websocket_path a)
           ())
  in
  let client = Client.create ~connect:open_ in
  match%bind Client.connect client with
  | Error e ->
    printf "cannot connect: %s\n" (Error.to_string_hum e);
    return ()
  | Ok () ->
    let call ?(client = client) method_ params =
      match%map Client.call client method_ params with
      | Ok json ->
        printf
          "<- %s: ok%s\n"
          method_
          (match json with
           | `Object fields ->
             (match List.Assoc.find fields ~equal:String.equal "user" with
              | Some _ ->
                " (" ^ String.concat ~sep:"," (List.map fields ~f:fst) ^ ")"
              | None -> "")
           | _ -> "")
      | Error e -> printf "<- %s ERROR: %s\n" method_ (Error.to_string_hum e)
    in
    let%bind () = call "ping" [] in
    let%bind () = call "hello" [ "token", `String "sesame" ] in
    let%bind () = call "ping" [] in
    (* A turn: its events stream back over the socket. *)
    let%bind () = call "prompt" [ "text", `String ("hello over " ^ label) ] in
    let rec events () =
      match%bind Pipe.read (Client.incoming client) with
      | `Eof -> return ()
      | `Ok (Event (Message_update { delta = Text_delta text; _ })) ->
        printf "  event text_delta %S\n" text;
        events ()
      | `Ok (Event (State { running = false; _ })) -> return ()
      | `Ok _ -> events ()
    in
    let%bind () =
      match%map Clock.with_timeout (Time_float.Span.of_sec 30.) (events ()) with
      | `Result () -> ()
      | `Timeout -> print_endline "  the turn did not end"
    in
    let%bind () = Client.close client in
    let client = Client.create ~connect:open_ in
    let%bind () =
      match%bind Client.connect client with
      | Error e ->
        printf "cannot reconnect: %s\n" (Error.to_string_hum e);
        return ()
      | Ok () ->
        let%bind () = call ~client "hello" [ "token", `String "wrong" ] in
        let%bind () = call ~client "ping" [] in
        Client.close client
    in
    return ()
;;

let main () =
  let tmp = Filename_unix.temp_dir "prigh-e2e-ws" "" in
  match%bind start_backend ~tmp with
  | None ->
    print_endline "the backend did not start its -web port";
    return ()
  | Some (process, port) ->
    let%bind () = session "ws://" (sprintf "ws://127.0.0.1:%d/ws" port) in
    let%bind () =
      session "http:// (its /ws)" (sprintf "http://127.0.0.1:%d/" port)
    in
    let cert = Cert.create () in
    let%bind proxy = Server.tls_proxy cert.server ~port in
    let url = sprintf "wss://localhost:%d/ws" (Server.port proxy) in
    let%bind () =
      session "wss:// through a TLS proxy" url ~authenticator:cert.authenticator
    in
    let%bind () =
      session
        "wss:// with a CA that did not issue the certificate"
        url
        ~authenticator:(Cert.stranger ())
    in
    let%bind () = Server.close proxy in
    Signal_unix.send_i Signal.term (`Pid (Process.pid process));
    let%map (_ : Core_unix.Exit_or_signal.t) = Process.wait process in
    print_endline "backend stopped"
;;

let () =
  Command_unix.run
    (Command.async
       ~summary:"the TUI's WebSocket transport against the real backend"
       (Command.Param.return main))
;;
