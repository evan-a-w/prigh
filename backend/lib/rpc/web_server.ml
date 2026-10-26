open! Core
open! Import

module Request = struct
  type t =
    { meth : string
    ; target : string
    ; path : string
    ; query : (string * string) list
    ; headers : (string * string) list
    }
  [@@deriving sexp_of]

  let parse_query query =
    String.split query ~on:'&'
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:(fun pair ->
      let key, value =
        match String.lsplit2 pair ~on:'=' with
        | Some (k, v) -> k, v
        | None -> pair, ""
      in
      Uri.pct_decode key, Uri.pct_decode value)
  ;;

  let header t name =
    List.Assoc.find t.headers ~equal:String.Caseless.equal name
  ;;

  let parse reader =
    match Eio.Buf_read.line reader with
    | exception (End_of_file | Eio.Io _) -> None
    | request_line ->
      (match String.split request_line ~on:' ' with
       | [ meth; target; _version ] ->
         let path, query =
           match String.lsplit2 target ~on:'?' with
           | Some (path, query) -> path, parse_query query
           | None -> target, []
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
         Some { meth; target; path; query; headers = headers [] }
       | _ -> None)
  ;;
end

let format_response
      ({ status; reason; headers; content_length; body } :
        Static_site.Response.t)
  =
  let headers =
    Option.value_map content_length ~default:[] ~f:(fun n ->
      [ "Content-Length", Int.to_string n ])
    @ [ "Connection", "close"; "Cache-Control", "no-cache" ]
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

let respond flow response = Eio.Flow.copy_string (format_response response) flow

let response ~status ~reason body =
  format_response (Static_site.Response.text ~status ~reason body)
;;

let browser_url ~host ~port =
  let host =
    if
      String.is_substring host ~substring:":"
      && not (String.is_prefix host ~prefix:"[")
    then "[" ^ host ^ "]"
    else host
  in
  sprintf "http://%s:%d/" host port
;;

let plaintext_warning ~flag addr =
  let shown = Format.asprintf "%a" Eio.Net.Ipaddr.pp addr in
  if String.is_prefix shown ~prefix:"127." || String.equal shown "::1"
  then None
  else
    Some
      (sprintf
         "prigh: warning: %s %s is reachable from other machines without TLS: \
          tokens and everything else travel in the clear. Put an HTTPS reverse \
          proxy in front of -web (see DOCKER.md, \"Behind a reverse proxy\"; \
          then prigh-tui -connect wss://HOST/ws), use an SSH tunnel, or listen \
          on 127.0.0.1 only."
         flag
         shown)
;;

type on_lines =
  read_line:(unit -> string option) -> write_line:(string -> unit) -> unit

type on_websocket = query:(string * string) list -> Websocket.t -> unit
type flow = Eio.Flow.two_way_ty Eio.Resource.t
type on_http = Request.t -> reader:Eio.Buf_read.t -> flow -> unit

let upgrade_handler ~websockets (request : Request.t) =
  if
    Option.value_map
      (Request.header request "Upgrade")
      ~default:false
      ~f:(fun v -> String.Caseless.equal (String.strip v) "websocket")
  then List.Assoc.find websockets ~equal:String.equal request.path
  else None
;;

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

let handle ?clock ?(routes = fun _ -> None) ~root ~websockets ~on_lines flow =
  let reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) in
  if is_json_lines reader
  then serve_json_lines ~on_lines ~reader flow
  else (
    match Request.parse reader with
    | None -> ()
    | Some request when Option.is_some (upgrade_handler ~websockets request) ->
      let on_websocket =
        Option.value_exn (upgrade_handler ~websockets request)
      in
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
         (match clock with
          | None -> on_websocket ~query:request.query ws
          | Some clock ->
            Fiber.first
              (fun () -> on_websocket ~query:request.query ws)
              (fun () ->
                 Websocket.keepalive
                   ws
                   ~clock
                   ~shutdown:(fun () ->
                     try Eio.Flow.shutdown flow `All with
                     | _ -> ())
                   ()));
         Websocket.close ws)
    | Some request when Option.is_some (routes request) ->
      (Option.value_exn (routes request)) request ~reader (flow :> flow)
    | Some request when File_http.handles ~path:request.path ->
      File_http.serve
        ~meth:request.meth
        ~path:request.path
        ~query:request.query
        ~headers:request.headers
        ~reader
        flow
    | Some request ->
      (match root with
       | None ->
         Eio.Flow.copy_string
           (response
              ~status:404
              ~reason:"Not Found"
              "no web root configured; only WebSockets are served\n")
           flow
       | Some root ->
         Eio.Flow.copy_string
           (format_response
              (Static_site.respond
                 ~root
                 ~meth:request.meth
                 ~path:request.path
                 ~headers:request.headers))
           flow))
;;

(* Replies go out whole at once: with Nagle's algorithm the end of a large
   one waits for the browser's delayed ACK (40 ms). *)
let no_delay flow =
  Option.iter (Eio_unix.Resource.fd_opt flow) ~f:(fun fd ->
    Eio_unix.Fd.use_exn "no_delay" fd (fun fd ->
      try Core_unix.setsockopt fd TCP_NODELAY true with
      | _ -> ()))
;;

(* A peer that vanished without a word (a laptop suspended, a network gone)
   is noticed within a minute even while nothing is sent to it: until then
   it still counts as connected, and as a tool host it would be sent tool
   calls nobody answers. *)
let keepalive flow =
  try
    Eio.Net.setsockopt flow Eio.Net.Sockopt.SO_KEEPALIVE true;
    Eio.Net.setsockopt flow Eio.Net.Sockopt.TCP_KEEPIDLE 30;
    Eio.Net.setsockopt flow Eio.Net.Sockopt.TCP_KEEPINTVL 10;
    Eio.Net.setsockopt flow Eio.Net.Sockopt.TCP_KEEPCNT 3
  with
  | _ -> ()
;;

let serve_rpc router ~query:_ ws =
  Rpc_router.serve_lines
    router
    ~read_line:(fun () -> Websocket.read_text ws)
    ~write_line:(fun line ->
      Websocket.send_text ws line;
      if Websocket.is_closed ws then failwith "the WebSocket is closed")
;;

let serve_terminal router terminals ~query ws =
  let param name = List.Assoc.find query ~equal:String.equal name in
  let size name ~default =
    Option.bind (param name) ~f:Int.of_string_opt |> Option.value ~default
  in
  let channel = Terminal_channel.of_websocket ws in
  let error message =
    Terminal_channel.send_text
      channel
      (Json.to_string
         (`Object [ "type", `String "error"; "message", `String message ]))
  in
  match
    Rpc_router.lookup
      router
      ?user:(param "user")
      ?as_user:(param "as_user")
      ~token:(param "token")
      ()
  with
  | None -> error Rpc_server.unauthorised
  | Some server ->
    let session = Option.filter (param "session") ~f:(Fn.non String.is_empty) in
    (* [dir]: a shell of its own there (a worktree's, say), not the
       session's. *)
    let dir = Option.filter (param "dir") ~f:(Fn.non String.is_empty) in
    let key =
      Rpc_server.terminal_key
        server
        ~session:
          (Option.value session ~default:"default"
           ^ Option.value_map dir ~default:"" ~f:(fun dir -> "@" ^ dir))
    in
    let cols = size "cols" ~default:80 in
    let rows = size "rows" ~default:24 in
    let in_dir cwd =
      match dir with
      | None -> cwd
      | Some dir when Filename.is_relative dir -> Filename.concat cwd dir
      | Some dir -> dir
    in
    (match Rpc_server.terminal_target server ~session with
     | `Backend cwd ->
       Terminals.serve terminals ~key ~cwd:(in_dir cwd) ~cols ~rows channel
     | `Host (host, cwd) ->
       Rpc_server.relay_terminal
         server
         ~host
         ~key
         ~cwd:(in_dir cwd)
         ~cols
         ~rows
         channel
     | `Unavailable reason -> error ("no terminal: " ^ reason))
;;

let listen ~env ~sw ?routes ~addr ~port ~root ~websockets ~on_lines () =
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
    Guarded.accept_loop
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~on_error:(fun exn ->
        eprintf "prigh: web connection failed: %s\n%!" (Exn.to_string exn))
      socket
      (fun flow _addr ->
         no_delay flow;
         keepalive flow;
         handle
           ~clock:(Eio.Stdenv.clock env)
           ?routes
           ~root
           ~websockets
           ~on_lines
           flow));
  port
;;

module For_testing = struct
  let parse_query = Request.parse_query
end
