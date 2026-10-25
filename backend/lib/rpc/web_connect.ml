open! Core
open! Import

module Tools = struct
  type t =
    | Local
    | Remote
  [@@deriving sexp_of, equal]

  let of_string = function
    | "local" -> Ok Local
    | "remote" -> Ok Remote
    | other ->
      Or_error.errorf
        "-tools %s: local (run tools on this machine) or remote (on the \
         backend)"
        other
  ;;
end

module Credentials = struct
  type t =
    { token : string option
    ; user : string option
    }
end

module Check = struct
  module Error = struct
    type t =
      | Unreachable of string
      | Refused of string
      | Not_prigh_web of string
      | Failed of string
    [@@deriving sexp_of]
  end

  let reason = function
    | Failure message | Backend_address.Not_upgraded message -> message
    | exn -> Net_error.to_string_hum exn
  ;;

  let hello ({ token; user } : Credentials.t) =
    let string name = Option.map ~f:(fun v -> name, `String v) in
    Json.to_string
      (`Object
          [ "id", `String "check"
          ; "method", `String "hello"
          ; ( "params"
            , `Object
                (List.filter_opt
                   [ Some ("name", `String "prigh web")
                   ; string "token" token
                   ; string "user" user
                   ]) )
          ])
  ;;

  let field json name =
    match json with
    | `Object fields -> List.Assoc.find fields ~equal:String.equal name
    | _ -> None
  ;;

  (* The hello's answer, after any events. *)
  let rec answer socket =
    match Backend_socket.read_line socket with
    | None ->
      Error
        (Error.Unreachable "the connection closed before hello was answered")
    | Some line ->
      (match Json.parse line with
       | Ok json
         when match field json "id" with
              | Some (`String "check") -> true
              | _ -> false ->
         (match field json "ok", field json "error" with
          | Some `True, _ -> Ok ()
          | _, Some (`String error)
            when String.is_prefix error ~prefix:"unauthorised" ->
            Error (Error.Refused error)
          | _, Some (`String error) -> Error (Failed error)
          | _ -> Error (Failed line))
       | Ok _ -> answer socket
       | Error _ ->
         Error
           (Failed
              (sprintf
                 "the server answered %S, not prigh's RPC"
                 (String.prefix line 80))))
  ;;

  let run ~env ?(timeout = Time_ns.Span.of_sec 15.) remote credentials =
    match
      Eio.Time.with_timeout_exn
        (Eio.Stdenv.clock env)
        (Time_ns.Span.to_sec timeout)
        (fun () ->
           Switch.run (fun sw ->
             let socket = Backend_socket.connect ~env ~sw remote in
             Exn.protect
               ~finally:(fun () -> Backend_socket.close socket)
               ~f:(fun () ->
                 Backend_socket.write_line socket (hello credentials);
                 answer socket)))
    with
    | result -> result
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception Eio.Time.Timeout ->
      Error
        (Unreachable
           (sprintf
              "no answer within %s"
              (Time_ns.Span.to_string_hum ~decimals:0 timeout)))
    | exception Backend_address.Not_upgraded why -> Error (Not_prigh_web why)
    | exception exn -> Error (Unreachable (reason exn))
  ;;

  module Source = struct
    type t =
      | Flag
      | Env
      | Saved of string
  end

  let explain
        (error : Error.t)
        remote
        ~(source : Source.t)
        ({ token; user } : Credentials.t)
    =
    let address = Backend_address.to_string remote in
    match error with
    | Unreachable why ->
      sprintf
        "cannot reach the backend at %s: %s\n\
         Is it running there (prigh serve -web HOST:PORT, or -listen)? %s"
        address
        why
        (match source with
         | Flag ->
           "Check the address given to -connect: HOST:PORT, or a prigh-web URL \
            such as wss://host/ws."
         | Env -> "Check $PRIGH_CONNECT, or pass -connect HOST:PORT."
         | Saved file ->
           sprintf
             "The address is the saved connection (%s): pass -connect \
              HOST:PORT to replace it."
             file)
    | Refused why ->
      sprintf
        "%s refused the login: %s\n%s"
        address
        why
        (match token, user with
         | None, _ ->
           "No token was given: pass the backend's with -token SECRET (or \
            $PRIGH_TOKEN); it is saved for next time."
         | Some _, None ->
           "Check -token (or $PRIGH_TOKEN) against the backend's; if it serves \
            several users (-tokens), also give -user NAME (or $PRIGH_USER)."
         | Some _, Some _ ->
           "Check -user and -token (or $PRIGH_USER and $PRIGH_TOKEN) against \
            the backend's -tokens NAME=TOKEN entries.")
    | Not_prigh_web why ->
      sprintf
        "the backend at %s did not accept prigh web: %s\n\
         Is the URL's path where its prigh-web is (the page's address, without \
         index.html)?"
        address
        why
    | Failed why ->
      sprintf
        "the backend at %s did not accept prigh web: %s\n\
         Is it prigh serve, and as new as this prigh?"
        address
        why
  ;;
end

module Listen = struct
  type t =
    | At of Eio.Net.Ipaddr.v4v6 * int
    | From of Eio.Net.Ipaddr.v4v6 * int
end

module Config = struct
  type t =
    { remote : Backend_address.t
    ; credentials : Credentials.t
    ; tools : Tools.t
    ; name : string
    ; host_id : string
    ; cwd : string
    ; listen : Listen.t
    ; root : string
    }
end

module Started = struct
  type t =
    { url : string
    ; port : int
    ; tool_host : [ `Connected | `Connecting | `Refused of string | `Off ]
    }
end

let serve ~env ~sw ~log ~handoff (config : Config.t) ~addr ~port =
  Web_server.listen
    ~env
    ~sw
    ~routes:(fun request ->
      match Signin_handoff.route handoff request with
      | Some handler -> Some handler
      | None -> Web_proxy.routes ~env ~log config.remote request)
    ~addr
    ~port
    ~root:(Some config.root)
    ~websockets:(Web_proxy.websockets ~env ~log config.remote)
      (* The site does not speak for the backend to terminals: they can
         connect to it themselves. *)
    ~on_lines:(fun ~read_line:_ ~write_line ->
      write_line
        (Json.to_string
           (`Object
               [ "type", `String "response"
               ; "id", `Null
               ; "ok", `False
               ; ( "error"
                 , `String
                     (sprintf
                        "this is prigh web's site, not a backend: connect to %s"
                        (Backend_address.to_string config.remote)) )
               ])))
    ()
;;

let listen ~env ~sw ~log ~handoff (config : Config.t) =
  match config.listen with
  | At (addr, port) -> addr, serve ~env ~sw ~log ~handoff config ~addr ~port
  | From (addr, first) ->
    let rec attempt port =
      match serve ~env ~sw ~log ~handoff config ~addr ~port with
      | bound -> addr, bound
      | exception exn
        when port < first + 9
             &&
             match Net_error.unix_error exn with
             | Some EADDRINUSE -> true
             | _ -> false -> attempt (port + 1)
    in
    attempt first
;;

let tool_host ~env ~sw ~log ~wait (config : Config.t) =
  let state, set_state = Promise.create () in
  let settle value =
    if not (Promise.is_resolved state) then Promise.resolve set_state value
  in
  Fiber.fork_daemon ~sw (fun () ->
    let refused =
      Tool_host.connect
        ~env
        ~log:(fun line -> log ("tool host: " ^ line))
        ~on_connect:(fun () -> settle `Connected)
        ~backend:config.remote
        ~token:config.credentials.token
        ?user:config.credentials.user
        ~host_id:config.host_id
        ~name:config.name
        ~cwd:config.cwd
        ()
    in
    let refused = Error.to_string_hum refused in
    log
      (sprintf
         "tool host: %s; sessions cannot run tools on this machine (/host \
          picks another host)"
         refused);
    settle (`Refused refused);
    `Stop_daemon);
  Fiber.first
    (fun () -> Promise.await state)
    (fun () ->
       Eio.Time.sleep (Eio.Stdenv.clock env) (Time_ns.Span.to_sec wait);
       `Connecting)
;;

let start
      ~env
      ~sw
      ?(tool_host_wait = Time_ns.Span.of_sec 10.)
      ~log
      (config : Config.t)
  =
  let handoff =
    Signin_handoff.create
      ~user:config.credentials.user
      ~token:config.credentials.token
      ~prefer_host:
        (match config.tools with
         | Local -> Some config.host_id
         | Remote -> None)
  in
  let addr, port = listen ~env ~sw ~log ~handoff config in
  let tool_host_state =
    match config.tools with
    | Remote -> `Off
    | Local -> tool_host ~env ~sw ~log ~wait:tool_host_wait config
  in
  let host =
    match Format.asprintf "%a" Eio.Net.Ipaddr.pp addr with
    | "0.0.0.0" -> "127.0.0.1"
    | "::" -> "::1"
    | host -> host
  in
  { Started.url =
      Web_server.browser_url ~host ~port
      ^ "#signin="
      ^ Signin_handoff.code handoff
  ; port
  ; tool_host = tool_host_state
  }
;;
