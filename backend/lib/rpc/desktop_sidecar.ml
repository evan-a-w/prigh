open! Core
open! Import

module Initial = struct
  type t =
    | Form of
        { address : string option
        ; user : string option
        ; error : string option
        }
    | Check of
        { remote : Backend_address.t
        ; credentials : Web_connect.Credentials.t
        ; source : Web_connect.Check.Source.t
        ; save : Saved_connection.t option
        }
end

module Config = struct
  type t =
    { initial : Initial.t
    ; tools : Web_connect.Tools.t
    ; name : string
    ; host_id : string
    ; cwd : string
    ; listen : Web_connect.Listen.t
    ; root : string
    ; save : Saved_connection.t -> unit Or_error.t
    }
end

module Ended = struct
  type t =
    | Quit
    | Stdin_closed
  [@@deriving sexp_of]
end

let path_prefix = "/desktop/"

module Form = struct
  type t =
    { address : string option
    ; user : string option
    ; error : string option
    }
end

module Connected = struct
  type t =
    { remote : Backend_address.t
    ; handoff : Signin_handoff.t
    ; websockets : (string * Web_server.on_websocket) list
    }
end

module State = struct
  type t =
    | Checking of unit Promise.t
    | Form of Form.t
    | Connected of Connected.t
end

type t =
  { env : Env.t
  ; sw : Switch.t
  ; log : string -> unit
  ; config : Config.t
  ; key : string
  ; tool_host_wait : Time_ns.Span.t
  ; check_timeout : Time_ns.Span.t
  ; mutable state : State.t
  ; quit : unit Promise.u
  }

let digest s = Digestif.SHA256.to_raw_string (Digestif.SHA256.digest_string s)
let string_or_null = Option.value_map ~default:`Null ~f:(fun s -> `String s)

(* The page continues signed in, the tool host connected (or given up on
   after [tool_host_wait]). *)
let connected t ~remote ~(credentials : Web_connect.Credentials.t) ~save =
  Option.iter save ~f:(fun connection ->
    match t.config.save connection with
    | Ok () -> ()
    | Error e ->
      t.log (sprintf "cannot save the connection: %s" (Error.to_string_hum e)));
  let handoff =
    Signin_handoff.create
      ~user:credentials.user
      ~token:credentials.token
      ~prefer_host:
        (match t.config.tools with
         | Local -> Some t.config.host_id
         | Remote -> None)
  in
  (match t.config.tools with
   | Remote -> t.log "tools run on the backend (-tools remote)"
   | Local ->
     (match
        Web_connect.start_tool_host
          ~env:t.env
          ~sw:t.sw
          ~log:t.log
          ~wait:t.tool_host_wait
          { remote
          ; credentials
          ; name = t.config.name
          ; host_id = t.config.host_id
          ; cwd = t.config.cwd
          }
      with
      | `Connected ->
        t.log (sprintf "tools run on this machine, in %s" t.config.cwd)
      | `Connecting ->
        t.log
          "the tool host is still connecting: tools run on this machine once \
           it is"
      | `Refused _ -> ()));
  t.log (sprintf "connected to %s" (Backend_address.to_string remote));
  { Connected.remote
  ; handoff
  ; websockets = Web_proxy.websockets ~env:t.env ~log:t.log remote
  }
;;

(* Says hello to [remote]; a refusal goes back to the form, with what to do
   ([prefix] says which connection it was when not typed in). *)
let connect ?(prefix = "") t ~remote ~credentials ~source ~save =
  let settled, settle = Promise.create () in
  t.state <- Checking settled;
  let result =
    match
      Web_connect.Check.run ~env:t.env ~timeout:t.check_timeout remote credentials
    with
    | Ok () ->
      let c = connected t ~remote ~credentials ~save in
      t.state <- Connected c;
      Ok (Signin_handoff.code c.handoff)
    | Error error ->
      let explained =
        prefix ^ Web_connect.Check.explain error remote ~source credentials
      in
      t.log explained;
      t.state
      <- Form
           { address = Some (Backend_address.to_string remote)
           ; user = credentials.user
           ; error = Some explained
           };
      Error explained
  in
  Promise.resolve settle ();
  result
;;

let respond_json flow ?(status = 200) ?(reason = "OK") json =
  let body = Json.to_string json in
  Web_server.respond
    flow
    { status
    ; reason
    ; headers = [ "Content-Type", "application/json"; "Cache-Control", "no-store" ]
    ; content_length = Some (String.length body)
    ; body
    }
;;

let respond_text flow ~status ~reason body =
  let response = Static_site.Response.text ~status ~reason body in
  Web_server.respond
    flow
    { response with
      headers =
        ("Content-Type", "text/plain; charset=utf-8") :: response.headers
    }
;;

let field json name =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal name with
     | Some (`String s) ->
       (match String.strip s with
        | "" -> None
        | s -> Some s)
     | _ -> None)
  | _ -> None
;;

let rec status t flow =
  match t.state with
  | Checking settled ->
    Promise.await settled;
    status t flow
  | Form { address; user; error } ->
    respond_json
      flow
      (`Object
          [ "state", `String "connect"
          ; "address", string_or_null address
          ; "user", string_or_null user
          ; "error", string_or_null error
          ])
  | Connected { remote; handoff; _ } ->
    respond_json
      flow
      (`Object
          [ "state", `String "connected"
          ; "address", `String (Backend_address.to_string remote)
          ; ( "code"
            , if Signin_handoff.taken handoff
              then `Null
              else `String (Signin_handoff.code handoff) )
          ])
;;

let form_connect t flow json =
  let failed error =
    respond_json flow (`Object [ "ok", `False; "error", `String error ])
  in
  match t.state with
  | Checking _ -> failed "Already connecting: wait for it to finish."
  | Connected { remote; _ } ->
    failed
      (sprintf
         "Already connected to %s: reload the page."
         (Backend_address.to_string remote))
  | Form _ ->
    (match field json "address" with
     | None ->
       failed
         "Enter the backend's address: HOST:PORT (its -web or -listen port), \
          or a prigh-web URL such as wss://host/ws."
     | Some typed ->
       let address = Backend_address.normalize typed in
       let user = field json "user" in
       let token = field json "token" in
       (match Backend_address.parse address with
        | Error e ->
          let error = Error.to_string_hum e in
          t.state <- Form { address = Some typed; user; error = Some error };
          failed error
        | Ok remote ->
          (match
             connect
               t
               ~remote
               ~credentials:{ token; user }
               ~source:Form
               ~save:(Some { Saved_connection.connect = address; token; user })
           with
           | Ok code ->
             respond_json flow (`Object [ "ok", `True; "code", `String code ])
           | Error error -> failed error)))
;;

let max_body = 64 * 1024

let handle t (request : Web_server.Request.t) ~reader flow =
  let length =
    Option.bind (Web_server.Request.header request "Content-Length") ~f:(fun n ->
      Int.of_string_opt (String.strip n))
  in
  match request.meth, length with
  | "POST", Some length when length > 0 && length <= max_body ->
    let json =
      match Json.parse (Eio.Buf_read.take length reader) with
      | Ok json -> json
      | Error _ -> `Null
    in
    let key_ok =
      match json with
      | `Object fields ->
        (match List.Assoc.find fields ~equal:String.equal "key" with
         | Some (`String key) -> String.equal (digest key) (digest t.key)
         | _ -> false)
      | _ -> false
    in
    if not key_ok
    then
      respond_text
        flow
        ~status:403
        ~reason:"Forbidden"
        "this is not the page prigh desktop opened: open prigh again\n"
    else (
      match String.chop_prefix request.path ~prefix:path_prefix with
      | Some "status" -> status t flow
      | Some "connect" -> form_connect t flow json
      | Some "quit" ->
        respond_json flow (`Object [ "ok", `True ]);
        t.log "quit from the page";
        ignore (Promise.try_resolve t.quit () : bool)
      | _ -> respond_text flow ~status:404 ~reason:"Not Found" "not found\n")
  | "POST", _ ->
    respond_text flow ~status:400 ~reason:"Bad Request" "a JSON body expected\n"
  | _ -> respond_text flow ~status:405 ~reason:"Method Not Allowed" "method not allowed\n"
;;

let routes t (request : Web_server.Request.t) =
  if String.is_prefix request.path ~prefix:path_prefix
  then Some (handle t)
  else (
    match t.state with
    | Connected { remote; handoff; _ } ->
      (match Signin_handoff.route handoff request with
       | Some handler -> Some handler
       | None -> Web_proxy.routes ~env:t.env ~log:t.log remote request)
    | Checking _ | Form _ -> None)
;;

(* The relayed sockets go to the backend once connected; before that there
   is nothing to relay to. *)
let websocket t path ~query ws =
  match t.state with
  | Connected { websockets; _ } ->
    Option.iter (List.Assoc.find websockets ~equal:String.equal path) ~f:(fun f ->
      f ~query ws)
  | Checking _ | Form _ -> ()
;;

let on_lines t ~read_line ~write_line =
  Web_connect.not_a_backend
    ~what:"prigh desktop"
    ~connect_to:
      (match t.state with
       | Connected { remote; _ } -> Backend_address.to_string remote
       | Checking _ | Form _ -> "the backend prigh desktop's window connects to")
    ~read_line
    ~write_line
;;

let new_key () =
  Mirage_crypto_rng_unix.use_default ();
  Ohex.encode (Mirage_crypto_rng.generate 16)
;;

let drain stdin =
  let buf = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read stdin buf with
    | (_ : int) -> loop ()
    | exception End_of_file -> ()
  in
  loop ()
;;

let run
      ~env
      ?(tool_host_wait = Time_ns.Span.of_sec 10.)
      ?(check_timeout = Time_ns.Span.of_sec 15.)
      ~log
      ~on_url
      ~stdin
      (config : Config.t)
  =
  let quit, set_quit = Promise.create () in
  Fiber.first
    (fun () ->
       Switch.run (fun sw ->
         let t =
           { env
           ; sw
           ; log
           ; config
           ; key = new_key ()
           ; tool_host_wait
           ; check_timeout
           ; state = Form { address = None; user = None; error = None }
           ; quit = set_quit
           }
         in
         let addr, port =
           Web_connect.Listen.bind config.listen (fun ~addr ~port ->
             Web_server.listen
               ~env
               ~sw
               ~routes:(routes t)
               ~addr
               ~port
               ~root:(Some config.root)
               ~websockets:
                 (List.map [ "/ws"; "/terminal" ] ~f:(fun path ->
                    path, websocket t path))
               ~on_lines:(on_lines t)
               ())
         in
         (match config.initial with
          | Form { address; user; error } ->
            t.state <- Form { address; user; error }
          | Check { remote; credentials; source; save } ->
            let settled, settle = Promise.create () in
            t.state <- Checking settled;
            Fiber.fork ~sw (fun () ->
              let prefix =
                match source with
                | Flag -> "-connect did not work: "
                | Env -> "$PRIGH_CONNECT did not work: "
                | Saved file -> sprintf "The saved connection (%s) did not work: " file
                | Form -> ""
              in
              ignore
                (connect
                   t
                   ~prefix
                   ~remote
                   ~credentials
                   ~source:Form
                   ~save
                 : (string, string) Result.t);
              Promise.resolve settle ()));
         on_url
           (Web_server.browser_url ~host:(Web_connect.browser_host addr) ~port
            ^ "#desktop="
            ^ t.key);
         Fiber.await_cancel ()))
    (fun () ->
       Fiber.first
         (fun () ->
            Promise.await quit;
            Ended.Quit)
         (fun () ->
            drain stdin;
            Ended.Stdin_closed))
;;
