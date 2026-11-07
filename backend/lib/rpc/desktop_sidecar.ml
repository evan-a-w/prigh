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

module Ui = struct
  type t =
    | Remote
    | Local
  [@@deriving sexp_of]
end

module Page = struct
  type t =
    { url : string
    ; remote : bool
    ; cookie : (string[@sexp.opaque]) option [@sexp.option]
    ; signin : string option [@sexp.option]
    ; here : bool [@sexp.bool]
    ; external_ : bool [@sexp.bool]
    }
  [@@deriving sexp_of]

  let here url =
    { url
    ; remote = false
    ; cookie = None
    ; signin = None
    ; here = false
    ; external_ = false
    }
  ;;

  let to_json_line t =
    let flag name b = if b then [ name, `True ] else [] in
    let string name =
      Option.value_map ~default:[] ~f:(fun s -> [ name, `String s ])
    in
    Json.to_string
      (`Object
          ((("url", `String t.url) :: flag "remote" t.remote)
           @ string "cookie" t.cookie
           @ string "signin" t.signin
           @ flag "here" t.here
           @ flag "external" t.external_))
  ;;
end

module Config = struct
  type t =
    { initial : Initial.t
    ; ui : Ui.t
    ; tools : Web_connect.Tools.t
    ; name : string
    ; host_id : string
    ; cwd : string
    ; listen : Web_connect.Listen.t
    ; computer : Computer_helper.t option
    ; root : string
    ; save : Saved_connection.t -> unit Or_error.t
    ; remember_signin :
        Backend_address.t -> Browser_signin.Grant.t -> unit Or_error.t
    }
end

module Ended = struct
  type t =
    | Quit
    | Stdin_closed
    | Stopped
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

(* What the page here needs under [Ui.Local]: the sign-in and the relay. *)
module Relay = struct
  type t =
    { handoff : Signin_handoff.t
    ; websockets : (string * Web_server.on_websocket) list
    }
end

module Connected = struct
  type t =
    { remote : Backend_address.t
    ; relay : Relay.t option (** [None] under [Ui.Remote] *)
    }
end

(* A sign-in through the browser under way: the URL it opens, and the
   answer to [/desktop/signin] once it ends. *)
module Browser_attempt = struct
  type t =
    { url : string Promise.t
    ; ended : Json.t Promise.t
    ; cancel : unit Promise.u
    }
end

(* What continues once signed in to the site's proxy. *)
module Pending = struct
  type t =
    | Connected of Connected.t (** [/ws] needs no cookie: checked, started *)
    | Unchecked of
        { remote : Backend_address.t
        ; credentials : Web_connect.Credentials.t
        ; save : Saved_connection.t option
        } (** [/ws] is behind the proxy too: checked once signed in *)

  let remote = function
    | Connected c -> c.remote
    | Unchecked { remote; _ } -> remote
  ;;
end

module Signin = struct
  type t =
    { pending : Pending.t
    ; login : string option
    ; expired : bool
    ; mutable browser : Browser_attempt.t option
    ; mutable error : string option
    }
end

module State = struct
  type t =
    | Checking of unit Promise.t
    | Form of Form.t
    | Connected of Connected.t
    | Signin of Signin.t
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
  ; on_page : Page.t -> unit
  ; mutable page_url : string (** the page here, with the key *)
  }

let digest s = Digestif.SHA256.to_raw_string (Digestif.SHA256.digest_string s)
let string_or_null = Option.value_map ~default:`Null ~f:(fun s -> `String s)

(* The backend's own prigh-web, told it is in the desktop app and which tool
   host is this machine's. *)
let remote_url t remote =
  Backend_address.web_url remote
  ^ "#app=desktop"
  ^
  match t.config.tools with
  | Local -> "&host=" ^ Uri.pct_encode ~component:`Query_value t.config.host_id
  | Remote -> ""
;;

let site remote = Backend_address.authority remote

(* The backend's site, with the cookie its sign-in proxy wants (if any) and
   the page here to come back to for signing in. *)
let remote_page ?(here = false) t (remote : Backend_address.t) =
  { Page.url = remote_url t remote
  ; remote = true
  ; cookie = (if here || remote.lines then None else remote.proxy_cookie)
  ; signin = Some (t.page_url ^ "&signin=1")
  ; here
  ; external_ = false
  }
;;

(* The page continues signed in, the tool host connected (or given up on
   after [tool_host_wait]); without [tool_host] (it would not get past the
   site's sign-in proxy) it is not started. *)
let connected
      ?(tool_host = true)
      t
      ~remote
      ~(credentials : Web_connect.Credentials.t)
      ~save
  =
  Option.iter save ~f:(fun connection ->
    match t.config.save connection with
    | Ok () -> ()
    | Error e ->
      t.log (sprintf "cannot save the connection: %s" (Error.to_string_hum e)));
  (match t.config.tools with
   | Remote -> t.log "tools run on the backend (-tools remote)"
   | Local when not tool_host ->
     t.log
       (sprintf
          "tools cannot run on this machine: its tool host needs the sign-in \
           to %s too (sign in with the browser for that)"
          (site remote))
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
          ; computer = t.config.computer
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
  let relay =
    match t.config.ui with
    | Remote -> None
    | Local ->
      Some
        { Relay.handoff =
            Signin_handoff.create
              ~user:credentials.user
              ~token:credentials.token
              ~prefer_host:
                (match t.config.tools with
                 | Local -> Some t.config.host_id
                 | Remote -> None)
        ; websockets = Web_proxy.websockets ~env:t.env ~log:t.log remote
        }
  in
  { Connected.remote; relay }
;;

(* Where the page goes once connected: signed in here, or the backend's. *)
module Next = struct
  type t =
    | Code of string
    | Remote of string
    | Signin (** the site's sign-in proxy first: the page here offers it *)
end

let next t (c : Connected.t) : Next.t =
  match c.relay with
  | Some relay -> Code (Signin_handoff.code relay.handoff)
  | None -> Remote (remote_url t c.remote)
;;

let to_signin ?(expired = false) t ~login pending =
  let site = site (Pending.remote pending) in
  t.log
    (if expired
     then sprintf "the sign-in to %s expired: signing in again" site
     else
       sprintf
         "%s is behind a sign-in%s"
         site
         (Option.value_map login ~default:"" ~f:(sprintf " (%s)")));
  t.state <- Signin { pending; login; expired; browser = None; error = None }
;;

let probe t remote = Site_probe.run ~env:t.env ~timeout:t.check_timeout remote

(* Says hello to [remote]; a refusal goes back to the form, with what to do
   ([prefix] says which connection it was when not typed in). *)
let connect ?(prefix = "") t ~remote ~credentials ~save =
  let settled, settle = Promise.create () in
  t.state <- Checking settled;
  let back_to_form error =
    t.log error;
    t.state
    <- Form
         { address = Some (Backend_address.to_string remote)
         ; user = credentials.Web_connect.Credentials.user
         ; error = Some error
         };
    Error error
  in
  (* Whatever happens, the page waiting on [/desktop/status] is answered. *)
  Exn.protect
    ~finally:(fun () ->
      (match t.state with
       | Checking _ ->
         t.state
         <- Form
              { address = Some (Backend_address.to_string remote)
              ; user = credentials.user
              ; error = Some "Connecting was interrupted: connect again."
              }
       | Form _ | Connected _ | Signin _ -> ());
      Promise.resolve settle ())
    ~f:(fun () ->
      match
        Web_connect.Check.run
          ~env:t.env
          ~timeout:t.check_timeout
          remote
          credentials
      with
      | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
      | exception exn ->
        back_to_form
          (sprintf
             "%sconnecting to %s failed: %s"
             prefix
             (Backend_address.to_string remote)
             (Exn.to_string exn))
      | Error (Signin_required _)
        when match t.config.ui with
             | Remote -> true
             | Local -> false ->
        let login =
          match probe t remote with
          | Signin_required { login; _ } -> login
          | Open | Unreachable _ -> None
        in
        to_signin t ~login (Unchecked { remote; credentials; save });
        Ok Next.Signin
      | Error error ->
        (* Explained for the form, where it is shown, whatever [prefix]
           says the address came from. *)
        back_to_form
          (prefix
           ^ Web_connect.Check.explain error remote ~source:Form credentials)
      | Ok () ->
        (match connected t ~remote ~credentials ~save with
         | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
         | exception exn ->
           back_to_form
             (sprintf
                "%sconnected to %s, but starting failed: %s"
                prefix
                (Backend_address.to_string remote)
                (Exn.to_string exn))
         | c ->
           (match
              match c.relay with
              | Some _ -> Site_probe.Result.Open
              | None -> probe t c.remote
            with
            | Signin_required { why = _; login } ->
              to_signin t ~login (Connected c);
              Ok Next.Signin
            | Open | Unreachable _ ->
              t.state <- Connected c;
              let next = next t c in
              (match next with
               | Remote _ -> t.on_page (remote_page t c.remote)
               | Code _ | Signin -> ());
              Ok next)))
;;

(* Tokens and everything else would cross the network in the clear. *)
let plaintext (remote : Backend_address.t) =
  let host =
    String.chop_prefix_if_exists remote.host ~prefix:"["
    |> String.chop_suffix_if_exists ~suffix:"]"
  in
  let loopback =
    String.Caseless.equal host "localhost"
    || String.is_prefix host ~prefix:"127."
    || String.equal host "::1"
  in
  (not remote.tls) && not loopback
;;

let plaintext_warning remote =
  sprintf
    "%s is not encrypted: the password and everything prigh sends would cross \
     the network in the clear, and anyone on the way could run commands on \
     this computer. Use the backend's HTTPS address (wss://host/ws, behind a \
     TLS reverse proxy), or an SSH tunnel (ssh -L 7790:localhost:7790 host, \
     then 127.0.0.1:7790). Connect anyway only on a network you trust."
    (Backend_address.to_string remote)
;;

let respond_json flow ?(status = 200) ?(reason = "OK") json =
  let body = Json.to_string json in
  Web_server.respond
    flow
    { status
    ; reason
    ; headers =
        [ "Content-Type", "application/json"; "Cache-Control", "no-store" ]
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

let confirmed json name =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal name with
     | Some `True -> true
     | _ -> false)
  | _ -> false
;;

(* [signin]: the window was sent back here by the site's sign-in proxy,
   after having been signed in: its sign-in expired. *)
let rec status t flow ~signin =
  match t.state with
  | Checking settled ->
    Promise.await settled;
    status t flow ~signin
  | Connected ({ relay = None; _ } as c) when signin ->
    to_signin t ~expired:true ~login:None (Connected c);
    status t flow ~signin
  | Form { address; user; error } ->
    respond_json
      flow
      (`Object
          [ "state", `String "connect"
          ; "address", string_or_null address
          ; "user", string_or_null user
          ; "error", string_or_null error
          ])
  | Connected { remote; relay = Some { handoff; _ } } ->
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
  | Connected ({ relay = None; _ } as c) ->
    respond_json
      flow
      (`Object
          [ "state", `String "remote"; "url", `String (remote_url t c.remote) ])
  | Signin s ->
    respond_json
      flow
      (`Object
          [ "state", `String "signin"
          ; "site", `String (site (Pending.remote s.pending))
          ; "login", string_or_null s.login
          ; ( "waiting"
            , string_or_null
                (Option.bind s.browser ~f:(fun b -> Promise.peek b.url)) )
          ; "error", string_or_null s.error
          ; ("expired", if s.expired then `True else `False)
          ])
;;

let still_signing_in t (s : Signin.t) =
  match t.state with
  | Signin s' -> phys_equal s s'
  | Checking _ | Form _ | Connected _ -> false
;;

let remember t remote grant =
  match t.config.remember_signin remote grant with
  | Ok () -> ()
  | Error e ->
    t.log (sprintf "cannot save the sign-in: %s" (Error.to_string_hum e))
;;

let signed_in_remote t (c : Connected.t) =
  t.state <- Connected c;
  t.on_page (remote_page t c.remote);
  `Object [ "ok", `True; "remote", `String (remote_url t c.remote) ]
;;

let failed error = `Object [ "ok", `False; "error", `String error ]

(* The grant's cookie for the site, and what was waiting on it. *)
let granted t (s : Signin.t) (grant : Browser_signin.Grant.t) =
  t.log "signed in through the browser";
  let cookie = Some grant.proxy_cookie in
  match s.pending with
  | Connected c ->
    let remote = Backend_address.with_proxy_cookie c.remote cookie in
    remember t remote grant;
    signed_in_remote t { c with remote }
  | Unchecked { remote; credentials; save } ->
    let remote = Backend_address.with_proxy_cookie remote cookie in
    let credentials : Web_connect.Credentials.t =
      match credentials.token with
      | Some _ -> credentials
      | None -> { token = grant.token; user = grant.user }
    in
    (match
       Web_connect.Check.run
         ~env:t.env
         ~timeout:t.check_timeout
         remote
         credentials
     with
     | Ok () ->
       let c = connected t ~remote ~credentials ~save in
       remember t remote grant;
       signed_in_remote t c
     | Error (Signin_required why) ->
       let error =
         sprintf
           "Signed in through the browser, but %s still asks for a sign-in: \
            %s. Sign in again."
           (site remote)
           why
       in
       t.log error;
       s.error <- Some error;
       failed error
     | Error error ->
       let error =
         Web_connect.Check.explain error remote ~source:Form credentials
       in
       t.log error;
       t.state
       <- Form
            { address = Some (Backend_address.to_string remote)
            ; user = credentials.user
            ; error = Some error
            };
       failed error)
;;

let start_browser t (s : Signin.t) =
  let url, set_url = Promise.create () in
  let ended, set_ended = Promise.create () in
  let cancelled, cancel = Promise.create () in
  let attempt = { Browser_attempt.url; ended; cancel } in
  s.browser <- Some attempt;
  s.error <- None;
  t.log "signing in through the browser";
  Fiber.fork ~sw:t.sw (fun () ->
    let outcome =
      Fiber.first
        (fun () ->
           match
             Browser_signin.run
               ~env:t.env
               ~program:Desktop
               ~on_url:(fun ~url ~port:_ -> Promise.resolve set_url url)
               (Pending.remote s.pending)
           with
           | result -> `Ended result
           | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
           | exception exn -> `Ended (Error (Exn.to_string exn)))
        (fun () ->
           Promise.await cancelled;
           `Cancelled)
    in
    s.browser <- None;
    let answer =
      match (outcome : [ `Cancelled | `Ended of _ ]) with
      | _ when not (still_signing_in t s) ->
        `Object [ "ok", `False; "cancelled", `True ]
      | `Cancelled ->
        t.log "the sign-in through the browser was cancelled";
        `Object [ "ok", `False; "cancelled", `True ]
      | `Ended (Error why) ->
        let error =
          sprintf
            "Signing in through the browser did not work: %s. Try again, or \
             sign in here."
            why
        in
        t.log error;
        s.error <- Some error;
        failed error
      | `Ended (Ok grant) ->
        (match granted t s grant with
         | answer -> answer
         | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
         | exception exn ->
           let error =
             sprintf "Signed in, but starting failed: %s" (Exn.to_string exn)
           in
           t.log error;
           s.error <- Some error;
           failed error)
    in
    Promise.resolve set_ended answer);
  attempt
;;

let open_in_browser t url = t.on_page { (Page.here url) with external_ = true }

let cancel_browser (s : Signin.t) =
  Option.iter s.browser ~f:(fun b ->
    ignore (Promise.try_resolve b.cancel () : bool))
;;

let signin t flow json =
  let respond = respond_json flow in
  match t.state, field json "how" with
  | Signin s, Some "browser" ->
    let attempt =
      match s.browser with
      | Some attempt -> attempt
      | None ->
        let attempt = start_browser t s in
        Fiber.any
          [ (fun () -> open_in_browser t (Promise.await attempt.url))
          ; (fun () -> ignore (Promise.await attempt.ended : Json.t))
          ];
        attempt
    in
    respond (Promise.await attempt.ended)
  | Signin s, Some "again" ->
    (match Option.bind s.browser ~f:(fun b -> Promise.peek b.url) with
     | Some url ->
       open_in_browser t url;
       respond (`Object [ "ok", `True ])
     | None ->
       respond
         (failed "No sign-in through the browser is under way: start one."))
  | Signin s, Some "cancel" ->
    cancel_browser s;
    respond (`Object [ "ok", `True ])
  | Signin s, Some "here" ->
    cancel_browser s;
    t.log "signing in in the window";
    let c =
      match s.pending with
      | Connected c -> c
      | Unchecked { remote; credentials; save } ->
        connected ~tool_host:false t ~remote ~credentials ~save
    in
    t.state <- Connected c;
    t.on_page (remote_page ~here:true t c.remote);
    respond (`Object [ "ok", `True; "remote", `String (remote_url t c.remote) ])
  | Signin _, _ ->
    respond (failed {|"how": "browser", "again", "cancel" or "here" expected|})
  | (Checking _ | Form _ | Connected _), _ ->
    respond (failed "Not signing in now: reload the page.")
;;

let form_connect t flow json =
  let failed ?(plaintext = false) error =
    respond_json
      flow
      (`Object
          ([ "ok", `False; "error", `String error ]
           @ if plaintext then [ "plaintext", `True ] else []))
  in
  match t.state with
  | Checking _ -> failed "Already connecting: wait for it to finish."
  | Connected { remote; _ } ->
    failed
      (sprintf
         "Already connected to %s: reload the page."
         (Backend_address.to_string remote))
  | Signin s ->
    failed
      (sprintf
         "Signing in to %s: reload the page."
         (site (Pending.remote s.pending)))
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
        | Ok remote when plaintext remote && not (confirmed json "plaintext") ->
          failed ~plaintext:true (plaintext_warning remote)
        | Ok remote ->
          (match
             connect
               t
               ~remote
               ~credentials:{ token; user }
               ~save:
                 (Some
                    { Saved_connection.connect = address
                    ; token
                    ; user
                    ; proxy_cookie = None
                    })
           with
           | Ok (Code code) ->
             respond_json flow (`Object [ "ok", `True; "code", `String code ])
           | Ok (Remote url) ->
             respond_json flow (`Object [ "ok", `True; "remote", `String url ])
           | Ok Signin ->
             respond_json flow (`Object [ "ok", `True; "signin", `True ])
           | Error error -> failed error)))
;;

let max_body = 64 * 1024

let handle t (request : Web_server.Request.t) ~reader flow =
  let length =
    Option.bind
      (Web_server.Request.header request "Content-Length")
      ~f:(fun n -> Int.of_string_opt (String.strip n))
  in
  let json_body =
    match Web_server.Request.header request "Content-Type" with
    | Some content_type ->
      String.Caseless.equal
        (String.strip (List.hd_exn (String.split content_type ~on:';')))
        "application/json"
    | None -> false
  in
  match request.meth, length with
  | "POST", _ when not json_body ->
    respond_text
      flow
      ~status:415
      ~reason:"Unsupported Media Type"
      "a JSON body (Content-Type: application/json) expected\n"
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
      | Some "status" -> status t flow ~signin:(confirmed json "signin")
      | Some "signin" -> signin t flow json
      | Some "connect" -> form_connect t flow json
      | Some "quit" ->
        respond_json flow (`Object [ "ok", `True ]);
        t.log "quit from the page";
        ignore (Promise.try_resolve t.quit () : bool)
      | _ -> respond_text flow ~status:404 ~reason:"Not Found" "not found\n")
  | "POST", _ ->
    respond_text flow ~status:400 ~reason:"Bad Request" "a JSON body expected\n"
  | _ ->
    respond_text
      flow
      ~status:405
      ~reason:"Method Not Allowed"
      "method not allowed\n"
;;

let routes t (request : Web_server.Request.t) =
  if String.is_prefix request.path ~prefix:path_prefix
  then Some (handle t)
  else (
    match t.state with
    | Connected { remote; relay = Some { handoff; _ } } ->
      (match Signin_handoff.route handoff request with
       | Some handler -> Some handler
       | None -> Web_proxy.routes ~env:t.env ~log:t.log remote request)
    | Connected { relay = None; _ } | Checking _ | Form _ | Signin _ -> None)
;;

(* The relayed sockets go to the backend once connected; before that there
   is nothing to relay to. *)
let websocket t path ~query ws =
  match t.state with
  | Connected { relay = Some { websockets; _ }; _ } ->
    Option.iter
      (List.Assoc.find websockets ~equal:String.equal path)
      ~f:(fun f -> f ~query ws)
  | Connected { relay = None; _ } | Checking _ | Form _ | Signin _ -> ()
;;

let on_lines t ~read_line ~write_line =
  Web_connect.not_a_backend
    ~what:"prigh desktop"
    ~connect_to:
      (match t.state with
       | Connected { remote; _ } -> Backend_address.to_string remote
       | Signin s -> Backend_address.to_string (Pending.remote s.pending)
       | Checking _ | Form _ -> "the backend prigh desktop's window connects to")
    ~read_line
    ~write_line
;;

let new_key () =
  Mirage_crypto_rng_unix.use_default ();
  Ohex.encode (Mirage_crypto_rng.generate 16)
;;

(* Until stdin ends, or fails (the shell is gone either way). *)
let drain stdin =
  let buf = Cstruct.create 4096 in
  let rec loop () =
    match Eio.Flow.single_read stdin buf with
    | (_ : int) -> loop ()
    | exception (End_of_file | Eio.Io _) -> ()
  in
  loop ()
;;

let run
      ~env
      ?(tool_host_wait = Time_ns.Span.of_sec 10.)
      ?(check_timeout = Time_ns.Span.of_sec 15.)
      ?(stopped = Fiber.await_cancel)
      ~log
      ~on_page
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
           ; on_page
           ; page_url = ""
           }
         in
         let addr, port =
           Web_connect.Listen.bind config.listen (fun ~addr ~port ->
             Web_server.listen
               ~env
               ~sw
               ~local:true
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
         let url =
           Web_server.browser_url ~host:(Web_connect.browser_host addr) ~port
         in
         t.page_url <- url ^ "#desktop=" ^ t.key;
         (match config.initial with
          | Form { address; user; error } ->
            t.state <- Form { address; user; error }
          | Check { remote; credentials; source; save } ->
            let prefix =
              match source with
              | Flag -> "-connect did not work: "
              | Env -> "$PRIGH_CONNECT did not work: "
              | Saved file ->
                sprintf "The saved connection (%s) did not work: " file
              | Form -> ""
            in
            (match config.ui with
             | Local ->
               (* [connect] is [Checking] before it first waits, so before
                  the page can ask. *)
               Fiber.fork ~sw (fun () ->
                 ignore
                   (connect t ~prefix ~remote ~credentials ~save
                    : (Next.t, string) Result.t))
             | Remote ->
               (* The window goes straight to the backend when it answers
                  (said by [connect]); the page here only when it does
                  not, to show the form. *)
               ignore
                 (connect t ~prefix ~remote ~credentials ~save
                  : (Next.t, string) Result.t)));
         (* The key only to the shell: logs end up in files. *)
         log (sprintf "prigh-web on %s" url);
         (match t.state with
          | Connected { relay = None; _ } -> ()
          | Connected { relay = Some _; _ } | Checking _ | Form _ | Signin _ ->
            on_page (Page.here t.page_url));
         Fiber.await_cancel ()))
    (fun () ->
       Fiber.any
         [ (fun () ->
             Promise.await quit;
             Ended.Quit)
         ; (fun () ->
             drain stdin;
             Ended.Stdin_closed)
         ; (fun () ->
             stopped ();
             Ended.Stopped)
         ])
;;
