open! Core
open! Async
module Client = Prigh_client.Client
module Transport = Prigh_client.Transport
module Server_link = Prigh_ui.Server_link
module Saved_connection = Prigh_client_unix.Saved_connection
module Browser_signin = Prigh_client_unix.Browser_signin
module Websocket_transport = Prigh_client_unix.Websocket_transport
module Tool_host = Prigh_client_unix.Tool_host

module Target = struct
  type t =
    | Spawn
    | Remote of string
  [@@deriving sexp_of, equal]

  let describe = function
    | Spawn -> "a local backend"
    | Remote address -> Prigh_ui.Server_address.display_string address
  ;;
end

module Endpoint = struct
  type t =
    | Process of
        { prog : string
        ; args : string list
        }
    | Tcp of
        { host : string
        ; port : int
        }
    | Websocket of
        { url : string
        ; address : Prigh_ui.Server_address.t
        ; headers : (string * string) list
        }
  [@@deriving sexp_of]

  let open_ = function
    | Process { prog; args } ->
      Prigh_client_unix.Stdio_transport.spawn ~prog ~args ()
    | Tcp { host; port } -> Prigh_client_unix.Tcp_transport.connect ~host ~port
    | Websocket { url; address; headers } ->
      Prigh_client_unix.Websocket_transport.connect
        ~headers
        ~url
        ~tls:address.tls
        ~host:address.host
        ~port:address.port
        ~authority:(Prigh_ui.Server_address.authority address)
        ~path:(Prigh_ui.Server_address.websocket_path address)
        ()
  ;;
end

module Tools = struct
  type t =
    | Local
    | Remote
  [@@deriving sexp_of]

  let of_string = function
    | "local" -> Ok Local
    | "remote" -> Ok Remote
    | other -> Or_error.errorf "-tools must be local or remote, got %s" other
  ;;
end

let connection_hello ?home hello ~host_id =
  match host_id with
  | None -> hello @ [ "tools", `False ]
  | Some id ->
    hello
    @ Option.value_map home ~default:[] ~f:(fun home ->
      [ "home", `String home ])
    @ [ "tools", `True
      ; "host_id", `String id
      ; ( "features"
        , `Array
            [ `String "exec_input"
            ; `String "terminal_kill"
            ; `String "git_location"
            ] )
      ]
;;

let hello_params hello ~session ~as_user =
  let set name value params =
    match value with
    | None -> params
    | Some v ->
      List.Assoc.remove params ~equal:String.equal name @ [ name, `String v ]
  in
  hello
  |> List.filter ~f:(fun (name, _) -> not (String.equal name "as_user"))
  |> set "session" session
  |> set "as_user" as_user
;;

let send_hello client hello ~session ~as_user =
  Deferred.map
    (Client.call client "hello" (hello_params hello ~session ~as_user))
    ~f:(Result.map_error ~f:Error.to_string_hum)
;;

(* Our tool host's executions that are still running carry on: the backend
   keeps them for a host that comes back with them, and what the worker
   answered meanwhile goes out once the backend knows us again. *)
let reconnect_client ?tool_host client ~hello ~delay_ms ~session ~as_user =
  let%bind () = Clock.after (Time_float.Span.of_ms (Float.of_int delay_ms)) in
  match%bind Client.connect client with
  | Error e -> return (Error (Error.to_string_hum e))
  | Ok () ->
    let running =
      Option.value_map tool_host ~default:[] ~f:Tool_host.running_execs
    in
    let hello =
      if List.is_empty running
      then hello
      else
        hello
        @ [ "running_execs", `Array (List.map running ~f:(fun id -> `String id))
          ]
    in
    let%map reply = send_hello client hello ~session ~as_user in
    if Result.is_ok reply
    then Option.iter tool_host ~f:(fun host -> Tool_host.set_online host true);
    reply
;;

type t =
  { home : string
  ; backend : string
  ; spawn_args : string list
  ; tools : Tools.t option
  ; headers : (string * string) list
  ; hello : (string * Prigh_protocol.Json.t) list
  ; open_transport : Endpoint.t -> Transport.t Deferred.Or_error.t
  ; browser_signin :
      connect:string
      -> on_url:
           (url:string -> port:int -> paste:Browser_signin.Code_input.t option -> unit)
      -> on_paste_refused:(string -> unit)
      -> (Browser_signin.Grant.t, string) Result.t Deferred.t
  ; mutable code_input : Browser_signin.Code_input.t option
    (** where a code pasted for the browser sign-in under way goes *)
  ; mutable code_prompt : Server_link.Code_prompt.t -> unit
  ; open_url : string -> unit
  ; mutable proxy_cookie : (string * string) option
    (** the address, and the [Cookie] its sign-in proxy wants *)
  ; spawn_worker : unit -> Transport.t Deferred.Or_error.t
  ; client : Client.t
  ; mutable target : Target.t
  ; mutable token : string option
  ; mutable user : string option
  ; mutable session : string option
  ; mutable saved : Saved_connection.t option
  ; mutable host_id : string option
  ; mutable tool_host : Tool_host.t option
  ; mutable pending : Transport.t option
    (** opened by [relink], for the client's next [connect] *)
  ; mutable switches : int
  ; mutable dropped : int
    (** connections dropped whose [Closed] has not come through yet *)
  ; mutable notice : string -> unit
  ; held_notices : string Queue.t
  }

let client t = t.client
let tool_host t = t.tool_host

let info t : Server_link.t =
  { address =
      (match t.target with
       | Spawn -> None
       | Remote address -> Some address)
  ; user = t.user
  ; saved = Option.map t.saved ~f:(fun (s : Saved_connection.t) -> s.connect)
  }
;;

let notify t text = t.notice text

let set_notice t f =
  t.notice <- f;
  Queue.iter t.held_notices ~f;
  Queue.clear t.held_notices
;;

let set_code_prompt t f = t.code_prompt <- f

let signin_code t code =
  Option.iter t.code_input ~f:(fun input ->
    match code with
    | Some code -> Browser_signin.Code_input.send input code
    | None -> Browser_signin.Code_input.cancel input)
;;

let endpoint t ~session : Target.t -> Endpoint.t Or_error.t = function
  | Spawn ->
    Ok
      (Process
         { prog = t.backend
         ; args =
             (t.spawn_args
              @
              match session with
              | Some s -> [ "-session"; s ]
              | None -> [])
         })
  | Remote address ->
    (match Prigh_ui.Server_address.parse address with
     | Ok { lines = true; host; port; _ } -> Ok (Tcp { host; port })
     | Ok parsed ->
       let cookie =
         match t.proxy_cookie with
         | Some (for_, cookie) when String.equal for_ address ->
           [ "Cookie", cookie ]
         | Some _ | None -> []
       in
       Ok
         (Websocket
            { url = address; address = parsed; headers = t.headers @ cookie })
     | Error e -> Or_error.error_string e)
;;

let open_target ?(session = None) t target =
  match endpoint t ~session target with
  | Error e -> return (Error (`Failed e))
  | Ok endpoint ->
    (match%map t.open_transport endpoint with
     | Ok transport -> Ok transport
     | Error e ->
       (match Websocket_transport.signin_required e with
        | Some why -> Error (`Signin_required why)
        | None -> Error (`Failed e)))
;;

let open_error = function
  | `Failed e -> e
  | `Signin_required why -> Error.of_string (Server_link.proxy_signin_error why)
;;

let tools_here t =
  match t.tools, t.target with
  | Some Local, _ | None, Remote _ -> true
  | Some Remote, _ | None, Spawn -> false
;;

let host_id t =
  match t.host_id with
  | Some id -> id
  | None ->
    let id = Prigh_client_unix.Host_id.choose ~home:t.home ~warn:(notify t) in
    t.host_id <- Some id;
    id
;;

let hello t =
  let opt name =
    Option.value_map ~default:[] ~f:(fun v -> [ name, `String v ])
  in
  connection_hello
    ~home:t.home
    (t.hello @ opt "token" t.token @ opt "user" t.user @ opt "session" t.session)
    ~host_id:(if tools_here t then Some (host_id t) else None)
;;

let new_tool_host t =
  if tools_here t
  then
    Some
      (Tool_host.create
         ~on_exit:(fun text -> notify t text)
         ~client:t.client
         ~spawn:t.spawn_worker
         ())
  else None
;;

let open_url url =
  let prog =
    if Sys_unix.file_exists_exn "/usr/bin/open" then "open" else "xdg-open"
  in
  don't_wait_for
    (Process.run ~prog ~args:[ url ] ()
     |> Deferred.map ~f:(fun (_ : string Or_error.t) -> ()))
;;

let create
      ?(open_transport = Endpoint.open_)
      ?browser_signin
      ?(open_url = open_url)
      ?spawn_worker
      ?(headers = [])
      ~home
      ~backend
      ~spawn_args
      ~tools
      ~hello
      ~session
      ~target
      ~token
      ~user
      ~saved
      ()
  =
  let self = ref None in
  let client =
    Client.create ~connect:(fun () ->
      let t = Option.value_exn !self in
      match t.pending with
      | Some transport ->
        t.pending <- None;
        return (Ok transport)
      | None ->
        open_target t t.target ~session:t.session
        >>| Result.map_error ~f:open_error)
  in
  let held_notices = Queue.create () in
  let t =
    { home
    ; backend
    ; spawn_args
    ; tools
    ; headers
    ; hello
    ; open_transport
    ; browser_signin =
        Option.value
          browser_signin
          ~default:(fun ~connect ~on_url ~on_paste_refused ->
            Browser_signin.run
              ~backend
              ~connect
              ~paste:(Browser_signin.paste_wanted ~getenv:Sys.getenv)
              ~on_url
              ~on_paste_refused)
    ; code_input = None
    ; code_prompt = ignore
    ; open_url
    ; proxy_cookie =
        (match (target : Target.t) with
         | Spawn -> None
         | Remote address ->
           Option.map
             (Saved_connection.proxy_cookie saved ~connect:(Some address))
             ~f:(fun cookie -> address, cookie))
    ; spawn_worker =
        Option.value spawn_worker ~default:(Tool_host.spawn_worker ~backend)
    ; client
    ; target
    ; token
    ; user
    ; session
    ; saved
    ; host_id = None
    ; tool_host = None
    ; pending = None
    ; switches = 0
    ; dropped = 0
    ; notice = Queue.enqueue held_notices
    ; held_notices
    }
  in
  self := Some t;
  t.tool_host <- new_tool_host t;
  t
;;

let greet t connected : Server_link.Outcome.result Deferred.t =
  match%bind connected with
  | Error e
    when Option.is_some
           (Server_link.proxy_signin_of_error (Error.to_string_hum e)) ->
    return
      (Server_link.Outcome.Signin_required
         (Option.value_exn
            (Server_link.proxy_signin_of_error (Error.to_string_hum e))))
  | Error e ->
    return
      (Server_link.Outcome.Unreachable
         (sprintf
            "cannot reach %s: %s"
            (Target.describe t.target)
            (Error.to_string_hum e)))
  | Ok () ->
    (match%map send_hello t.client (hello t) ~session:None ~as_user:None with
     | Ok reply ->
       Option.iter t.tool_host ~f:(fun host -> Tool_host.set_online host true);
       Server_link.Outcome.Connected reply
     | Error e -> Rejected e)
;;

let start t = greet t (Client.connect t.client)

(* A connection attempt from before the switch (a reconnect) may still be
   under way: what it brings is replaced by the transport the switch
   opened. *)
let rec connect_pending t =
  let%bind result = Client.connect t.client in
  match t.pending, result with
  | None, _ -> return result
  | Some _, Error _ -> connect_pending t
  | Some _, Ok () ->
    Client.disconnect t.client;
    let%bind () = Client.disconnected t.client in
    connect_pending t
;;

let reconnect t ~delay_ms ~session ~as_user =
  let switches = t.switches in
  let superseded () =
    if t.switches = switches
    then None
    else Some (Error "superseded by another connection")
  in
  let%bind () = Clock.after (Time_float.Span.of_ms (Float.of_int delay_ms)) in
  match superseded () with
  | Some error -> return error
  | None ->
    (match%bind Client.connect t.client with
     | Error e -> return (Error (Error.to_string_hum e))
     | Ok () ->
       (match superseded () with
        | Some error -> return error
        | None ->
          reconnect_client
            ?tool_host:t.tool_host
            t.client
            ~hello:(hello t)
            ~delay_ms:0
            ~session
            ~as_user))
;;

let save t (saved : Saved_connection.t) =
  let saved =
    { saved with
      proxy_cookie =
        (match t.proxy_cookie with
         | Some (address, cookie) when String.equal address saved.connect ->
           Some cookie
         | Some _ | None -> None)
    }
  in
  if not (Option.equal Saved_connection.equal (Some saved) t.saved)
  then (
    match Saved_connection.save ~home:t.home saved with
    | Ok () -> t.saved <- Some saved
    | Error e ->
      notify
        t
        (sprintf "could not save the connection: %s" (Error.to_string_hum e)))
;;

let drop_connection t =
  t.switches <- t.switches + 1;
  Option.iter t.tool_host ~f:Tool_host.close;
  t.tool_host <- None;
  if Client.is_connected t.client then t.dropped <- t.dropped + 1;
  Client.disconnect t.client;
  Client.disconnected t.client
;;

(* The proxy's cookie for [address], and the page's prigh credentials when we
   have none. *)
let sign_in_through_browser t address =
  let%map result =
    t.browser_signin
      ~connect:address
      ~on_url:(fun ~url ~port ~paste ->
        notify
          t
          (match paste with
           | None ->
             sprintf
               "sign in through the browser at %s (opening it; over SSH, \
                forward the port first: ssh -L %d:127.0.0.1:%d …)"
               url
               port
               port
           | Some _ ->
             sprintf
               "sign in through the browser at %s, then paste the code it \
                shows after Allow (or forward the port: ssh -L \
                %d:127.0.0.1:%d …)"
               url
               port
               port);
        t.open_url url;
        Option.iter paste ~f:(fun input ->
          t.code_input <- Some input;
          t.code_prompt Wanted))
      ~on_paste_refused:(fun why -> t.code_prompt (Refused why))
  in
  t.code_input <- None;
  Result.map result ~f:(fun (grant : Browser_signin.Grant.t) ->
    t.proxy_cookie <- Some (address, grant.proxy_cookie);
    let token, user =
      match t.target, t.token with
      | Remote current, Some token when String.equal current address ->
        Some token, t.user
      | _ -> grant.token, grant.user
    in
    token, user)
;;

let relink t (change : Server_link.Change.t) : Server_link.Outcome.t Deferred.t =
  let unreachable e =
    return { Server_link.Outcome.link = info t; result = Unreachable e }
  in
  let switch =
    match change, t.target with
    | Connect None, _ -> Ok (`Now (Target.Spawn, None, None, None))
    | Connect (Some address), _ ->
      let { Saved_connection.Resolved.settings; save } =
        Saved_connection.resolve
          ~saved:t.saved
          ~flags:{ connect = Some address; token = None; user = None }
          ~env:{ connect = None; token = None; user = None }
          ~local:false
      in
      Ok (`Now (Target.Remote address, settings.token, settings.user, save))
    | Sign_in { user; token }, Remote address ->
      Ok
        (`Now
            ( Target.Remote address
            , Some token
            , user
            , Some
                { Saved_connection.connect = address
                ; token = Some token
                ; user
                ; proxy_cookie = None
                } ))
    | Browser_signin, Remote address -> Ok (`Browser address)
    | Sign_in _, Spawn ->
      Error "prigh-tui started this backend: there is nothing to sign in to"
    | Browser_signin, Spawn ->
      Error
        "prigh-tui started this backend: there is no sign-in proxy before it"
  in
  let%bind switch =
    match switch with
    | Error e -> return (Error (`Unreachable e))
    | Ok (`Now switch) -> return (Ok switch)
    | Ok (`Browser address) ->
      (match%map sign_in_through_browser t address with
       | Error e -> Error (`Not_signed_in e)
       | Ok (token, user) ->
         Ok
           ( Target.Remote address
           , token
           , user
           , Some
               { Saved_connection.connect = address
               ; token
               ; user
               ; proxy_cookie = None
               } ))
  in
  match switch with
  | Error (`Not_signed_in e) ->
    return
      { Server_link.Outcome.link = info t
      ; result =
          Signin_required (sprintf "not signed in through the browser: %s" e)
      }
  | Error (`Unreachable e) -> unreachable e
  | Ok (target, token, user, to_save) ->
    (match%bind open_target t target with
     | Error (`Signin_required why) ->
       return
         { Server_link.Outcome.link = info t; result = Signin_required why }
     | Error (`Failed e) ->
       unreachable
         (sprintf
            "cannot reach %s: %s"
            (Target.describe target)
            (Error.to_string_hum e))
     | Ok transport ->
       let%bind () = drop_connection t in
       t.target <- target;
       t.token <- token;
       t.user <- user;
       t.session <- None;
       t.pending <- Some transport;
       t.tool_host <- new_tool_host t;
       let%map result = greet t (connect_pending t) in
       (match result with
        | Connected _ -> Option.iter to_save ~f:(save t)
        | Unreachable _ | Rejected _ | Signin_required _ -> ());
       { Server_link.Outcome.link = info t; result })
;;

let sign_out t =
  match t.target with
  | Spawn ->
    return
      (Error "prigh-tui started this backend: there is nothing to sign out of")
  | Remote address ->
    t.token <- None;
    save
      t
      { connect = address; token = None; user = t.user; proxy_cookie = None };
    let%map () = drop_connection t in
    Ok ()
;;

(* A connected client's transport is detached before its [Closed] is
   pushed, so each one dropped sends exactly one, which may come after the
   next connection's events: the UI must not see it, whenever it comes. *)
let handle_incoming t (incoming : Client.Incoming.t) =
  match incoming, t.tool_host with
  | Closed, _ when t.dropped > 0 ->
    t.dropped <- t.dropped - 1;
    None
  | Event e, Some host ->
    Tool_host.handle host e;
    Some incoming
  | Closed, Some host ->
    Tool_host.set_online host false;
    Some incoming
  | _ -> Some incoming
;;

let close t =
  Option.iter t.tool_host ~f:Tool_host.close;
  Deferred.any_unit
    [ Client.close t.client; Clock.after (Time_float.Span.of_sec 3.) ]
;;
