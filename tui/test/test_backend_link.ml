(* The terminal's connection management ([Backend_link]) against in-memory
   backends: which transport each change opens, the hello it sends, what is
   saved, and the tool host that goes with it. A temporary HOME. *)
open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async
module Link = Prigh_ui_term.Backend_link
module Client = Prigh_client.Client
module Transport = Prigh_client.Transport
module Json = Prigh_protocol.Json
module Saved_connection = Prigh_client_unix.Saved_connection

(* Backends by address: the credentials each accepts ([None]: anything; a
   spawned backend asks for none). [Some (None, t)]: a [-token t] server,
   without users. One that is down refuses the connection. *)
module Servers = struct
  type t =
    { accepts : string -> (string option * string) option option
    ; mutable down : String.Set.t
    ; mutable backends : Transport.In_memory.Backend.t list
    ; mutable hold : unit Ivar.t option (** connections wait for it *)
    }

  let address : Link.Endpoint.t -> string = function
    | Process _ -> "spawned"
    | Tcp { host; port } -> sprintf "%s:%d" host port
  ;;

  let param params name =
    match Option.bind params ~f:(Jsonaf.member name) with
    | Some (`String s) -> Some s
    | _ -> None
  ;;

  let hello_line ~home params =
    let line = Json.to_string (Option.value params ~default:`Null) in
    let file = Prigh_client_unix.Host_id.file ~home in
    if Sys_unix.file_exists_exn file
    then
      String.substr_replace_all
        line
        ~pattern:(String.strip (In_channel.read_all file))
        ~with_:"<host-id>"
    else line
  ;;

  let serve t ~home endpoint backend =
    don't_wait_for
      (Pipe.iter_without_pushback
         (Transport.In_memory.Backend.requests backend)
         ~f:(fun line ->
           let request = Or_error.ok_exn (Json.parse line) in
           let id = Option.value_exn (Jsonaf.member "id" request) in
           let params = Jsonaf.member "params" request in
           let respond ok result =
             Transport.In_memory.Backend.send
               backend
               (sprintf
                  {|{"type":"response","id":%s,"ok":%b,%s}|}
                  (Json.to_string id)
                  ok
                  result)
           in
           match Jsonaf.member "method" request with
           | Some (`String "hello") ->
             printf "%s hello %s\n" (address endpoint) (hello_line ~home params);
             let user = param params "user" in
             let ok =
               match t.accepts (address endpoint) with
               | None -> true
               | Some None -> false
               | Some (Some (want_user, token)) ->
                 Option.equal String.equal (param params "token") (Some token)
                 && Option.for_all want_user ~f:(fun u ->
                   Option.equal String.equal user (Some u))
             in
             if ok
             then
               respond
                 true
                 (sprintf
                    {|"result":{"client_id":"c","namespace":%s,"user":%s}|}
                    (Option.value_map user ~default:"null" ~f:(sprintf "%S"))
                    (Option.value_map user ~default:"null" ~f:(sprintf "%S")))
             else (
               (* As [prigh serve -tokens] does: the answer, then the end. *)
               respond
                 false
                 {|"error":"unauthorised: bad user name or password"|};
               Transport.In_memory.Backend.close backend)
           | _ -> respond true {|"result":null|}))
  ;;

  let open_ t ~home endpoint =
    printf !"open %{sexp: Link.Endpoint.t}\n" endpoint;
    if Set.mem t.down (address endpoint)
    then Deferred.Or_error.error_string "Connection refused"
    else (
      let transport, backend = Transport.In_memory.create () in
      t.backends <- backend :: t.backends;
      serve t ~home endpoint backend;
      let%map () =
        match t.hold with
        | Some hold -> Ivar.read hold
        | None -> return ()
      in
      Ok transport)
  ;;
end

let saved ~home =
  print_s
    [%sexp
      (Or_error.ok_exn (Saved_connection.load ~home)
       : Saved_connection.t option)]
;;

let info link = print_s [%sexp (Link.info link : Prigh_ui.Server_link.t)]

let outcome (o : Prigh_ui.Server_link.Outcome.t) =
  print_s
    [%sexp
      ((match o.result with
        | Unreachable e -> "unreachable", e
        | Rejected e -> "rejected", e
        | Connected _ -> "connected", "")
       : string * string)];
  print_s [%sexp (o.link : Prigh_ui.Server_link.t)]
;;

let make
      ?(tools = None)
      ?(session = None)
      ?(token = None)
      ?(user = None)
      ~home
      ~servers
      ~target
      ~saved
      ()
  =
  Link.create
    ~open_transport:(Servers.open_ servers ~home)
    ~spawn_worker:(fun () -> Deferred.Or_error.error_string "no worker here")
    ~home
    ~backend:"/bin/prigh"
    ~spawn_args:[ "serve"; "-cwd"; "/w" ]
    ~tools
    ~hello:[ "name", `String "laptop" ]
    ~session
    ~target
    ~token
    ~user
    ~saved
    ()
;;

let with_home f = f (Filename_unix.temp_dir "prigh-tui-link" "")

let servers () =
  { Servers.accepts =
      (function
        | "srv:1" -> Some (Some (Some "alice", "pw"))
        | "srv:2" -> Some (Some (None, "t2"))
        | _ -> None)
  ; down = String.Set.of_list [ "srv:9" ]
  ; backends = []
  ; hold = None
  }
;;

let tool_host link =
  printf "tool host: %b\n" (Option.is_some (Link.tool_host link))
;;

let%expect_test "refused at the start, signed in, switched, signed out" =
  with_home
  @@ fun home ->
  let servers = servers () in
  let initial =
    { Saved_connection.connect = "srv:1"; token = None; user = Some "alice" }
  in
  Or_error.ok_exn (Saved_connection.save ~home initial);
  let link =
    make
      ~home
      ~servers
      ~target:(Remote "srv:1")
      ~user:(Some "alice")
      ~saved:(Some initial)
      ()
  in
  let%bind result = Link.start link in
  print_s [%sexp (result : Prigh_ui.Server_link.Outcome.result)];
  tool_host link;
  [%expect
    {|
    open (Tcp (host srv) (port 1))
    srv:1 hello {"name":"laptop","user":"alice","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (Rejected "unauthorised: bad user name or password")
    tool host: true
    |}];
  (* The sign-in dialog's answer: a new connection with these credentials,
     saved once it works. *)
  let%bind o =
    Link.relink link (Sign_in { user = Some "alice"; token = "nope" })
  in
  outcome o;
  saved ~home;
  let%bind o =
    Link.relink link (Sign_in { user = Some "alice"; token = "pw" })
  in
  outcome o;
  saved ~home;
  print_s [%sexp (Client.is_connected (Link.client link) : bool)];
  [%expect
    {|
    open (Tcp (host srv) (port 1))
    srv:1 hello {"name":"laptop","token":"nope","user":"alice","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (rejected "unauthorised: bad user name or password")
    ((address (srv:1))
     (user    (alice))
     (saved   (srv:1)))
    ((
      (connect srv:1)
      (user    alice)))
    open (Tcp (host srv) (port 1))
    srv:1 hello {"name":"laptop","token":"pw","user":"alice","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (connected "")
    ((address (srv:1))
     (user    (alice))
     (saved   (srv:1)))
    ((
      (connect srv:1)
      (token   pw)
      (user    alice)))
    true
    |}];
  (* A server that is down: nothing changes, the connection still works. *)
  let%bind o = Link.relink link (Connect (Some "srv:9")) in
  outcome o;
  let%bind reply = Client.call (Link.client link) "get_state" [] in
  print_s [%sexp (reply : Json.t Or_error.t)];
  [%expect
    {|
    open (Tcp (host srv) (port 9))
    (unreachable "cannot reach srv:9: Connection refused")
    ((address (srv:1))
     (user    (alice))
     (saved   (srv:1)))
    (Ok null)
    |}];
  (* Another server: the saved credentials are for srv:1, so none are sent;
     signing in there replaces the saved connection. *)
  let%bind o = Link.relink link (Connect (Some "srv:2")) in
  outcome o;
  saved ~home;
  let%bind o = Link.relink link (Sign_in { user = None; token = "t2" }) in
  outcome o;
  saved ~home;
  [%expect
    {|
    open (Tcp (host srv) (port 2))
    srv:2 hello {"name":"laptop","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (rejected "unauthorised: bad user name or password")
    ((address (srv:2)) (user ()) (saved (srv:1)))
    ((
      (connect srv:1)
      (token   pw)
      (user    alice)))
    open (Tcp (host srv) (port 2))
    srv:2 hello {"name":"laptop","token":"t2","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (connected "")
    ((address (srv:2)) (user ()) (saved (srv:2)))
    ((
      (connect srv:2)
      (token   t2)))
    |}];
  (* A local backend, as with -local: its tools run there, and the saved
     connection stays. Back to the saved server: its token is used. *)
  let%bind o = Link.relink link (Connect None) in
  outcome o;
  tool_host link;
  let%bind o = Link.relink link (Connect (Some "srv:2")) in
  outcome o;
  tool_host link;
  saved ~home;
  [%expect
    {|
    open (Process (prog /bin/prigh) (args (serve -cwd /w)))
    spawned hello {"name":"laptop","tools":false}
    (connected "")
    ((address ())
     (user    ())
     (saved (srv:2)))
    tool host: false
    open (Tcp (host srv) (port 2))
    srv:2 hello {"name":"laptop","token":"t2","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (connected "")
    ((address (srv:2)) (user ()) (saved (srv:2)))
    tool host: true
    ((
      (connect srv:2)
      (token   t2)))
    |}];
  (* Signing out forgets the token (the address and user stay) and drops the
     connection; reconnecting now sends no token. *)
  let%bind result = Link.sign_out link in
  print_s [%sexp (result : (unit, string) Result.t)];
  saved ~home;
  info link;
  print_s [%sexp (Client.is_connected (Link.client link) : bool)];
  tool_host link;
  let%bind reply =
    Link.reconnect link ~delay_ms:0 ~session:None ~as_user:None
  in
  print_s [%sexp (reply : (Json.t, string) Result.t)];
  [%expect
    {|
    (Ok ())
    (((connect srv:2)))
    ((address (srv:2)) (user ()) (saved (srv:2)))
    false
    tool host: false
    open (Tcp (host srv) (port 2))
    srv:2 hello {"name":"laptop","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (Error "unauthorised: bad user name or password")
    |}];
  let%bind () = Link.close link in
  return ()
;;

let%expect_test
    "a spawned backend: -session only at first, -tools, nothing to sign in to"
  =
  with_home
  @@ fun home ->
  let servers = servers () in
  let link =
    make
      ~home
      ~servers
      ~target:Spawn
      ~session:(Some "/s/1.jsonl")
      ~tools:(Some Link.Tools.Local)
      ~saved:None
      ()
  in
  let%bind result = Link.start link in
  print_s [%sexp (result : Prigh_ui.Server_link.Outcome.result)];
  tool_host link;
  [%expect
    {|
    open (Process (prog /bin/prigh) (args (serve -cwd /w -session /s/1.jsonl)))
    spawned hello {"name":"laptop","session":"/s/1.jsonl","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (Connected (
      (client_id c)
      (namespace null)
      (user      null)))
    tool host: true
    |}];
  let%bind o = Link.relink link (Sign_in { user = None; token = "x" }) in
  outcome o;
  let%bind result = Link.sign_out link in
  print_s [%sexp (result : (unit, string) Result.t)];
  [%expect
    {|
    (unreachable "prigh-tui started this backend: there is nothing to sign in to")
    ((address ())
     (user    ())
     (saved   ()))
    (Error "prigh-tui started this backend: there is nothing to sign out of")
    |}];
  let%bind o = Link.relink link (Connect None) in
  outcome o;
  saved ~home;
  [%expect
    {|
    open (Process (prog /bin/prigh) (args (serve -cwd /w)))
    spawned hello {"name":"laptop","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (connected "")
    ((address ())
     (user    ())
     (saved   ()))
    ()
    |}];
  (* -tools remote: not even a remote backend runs tools here. *)
  let link =
    make
      ~home
      ~servers
      ~target:(Remote "srv:3")
      ~tools:(Some Remote)
      ~saved:None
      ()
  in
  let%bind (_ : Prigh_ui.Server_link.Outcome.result) = Link.start link in
  tool_host link;
  [%expect
    {|
    open (Tcp (host srv) (port 3))
    srv:3 hello {"name":"laptop","tools":false}
    tool host: false
    |}];
  return ()
;;

let%expect_test
    "what the UI sees: the end of a replaced connection is not news; a \
     reconnect overtaken by a switch gives way"
  =
  with_home
  @@ fun home ->
  let servers = servers () in
  let link = make ~home ~servers ~target:(Remote "srv:3") ~saved:None () in
  let%bind (_ : Prigh_ui.Server_link.Outcome.result) = Link.start link in
  let reconnect =
    Link.reconnect link ~delay_ms:50 ~session:None ~as_user:None
  in
  let%bind (_ : Prigh_ui.Server_link.Outcome.t) =
    Link.relink link (Connect (Some "srv:4"))
  in
  (* The UI reads the old connection's end only now, behind the switch. *)
  let seen = ref [] in
  don't_wait_for
    (Pipe.iter_without_pushback
       (Client.incoming (Link.client link))
       ~f:(fun i ->
         Option.iter (Link.handle_incoming link i) ~f:(fun i ->
           seen := i :: !seen)));
  let%bind reply = reconnect in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%sexp (reply : (Json.t, string) Result.t)];
  print_s [%sexp (!seen : Client.Incoming.t list)];
  [%expect
    {|
    open (Tcp (host srv) (port 3))
    srv:3 hello {"name":"laptop","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    open (Tcp (host srv) (port 4))
    srv:4 hello {"name":"laptop","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (Error "superseded by another connection")
    ()
    |}];
  (* The current one closing is. *)
  List.iter servers.backends ~f:Transport.In_memory.Backend.close;
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%sexp (!seen : Client.Incoming.t list)];
  [%expect {| (Closed) |}];
  return ()
;;

let%expect_test
    "a switch while a reconnect is still connecting: the switch's connection \
     wins"
  =
  with_home
  @@ fun home ->
  let servers = servers () in
  let link = make ~home ~servers ~target:(Remote "srv:3") ~saved:None () in
  let%bind (_ : Prigh_ui.Server_link.Outcome.result) = Link.start link in
  List.iter servers.backends ~f:Transport.In_memory.Backend.close;
  let%bind () = Link.client link |> Client.disconnected in
  (* The backend is gone; reconnecting to it hangs. *)
  let hold = Ivar.create () in
  servers.hold <- Some hold;
  let reconnect = Link.reconnect link ~delay_ms:0 ~session:None ~as_user:None in
  let%bind () = after (Time_float.Span.of_ms 20.) in
  servers.hold <- None;
  let relinked = Link.relink link (Connect (Some "srv:4")) in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  Ivar.fill_exn hold ();
  let%bind reply = reconnect in
  let%bind o = relinked in
  print_s [%sexp (reply : (Json.t, string) Result.t)];
  outcome o;
  let%bind state = Client.call (Link.client link) "get_state" [] in
  print_s [%sexp (state : Json.t Or_error.t)];
  [%expect
    {|
    open (Tcp (host srv) (port 3))
    srv:3 hello {"name":"laptop","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    open (Tcp (host srv) (port 3))
    open (Tcp (host srv) (port 4))
    srv:4 hello {"name":"laptop","tools":true,"host_id":"<host-id>","features":["exec_input","terminal_kill"]}
    (Error "superseded by another connection")
    (connected "")
    ((address (srv:4)) (user ()) (saved (srv:4)))
    (Ok null)
    |}];
  return ()
;;
