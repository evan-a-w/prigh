open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let remote ?(user = Some "lloyd") ?(saved = Some "10.0.0.5:7777") address
  : Server_link.t
  =
  { address = Some address; user; saved }
;;

let local ?saved () : Server_link.t = { address = None; user = None; saved }
let rejection = "unauthorised: bad user name or password"

(* The terminal: the platform reports the server, then the hello reply. *)
let on ?(width = 100) ?(height = 14) (link : Server_link.t) =
  let h = Test_app.connected ~width ~height () in
  H.step h (Server_link link);
  H.step h (Hello { client_id = "c1"; namespace = link.user; user = link.user });
  h
;;

let relinked h ?(generation = 1) ?(link = remote "10.0.0.5:7777") result =
  H.step h (Relinked { generation; outcome = { link; result } })
;;

let hello json = Or_error.ok_exn (P.Json.parse json)

let connection (h : H.t) =
  print_s [%sexp (h.model.connection : App.Connection.t)]
;;

let%expect_test "status line: the remote server, next to the user" =
  let h = on (remote "10.0.0.5:7777") in
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  server:10.0.0.5:7777  user:lloyd
    |}];
  (* A spawned backend has no address to show. *)
  H.step h (Server_link (local ()));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  user:lloyd
    |}]
;;

let%expect_test
    "/connect without an address offers the addresses, or asks with the \
     current one typed in"
  =
  let h = on (remote "10.0.0.5:7777") in
  H.keys h "/connect";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /connect ▏
    ▸ 10.0.0.5:7777  the current server
      local          a backend started here, like prigh-tui -local
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Tab accepts · Enter as typed · Esc hides
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Connect to HOST:PORT or local (now: 10.0.0.5:7777) ─────────────────────────────────────────────┐
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    ? 10.0.0.5:7777▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  user:lloyd  Enter submits · Esc cancels
    |}];
  (* Esc: nothing happens. *)
  H.esc h;
  H.mode h;
  [%expect {| editing |}];
  (* On a local backend, the saved address is offered; Enter takes it. *)
  H.step h (Server_link (local ~saved:"10.0.0.5:7777" ()));
  H.keys h "/connect";
  H.enter h;
  H.enter h;
  H.enter h;
  [%expect {| (Relink (generation 1) (change (Connect (10.0.0.5:7777)))) |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      connecting to 10.0.0.5:7777…
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  user:lloyd  connecting…
    |}];
  (* Nothing saved, nothing connected to: an empty prompt; Enter on it does
     nothing. *)
  let h = on (local ()) in
  H.keys h "/connect";
  H.enter h;
  H.enter h;
  H.show h;
  H.enter h;
  H.mode h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Connect to HOST:PORT or local (now: a local backend) ───────────────────────────────────────────┐
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  Enter submits · Esc cancels
    editing
    |}]
;;

let%expect_test "/connect completes local, the current and the saved address" =
  let h = on (remote ~saved:(Some "10.0.0.9:7000") "10.0.0.5:7777") in
  H.keys h "/connect ";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /connect ▏
    ▸ 10.0.0.5:7777  the current server
      10.0.0.9:7000  the saved server
      local          a backend started here, like prigh-tui -local
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Tab accepts · Enter as typed · Esc hides
    |}];
  H.keys h "lo";
  H.key h (Key.plain Tab);
  H.enter h;
  [%expect {| (Relink (generation 1) (change (Connect ()))) |}]
;;

let%expect_test "/connect: a bad address says what to type" =
  let h = on (remote "10.0.0.5:7777") in
  List.iter
    [ "/connect 10.0.0.6"; "/connect host:http"; "/connect a b:1" ]
    ~f:(fun command ->
      H.keys h command;
      H.enter h);
  H.show h;
  connection h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      "10.0.0.6" is not HOST:PORT (e.g. 127.0.0.1:7777); /connect HOST:PORT (of prigh serve -listen), or
      /connect local
      "host:http" is not HOST:PORT (e.g. 127.0.0.1:7777); /connect HOST:PORT (of prigh serve -listen),
      or /connect local
      "a b:1" is not HOST:PORT (e.g. 127.0.0.1:7777); /connect HOST:PORT (of prigh serve -listen), or
      /connect local
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  server:10.0.0.5:7777  user:lloyd
    Connected
    |}]
;;

let%expect_test
    "/connect HOST:PORT: everything of the old server goes, the new one's is \
     fetched"
  =
  let h = on (remote "10.0.0.5:7777") in
  H.event
    h
    (Queue_update { steer = [ "steer on the old server" ]; follow_up = [] });
  H.keys h "/connect 10.0.0.6:7777";
  H.enter h;
  [%expect {| (Relink (generation 1) (change (Connect (10.0.0.6:7777)))) |}];
  connection h;
  (* The old connection closing is part of the switch; what the old server
     still sends waits. *)
  H.step h Backend_closed;
  H.event
    h
    (Tool_confirm
       { call_id = "c1"
       ; agent_id = None
       ; name = "bash"
       ; summary = "make"
       ; rules = []
       });
  (* Typing meanwhile is not sent anywhere. *)
  H.keys h "hello";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Switching
      (generation 1)
      (previous   Connected))
    (Append_history hello)



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      connecting to 10.0.0.6:7777…
      not connected yet
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    queued (1) · Alt+Up: edit the last · /queue: edit or remove
      steer      steer on the old server
    > ▏
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  queued:1  to answer:1  connecting…
    |}];
  relinked
    h
    ~link:(remote ~user:None ~saved:(Some "10.0.0.6:7777") "10.0.0.6:7777")
    (Connected (hello {|{"client_id":"c2","namespace":"alice","user":"alice"}|}));
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    |}];
  H.show h;
  [%expect
    {|
      connected to 10.0.0.6:7777 as alice
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    connecting…
    |}];
  H.reply ~quiet:true h Initial_state (state_json ~cwd:"/srv" ());
  H.reply ~quiet:true h Initial_messages "[]";
  H.show h;
  connection h;
  print_s
    [%sexp
      { mode = (Mode.name h.model.mode : string)
      ; queued = (List.length h.model.queued_messages : int)
      ; pending = (List.length h.model.pending_confirms : int)
      ; client_id = (h.model.client_id : string option)
      }];
  [%expect
    {|
      connected to 10.0.0.6:7777 as alice
      prigh in /srv · /help · Esc aborts · Ctrl+C twice quits
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /srv  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  server:10.0.0.6:7777  user:alice
    Connected
    ((mode    editing)
     (queued  0)
     (pending 0)
     (client_id (c2)))
    |}]
;;

let%expect_test
    "/connect to a server that cannot be reached: still on the old one"
  =
  let h = on (remote "10.0.0.5:7777") in
  H.keys h "/connect 10.0.0.6:1";
  H.enter h;
  (* While it tries, another switch waits. *)
  H.keys h "/connect local";
  H.enter h;
  relinked h (Unreachable "cannot reach 10.0.0.6:1: Connection refused");
  H.show h;
  connection h;
  [%expect
    {|
    (Relink (generation 1) (change (Connect (10.0.0.6:1))))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      connecting to 10.0.0.6:1…
      still connecting; wait for it, or Ctrl+C quits
      cannot reach 10.0.0.6:1: Connection refused; still on 10.0.0.5:7777 (is prigh serve -listen
      running there?)
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  server:10.0.0.5:7777  user:lloyd
    Connected
    |}];
  (* An answer to an older switch is ignored. *)
  relinked h ~generation:1 (Connected (hello {|{"client_id":"x"}|}));
  [%expect {| |}];
  (* Lost while reconnecting: the retries go on. *)
  H.step ~quiet:true h Backend_closed;
  H.keys h "/connect 10.0.0.6:1";
  H.enter h;
  relinked h ~generation:3 (Unreachable "cannot reach 10.0.0.6:1: refused");
  connection h;
  [%expect
    {|
    (Relink (generation 3) (change (Connect (10.0.0.6:1))))
    (Reconnect
      (generation 4)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    (Reconnecting
      (attempt    1)
      (generation 4)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/1.jsonl)))
    |}]
;;

let%expect_test "/connect local" =
  let h = on (remote "10.0.0.5:7777") in
  H.keys h "/connect local";
  H.enter h;
  relinked
    h
    ~link:(local ~saved:"10.0.0.5:7777" ())
    (Connected (hello {|{"client_id":"c2"}|}));
  H.reply ~quiet:true h Initial_state (state_json ());
  H.show h;
  [%expect
    {|
    (Relink (generation 1) (change (Connect ())))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    (Rpc (method_ get_pending) (params ()) (tag Pending))









      connected to a local backend
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  (* There is nothing to sign in to or out of. *)
  H.keys h "/signin";
  H.enter h;
  H.keys h "/signout";
  H.enter h;
  H.show h;
  [%expect
    {|
      connected to a local backend
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
      prigh-tui started this backend: there is nothing to sign in to (/connect HOST:PORT for a remote
      one)
      prigh-tui started this backend: there is nothing to sign out of (/connect HOST:PORT for a remote
      one)
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/connect to a server that refuses the saved credentials: the sign-in \
     dialog"
  =
  let h = on (remote "10.0.0.5:7777") in
  H.keys h "/connect 10.0.0.6:7777";
  H.enter h;
  relinked h ~link:(remote ~user:None "10.0.0.6:7777") (Rejected rejection);
  H.show h;
  connection h;
  [%expect
    {|
    (Relink (generation 1) (change (Connect (10.0.0.6:7777))))






    ┌─ Sign in to 10.0.0.6:7777 ───────────────────────────────────────────────────────────────────────┐
    │ user   lloyd                                                                                     │
    │ token  ▏                                                                                         │
    │ unauthorised: bad user name or password                                                          │
    │ prigh serve -tokens: your user name and password; -token: no user                                │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    Tab next field · Enter signs in · Esc closes
    (Disconnected (reason "unauthorised: bad user name or password"))
    |}]
;;

let%expect_test "the sign-in dialog: prefilled, masked, owns the keyboard" =
  let h = on (remote "10.0.0.5:7777") in
  H.keys h "/signin";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Sign in to 10.0.0.5:7777 ───────────────────────────────────────────────────────────────────────┐
    │ user   lloyd                                                                                     │
    │ token  ▏                                                                                         │
    │ prigh serve -tokens: your user name and password; -token: no user                                │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Tab next field · Enter signs in · Esc closes
    |}];
  (* Enter without a token asks for one. *)
  H.enter h;
  (* Keys that mean something else outside do nothing here. *)
  H.key h (Key.ctrl 'o');
  H.keys h "s3cr";
  H.key h (Key.plain Backspace);
  H.keys h "et\n";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Sign in to 10.0.0.5:7777 ───────────────────────────────────────────────────────────────────────┐
    │ user   lloyd                                                                                     │
    │ token  *****▏                                                                                    │
    │ type the token or password                                                                       │
    │ prigh serve -tokens: your user name and password; -token: no user                                │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Tab next field · Enter signs in · Esc closes
    |}];
  (* Tab, Up and Down move between the fields; Ctrl+U empties one. *)
  H.key h (Key.plain Tab);
  H.key h (Key.ctrl 'u');
  H.keys h "bobb";
  H.key h (Key.plain Backspace);
  H.key h (Key.plain Down);
  H.key h (Key.plain Up);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Sign in to 10.0.0.5:7777 ───────────────────────────────────────────────────────────────────────┐
    │ user   bob▏                                                                                      │
    │ token  *****                                                                                     │
    │ type the token or password                                                                       │
    │ prigh serve -tokens: your user name and password; -token: no user                                │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Tab next field · Enter signs in · Esc closes
    |}];
  (* Esc closes it: nothing sent, still connected as before. *)
  H.esc h;
  H.mode h;
  connection h;
  [%expect
    {|
    editing
    Connected
    |}];
  (* [/signin NAME] fills in the user; Enter there goes to the token. *)
  H.keys h "/signin bob";
  H.enter h;
  H.key h (Key.plain Up);
  H.enter h;
  H.keys h "s3cret";
  H.enter h;
  [%expect
    {| (Relink (generation 1) (change (Sign_in (user (bob)) (token <opaque>)))) |}];
  H.show h;
  (* The answer is awaited. *)
  H.keys h "x";
  H.enter h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Sign in to 10.0.0.5:7777 ───────────────────────────────────────────────────────────────────────┐
    │ user   bob                                                                                       │
    │ token  ******▏                                                                                   │
    │ signing in…                                                                                      │
    │ prigh serve -tokens: your user name and password; -token: no user                                │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  server:10.0.0.5:7777  connecting…
    |}];
  (* Refused: the dialog stays, says why and asks for the token again. *)
  relinked
    h
    ~link:(remote ~user:(Some "bob") "10.0.0.5:7777")
    (Rejected rejection);
  H.show h;
  connection h;
  [%expect
    {|
    ┌─ Sign in to 10.0.0.5:7777 ───────────────────────────────────────────────────────────────────────┐
    │ user   bob                                                                                       │
    │ token  ▏                                                                                         │
    │ unauthorised: bad user name or password                                                          │
    │ prigh serve -tokens: your user name and password; -token: no user                                │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    Tab next field · Enter signs in · Esc closes
    (Disconnected (reason "unauthorised: bad user name or password"))
    |}];
  H.keys h "right";
  H.enter h;
  relinked
    h
    ~generation:2
    ~link:(remote ~user:(Some "bob") "10.0.0.5:7777")
    (Connected (hello {|{"client_id":"c2","namespace":"bob","user":"bob"}|}));
  H.reply ~quiet:true h Initial_state (state_json ());
  H.show h;
  connection h;
  [%expect
    {|
    (Relink (generation 2) (change (Sign_in (user (bob)) (token <opaque>))))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    (Rpc (method_ get_pending) (params ()) (tag Pending))









      connected to 10.0.0.5:7777 as bob
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  server:10.0.0.5:7777  user:bob
    Connected
    |}];
  H.keys h "/signin a b";
  H.enter h;
  H.mode h;
  [%expect {| editing |}]
;;

let%expect_test "a sign-in to a server gone meanwhile: said in the dialog" =
  let h = on (remote "10.0.0.5:7777") in
  H.keys h "/signin";
  H.enter h;
  H.keys h "tok";
  H.enter h;
  relinked h (Unreachable "cannot reach 10.0.0.5:7777: Connection refused");
  H.show h;
  connection h;
  [%expect
    {|
    (Relink (generation 1) (change (Sign_in (user (lloyd)) (token <opaque>))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Sign in to 10.0.0.5:7777 ───────────────────────────────────────────────────────────────────────┐
    │ user   lloyd                                                                                     │
    │ token  ▏                                                                                         │
    │ cannot reach 10.0.0.5:7777: Connection refused                                                   │
    │ prigh serve -tokens: your user name and password; -token: no user                                │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Tab next field · Enter signs in · Esc closes
    Connected
    |}]
;;

let%expect_test
    "/signout: forgets the token, drops the server's state, asks to sign in \
     again"
  =
  let h = on (remote "10.0.0.5:7777") in
  H.keys h "/signout";
  H.enter h;
  H.show h;
  connection h;
  [%expect
    {|
    Sign_out





      signed out of 10.0.0.5:7777; the saved token is forgotten
    ┌─ Sign in to 10.0.0.5:7777 ───────────────────────────────────────────────────────────────────────┐
    │ user   lloyd                                                                                     │
    │ token  ▏                                                                                         │
    │ signed out                                                                                       │
    │ prigh serve -tokens: your user name and password; -token: no user                                │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    Tab next field · Enter signs in · Esc closes
    (Disconnected (reason "signed out"))
    |}];
  (* The connection closing is expected: no reconnecting. Esc leaves us
     disconnected, saying how to go on. *)
  H.step h Backend_closed;
  H.esc h;
  H.keys h "hello";
  H.enter h;
  H.keys h "/retry-backend-connection";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Append_history hello)








      signed out of 10.0.0.5:7777; the saved token is forgotten
      not connected: /signin signs in, /connect switches server
      not connected (signed out): /signin signs in, /connect switches server
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    not signed in to 10.0.0.5:7777 · /signin signs in · /connect switches server
    |}];
  (* /signin brings the dialog back, with the user. *)
  H.keys h "/signin";
  H.enter h;
  print_s
    [%sexp
      ((match h.model.mode with
        | Sign_in form -> Some (Sign_in_form.user form)
        | _ -> None)
       : string option)];
  [%expect {| (lloyd) |}];
  (* Ctrl+C quits at once: there is nothing to abort. *)
  H.esc h;
  H.key h (Key.ctrl 'c');
  [%expect {| Quit |}]
;;

let%expect_test "startup: the server refuses the saved credentials" =
  let h = H.create ~width:80 ~height:12 () in
  H.step h (Server_link (remote "10.0.0.5:7777"));
  H.step h (Rejected rejection);
  H.step h Start;
  H.show h;
  [%expect
    {|
    Load_history




    ┌─ Sign in to 10.0.0.5:7777 ───────────────────────────────────────────────────┐
    │ user   lloyd                                                                 │
    │ token  ▏                                                                     │
    │ unauthorised: bad user name or password                                      │
    │ prigh serve -tokens: your user name and password; -token: no user            │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    Tab next field · Enter signs in · Esc closes
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|









    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    not signed in to 10.0.0.5:7777 · /signin signs in · /connect switches server
    |}];
  (* Another server instead. *)
  H.keys h "/connect 10.0.0.6:7777";
  H.enter h;
  relinked
    h
    ~link:(remote ~user:None "10.0.0.6:7777")
    (Connected (hello {|{"client_id":"c2"}|}));
  connection h;
  [%expect
    {|
    (Relink (generation 1) (change (Connect (10.0.0.6:7777))))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    Connected
    |}]
;;

let%expect_test "a reconnect refused: the sign-in dialog, no more retries" =
  let h = on (remote "10.0.0.5:7777") in
  H.step ~quiet:true h Backend_closed;
  H.reply_error h (Reconnect 1) rejection;
  H.show h;
  connection h;
  [%expect
    {|
    ┌─ Sign in to 10.0.0.5:7777 ───────────────────────────────────────────────────────────────────────┐
    │ user   lloyd                                                                                     │
    │ token  ▏                                                                                         │
    │ unauthorised: bad user name or password                                                          │
    │ prigh serve -tokens: your user name and password; -token: no user                                │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    Tab next field · Enter signs in · Esc closes
    (Disconnected (reason "unauthorised: bad user name or password"))
    |}];
  (* Other errors (not about the credentials) still retry. *)
  let h = on (remote "10.0.0.5:7777") in
  H.step ~quiet:true h Backend_closed;
  H.reply_error h (Reconnect 1) "unauthorised: lloyd is not a superuser";
  connection h;
  [%expect
    {|
    (Reconnect
      (generation 2)
      (delay_ms   500)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    (Reconnecting
      (attempt    2)
      (generation 2)
      (delay_ms   500)
      (session (/home/u/.prigh/sessions/1.jsonl)))
    |}]
;;

let%expect_test "in the browser the platform owns the connection" =
  let h = Test_app.connected ~width:100 () in
  H.keys h "/signout";
  H.enter h;
  H.keys h "/connect 10.0.0.6:7777";
  H.enter h;
  H.keys h "/signin";
  H.enter h;
  H.show h;
  [%expect
    {|
    Sign_out



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      /connect is for the terminal UI (prigh-tui); here, /signout signs in again or to another backend
      /signin is for the terminal UI (prigh-tui); here, /signout signs in again or to another backend
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;
