open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* In-process tests for {!Harness_mux}, {!Harness_pool}, per-session routed
   MCP over {!Host_transport}, and {!Rpc_server} pool reuse across catalog
   fetches, main sessions, and {!Composer_subagent}. Zero real subprocesses
   and zero wall-clock sleeps. *)

let str k j = Option.bind (Json.member k j) ~f:Json.string
let id_of j = Option.value (Json.member "id" j) ~default:`Null

let read_json t =
  Option.map (Line_transport.read_line t) ~f:(fun l ->
    Json.parse l |> Or_error.ok_exn)
;;

let write_json t j = Line_transport.write_line t (Json.to_string j)

let peer_read p =
  Option.map (Line_transport.Peer.read_line p) ~f:(fun l ->
    Json.parse l |> Or_error.ok_exn)
;;

let peer_write p j = Line_transport.Peer.write_line p (Json.to_string j)

let base_key ?(host = "h1") ?(adapter = "acp:antigravity") ()
  : Harness_pool.Key.t
  =
  { host
  ; adapter
  ; member = None
  ; login_key = None
  ; no_sandbox = false
  ; argv = [ "agy"; "acp" ]
  ; env_set = []
  ; env_unset = []
  }
;;

(* An ACP peer advertising [sessionCapabilities.close] that handles arbitrary
   interleaved sessions on one connection. *)
let acp_caps_close =
  `Object
    [ "protocolVersion", `Number "1"
    ; ( "agentCapabilities"
      , `Object
          [ "loadSession", `True
          ; "sessionCapabilities", `Object [ "close", `Object [] ]
          ] )
    ]
;;

let%expect_test
    "ACP mux: concurrent sessions share one process with strict routing, \
     permission cancel, and session/close"
  =
  Eio_main.run
  @@ fun _ ->
  Eio.Switch.run
  @@ fun sw ->
  let init_count = ref 0 in
  let closed = Queue.create () in
  let perm_replies = Queue.create () in
  let emptied = ref 0 in
  let s1_prompt_seen, mark_s1_prompt = Eio.Promise.create () in
  let s2_prompt_seen, mark_s2_prompt = Eio.Promise.create () in
  let s1_closed_done, mark_s1_closed = Eio.Promise.create () in
  let s2_closed_done, mark_s2_closed = Eio.Promise.create () in
  let phys =
    Line_transport.Peer.spawn ~sw (fun p ->
      let next_ses = ref 0 in
      let pending_new = Queue.create () in
      let pending_prompt = String.Table.create () in
      let rec loop () =
        match peer_read p with
        | None -> ()
        | Some j ->
          let params = Option.value (Json.member "params" j) ~default:`Null in
          (match str "method" j, Json.member "id" j with
           | Some "initialize", Some id ->
             incr init_count;
             peer_write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"
                   ; "id", id
                   ; "result", acp_caps_close
                   ])
           | Some "session/new", Some id ->
             incr next_ses;
             let sid = sprintf "ses-%d" !next_ses in
             Queue.enqueue pending_new (id, sid);
             (* Answer in reverse order once both arrive to verify wire ID
                rewriting under out-of-order responses. *)
             if Queue.length pending_new = 2
             then (
               let a = Queue.dequeue_exn pending_new in
               let b = Queue.dequeue_exn pending_new in
               List.iter [ b; a ] ~f:(fun (rid, s) ->
                 peer_write
                   p
                   (`Object
                       [ "jsonrpc", `String "2.0"
                       ; "id", rid
                       ; "result", `Object [ "sessionId", `String s ]
                       ])))
           | Some "session/prompt", Some id ->
             let sid = Option.value_exn (str "sessionId" params) in
             Hashtbl.set pending_prompt ~key:sid ~data:id;
             if String.equal sid "ses-1"
             then (
               peer_write
                 p
                 (`Object
                     [ "jsonrpc", `String "2.0"
                     ; "method", `String "session/update"
                     ; ( "params"
                       , `Object
                           [ "sessionId", `String "ses-1"
                           ; "update", `Object [ "tag", `String "u-for-1" ]
                           ] )
                     ]);
               (* Unanswered permission on ses-1, plus one cancelled via
                  $/cancel_request. *)
               peer_write
                 p
                 (`Object
                     [ "jsonrpc", `String "2.0"
                     ; "id", `String "perm-cancel"
                     ; "method", `String "session/request_permission"
                     ; "params", `Object [ "sessionId", `String "ses-1" ]
                     ]);
               peer_write
                 p
                 (`Object
                     [ "jsonrpc", `String "2.0"
                     ; "method", `String "$/cancel_request"
                     ; "params", `Object [ "requestId", `String "perm-cancel" ]
                     ]);
               peer_write
                 p
                 (`Object
                     [ "jsonrpc", `String "2.0"
                     ; "id", `String "perm-dangle"
                     ; "method", `String "session/request_permission"
                     ; "params", `Object [ "sessionId", `String "ses-1" ]
                     ]);
               Eio.Promise.resolve mark_s1_prompt ())
             else (
               peer_write
                 p
                 (`Object
                     [ "jsonrpc", `String "2.0"
                     ; "method", `String "session/update"
                     ; ( "params"
                       , `Object
                           [ "sessionId", `String "ses-2"
                           ; "update", `Object [ "tag", `String "u-for-2" ]
                           ] )
                     ]);
               Eio.Promise.resolve mark_s2_prompt ())
           | Some "session/close", Some id ->
             let sid = Option.value_exn (str "sessionId" params) in
             Queue.enqueue closed sid;
             peer_write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"; "id", id; "result", `Object [] ]);
             if String.equal sid "ses-1"
             then (
               (* Late update for closed ses-1 must be dropped, never leaked. *)
               peer_write
                 p
                 (`Object
                     [ "jsonrpc", `String "2.0"
                     ; "method", `String "session/update"
                     ; ( "params"
                       , `Object
                           [ "sessionId", `String "ses-1"
                           ; "update", `Object [ "tag", `String "late-for-1" ]
                           ] )
                     ]);
               (* Finish ses-2's prompt now that ses-1 is closed. *)
               Option.iter
                 (Hashtbl.find_and_remove pending_prompt "ses-2")
                 ~f:(fun pid ->
                   peer_write
                     p
                     (`Object
                         [ "jsonrpc", `String "2.0"
                         ; "id", pid
                         ; ( "result"
                           , `Object [ "stopReason", `String "end_turn" ] )
                         ]));
               Eio.Promise.resolve mark_s1_closed ())
             else if String.equal sid "ses-2"
             then Eio.Promise.resolve mark_s2_closed ()
           | None, Some id ->
             Queue.enqueue perm_replies (Json.to_string id, Json.to_string j)
           | _ -> ());
          loop ()
      in
      loop ())
  in
  let mux, poolable =
    Harness_mux.create ~sw ~protocol:Acp ~on_empty:(fun _ -> incr emptied) phys
    |> Or_error.ok_exn
  in
  let t1 = Harness_mux.lease mux in
  let t2 = Harness_mux.lease mux in
  (* Both sessions send initialize with identical client id=7 -> cached reply
     replayed with id=7 and zero extra initialize calls on wire. *)
  write_json
    t1
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", `Number "7"
        ; "method", `String "initialize"
        ]);
  write_json
    t2
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", `Number "7"
        ; "method", `String "initialize"
        ]);
  let i1 = Option.value_exn (read_json t1) in
  let i2 = Option.value_exn (read_json t2) in
  (* Open both sessions with identical client id=1. *)
  write_json
    t1
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", `Number "1"
        ; "method", `String "session/new"
        ; "params", `Object [ "cwd", `String "/a"; "mcpServers", `Array [] ]
        ]);
  write_json
    t2
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", `Number "1"
        ; "method", `String "session/new"
        ; "params", `Object [ "cwd", `String "/b"; "mcpServers", `Array [] ]
        ]);
  let o1 = Option.value_exn (read_json t1) in
  let o2 = Option.value_exn (read_json t2) in
  let sid1 =
    Option.bind (Json.member "result" o1) ~f:(str "sessionId")
    |> Option.value_exn
  in
  let sid2 =
    Option.bind (Json.member "result" o2) ~f:(str "sessionId")
    |> Option.value_exn
  in
  (* Run prompts concurrently on both sessions. *)
  write_json
    t1
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", `Number "2"
        ; "method", `String "session/prompt"
        ; "params", `Object [ "sessionId", `String sid1 ]
        ]);
  write_json
    t2
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", `Number "2"
        ; "method", `String "session/prompt"
        ; "params", `Object [ "sessionId", `String sid2 ]
        ]);
  Eio.Promise.await s1_prompt_seen;
  Eio.Promise.await s2_prompt_seen;
  let s1_msgs =
    let q = Queue.create () in
    for _ = 1 to 4 do
      Queue.enqueue q (read_json t1)
    done;
    Queue.to_list q
  in
  let s2_upd = Option.value_exn (read_json t2) in
  (* Closing t1 cancels dangling perm-dangle and sends session/close without
     killing the shared process. *)
  Line_transport.stop t1;
  Eio.Promise.await s1_closed_done;
  let alive_after_t1 = Harness_mux.alive mux in
  let active_after_t1 = Harness_mux.active_sessions mux in
  let s2_done = Option.value_exn (read_json t2) in
  Line_transport.stop t2;
  Eio.Promise.await s2_closed_done;
  let alive_after_t2 = Harness_mux.alive mux in
  Harness_mux.stop mux;
  printf
    "poolable=%b wire_inits=%d init_ids=(%s,%s)\n"
    poolable
    !init_count
    (Json.to_string (id_of i1))
    (Json.to_string (id_of i2));
  printf
    "sessions=(%s id=%s, %s id=%s)\n"
    sid1
    (Json.to_string (id_of o1))
    sid2
    (Json.to_string (id_of o2));
  printf "s1 got %d frames:\n" (List.length (List.filter_opt s1_msgs));
  List.iter (List.filter_opt s1_msgs) ~f:(fun j ->
    printf "  %s\n" (Json.to_string j));
  printf "s2 update: %s\n" (Json.to_string s2_upd);
  printf "s2 prompt reply: %s\n" (Json.to_string s2_done);
  printf
    "after t1 stop: alive=%b active=%d; after t2 stop: alive=%b emptied=%d\n"
    alive_after_t1
    active_after_t1
    alive_after_t2
    !emptied;
  printf "closed on wire: %s\n" (String.concat ~sep:"," (Queue.to_list closed));
  Queue.iter perm_replies ~f:(fun (id, body) ->
    printf "perm reply %s: %s\n" id body);
  [%expect
    {|
    poolable=true wire_inits=1 init_ids=(7,7)
    sessions=(ses-1 id=1, ses-2 id=1)
    s1 got 4 frames:
      {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses-1","update":{"tag":"u-for-1"}}}
      {"jsonrpc":"2.0","id":"perm-cancel","method":"session/request_permission","params":{"sessionId":"ses-1"}}
      {"jsonrpc":"2.0","method":"$/cancel_request","params":{"requestId":"perm-cancel"}}
      {"jsonrpc":"2.0","id":"perm-dangle","method":"session/request_permission","params":{"sessionId":"ses-1"}}
    s2 update: {"jsonrpc":"2.0","method":"session/update","params":{"sessionId":"ses-2","update":{"tag":"u-for-2"}}}
    s2 prompt reply: {"id":2,"jsonrpc":"2.0","result":{"stopReason":"end_turn"}}
    after t1 stop: alive=true active=1; after t2 stop: alive=true emptied=1
    closed on wire: ses-1,ses-2
    perm reply "perm-dangle": {"jsonrpc":"2.0","id":"perm-dangle","result":{"outcome":{"outcome":"cancelled"}}}
    |}]
;;

let%expect_test
    "ACP mux: non-poolable capability fallback, close_session codec, and late \
     session/new close"
  =
  Eio_main.run
  @@ fun _ ->
  Eio.Switch.run
  @@ fun sw ->
  (* 1. ACP codec close_session frame. *)
  let codec = Acp_client_codec.create () in
  print_endline (Acp_client_codec.close_session_for codec ~session_id:"ses-42");
  (* 2. Agent without sessionCapabilities.close is not poolable. *)
  let check_poolable init_result =
    let phys =
      Line_transport.Peer.spawn ~sw (fun p ->
        match peer_read p with
        | Some j ->
          peer_write
            p
            (`Object
                [ "jsonrpc", `String "2.0"
                ; "id", id_of j
                ; "result", init_result
                ]);
          let rec drain () =
            match peer_read p with
            | Some _ -> drain ()
            | None -> ()
          in
          drain ()
        | None -> ())
    in
    let mux, poolable =
      Harness_mux.create ~sw ~protocol:Acp phys |> Or_error.ok_exn
    in
    Harness_mux.stop mux;
    poolable
  in
  let no_close =
    check_poolable
      (`Object
          [ "protocolVersion", `Number "1"
          ; "agentCapabilities", `Object [ "loadSession", `True ]
          ])
  in
  let wrong_version =
    check_poolable
      (`Object
          [ "protocolVersion", `Number "2"
          ; ( "agentCapabilities"
            , `Object [ "sessionCapabilities", `Object [ "close", `Object [] ] ]
            )
          ])
  in
  printf "no_close=%b wrong_version=%b\n" no_close wrong_version;
  (* 3. Stopping a session while session/new is in flight sends session/close
     once the late reply arrives. *)
  let release_new, do_release_new = Eio.Promise.create () in
  let new_received, mark_new_received = Eio.Promise.create () in
  let late_closed, mark_late_closed = Eio.Promise.create () in
  let phys =
    Line_transport.Peer.spawn ~sw (fun p ->
      let rec loop () =
        match peer_read p with
        | None -> ()
        | Some j ->
          let params = Option.value (Json.member "params" j) ~default:`Null in
          (match str "method" j, Json.member "id" j with
           | Some "initialize", Some id ->
             peer_write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"
                   ; "id", id
                   ; "result", acp_caps_close
                   ])
           | Some "session/new", Some id ->
             Eio.Promise.resolve mark_new_received ();
             Eio.Promise.await release_new;
             peer_write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"
                   ; "id", id
                   ; "result", `Object [ "sessionId", `String "ses-orphan" ]
                   ])
           | Some "session/close", Some id ->
             let sid = Option.value_exn (str "sessionId" params) in
             peer_write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"; "id", id; "result", `Object [] ]);
             Eio.Promise.resolve mark_late_closed sid
           | _ -> ());
          loop ()
      in
      loop ())
  in
  let mux, _ = Harness_mux.create ~sw ~protocol:Acp phys |> Or_error.ok_exn in
  let t1 = Harness_mux.lease mux in
  write_json
    t1
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", `Number "1"
        ; "method", `String "session/new"
        ; "params", `Object [ "cwd", `String "/"; "mcpServers", `Array [] ]
        ]);
  Eio.Promise.await new_received;
  Line_transport.stop t1;
  Eio.Promise.resolve do_release_new ();
  let closed_sid = Eio.Promise.await late_closed in
  printf "late-closed=%s\n" closed_sid;
  Harness_mux.stop mux;
  [%expect
    {|
    {"jsonrpc":"2.0","id":0,"method":"session/close","params":{"sessionId":"ses-42"}}
    no_close=false wrong_version=false
    late-closed=ses-orphan
    |}]
;;

let%expect_test
    "Codex mux: concurrent threads, login dedup, subagent adoption, rate-limit \
     broadcast, token refresh, and thread/unsubscribe"
  =
  Eio_main.run
  @@ fun _ ->
  Eio.Switch.run
  @@ fun sw ->
  let wire_inits = ref 0 in
  let wire_initialized = ref 0 in
  let wire_logins = Queue.create () in
  let unsubscribed = Queue.create () in
  let unsub_done, mark_unsub = Eio.Promise.create () in
  let refresh_replied, mark_refresh = Eio.Promise.create () in
  let phys =
    Line_transport.Peer.spawn ~sw (fun p ->
      let next_thr = ref 0 in
      let rec loop () =
        match peer_read p with
        | None -> ()
        | Some j ->
          let params = Option.value (Json.member "params" j) ~default:`Null in
          (match str "method" j, Json.member "id" j with
           | Some "initialize", Some id ->
             incr wire_inits;
             peer_write
               p
               (`Object
                   [ "id", id
                   ; "result", `Object [ "userAgent", `String "codex/0.171.0" ]
                   ])
           | Some "initialized", None -> incr wire_initialized
           | Some "account/login/start", Some id ->
             Queue.enqueue
               wire_logins
               (Option.value (str "accessToken" params) ~default:"");
             peer_write p (`Object [ "id", id; "result", `Object [] ])
           | Some "thread/start", Some id ->
             incr next_thr;
             let tid = sprintf "th-%d" !next_thr in
             peer_write
               p
               (`Object
                   [ "id", id
                   ; ( "result"
                     , `Object [ "thread", `Object [ "id", `String tid ] ] )
                   ]);
             if !next_thr = 2
             then (
               (* Broadcast rate limits to both active sessions. *)
               peer_write
                 p
                 (`Object
                     [ "method", `String "account/rateLimits/updated"
                     ; "params", `Object [ "plan", `String "pro" ]
                     ]);
               (* th-1 spawns a Codex-internal subagent th-child-1; child
                  events must route ONLY to session 1. *)
               peer_write
                 p
                 (`Object
                     [ "method", `String "item/started"
                     ; ( "params"
                       , `Object
                           [ "threadId", `String "th-1"
                           ; ( "item"
                             , `Object
                                 [ "type", `String "collabAgentToolCall"
                                 ; "tool", `String "spawnAgent"
                                 ; ( "receiverThreadIds"
                                   , `Array [ `String "th-child-1" ] )
                                 ] )
                           ] )
                     ]);
               peer_write
                 p
                 (`Object
                     [ "method", `String "item/agentMessage/delta"
                     ; ( "params"
                       , `Object
                           [ "threadId", `String "th-child-1"
                           ; "delta", `String "from-child"
                           ] )
                     ]);
               peer_write
                 p
                 (`Object
                     [ "method", `String "item/agentMessage/delta"
                     ; ( "params"
                       , `Object
                           [ "threadId", `String "th-2"
                           ; "delta", `String "for-s2"
                           ] )
                     ]))
           | Some "thread/unsubscribe", Some id ->
             let tid = Option.value_exn (str "threadId" params) in
             Queue.enqueue unsubscribed tid;
             peer_write p (`Object [ "id", id; "result", `Object [] ]);
             if String.equal tid "th-1"
             then (
               (* Ask remaining active session (s2) to refresh auth token. *)
               peer_write
                 p
                 (`Object
                     [ "id", `String "auth-ref-1"
                     ; "method", `String "account/chatgptAuthTokens/refresh"
                     ; "params", `Object []
                     ]);
               Eio.Promise.resolve mark_unsub ())
           | None, Some (`String "auth-ref-1") ->
             Eio.Promise.resolve mark_refresh (Json.to_string j)
           | _ -> ());
          loop ()
      in
      loop ())
  in
  let mux, poolable =
    Harness_mux.create ~sw ~protocol:Codex phys |> Or_error.ok_exn
  in
  let s1 = Harness_mux.lease mux in
  let s2 = Harness_mux.lease mux in
  (* Both initialize + send initialized (swallowed) + login with tok-1. *)
  List.iter [ s1; s2 ] ~f:(fun s ->
    write_json s (`Object [ "id", `Number "0"; "method", `String "initialize" ]);
    ignore (Option.value_exn (read_json s) : Json.t);
    write_json s (`Object [ "method", `String "initialized" ]);
    write_json
      s
      (`Object
          [ "id", `Number "1"
          ; "method", `String "account/login/start"
          ; "params", `Object [ "accessToken", `String "tok-1" ]
          ]);
    ignore (Option.value_exn (read_json s) : Json.t));
  (* Changing token sends a new login on the wire. *)
  write_json
    s2
    (`Object
        [ "id", `Number "2"
        ; "method", `String "account/login/start"
        ; "params", `Object [ "accessToken", `String "tok-2" ]
        ]);
  ignore (Option.value_exn (read_json s2) : Json.t);
  (* Start both threads. *)
  write_json
    s1
    (`Object [ "id", `Number "3"; "method", `String "thread/start" ]);
  let t1 = Option.value_exn (read_json s1) in
  write_json
    s2
    (`Object [ "id", `Number "3"; "method", `String "thread/start" ]);
  let t2 = Option.value_exn (read_json s2) in
  let s1_a = Option.value_exn (read_json s1) in
  let s1_b = Option.value_exn (read_json s1) in
  let s1_c = Option.value_exn (read_json s1) in
  let s2_a = Option.value_exn (read_json s2) in
  let s2_b = Option.value_exn (read_json s2) in
  (* Close s1: unsubscribes th-1 while s2 stays alive and answers token refresh. *)
  Line_transport.stop s1;
  Eio.Promise.await unsub_done;
  let refresh_req = Option.value_exn (read_json s2) in
  write_json
    s2
    (`Object
        [ "id", id_of refresh_req
        ; "result", `Object [ "accessToken", `String "tok-3" ]
        ]);
  let refresh_wire = Eio.Promise.await refresh_replied in
  (* New session logging in with tok-3 is deduplicated because refresh updated
     the cached token. *)
  let s3 = Harness_mux.lease mux in
  write_json
    s3
    (`Object
        [ "id", `Number "1"
        ; "method", `String "account/login/start"
        ; "params", `Object [ "accessToken", `String "tok-3" ]
        ]);
  ignore (Option.value_exn (read_json s3) : Json.t);
  Line_transport.stop s2;
  Line_transport.stop s3;
  Harness_mux.stop mux;
  printf
    "poolable=%b wire_inits=%d wire_initialized=%d wire_logins=[%s]\n"
    poolable
    !wire_inits
    !wire_initialized
    (String.concat ~sep:"," (Queue.to_list wire_logins));
  printf "threads: %s | %s\n" (Json.to_string t1) (Json.to_string t2);
  printf "s1 notifications:\n";
  List.iter [ s1_a; s1_b; s1_c ] ~f:(fun j ->
    printf "  %s\n" (Json.to_string j));
  printf "s2 notifications:\n";
  List.iter [ s2_a; s2_b ] ~f:(fun j -> printf "  %s\n" (Json.to_string j));
  printf
    "unsubscribed=[%s] refresh_wire=%s\n"
    (String.concat ~sep:"," (Queue.to_list unsubscribed))
    refresh_wire;
  [%expect
    {|
    poolable=true wire_inits=1 wire_initialized=1 wire_logins=[tok-1,tok-2]
    threads: {"id":3,"result":{"thread":{"id":"th-1"}}} | {"id":3,"result":{"thread":{"id":"th-2"}}}
    s1 notifications:
      {"method":"account/rateLimits/updated","params":{"plan":"pro"}}
      {"method":"item/started","params":{"threadId":"th-1","item":{"type":"collabAgentToolCall","tool":"spawnAgent","receiverThreadIds":["th-child-1"]}}}
      {"method":"item/agentMessage/delta","params":{"threadId":"th-child-1","delta":"from-child"}}
    s2 notifications:
      {"method":"account/rateLimits/updated","params":{"plan":"pro"}}
      {"method":"item/agentMessage/delta","params":{"threadId":"th-2","delta":"for-s2"}}
    unsubscribed=[th-1] refresh_wire={"id":"auth-ref-1","result":{"accessToken":"tok-3"}}
    |}]
;;

let%expect_test
    "Per-session routed MCP: zero cross-talk, route release on close, and \
     legacy host fallback"
  =
  (* 1. Host_process.Up wire round-trip + backwards compatibility. *)
  let check_up frame =
    let line = Host_process.Up.to_line frame in
    let parsed = Host_process.Up.of_line line |> Or_error.ok_exn in
    print_s [%sexp (parsed : Host_process.Up.t)]
  in
  check_up
    (Started
       { pid = 42
       ; path = "/bin/agy"
       ; prigh = "/bin/prigh"
       ; mcp_socket = Some "/tmp/mcp.sock"
       ; mcp_token = Some "tok"
       ; mcp_routes = true
       ; secret = true
       });
  check_up (Mcp_open { conn = 7; route = "r1-abc" });
  let legacy_started =
    Host_process.Up.of_line
      {|{"t":"started","pid":1,"path":"/bin/agy","prigh":"/bin/prigh","mcp_socket":"/tmp/s","mcp_token":"tok"}|}
    |> Or_error.ok_exn
  in
  print_s [%sexp (legacy_started : Host_process.Up.t)];
  (* 2. Routed MCP over Host_transport + Harness_mux using an in-memory host
     stream (zero subprocesses). *)
  Eio_main.run
  @@ fun _ ->
  Eio.Switch.run
  @@ fun sw ->
  let whoami_tool label =
    { Tool.spec =
        { Tool_spec.name = "whoami"
        ; description = "identifies the owning session"
        ; parameters = Tool_args.schema []
        ; parallel_safe = true
        ; destructive = Never
        ; on_host = false
        ; freeform = None
        ; native = None
        }
    ; run = (fun _ _ -> Tool_result.ok label)
    }
  in
  let make_tools label =
    let tool = whoami_tool label in
    Injected_tools.create ~tools:[ tool ] ~execute:(fun _ _ ->
      Tool_result.ok label)
  in
  let mcp_replies = Eio.Stream.create Int.max_value in
  let mcp_closes = Eio.Stream.create Int.max_value in
  let emit_up = ref ignore in
  let exec ~mcp_routes : Host_stream.Exec.t =
    fun ~stdin ~cancel ~on_output ~name:_ _args ->
    let seq = ref 0 in
    let send_up frame = on_output (Host_process.Up.to_line frame ^ "\n") in
    emit_up := send_up;
    send_up
      (Started
         { pid = 99
         ; path = "/bin/agy"
         ; prigh = "/bin/prigh"
         ; mcp_socket = Some "/tmp/mcp.sock"
         ; mcp_token = Some "secret-tok"
         ; mcp_routes
         ; secret = true
         });
    let rec read_down () =
      match Stdin_feed.take stdin with
      | Close -> ()
      | Data chunk ->
        List.iter (String.split_lines chunk) ~f:(fun line ->
          if not (String.is_empty line)
          then (
            match Host_process.Down.of_line line with
            | Ok (In line) ->
              (match Json.parse line with
               | Ok j
                 when Option.equal
                        String.equal
                        (str "method" j)
                        (Some "initialize") ->
                 incr seq;
                 send_up
                   (Out
                      { seq = !seq
                      ; line =
                          Json.to_string
                            (`Object
                                [ "jsonrpc", `String "2.0"
                                ; "id", id_of j
                                ; "result", acp_caps_close
                                ])
                      })
               | _ -> ())
            | Ok (Mcp_in { conn; line }) ->
              Eio.Stream.add mcp_replies (conn, line)
            | Ok (Mcp_close conn) -> Eio.Stream.add mcp_closes conn
            | Ok (Ack _ | Close | Signal _) | Error _ -> ()));
        read_down ()
    in
    ignore (Cancellation.protect cancel ~f:read_down : unit option);
    Host_process.Outcome.to_result (Ran { exit = Exited 0; stderr_tail = "" })
  in
  let launch : Adapter.Launch.t =
    { argv = [ "agy"; "acp" ]
    ; env_set = []
    ; env_unset = []
    ; cwd = "/"
    ; mcp = true
    ; secret = None
    }
  in
  (* Routed host -> poolable = true, isolated routes. *)
  let phys =
    Host_transport.spawn ~sw ~exec:(exec ~mcp_routes:true) ~host:"laptop" launch
    |> Or_error.ok_exn
  in
  let mux, poolable_new =
    Harness_mux.create ~sw ~protocol:Acp phys |> Or_error.ok_exn
  in
  let ta = Harness_mux.lease mux in
  let tb = Harness_mux.lease mux in
  let mcpa = Option.value_exn (Line_transport.mcp ta) in
  let mcpb = Option.value_exn (Line_transport.mcp tb) in
  mcpa.serve (make_tools "session-A");
  mcpb.serve (make_tools "session-B");
  let route_of (mcp : Line_transport.Mcp.t) =
    let tok =
      List.Assoc.find_exn
        mcp.bridge.env
        ~equal:String.equal
        Mcp_bridge.token_env
    in
    snd (String.lsplit2_exn tok ~on:':')
  in
  let route_a = route_of mcpa in
  let route_b = route_of mcpb in
  let call_line id =
    Json.to_string
      (`Object
          [ "jsonrpc", `String "2.0"
          ; "id", `Number (Int.to_string id)
          ; "method", `String "tools/call"
          ; ( "params"
            , `Object [ "name", `String "whoami"; "arguments", `Object [] ] )
          ])
  in
  !emit_up (Mcp_open { conn = 1; route = route_a });
  !emit_up (Mcp_open { conn = 2; route = route_b });
  !emit_up (Mcp { conn = 1; line = call_line 10 });
  !emit_up (Mcp { conn = 2; line = call_line 20 });
  let r1 = Eio.Stream.take mcp_replies in
  let r2 = Eio.Stream.take mcp_replies in
  let sorted =
    List.sort [ r1; r2 ] ~compare:(fun (a, _) (b, _) -> Int.compare a b)
  in
  (* Closing session A releases route A and closes conn 1; a late line on
     conn 1 and a new conn 3 on route A are both closed without cross-talk,
     while conn 2 on route B still answers session-B. *)
  Line_transport.stop ta;
  let closed1 = Eio.Stream.take mcp_closes in
  !emit_up (Mcp { conn = 1; line = call_line 11 });
  let closed1_late = Eio.Stream.take mcp_closes in
  !emit_up (Mcp_closed 1);
  !emit_up (Mcp_open { conn = 3; route = route_a });
  let closed3 = Eio.Stream.take mcp_closes in
  !emit_up (Mcp { conn = 2; line = call_line 21 });
  let r3 = Eio.Stream.take mcp_replies in
  Line_transport.stop tb;
  let closed2 = Eio.Stream.take mcp_closes in
  Harness_mux.stop mux;
  (* Legacy host (mcp_routes = false) -> not poolable, falls back cleanly. *)
  let legacy_phys =
    Host_transport.spawn ~sw ~exec:(exec ~mcp_routes:false) ~host:"old" launch
    |> Or_error.ok_exn
  in
  let legacy_mux, poolable_legacy =
    Harness_mux.create ~sw ~protocol:Acp legacy_phys |> Or_error.ok_exn
  in
  let t_leg = Harness_mux.lease legacy_mux in
  let mcp_leg = Option.value_exn (Line_transport.mcp t_leg) in
  mcp_leg.serve (make_tools "legacy-session");
  !emit_up (Mcp { conn = 9; line = call_line 99 });
  let r_leg = Eio.Stream.take mcp_replies in
  Line_transport.stop t_leg;
  Harness_mux.stop legacy_mux;
  printf
    "poolable_new=%b distinct_routes=%b poolable_legacy=%b\n"
    poolable_new
    (not (String.equal route_a route_b))
    poolable_legacy;
  List.iter sorted ~f:(fun (c, body) -> printf "conn %d -> %s\n" c body);
  printf
    "closed after ta stop: %d, late=%d, new_on_dead=%d\n"
    closed1
    closed1_late
    closed3;
  printf "conn %d still works -> %s\n" (fst r3) (snd r3);
  printf "closed after tb stop: %d\n" closed2;
  printf "legacy conn %d -> %s\n" (fst r_leg) (snd r_leg);
  [%expect
    {|
    (Started (pid 42) (path /bin/agy) (prigh /bin/prigh)
     (mcp_socket (/tmp/mcp.sock)) (mcp_token (tok)) (mcp_routes true)
     (secret true))
    (Mcp_open (conn 7) (route r1-abc))
    (Started (pid 1) (path /bin/agy) (prigh /bin/prigh) (mcp_socket (/tmp/s))
     (mcp_token (tok)) (mcp_routes false) (secret false))
    poolable_new=true distinct_routes=true poolable_legacy=false
    conn 1 -> {"jsonrpc":"2.0","id":10,"result":{"content":[{"type":"text","text":"session-A"}],"isError":false}}
    conn 2 -> {"jsonrpc":"2.0","id":20,"result":{"content":[{"type":"text","text":"session-B"}],"isError":false}}
    closed after ta stop: 1, late=1, new_on_dead=3
    conn 2 still works -> {"jsonrpc":"2.0","id":21,"result":{"content":[{"type":"text","text":"session-B"}],"isError":false}}
    closed after tb stop: 2
    legacy conn 9 -> {"jsonrpc":"2.0","id":99,"result":{"content":[{"type":"text","text":"legacy-session"}],"isError":false}}
    |}]
;;

let%expect_test
    "Harness_pool: isolation keys, boot coalescing, crash recovery, token \
     draining, idle timeout, host eviction, and switch cleanup"
  =
  Eio_main.run
  @@ fun _ ->
  let spawns = ref 0 in
  let make_acp_peer ?crash ?(auth_err_on_prompt = false) ?on_stop ~sw () =
    incr spawns;
    let id = !spawns in
    Line_transport.Peer.spawn ~sw (fun p ->
      let next_ses = ref 0 in
      let rec loop () =
        match peer_read p with
        | None -> Option.iter on_stop ~f:(fun f -> f id)
        | Some j ->
          (match str "method" j, Json.member "id" j with
           | Some "initialize", Some rid ->
             peer_write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"
                   ; "id", rid
                   ; "result", acp_caps_close
                   ])
           | Some "session/new", Some rid ->
             incr next_ses;
             let sid = sprintf "s-%d-%d" id !next_ses in
             peer_write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"
                   ; "id", rid
                   ; "result", `Object [ "sessionId", `String sid ]
                   ])
           | Some "session/prompt", Some rid ->
             if auth_err_on_prompt
             then
               peer_write
                 p
                 (`Object
                     [ "jsonrpc", `String "2.0"
                     ; "id", rid
                     ; ( "error"
                       , `Object
                           [ "code", `Number "-32000"
                           ; "message", `String "Authentication required"
                           ] )
                     ])
             else
               peer_write
                 p
                 (`Object
                     [ "jsonrpc", `String "2.0"
                     ; "id", rid
                     ; ( "result"
                       , `Object [ "server", `Number (Int.to_string id) ] )
                     ])
           | Some "session/close", Some rid ->
             peer_write
               p
               (`Object
                   [ "jsonrpc", `String "2.0"; "id", rid; "result", `Object [] ])
           | _ -> ());
          loop ()
      in
      match crash with
      | None -> loop ()
      | Some trigger ->
        Eio.Fiber.first loop (fun () ->
          Eio.Promise.await trigger;
          Line_transport.Peer.write_stderr p "boom";
          Line_transport.Peer.exit p ~code:17))
  in
  let server_of t =
    write_json
      t
      (`Object
          [ "jsonrpc", `String "2.0"
          ; "id", `Number "1"
          ; "method", `String "session/prompt"
          ; "params", `Object []
          ]);
    let j = Option.value_exn (read_json t) in
    Option.bind (Json.member "result" j) ~f:(fun r ->
      Option.bind (Json.member "server" r) ~f:Json.int)
    |> Option.value_exn
  in
  (* 1. Isolation + concurrent boot coalescing. *)
  Eio.Switch.run
  @@ fun sw ->
  let sleeps = Queue.create () in
  let sleep _ =
    let p, r = Eio.Promise.create () in
    Queue.enqueue sleeps r;
    Eio.Promise.await p
  in
  let pool = Harness_pool.create ~sw ~sleep () in
  let gate, open_gate = Eio.Promise.create () in
  let k_a = base_key ~host:"h1" () in
  let t1, t2 =
    Eio.Fiber.pair
      (fun () ->
         Harness_pool.acquire
           pool
           ~protocol:Acp
           ~key:k_a
           ~spawn:(fun ~sw ->
             Eio.Promise.await gate;
             Ok (make_acp_peer ~sw ()))
           ()
         |> Or_error.ok_exn)
      (fun () ->
         Eio.Fiber.yield ();
         Eio.Promise.resolve open_gate ();
         Harness_pool.acquire
           pool
           ~protocol:Acp
           ~key:k_a
           ~spawn:(fun ~sw -> Ok (make_acp_peer ~sw ()))
           ()
         |> Or_error.ok_exn)
  in
  printf
    "coalesced: servers=(%d,%d) spawns=%d\n"
    (server_of t1)
    (server_of t2)
    !spawns;
  (* Distinct isolation keys must NEVER share a server. *)
  let isolation_keys =
    [ "host", { k_a with host = "h2" }
    ; "login", { k_a with login_key = Some "google/work" }
    ; "member", { k_a with member = Some "alice" }
    ; "sandbox", { k_a with no_sandbox = true }
    ; "argv", { k_a with argv = [ "agy"; "acp"; "--alt" ] }
    ; "env_set", { k_a with env_set = [ "FOO", "1" ] }
    ; "env_unset", { k_a with env_unset = [ "BAR" ] }
    ]
  in
  let isolated =
    List.map isolation_keys ~f:(fun (label, key) ->
      let tr =
        Harness_pool.acquire
          pool
          ~protocol:Acp
          ~key
          ~spawn:(fun ~sw -> Ok (make_acp_peer ~sw ()))
          ()
        |> Or_error.ok_exn
      in
      let s = server_of tr in
      Line_transport.stop tr;
      label, s)
  in
  printf
    "isolated: %s\n"
    (List.map isolated ~f:(fun (l, s) -> sprintf "%s=#%d" l s)
     |> String.concat ~sep:" ");
  Line_transport.stop t1;
  Line_transport.stop t2;
  (* Wait until all 8 idle timers (7 isolated + k_a) have entered [sleep]. *)
  let wait_for cond =
    let rec loop n =
      if cond ()
      then ()
      else if n = 0
      then failwith "condition not met"
      else (
        Eio.Fiber.yield ();
        loop (n - 1))
    in
    loop 100
  in
  wait_for (fun () -> Queue.length sleeps >= 8);
  (* Re-acquiring before the idle timer fires cancels k_a's pending timer. *)
  let t_re =
    Harness_pool.acquire
      pool
      ~protocol:Acp
      ~key:k_a
      ~spawn:(fun ~sw -> Ok (make_acp_peer ~sw ()))
      ()
    |> Or_error.ok_exn
  in
  printf "reacquired k_a=#%d\n" (server_of t_re);
  (* Drain timers started before re-acquire: k_a stays alive, the 7 isolated
     idle servers are evicted. *)
  printf "active before idle=%d\n" (Harness_pool.active_count pool);
  while not (Queue.is_empty sleeps) do
    Eio.Promise.resolve (Queue.dequeue_exn sleeps) ()
  done;
  wait_for (fun () -> Harness_pool.active_count pool = 1);
  printf "active after stale timers=%d\n" (Harness_pool.active_count pool);
  Line_transport.stop t_re;
  wait_for (fun () -> Queue.length sleeps >= 1);
  while not (Queue.is_empty sleeps) do
    Eio.Promise.resolve (Queue.dequeue_exn sleeps) ()
  done;
  wait_for (fun () -> Harness_pool.active_count pool = 0);
  printf "active after idle=%d\n" (Harness_pool.active_count pool);
  (* 2. Crash recovery: active sessions get exit status, dead server evicted,
     next session boots fresh. *)
  let crash, do_crash = Eio.Promise.create () in
  let k_crash = base_key ~host:"h-crash" () in
  let c1 =
    Harness_pool.acquire
      pool
      ~protocol:Acp
      ~key:k_crash
      ~spawn:(fun ~sw -> Ok (make_acp_peer ~crash ~sw ()))
      ()
    |> Or_error.ok_exn
  in
  let c2 =
    Harness_pool.acquire
      pool
      ~protocol:Acp
      ~key:k_crash
      ~spawn:(fun ~sw -> Ok (make_acp_peer ~sw ()))
      ()
    |> Or_error.ok_exn
  in
  let s_before = server_of c1 in
  Eio.Promise.resolve do_crash ();
  let e1 = Line_transport.ended c1 in
  let e2 = Line_transport.ended c2 in
  Eio.Fiber.yield ();
  let active_after_crash = Harness_pool.active_count pool in
  let c3 =
    Harness_pool.acquire
      pool
      ~protocol:Acp
      ~key:k_crash
      ~spawn:(fun ~sw -> Ok (make_acp_peer ~sw ()))
      ()
    |> Or_error.ok_exn
  in
  let s_after = server_of c3 in
  Line_transport.stop c3;
  printf
    "crash: before=#%d exits=(%s,%s) active_after_crash=%d fresh=#%d\n"
    s_before
    (Sexp.to_string [%sexp (e1 : Line_transport.Exit.t)])
    (Sexp.to_string [%sexp (e2 : Line_transport.Exit.t)])
    active_after_crash
    s_after;
  (* 3. Token renewal draining: old server drains while its active session
     finishes, new session boots fresh immediately, draining stops on last
     session close. *)
  let now = ref 100 in
  let k_tok =
    { (base_key ~host:"h-tok" ()) with login_key = Some "google/a" }
  in
  let tok1 : Harness_pool.Token_info.t =
    { token = "tok-1"
    ; expiring = (fun ~now_ms -> now_ms >= 200)
    ; secret_digest = Some "d1"
    }
  in
  let tok2 : Harness_pool.Token_info.t =
    { token = "tok-2"
    ; expiring = (fun ~now_ms -> now_ms >= 500)
    ; secret_digest = Some "d2"
    }
  in
  let stopped_servers = Queue.create () in
  let pool_tok = Harness_pool.create ~sw ~now_ms:(fun () -> !now) ~sleep () in
  let d1 =
    Harness_pool.acquire
      pool_tok
      ~protocol:Acp
      ~key:k_tok
      ~token_info:tok1
      ~spawn:(fun ~sw ->
        Ok (make_acp_peer ~on_stop:(Queue.enqueue stopped_servers) ~sw ()))
      ()
    |> Or_error.ok_exn
  in
  let s_d1 = server_of d1 in
  (* Token becomes expiring -> d2 drains old server and boots fresh. *)
  now := 250;
  let d2 =
    Harness_pool.acquire
      pool_tok
      ~protocol:Acp
      ~key:k_tok
      ~token_info:tok2
      ~spawn:(fun ~sw ->
        Ok (make_acp_peer ~on_stop:(Queue.enqueue stopped_servers) ~sw ()))
      ()
    |> Or_error.ok_exn
  in
  let s_d2 = server_of d2 in
  (* d1 can STILL finish work on the draining server! *)
  let s_d1_still = server_of d1 in
  let counts_mid =
    Harness_pool.active_count pool_tok, Harness_pool.draining_count pool_tok
  in
  Line_transport.stop d1;
  Eio.Fiber.yield ();
  let counts_after_d1 =
    Harness_pool.active_count pool_tok, Harness_pool.draining_count pool_tok
  in
  (* Auth error (-32000) also triggers draining via on_drain. *)
  let k_auth = base_key ~host:"h-auth" () in
  let a1 =
    Harness_pool.acquire
      pool_tok
      ~protocol:Acp
      ~key:k_auth
      ~spawn:(fun ~sw -> Ok (make_acp_peer ~auth_err_on_prompt:true ~sw ()))
      ()
    |> Or_error.ok_exn
  in
  write_json
    a1
    (`Object
        [ "jsonrpc", `String "2.0"
        ; "id", `Number "1"
        ; "method", `String "session/prompt"
        ]);
  ignore (Option.value_exn (read_json a1) : Json.t);
  let counts_after_auth_err =
    Harness_pool.active_count pool_tok, Harness_pool.draining_count pool_tok
  in
  Line_transport.stop a1;
  (* 4. evict_host evicts only the named host. *)
  let k_other = base_key ~host:"h-keep" () in
  let keep =
    Harness_pool.acquire
      pool_tok
      ~protocol:Acp
      ~key:k_other
      ~spawn:(fun ~sw -> Ok (make_acp_peer ~sw ()))
      ()
    |> Or_error.ok_exn
  in
  Harness_pool.evict_host pool_tok ~host:"h-tok";
  printf
    "drain: s1=#%d s2=#%d s1_still=#%d mid=(%d,%d) after_d1=(%d,%d) \
     auth_err=(%d,%d) after_evict_tok=%d keep_still=#%d\n"
    s_d1
    s_d2
    s_d1_still
    (fst counts_mid)
    (snd counts_mid)
    (fst counts_after_d1)
    (snd counts_after_d1)
    (fst counts_after_auth_err)
    (snd counts_after_auth_err)
    (Harness_pool.active_count pool_tok)
    (server_of keep);
  Line_transport.stop d2;
  Line_transport.stop keep;
  (* Exiting Switch.run WITHOUT calling Harness_pool.shutdown must not hang
     and must clean up all warm servers automatically. *)
  printf "switch exited cleanly\n";
  [%expect
    {|
    coalesced: servers=(1,1) spawns=1
    isolated: host=#2 login=#3 member=#4 sandbox=#5 argv=#6 env_set=#7 env_unset=#8
    reacquired k_a=#1
    active before idle=8
    active after stale timers=1
    active after idle=0
    crash: before=#9 exits=(((code(17))(stderr_tail boom)),((code(17))(stderr_tail boom))) active_after_crash=0 fresh=#10
    drain: s1=#11 s2=#12 s1_still=#11 mid=(1,1) after_d1=(1,0) auth_err=(1,1) after_evict_tok=1 keep_still=#14
    switch exited cleanly
    |}]
;;

let%expect_test
    "Rpc_server end-to-end: catalog warms pool, main sessions and \
     Composer_subagent reuse the same ACP server"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let sessions_dir = Filename.concat t.dir "sessions" in
  let engines =
    Or_error.ok_exn
      (Engines.create
         ~engines:[ Harness; Composer ]
         ~adapters:[ Acp "antigravity" ]
         ())
  in
  let spawns = ref 0 in
  let wire_inits = ref 0 in
  let sessions_opened = ref 0 in
  let sessions_closed = Queue.create () in
  let catalog_closed, mark_catalog_closed = Eio.Promise.create () in
  let server_stopped, mark_stopped = Eio.Promise.create () in
  let fake_agy_server (p : Line_transport.Peer.t) =
    let rec loop () =
      match peer_read p with
      | None -> ignore (Eio.Promise.try_resolve mark_stopped () : bool)
      | Some j ->
        let params = Option.value (Json.member "params" j) ~default:`Null in
        (match str "method" j, Json.member "id" j with
         | Some "initialize", Some id ->
           incr wire_inits;
           peer_write
             p
             (`Object
                 [ "jsonrpc", `String "2.0"
                 ; "id", id
                 ; "result", acp_caps_close
                 ])
         | Some "session/new", Some id ->
           incr sessions_opened;
           let sid = sprintf "agy-%d" !sessions_opened in
           peer_write
             p
             (`Object
                 [ "jsonrpc", `String "2.0"
                 ; "id", id
                 ; ( "result"
                   , `Object
                       [ "sessionId", `String sid
                       ; ( "models"
                         , `Object
                             [ "currentModelId", `String "gemini-flash"
                             ; ( "availableModels"
                               , `Array
                                   [ `Object
                                       [ "modelId", `String "gemini-flash"
                                       ; "name", `String "Gemini Flash"
                                       ]
                                   ] )
                             ] )
                       ] )
                 ])
         | Some "session/prompt", Some id ->
           let sid = Option.value_exn (str "sessionId" params) in
           let text =
             match Json.member "prompt" params with
             | Some (`Array (first :: _)) ->
               Option.value (str "text" first) ~default:""
             | _ -> ""
           in
           let reply =
             if String.is_substring text ~substring:"subagent-task"
             then "from-subagent:" ^ sid
             else "from-main:" ^ sid
           in
           peer_write
             p
             (`Object
                 [ "jsonrpc", `String "2.0"
                 ; "method", `String "session/update"
                 ; ( "params"
                   , `Object
                       [ "sessionId", `String sid
                       ; ( "update"
                         , `Object
                             [ "sessionUpdate", `String "agent_message_chunk"
                             ; ( "content"
                               , `Object
                                   [ "type", `String "text"
                                   ; "text", `String reply
                                   ] )
                             ] )
                       ] )
                 ]);
           peer_write
             p
             (`Object
                 [ "jsonrpc", `String "2.0"
                 ; "id", id
                 ; "result", `Object [ "stopReason", `String "end_turn" ]
                 ])
         | Some "session/close", Some id ->
           let sid = Option.value_exn (str "sessionId" params) in
           Queue.enqueue sessions_closed sid;
           peer_write
             p
             (`Object
                 [ "jsonrpc", `String "2.0"; "id", id; "result", `Object [] ]);
           if String.equal sid "agy-2"
           then ignore (Eio.Promise.try_resolve mark_catalog_closed () : bool)
         | Some _, Some id ->
           peer_write
             p
             (`Object
                 [ "jsonrpc", `String "2.0"; "id", id; "result", `Object [] ])
         | _ -> ());
        loop ()
    in
    loop ()
  in
  let provider =
    Faux_provider.create
      [ Faux_provider.Reply.tool_call
          ~id:"c1"
          ~name:"subagent"
          ~arguments:{|{"task":"subagent-task","engine":"acp:antigravity"}|}
          ()
      ; Faux_provider.Reply.text "delegated"
      ; Faux_provider.Reply.text "all done"
      ]
  in
  let new_agent ?session ~cwd () =
    let agent_ref = ref None in
    let hosts () =
      Option.value_map !agent_ref ~default:[] ~f:Agent.tool_hosts
    in
    let agent =
      Agent.create
        ~env:t.env
        ~sw
        ~provider
        ~tools:
          (Tools.interaction
           @ (Tool_subagent.create
                ~hosts
                ~engines:(fun () ->
                  Option.bind !agent_ref ~f:Agent.subagent_engines)
                ~provider
                ~current_model:(fun () -> Model.default)
                ~current_thinking:(fun () -> Off)
                ~home:t.dir
                ()
              :: Tool_subagent.control_tools))
        ~sessions_dir
        ~home:t.dir
        ?session
        ~cwd
        ()
    in
    agent_ref := Some agent;
    Agent.set_mode agent ~approval:Auto;
    agent
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login:
        (Login_manager.create
           ~env:t.env
           ~sw
           ~getenv:(fun _ -> None)
           ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
           ())
      ~sessions_dir
      ~cwd:t.dir
      ~engines
      ~spawn_external:(fun _agent ~host:_ _launch ->
        incr spawns;
        Ok (Line_transport.Peer.spawn ~sw fake_agy_server))
      ~new_agent
      ()
  in
  let client = Rpc_server.connect server ~send:ignore in
  let call ?(params = "{}") meth =
    Rpc_server.handle
      server
      client
      (Json.of_string
         (sprintf {|{"id":"r","method":%S,"params":%s}|} meth params))
  in
  ignore (call "hello" : Json.t);
  (* 1. Main session 1 boots the pooled server and opens agy-1. *)
  ignore (call ~params:{|{"adapter":"acp:antigravity"}|} "new_session" : Json.t);
  let agent1 = Rpc_server.agent_of_client server client in
  ignore (call ~params:{|{"text":"hello one"}|} "prompt" : Json.t);
  Agent.wait_idle agent1;
  let reply1 =
    List.last_exn (Agent.messages agent1)
    |> function
    | Message.Assistant a -> Message.Assistant.text a
    | _ -> "?"
  in
  (* 2. Catalog fetch (retry_models -> fetch_catalog) reuses the SAME server,
     opens agy-2, closes it via session/close, and keeps the server warm. *)
  ignore (call "retry_models" : Json.t);
  Eio.Promise.await catalog_closed;
  (* 3. Second main session on the same adapter reuses the SAME server (agy-3). *)
  let client2 = Rpc_server.connect server ~send:ignore in
  let call2 ?(params = "{}") meth =
    Rpc_server.handle
      server
      client2
      (Json.of_string
         (sprintf {|{"id":"r","method":%S,"params":%s}|} meth params))
  in
  ignore (call2 "hello" : Json.t);
  ignore
    (call2 ~params:{|{"adapter":"acp:antigravity"}|} "new_session" : Json.t);
  let agent2 = Rpc_server.agent_of_client server client2 in
  ignore (call2 ~params:{|{"text":"hello two"}|} "prompt" : Json.t);
  Agent.wait_idle agent2;
  let reply2 =
    List.last_exn (Agent.messages agent2)
    |> function
    | Message.Assistant a -> Message.Assistant.text a
    | _ -> "?"
  in
  (* 4. A harness session delegating to an ACP subagent via Composer_subagent
     reuses the EXACT SAME pooled server (agy-4) and closes it on finish. *)
  let client3 = Rpc_server.connect server ~send:ignore in
  let call3 ?(params = "{}") meth =
    Rpc_server.handle
      server
      client3
      (Json.of_string
         (sprintf {|{"id":"r","method":%S,"params":%s}|} meth params))
  in
  ignore (call3 "hello" : Json.t);
  ignore (call3 "new_session" : Json.t);
  let agent3 = Rpc_server.agent_of_client server client3 in
  let subagent_reply = ref "" in
  Agent.subscribe agent3 ~f:(function
    | Loop (Subagent_end { result; _ }) -> subagent_reply := result.text
    | _ -> ());
  ignore
    (call3 ~params:{|{"text":"please run an antigravity subagent"}|} "prompt"
     : Json.t);
  Agent.wait_idle agent3;
  printf
    "spawns=%d wire_inits=%d opened=%d closed=[%s]\n"
    !spawns
    !wire_inits
    !sessions_opened
    (String.concat ~sep:"," (Queue.to_list sessions_closed));
  printf
    "main1=%s main2=%s subagent=%s\n"
    reply1
    reply2
    (String.strip !subagent_reply |> String.split_lines |> List.hd_exn);
  Rpc_server.shutdown server;
  Eio.Promise.await server_stopped;
  printf "stopped_after_shutdown=%b\n" (Eio.Promise.is_resolved server_stopped);
  [%expect
    {|
    spawns=1 wire_inits=1 opened=4 closed=[agy-2,agy-4]
    main1=from-main:agy-1 main2=from-main:agy-3 subagent=from-subagent:agy-4
    stopped_after_shutdown=true
    |}]
;;
