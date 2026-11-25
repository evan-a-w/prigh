open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let id s = Or_error.ok_exn (Account_id.of_string s)
let work = id "work"
let codex : Provider_id.t = Openai_codex

let oauth ?(expires_ms = 0) ?(account = "acct-alice") n =
  Credential.Oauth
    { access = sprintf "alice-access-%d" n
    ; refresh = sprintf "alice-refresh-%d" n
    ; expires_ms
    ; account_id = Some account
    }
;;

(* Alice's ChatGPT logins: "Personal" ([main]) and "Work". *)
let alice_logins ?expires_ms store =
  List.iter
    [ "Personal", oauth ~account:"acct-personal" 0
    ; "Work", oauth ?expires_ms 1
    ]
    ~f:(fun (label, credential) ->
      ignore
        (Or_error.ok_exn
           (Provider_accounts.save_login ~label store codex credential)
         : Account_id.t))
;;

let store t user = Auth_store.create ~path:(t.dir ^/ user ^/ "auth.json")

(* Alice has a ChatGPT login "Work" (its token expired); bob and carol none. *)
let setup ?enabled t =
  let shares = Account_shares.create ?enabled () in
  let stores =
    List.map [ "alice"; "bob"; "carol" ] ~f:(fun user ->
      let store = store t user in
      Account_shares.register shares ~user ~store ~home:(t.dir ^/ user);
      user, store)
  in
  let store user = List.Assoc.find_exn stores ~equal:String.equal user in
  alice_logins (store "alice");
  shares, store
;;

let mask_ms =
  let re = Re.compile (Re.Perl.re {|"(since_ms|expires_ms)":\d+|}) in
  fun s -> Re.replace re s ~f:(fun g -> sprintf {|"%s":$MS|} (Re.Group.get g 1))
;;

let print_or_error sexp_of = function
  | Ok x -> print_s (sexp_of x)
  | Error e -> print_endline ("error: " ^ Error.to_string_hum e)
;;

let grants =
  print_or_error (fun grants ->
    [%sexp
      (List.map grants ~f:(fun (g : Account_meta.Grant.t) -> g.user)
       : string list)])
;;

let%expect_test "sharing: grant, list, resolve through the owner, hide, revoke" =
  with_sandbox
  @@ fun t ->
  let shares, store = setup t in
  let share users =
    grants (Account_shares.share shares ~owner:"alice" codex work ~users)
  in
  share [ "alice" ];
  share [ "dave" ];
  grants
    (Account_shares.share
       shares
       ~owner:"alice"
       (Custom "gw")
       Account_id.main
       ~users:[ "bob" ]);
  grants
    (Account_shares.share
       shares
       ~owner:"alice"
       codex
       (id "nope")
       ~users:[ "bob" ]);
  share [ "bob"; "bob" ];
  print_s
    [%sexp (Account_shares.targets shares ~user:"bob" : string list Or_error.t)];
  [%expect
    {|
    error: alice is you: share with bob, carol
    error: no user "dave": share with bob, carol
    error: gw is a custom provider: only the built-in providers' logins can be shared
    error: OpenAI Codex (ChatGPT) has no account "nope": it has Personal (main), Work (work)
    (bob)
    (Ok (alice carol))
    |}];
  (* What bob sees: the login, never its credential. *)
  let seen user =
    print_s
      [%sexp
        (Account_shares.shared_with shares ~user : Account_shares.Shared.t list)];
    print_or_error
      [%sexp_of: Login_choice.t list]
      (Login_choice.list ~member:{ shares; user } (store user) codex)
  in
  seen "bob";
  seen "carol";
  [%expect
    {|
    (((owner alice) (provider Openai_codex) (id work) (label Work)
      (method_ Oauth) (pref ((hidden false) (fallback false)))))
    (((ref_ (Shared (owner alice) (id work))) (label "Work (from alice)")
      (default true) (fallback false)))
    ()
    ()
    |}];
  (* Bob's requests: the token is refreshed once, in alice's file. *)
  let refreshes = ref 0 in
  let refresh _ ~refresh_token =
    incr refreshes;
    printf "refreshing with %s\n" refresh_token;
    Ok
      { Credential.Oauth.access = "alice-access-2"
      ; refresh = "alice-refresh-2"
      ; expires_ms = Credential.now_ms () + 3_600_000
      ; account_id = Some "acct-alice"
      }
  in
  let resolve recipient =
    print_or_error
      (fun (r : Provider_auth.Resolved.t) -> [%sexp (r.token : string)])
      (Account_shares.resolve
         shares
         ~env:t.env
         ~refresh
         ~recipient
         codex
         ~owner:"alice"
         work)
  in
  resolve "bob";
  resolve "bob";
  resolve "carol";
  print_s [%sexp (!refreshes : int)];
  let file user =
    let path = Auth_store.path (store user) in
    if Sys_unix.file_exists_exn path then In_channel.read_all path else ""
  in
  print_s
    [%sexp
      (String.is_substring (file "alice") ~substring:"alice-refresh-2" : bool)
    , (String.is_substring (file "bob") ~substring:"alice" : bool)];
  [%expect
    {|
    refreshing with alice-refresh-1
    alice-access-2
    alice-access-2
    error: alice does not share the OpenAI Codex (ChatGPT) login "Work" with you (any more): /accounts picks another
    1
    (true false)
    |}];
  (* Bob hides it, then lets it take over at usage limits. *)
  let pref f =
    Or_error.ok_exn
      (Account_shares.set_pref shares ~user:"bob" codex ~owner:"alice" work ~f)
  in
  pref (fun p -> { p with hidden = true });
  seen "bob";
  pref (fun _ -> { hidden = false; fallback = true });
  print_endline (file "bob");
  [%expect
    {|
    (((owner alice) (provider Openai_codex) (id work) (label Work)
      (method_ Oauth) (pref ((hidden true) (fallback false)))))
    ()
    {
      "prigh:accounts": {
        "version": 1,
        "providers": {},
        "shared": {
          "~alice/openai-codex/work": {
            "hidden": false,
            "fallback": true
          }
        }
      }
    }
    |}];
  (* Usage: alice sees everyone's, bob his own. *)
  let now = Time_ns.of_string_with_utc_offset "2026-11-24 10:00:00Z" in
  List.iter [ "bob"; "bob"; "carol" ] ~f:(fun recipient ->
    Account_shares.record
      ~now
      shares
      ~recipient
      codex
      ~owner:"alice"
      work
      { input = 100; output = 10; cache_read = 50 }
      ~cost_usd:0.25);
  let usage user =
    print_or_error
      [%sexp_of: Account_ledger.Row.t list]
      (Account_shares.usage ~now shares ~user codex ~owner:"alice" work)
  in
  usage "alice";
  usage "bob";
  [%expect
    {|
    (((user bob)
      (usage
       ((today
         ((requests 2) (input 200) (output 20) (cache_read 100) (cost_usd 0.5)))
        (last_7_days
         ((requests 2) (input 200) (output 20) (cache_read 100) (cost_usd 0.5)))
        (last_30_days
         ((requests 2) (input 200) (output 20) (cache_read 100) (cost_usd 0.5))))))
     ((user carol)
      (usage
       ((today
         ((requests 1) (input 100) (output 10) (cache_read 50) (cost_usd 0.25)))
        (last_7_days
         ((requests 1) (input 100) (output 10) (cache_read 50) (cost_usd 0.25)))
        (last_30_days
         ((requests 1) (input 100) (output 10) (cache_read 50) (cost_usd 0.25)))))))
    (((user bob)
      (usage
       ((today
         ((requests 2) (input 200) (output 20) (cache_read 100) (cost_usd 0.5)))
        (last_7_days
         ((requests 2) (input 200) (output 20) (cache_read 100) (cost_usd 0.5)))
        (last_30_days
         ((requests 2) (input 200) (output 20) (cache_read 100) (cost_usd 0.5)))))))
    |}];
  (* Revoked: bob's next request fails, and the login is gone from his list. *)
  share [];
  resolve "bob";
  seen "bob";
  print_s
    [%sexp
      (Account_shares.granted shares ~recipient:"bob" codex ~owner:"alice" work
       : bool)];
  [%expect
    {|
    ()
    error: alice does not share the OpenAI Codex (ChatGPT) login "Work" with you (any more): /accounts picks another
    ()
    ()
    false
    |}]
;;

let%expect_test "sharing off (-no-sharing)" =
  with_sandbox
  @@ fun t ->
  let shares, _ = setup ~enabled:false t in
  grants
    (Account_shares.share shares ~owner:"alice" codex work ~users:[ "bob" ]);
  print_s
    [%sexp
      (Account_shares.targets shares ~user:"alice" : string list Or_error.t)];
  print_s
    [%sexp
      (Account_shares.shared_with shares ~user:"bob"
       : Account_shares.Shared.t list)];
  [%expect
    {|
    error: sharing is off on this server (prigh serve -no-sharing)
    (Error "sharing is off on this server (prigh serve -no-sharing)")
    ()
    |}]
;;

(* Alice and bob as [prigh serve -tokens alice=..,bob=.. -host-tokens
   bob=host-b]; bob's provider resolves its requests' logins as the router
   does and records them. *)
let make_router t ~sw ~shares ~requests =
  let stores = ref [] in
  let access =
    User_access.create
      (Or_error.ok_exn (Namespace.parse_spec "alice=tok-a,bob=tok-b"))
      ~superusers:[]
      ~host_tokens:(Or_error.ok_exn (Namespace.parse_spec "bob=host-b"))
    |> Or_error.ok_exn
  in
  let router =
    Rpc_router.namespaced
      access
      ~home:(t.dir ^/ "home")
      ~legacy_auth_file:(t.dir ^/ "legacy-auth.json")
      ~create_server:(fun namespace world ->
        let user = namespace.name in
        stores := (user, world.store) :: !stores;
        Account_shares.register shares ~user ~store:world.store ~home:world.home;
        let member = { Account_shares.Member.shares; user } in
        let provider =
          Faux_provider.create
            ~on_request:(fun (r : Provider.Request.t) ->
              let token =
                match r.account with
                | Some (Shared { owner; id }) ->
                  (match
                     Account_shares.resolve
                       shares
                       ~env:t.env
                       ~recipient:user
                       r.model.provider
                       ~owner
                       id
                   with
                   | Ok r -> r.token
                   | Error e -> Error.to_string_hum e)
                | _ -> "own"
              in
              requests
              := sprintf
                   "%s: %s with %s"
                   user
                   (Option.value_map
                      r.account
                      ~default:"default"
                      ~f:Account_ref.to_string)
                   token
                 :: !requests)
            (List.init 4 ~f:(fun _ -> Reply.text "ok"))
        in
        (* As the router: what bob ran on alice's login is hers to see. *)
        let provider =
          { provider with
            stream =
              (fun r ~cancel ~on_event ->
                let reply = provider.stream r ~cancel ~on_event in
                Option.iter r.account ~f:(fun account ->
                  Account_shares.record_request
                    ~member
                    ~ledger:(Account_ledger.create ~home:world.home)
                    r.model.provider
                    account
                    reply.usage
                    ~cost_usd:0.);
                reply)
          }
        in
        let new_agent ?session ~cwd () =
          Agent.create
            ~env:t.env
            ~sw
            ~provider
            ~accounts:(fun provider ->
              Login_choice.list ~member world.store provider
              |> Result.ok
              |> Option.value ~default:[])
            ~tools:[]
            ~sessions_dir:world.sessions_dir
            ~home:world.home
            ?session
            ~cwd
            ()
        in
        Rpc_server.create
          ~env:t.env
          ~sw
          ~token:namespace.token
          ~namespace:user
          ~member
          ~login:
            (Login_manager.create
               ~env:t.env
               ~sw
               ~getenv:world.getenv
               ~store:world.store
               ())
          ~sessions_dir:world.sessions_dir
          ~cwd:t.dir
          ~new_agent
          ())
  in
  router, access, !stores
;;

let%expect_test "sharing over RPC: share, use, revoke mid-session, host tokens" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let shares = Account_shares.create () in
  let requests = ref [] in
  let router, access, stores = make_router t ~sw ~shares ~requests in
  let server name =
    List.Assoc.find_exn (Rpc_router.servers router) ~equal:String.equal name
  in
  let alice = server "alice" in
  let bob = server "bob" in
  let connect ?(token = "tok") server user =
    let sent = Queue.create () in
    let signed_in, _ =
      Or_error.ok_exn (User_access.authenticate access ~user (Some token))
    in
    let client =
      Rpc_server.connect ~signed_in server ~send:(Queue.enqueue sent)
    in
    client, sent
  in
  let ca, alice_sent = connect ~token:"tok-a" alice "alice" in
  let cb, bob_sent = connect ~token:"tok-b" bob "bob" in
  let bob_host, _ = connect ~token:"host-b" bob "bob" in
  let all_bob_saw = Buffer.create 1024 in
  let drain sent =
    Queue.iter sent ~f:(fun json ->
      let line = Json.to_string json in
      if phys_equal sent bob_sent then Buffer.add_string all_bob_saw line;
      match Json.member "event" json with
      | Some (`String ("accounts_changed" | "notice")) ->
        print_endline (mask_ms line)
      | _ -> ());
    Queue.clear sent
  in
  let call ?(quiet = false) server client meth params =
    let response =
      Rpc_server.handle
        server
        client
        (Json.of_string
           (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
    in
    if phys_equal server bob
    then Buffer.add_string all_bob_saw (Json.to_string response);
    if not quiet
    then
      print_endline
        (mask_ms
         @@ Json.to_string
              (match Json.member "result" response with
               | Some r -> r
               | None -> Option.value_exn (Json.member "error" response)))
  in
  let alice_store = List.Assoc.find_exn stores ~equal:String.equal "alice" in
  alice_logins ~expires_ms:(Credential.now_ms () + 3_600_000) alice_store;
  call alice ca "account_share_targets" "{}";
  call
    alice
    ca
    "account_share"
    {|{"ref": "openai-codex/work", "users": ["bob"]}|};
  drain alice_sent;
  drain bob_sent;
  [%expect
    {|
    ["bob"]
    {"shared_with":[{"user":"bob","since_ms":$MS}]}
    {"type":"event","event":"accounts_changed"}
    {"type":"event","event":"accounts_changed"}
    |}];
  (* Bob's view of ChatGPT: the shared login, and that he may share his own. *)
  let codex_accounts server client =
    match
      Rpc_server.handle
        server
        client
        (Json.of_string {|{"id": "r", "method": "auth_status"}|})
      |> Json.member "result"
    with
    | Some (`Array providers) ->
      List.iter providers ~f:(fun p ->
        if phys_equal server bob
        then Buffer.add_string all_bob_saw (Json.to_string p);
        match Json.member "provider" p with
        | Some (`String "openai-codex") ->
          print_endline
            (Json.to_string (Option.value_exn (Json.member "configured" p)));
          (match Json.member "accounts" p with
           | Some (`Array accounts) ->
             List.iter accounts ~f:(fun a ->
               print_endline (mask_ms (Json.to_string a)))
           | _ -> ())
        | _ -> ())
    | _ -> print_endline "?"
  in
  codex_accounts bob cb;
  codex_accounts alice ca;
  [%expect
    {|
    {"method":"oauth","source":"shared by alice"}
    {"ref":"~alice/openai-codex/work","id":"~alice/work","label":"Work","method":"oauth","source":"shared by alice","default":true,"fallback":false,"shared_by":"alice"}
    {"method":"oauth","source":"oauth"}
    {"ref":"openai-codex/main","id":"main","label":"Personal","method":"oauth","source":"oauth","default":true,"fallback":true,"expires_ms":$MS,"shareable":true}
    {"ref":"openai-codex/work","id":"work","label":"Work","method":"oauth","source":"oauth","default":false,"fallback":true,"expires_ms":$MS,"shareable":true,"shared_with":[{"user":"bob","since_ms":$MS}]}
    |}];
  (* Bob runs on it: the backend resolves alice's token; bob's client never
     sees it. *)
  let model = Model.key (Option.value_exn (Model.default_for codex)) in
  call
    bob
    cb
    "set_model"
    (sprintf {|{"model": "%s", "account": "~alice/work"}|} model);
  call bob cb "prompt" {|{"text": "hi"}|};
  let agent = Rpc_server.agent_of_client bob cb in
  Agent.wait_idle agent;
  List.iter (List.rev !requests) ~f:print_endline;
  print_s [%sexp ((Agent.state agent).account : Account_ref.t option)];
  [%expect
    {|
    {}
    {}
    bob: ~alice/work with alice-access-1
    ((Shared (owner alice) (id work)))
    |}];
  (* A host token may not manage logins. *)
  call bob bob_host "account_hide" {|{"ref": "~alice/openai-codex/work"}|};
  call bob bob_host "logout" {|{"provider": "openai-codex"}|};
  call
    bob
    bob_host
    "account_share"
    {|{"ref": "openai-codex/main", "users": []}|};
  codex_accounts bob bob_host;
  [%expect
    {|
    "this connection signed in with a host token, which cannot change model logins: sign in as the user (prigh-web, or the user's own token)"
    "this connection signed in with a host token, which cannot change model logins: sign in as the user (prigh-web, or the user's own token)"
    "this connection signed in with a host token, which cannot change model logins: sign in as the user (prigh-web, or the user's own token)"
    {"method":"oauth","source":"shared by alice"}
    {"ref":"~alice/openai-codex/work","id":"~alice/work","label":"Work","method":"oauth","source":"shared by alice","default":true,"fallback":false,"shared_by":"alice","usage":{"today":{"requests":1,"input":10,"output":5,"cache_read":0,"cost_usd":0},"last_7_days":{"requests":1,"input":10,"output":5,"cache_read":0,"cost_usd":0},"last_30_days":{"requests":1,"input":10,"output":5,"cache_read":0,"cost_usd":0}}}
    |}];
  (* A recipient may not rename the shared login; alice sees his use. *)
  call
    bob
    cb
    "account_update"
    {|{"ref": "~alice/openai-codex/work", "label": "Mine"}|};
  call alice ca "account_usage" {|{"ref": "openai-codex/work"}|};
  [%expect
    {|
    "alice shares this login with you: you can hide it or let it take over at usage limits (fallback), not rename, reorder or make it the default"
    [{"user":"bob","today":{"requests":1,"input":10,"output":5,"cache_read":0,"cost_usd":0},"last_7_days":{"requests":1,"input":10,"output":5,"cache_read":0,"cost_usd":0},"last_30_days":{"requests":1,"input":10,"output":5,"cache_read":0,"cost_usd":0}}]
    |}];
  (* Alice stops sharing it mid-session: bob's session goes back to his
     default login, with a notice. *)
  drain bob_sent;
  call alice ca "account_share" {|{"ref": "openai-codex/work", "users": []}|};
  drain bob_sent;
  print_s [%sexp ((Agent.state agent).account : Account_ref.t option)];
  codex_accounts bob cb;
  [%expect
    {|
    {"shared_with":[]}
    {"type":"event","event":"accounts_changed"}
    {"type":"event","event":"notice","text":"alice stopped sharing the OpenAI Codex (ChatGPT) login ~alice/work with you: carrying on with your default login (/account picks another)"}
    ()
    null
    |}];
  let saw = Buffer.contents all_bob_saw in
  let session_files =
    let path = (Agent.state agent).session_path in
    if Sys_unix.file_exists_exn path then In_channel.read_all path else ""
  in
  print_s
    [%sexp
      (List.exists [ "alice-access"; "alice-refresh" ] ~f:(fun substring ->
         String.is_substring saw ~substring
         || String.is_substring session_files ~substring)
       : bool)];
  [%expect {| false |}];
  Rpc_router.shutdown router
;;

let%expect_test "usage per login and per user, over the periods" =
  with_sandbox
  @@ fun t ->
  let shares, _ = setup t in
  let ledger = Account_ledger.create ~home:(t.dir ^/ "alice") in
  let now = Time_ns.of_string_with_utc_offset "2026-11-24 10:00:00Z" in
  let days_ago n = Time_ns.sub now (Time_ns.Span.of_day (Float.of_int n)) in
  let alice = { Account_shares.Member.shares; user = "alice" } in
  let bob = { Account_shares.Member.shares; user = "bob" } in
  let run ?member ~at account cost =
    Account_shares.record_request
      ~now:(days_ago at)
      ?member
      ~ledger
      codex
      account
      { input = 1000; output = 100; cache_read = 0 }
      ~cost_usd:cost
  in
  (* Alice on her own logins, today, 3, 20 and 40 days ago; bob on her
     shared one today; a single-user server's request (no users). *)
  run ~member:alice ~at:0 (Own work) 0.5;
  run ~member:alice ~at:3 (Own work) 1.;
  run ~member:alice ~at:20 (Own work) 2.;
  run ~member:alice ~at:40 (Own work) 4.;
  run ~member:alice ~at:0 Account_ref.main 0.1;
  run ~member:bob ~at:0 (Shared { owner = "alice"; id = work }) 0.25;
  run ~at:0 (Own work) 0.01;
  let totals (p : Account_ledger.Totals.t) =
    sprintf "%d req %d in $%.2f" p.requests p.input p.cost_usd
  in
  let periods (u : Account_ledger.Periods.t) =
    sprintf
      "today %s | 7d %s | 30d %s"
      (totals u.today)
      (totals u.last_7_days)
      (totals u.last_30_days)
  in
  List.iter [ "openai-codex/work"; "openai-codex/main" ] ~f:(fun login ->
    printf "%s: %s\n" login (periods (Account_ledger.usage ~now ledger ~login));
    List.iter (Account_ledger.rows ~now ledger ~login) ~f:(fun r ->
      printf "  %s: %s\n" r.user (periods r.usage)));
  (* The other process's (or server's) writes are seen. *)
  let other = Account_ledger.create ~home:(t.dir ^/ "alice") in
  Account_ledger.record
    ~now
    other
    ~login:"openai-codex/main"
    ~user:"alice"
    { input = 1; output = 1; cache_read = 0 }
    ~cost_usd:0.;
  printf
    "main after another writer: %s\n"
    (periods (Account_ledger.usage ~now ledger ~login:"openai-codex/main"));
  (* Bob sees only his own use of alice's login. *)
  (match
     Account_shares.usage ~now shares ~user:"bob" codex ~owner:"alice" work
   with
   | Ok rows ->
     List.iter rows ~f:(fun r ->
       printf "bob sees %s: %s\n" r.user (periods r.usage))
   | Error e -> print_endline (Error.to_string_hum e));
  [%expect
    {|
    openai-codex/work: today 3 req 3000 in $0.76 | 7d 4 req 4000 in $1.76 | 30d 5 req 5000 in $3.76
      alice: today 1 req 1000 in $0.50 | 7d 2 req 2000 in $1.50 | 30d 3 req 3000 in $3.50
      bob: today 1 req 1000 in $0.25 | 7d 1 req 1000 in $0.25 | 30d 1 req 1000 in $0.25
      default: today 1 req 1000 in $0.01 | 7d 1 req 1000 in $0.01 | 30d 1 req 1000 in $0.01
    openai-codex/main: today 1 req 1000 in $0.10 | 7d 1 req 1000 in $0.10 | 30d 1 req 1000 in $0.10
      alice: today 1 req 1000 in $0.10 | 7d 1 req 1000 in $0.10 | 30d 1 req 1000 in $0.10
    main after another writer: today 2 req 1001 in $0.10 | 7d 2 req 1001 in $0.10 | 30d 2 req 1001 in $0.10
    bob sees bob: today 1 req 1000 in $0.25 | 7d 1 req 1000 in $0.25 | 30d 1 req 1000 in $0.25
    |}]
;;

(* Without [-tokens]: the user's own logins' usage, in [auth_status] and
   [account_usage]. *)
let%expect_test "usage over RPC without users" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let store = store t "solo" in
  alice_logins store;
  let ledger = Account_ledger.create ~home:(t.dir ^/ "solo") in
  let now = Time_ns.now () in
  Account_shares.record_request
    ~now
    ~ledger
    codex
    (Own work)
    { input = 2000; output = 300; cache_read = 500 }
    ~cost_usd:0.75;
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:[]
      ~sessions_dir:(t.dir ^/ "sessions")
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~ledger
      ~login:
        (Login_manager.create ~env:t.env ~sw ~getenv:(fun _ -> None) ~store ())
      ~sessions_dir:(t.dir ^/ "sessions")
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client = Rpc_server.connect server ~send:ignore in
  let call meth params =
    let response =
      Rpc_server.handle
        server
        client
        (Json.of_string
           (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
    in
    match Json.member "result" response with
    | Some r -> r
    | None -> Option.value_exn (Json.member "error" response)
  in
  (match call "auth_status" "{}" with
   | `Array providers ->
     List.iter providers ~f:(fun p ->
       match Json.member "accounts" p with
       | Some (`Array accounts) ->
         List.iter accounts ~f:(fun a ->
           printf
             "%s: %s\n"
             (Json.to_string (Option.value_exn (Json.member "ref" a)))
             (Option.value_map
                (Json.member "usage" a)
                ~default:"no usage"
                ~f:Json.to_string))
       | _ -> ())
   | json -> print_endline (Json.to_string json));
  print_endline
    (Json.to_string (call "account_usage" {|{"ref": "openai-codex/work"}|}));
  print_endline
    (Json.to_string
       (call "account_usage" {|{"ref": "~alice/openai-codex/work"}|}));
  [%expect
    {|
    "openai-codex/main": no usage
    "openai-codex/work": {"today":{"requests":1,"input":2000,"output":300,"cache_read":500,"cost_usd":0.75},"last_7_days":{"requests":1,"input":2000,"output":300,"cache_read":500,"cost_usd":0.75},"last_30_days":{"requests":1,"input":2000,"output":300,"cache_read":500,"cost_usd":0.75}}
    [{"user":"default","you":true,"today":{"requests":1,"input":2000,"output":300,"cache_read":500,"cost_usd":0.75},"last_7_days":{"requests":1,"input":2000,"output":300,"cache_read":500,"cost_usd":0.75},"last_30_days":{"requests":1,"input":2000,"output":300,"cache_read":500,"cost_usd":0.75}}]
    "sharing logins needs users (prigh serve -tokens): this server has one"
    |}]
;;
