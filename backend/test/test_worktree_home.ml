open! Core
open! Prigh
open Tool_test_helpers

let sh (t : Tool_test_helpers.t) script =
  let command =
    sprintf
      "cd %s && export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 \
       GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t \
       GIT_COMMITTER_EMAIL=t@t && (%s) > /dev/null 2>&1"
      (Filename.quote t.dir)
      script
  in
  match Sys_unix.command command with
  | 0 -> ()
  | n -> raise_s [%message "setup failed" (script : string) (n : int)]
;;

(* [repo] (main) with worktrees [trees/a] and [trees/b], and [elsewhere]. *)
let setup t =
  sh
    t
    "mkdir -p repo/sub elsewhere && cd repo && git init -q -b main . && echo \
     one > sub/a.txt && git add -A && git commit -q -m init && git worktree \
     add -q -b a ../trees/a && git worktree add -q -b b ../trees/b && mkdir -p \
     ../trees/a/sub"
;;

let new_agent ?session ?(cwd = "repo") t ~sw =
  Agent.create
    ~env:t.env
    ~sw
    ~provider:(Faux_provider.create [])
    ~tools:Tools.all
    ~sessions_dir:(Filename.concat t.dir "sessions")
    ~home:t.dir
    ?session
    ~cwd:(Filename.concat t.dir cwd)
    ()
;;

let show_home t (home : Session.Home.t option) =
  match home with
  | None -> "none"
  | Some home ->
    sprintf
      "%s%s%s"
      (mask t home.cwd)
      (Option.value_map home.host ~default:"" ~f:(fun h -> " on " ^ h.id))
      (Option.value_map home.repo ~default:"" ~f:(fun r ->
         sprintf
           " (%s%s)"
           (Option.value r.branch ~default:"?")
           (if r.main then ", main" else "")))
;;

let print_agent t agent =
  let state = Agent.state agent in
  printf "  cwd=%s home=%s\n" (mask t state.cwd) (show_home t state.home)
;;

let step t agent what result =
  printf "%s\n" what;
  (match result with
   | Ok (_ : Dir_change.Outcome.t) -> ()
   | Error e -> printf "  error: %s\n" (mask t (Error.to_string_hum e)));
  print_agent t agent
;;

let switch t agent dir =
  step
    t
    agent
    ("switch to " ^ dir)
    (Agent.set_cwd agent ~worktree:true ~path:(Filename.concat t.dir dir))
;;

let cd t agent dir =
  step
    t
    agent
    ("cd " ^ dir)
    (Agent.set_cwd agent ~path:(Filename.concat t.dir dir))
;;

let back ?main t agent =
  step
    t
    agent
    (if Option.is_some main then "back main" else "back")
    (Agent.switch_back ?main agent)
;;

let%expect_test "main -> a -> b -> back lands in main" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  setup t;
  let agent = new_agent t ~sw ~cwd:"repo/sub" in
  back t agent;
  switch t agent "repo";
  switch t agent "trees/a";
  switch t agent "trees/b";
  switch t agent "trees/a/sub";
  back t agent;
  back t agent;
  [%expect
    {|
    back
      error: this session hasn't switched into a worktree; /worktree open TARGET switches
      cwd=$DIR/repo/sub home=none
    switch to repo
      cwd=$DIR/repo home=none
    switch to trees/a
      cwd=$DIR/trees/a home=$DIR/repo on backend (main, main)
    switch to trees/b
      cwd=$DIR/trees/b home=$DIR/repo on backend (main, main)
    switch to trees/a/sub
      cwd=$DIR/trees/a/sub home=$DIR/repo on backend (main, main)
    back
      cwd=$DIR/repo home=none
    back
      error: this session hasn't switched into a worktree; /worktree open TARGET switches
      cwd=$DIR/repo home=none
    |}];
  (* Switching into the home's checkout is going back. *)
  switch t agent "trees/a";
  switch t agent "repo/sub";
  [%expect
    {|
    switch to trees/a
      cwd=$DIR/trees/a home=$DIR/repo on backend (main, main)
    switch to repo/sub
      cwd=$DIR/repo/sub home=none
    |}]
;;

let%expect_test
    "a /cd within the worktrees keeps the home; out of them or home clears it"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  setup t;
  let agent = new_agent t ~sw in
  switch t agent "trees/a";
  cd t agent "trees/a/sub";
  cd t agent "trees/b";
  cd t agent "repo/sub";
  cd t agent "elsewhere";
  switch t agent "trees/a";
  cd t agent "repo";
  cd t agent "elsewhere";
  [%expect
    {|
    switch to trees/a
      cwd=$DIR/trees/a home=$DIR/repo on backend (main, main)
    cd trees/a/sub
      cwd=$DIR/trees/a/sub home=$DIR/repo on backend (main, main)
    cd trees/b
      cwd=$DIR/trees/b home=$DIR/repo on backend (main, main)
    cd repo/sub
      cwd=$DIR/repo/sub home=$DIR/repo on backend (main, main)
    cd elsewhere
      cwd=$DIR/elsewhere home=none
    switch to trees/a
      cwd=$DIR/trees/a home=$DIR/elsewhere on backend
    cd repo
      cwd=$DIR/repo home=$DIR/elsewhere on backend
    cd elsewhere
      cwd=$DIR/elsewhere home=none
    |}]
;;

let%expect_test "persisted: reload and restart keep it, fork copies it" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  setup t;
  let agent = new_agent t ~sw in
  Agent.set_session_name agent "home";
  switch t agent "trees/a";
  let path = Session.path (Agent.session agent) in
  let loaded = Or_error.ok_exn (Session.load path) in
  printf "loaded: %s\n" (show_home t (Session.home loaded));
  let summary =
    List.find_exn
      (Session.list ~dir:(Filename.concat t.dir "sessions"))
      ~f:(fun (s : Session.Summary.t) -> String.equal s.path path)
  in
  printf "summary: %s\n" (show_home t summary.home);
  let restarted = new_agent t ~sw ~session:loaded in
  print_agent t restarted;
  let forked =
    Or_error.ok_exn
      (Session.fork loaded ~dir:(Filename.concat t.dir "sessions"))
  in
  printf "forked: %s\n" (show_home t (Session.home forked));
  back t restarted;
  let loaded = Or_error.ok_exn (Session.load path) in
  printf "loaded after back: %s\n" (show_home t (Session.home loaded));
  [%expect
    {|
    switch to trees/a
      cwd=$DIR/trees/a home=$DIR/repo on backend (main, main)
    loaded: $DIR/repo on backend (main, main)
    summary: $DIR/repo on backend (main, main)
      cwd=$DIR/trees/a home=$DIR/repo on backend (main, main)
    forked: $DIR/repo on backend (main, main)
    back
      cwd=$DIR/repo home=none
    loaded after back: none
    |}]
;;

let%expect_test
    "the home's directory is gone: back main goes to the main checkout"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  setup t;
  let agent = new_agent t ~sw ~cwd:"trees/a/sub" in
  switch t agent "trees/b";
  sh t "rm -rf trees/a";
  back t agent;
  back ~main:true t agent;
  [%expect
    {|
    switch to trees/b
      cwd=$DIR/trees/b home=$DIR/trees/a/sub on backend (a)
    back
      error: $DIR/trees/a/sub is gone; /worktree back main goes to the repository's main checkout ($DIR/repo) instead
      cwd=$DIR/trees/b home=$DIR/trees/a/sub on backend (a)
    back main
      cwd=$DIR/repo home=none
    |}]
;;

let%expect_test "the home's host is not connected" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  setup t;
  let session =
    Session.create
      ~dir:(Filename.concat t.dir "sessions")
      ~cwd:(Filename.concat t.dir "trees/a")
      ()
  in
  ignore
    (Session.set_home
       session
       (Some
          { cwd = "/home/me/repo"
          ; host = Some { id = "laptop"; name = "my laptop"; pinned = true }
          ; repo = None
          })
     : Session.Entry.t);
  let agent = new_agent t ~sw ~session ~cwd:"trees/a" in
  back t agent;
  back ~main:true t agent;
  [%expect
    {|
    back
      error: the tool host my laptop that /home/me/repo is on is not connected: reconnect it (or /host picks another, and /cd a directory there)
      cwd=$DIR/trees/a home=/home/me/repo on laptop
    back main
      error: the tool host my laptop that /home/me/repo is on is not connected: reconnect it (or /host picks another, and /cd a directory there)
      cwd=$DIR/trees/a home=/home/me/repo on laptop
    |}]
;;

(* Hosts that cannot say where a directory is in git. *)
let%expect_test "without git's answers, by paths" =
  let place ?(host = "h") ?repo cwd : Worktree_home.Place.t =
    { cwd; host = Some { id = host; name = host; pinned = false }; repo }
  in
  let show (change : Worktree_home.Change.t) =
    match change with
    | Keep -> "keep"
    | Clear -> "clear"
    | Set home -> "set " ^ home.cwd
  in
  let home = Some (Worktree_home.Place.home (place "/r/sub")) in
  List.iter
    [ ( "switch from /r/sub to /w/a"
      , Worktree_home.switch None ~from:(place "/r/sub") ~to_:(place "/w/a") )
    ; ( "switch from /r to /r"
      , Worktree_home.switch None ~from:(place "/r") ~to_:(place "/r") )
    ; ( "switch to /w/b"
      , Worktree_home.switch home ~from:(place "/w/a") ~to_:(place "/w/b") )
    ; ( "switch to /r/sub"
      , Worktree_home.switch home ~from:(place "/w/a") ~to_:(place "/r/sub") )
    ; ( "cd /w/a/x"
      , Worktree_home.cd home ~from:(place "/w/a") ~to_:(place "/w/a/x") )
    ; "cd /w/b", Worktree_home.cd home ~from:(place "/w/a") ~to_:(place "/w/b")
    ; ( "cd /r/sub"
      , Worktree_home.cd home ~from:(place "/w/a") ~to_:(place "/r/sub") )
    ; ( "cd /w/a on another host"
      , Worktree_home.cd home ~from:(place "/w/a") ~to_:(place ~host:"g" "/w/a")
      )
    ]
    ~f:(fun (what, change) -> printf "%s: %s\n" what (show change));
  [%expect
    {|
    switch from /r/sub to /w/a: set /r/sub
    switch from /r to /r: keep
    switch to /w/b: keep
    switch to /r/sub: clear
    cd /w/a/x: keep
    cd /w/b: clear
    cd /r/sub: clear
    cd /w/a on another host: clear
    |}]
;;

let%expect_test
    "over RPC: set_cwd's worktree, home in the state and the list, \
     worktree_back"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  setup t;
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider:(Faux_provider.create [])
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~backend_host:true
      ~login:
        (Login_manager.create
           ~env:t.env
           ~sw
           ~getenv:(fun _ -> None)
           ~store:(Auth_store.create ~path:(Filename.concat t.dir "auth.json"))
           ())
      ~sessions_dir
      ~cwd:(Filename.concat t.dir "repo")
      ~new_agent
      ()
  in
  let client = Rpc_server.connect server ~send:ignore in
  let call ?(params = "{}") meth =
    Rpc_server.handle
      server
      client
      (Jsonaf.of_string
         (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
  in
  let print ?params meth =
    print_endline (mask t (Jsonaf.to_string (call ?params meth)))
  in
  (* The session's home, not a tool host's home directory in [hosts]. *)
  let home_of json =
    let rec find : Jsonaf.t -> Jsonaf.t option = function
      | `Object fields ->
        (match List.Assoc.find fields ~equal:String.equal "home" with
         | Some home -> Some home
         | None ->
           List.find_map fields ~f:(fun (k, v) ->
             if String.equal k "hosts" then None else find v))
      | `Array items -> List.find_map items ~f:find
      | _ -> None
    in
    printf
      "  home: %s\n"
      (Option.value_map (find json) ~default:"absent" ~f:(fun h ->
         mask t (Jsonaf.to_string h)))
  in
  print ~params:{|{"name": "rpc"}|} "set_session_name";
  print
    ~params:(sprintf {|{"path": "%s/trees/a", "worktree": true}|} t.dir)
    "set_cwd";
  home_of (call "get_state");
  home_of (call "list_sessions");
  print ~params:{|{"main": "yes"}|} "worktree_back";
  print "worktree_back";
  home_of (call "get_state");
  home_of (call "list_sessions");
  print "worktree_back";
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/trees/a","created":false}}
      home: {"cwd":"$DIR/repo","host":{"id":"backend","name":""},"repo":{"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}}
      home: {"cwd":"$DIR/repo","host":{"id":"backend","name":""},"repo":{"root":"$DIR/repo","worktree":"$DIR/repo","branch":"main","main":true}}
    {"type":"response","id":"r","ok":false,"error":"param \"main\" must be a boolean"}
    {"type":"response","id":"r","ok":true,"result":{"cwd":"$DIR/repo","created":false}}
      home: absent
      home: absent
    {"type":"response","id":"r","ok":false,"error":"this session hasn't switched into a worktree; /worktree open TARGET switches"}
    |}]
;;
