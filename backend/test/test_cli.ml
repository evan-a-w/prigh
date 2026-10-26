open! Core
open! Prigh
open Tool_test_helpers

(* The [prigh] binary as a user meets it: wrong flags, missing logins and
   broken files, each error saying what to do next. *)

let exe = Filename.concat (Sys_unix.getcwd ()) "../bin/main.exe"

let spawn ?(path = "/usr/bin:/bin") t args =
  Core_unix.create_process_env
    ~prog:exe
    ~args
    ~env:(`Replace [ "HOME", t.dir; "PATH", path ])
    ()
;;

(* Runs [prigh ARGS] to completion; prints stdout, stderr and the exit
   status, with the sandbox and [hide]'s patterns masked. *)
let prigh ?(hide = []) ?path t args =
  let p = spawn ?path t args in
  Core_unix.close p.stdin;
  let read fd = In_channel.input_all (Core_unix.in_channel_of_descr fd) in
  let out = read p.stdout in
  let err = read p.stderr in
  let status = Core_unix.waitpid p.pid in
  let show s =
    List.fold hide ~init:(mask t s) ~f:(fun s (pattern, with_) ->
      String.substr_replace_all s ~pattern ~with_)
    |> print_string
  in
  show (sprintf "$ prigh %s\n" (String.concat ~sep:" " args));
  show out;
  show err;
  match status with
  | Ok () -> ()
  | Error (`Exit_non_zero n) -> printf "[exit %d]\n" n
  | Error (`Signal s) -> printf "[%s]\n" (Signal.to_string s)
;;

let%expect_test "flags: bad values name the flag and the fix" =
  with_sandbox
  @@ fun t ->
  prigh t [ "run"; "-model"; "anthropic/nope"; "hi" ];
  prigh t [ "run"; "-thinking"; "medium"; "hi" ];
  prigh t [ "run"; "-cwd"; "/nonexistent"; "-faux"; "hi" ];
  prigh t [ "models"; "-provider"; "anthropc" ];
  prigh t [ "serve"; "-listen"; "7777" ];
  prigh t [ "serve"; "-web"; "localhost" ];
  prigh t [ "serve"; "-web"; ":99999" ];
  prigh t [ "tool-host"; "-connect"; "7777" ];
  [%expect
    {|
    $ prigh run -model anthropic/nope hi
    -model: unknown model "anthropic/nope"; did you mean: anthropic/claude-opus-5 (Claude Opus 5), anthropic/claude-fable-5 (Claude Fable 5), anthropic/claude-opus-4-5 (Claude Opus 4.5 (latest))
    prigh models lists them all
    [exit 2]
    $ prigh run -thinking medium hi
    -thinking medium: thinking must be one of: off, on, low, high, max
    [exit 2]
    $ prigh run -cwd /nonexistent -faux hi
    -cwd /nonexistent: no such directory (mkdir -p it first)
    [exit 2]
    $ prigh models -provider anthropc
    unknown provider anthropc; one of: anthropic, openai, openai-codex, deepseek (or custom, to add an OpenAI-compatible endpoint)
    [exit 2]
    $ prigh serve -listen 7777
    -listen must be HOST:PORT, got "7777"; did you mean 127.0.0.1:7777 (this machine only) or 0.0.0.0:7777 (all interfaces)?
    [exit 2]
    $ prigh serve -web localhost
    -web must be HOST:PORT such as 127.0.0.1:7790, got "localhost"
    [exit 2]
    $ prigh serve -web :99999
    -web :99999: the port must be a number from 0 to 65535
    [exit 2]
    $ prigh tool-host -connect 7777
    -connect: "7777" is not HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)
    [exit 2]
    |}]
;;

(* Each stops before serving anything, saying what to do; nothing is saved. *)
let%expect_test "web: no backend, a bad address, an unreachable one" =
  with_sandbox
  @@ fun t ->
  write t "site/index.html" "";
  let site = Filename.concat t.dir "site" in
  prigh t [ "web" ];
  prigh t [ "web"; "-connect"; "server" ];
  prigh t [ "web"; "-connect"; "127.0.0.1:1"; "-tools"; "elsewhere" ];
  prigh
    t
    [ "web"
    ; "-connect"
    ; "127.0.0.1:1"
    ; "-web-root"
    ; site
    ; "-cwd"
    ; "/nonexistent"
    ];
  prigh
    t
    [ "web"; "-connect"; "127.0.0.1:1"; "-token"; "sekrit"; "-web-root"; site ];
  printf
    "saved: %b\n"
    (Sys_unix.file_exists_exn (Saved_connection.file ~home:t.dir));
  write t ".prigh/tui-connection.sexp" "((connect 127.0.0.1:1)";
  prigh t [ "web"; "-web-root"; site ];
  write t ".prigh/tui-connection.sexp" "((connect 127.0.0.1:1))";
  prigh t [ "web"; "-remote"; "-web-root"; site ];
  [%expect
    {|
    $ prigh web
    prigh web: no backend to connect to: pass -connect HOST:PORT (or a prigh-web URL such as wss://host/ws), which is saved for next time.
    For prigh-web with a backend on this machine, run prigh serve -web 127.0.0.1:7790 (./prigh -web without -connect or -remote).
    [exit 2]
    $ prigh web -connect server
    prigh web: -connect: "server" is not HOST:PORT (the backend's -listen or -web port, e.g. server:7790) or a prigh-web URL (e.g. wss://server/ws or https://server/prigh/)
    [exit 2]
    $ prigh web -connect 127.0.0.1:1 -tools elsewhere
    prigh web: -tools elsewhere: local (run tools on this machine) or remote (on the backend)
    [exit 2]
    $ prigh web -connect 127.0.0.1:1 -web-root $DIR/site -cwd /nonexistent
    prigh web: -cwd /nonexistent: no such directory (mkdir -p it first)
    [exit 2]
    $ prigh web -connect 127.0.0.1:1 -token sekrit -web-root $DIR/site
    prigh web: cannot reach the backend at 127.0.0.1:1: connection refused
    Is it running there (prigh serve -web HOST:PORT, or -listen)? Check the address given to -connect: HOST:PORT, or a prigh-web URL such as wss://host/ws.
    [exit 1]
    saved: false
    $ prigh web -web-root $DIR/site
    prigh web: cannot read the saved connection: ($DIR/.prigh/tui-connection.sexp
     (Failure
      "Sexplib.Sexp.gen_load_sexp: EOF in $DIR/.prigh/tui-connection.sexp while in state Parsing_list"))
    Fix or delete $DIR/.prigh/tui-connection.sexp, or pass -connect HOST:PORT.
    [exit 2]
    $ prigh web -remote -web-root $DIR/site
    prigh web: cannot reach the backend at 127.0.0.1:1: connection refused
    Is it running there (prigh serve -web HOST:PORT, or -listen)? The address is the saved connection ($DIR/.prigh/tui-connection.sexp): pass -connect HOST:PORT to replace it.
    [exit 1]
    |}]
;;

let%expect_test "sessions list: archived sessions last, under their heading" =
  with_sandbox
  @@ fun t ->
  let dir = Filename.concat t.dir ".prigh/sessions" in
  let session ~days_ago text =
    let s = Session.create ~dir ~cwd:"/proj" () in
    ignore
      (Session.append_message
         s
         ~at:(Core_unix.gettimeofday () -. (days_ago *. 86400.))
         (Message.user text)
       : Session.Entry.t);
    s
  in
  let recent = session ~days_ago:0. "recent work" in
  let by_hand = session ~days_ago:1. "archived by hand" in
  ignore (Session.set_archived by_hand ~archived:true : Session.Entry.t);
  let idle = session ~days_ago:30. "idle for a month" in
  (* Listed newest file first: make the order not depend on timing. *)
  List.iteri [ idle; by_hand; recent ] ~f:(fun i s ->
    let at = Core_unix.gettimeofday () -. Float.of_int (10 - i) in
    Core_unix.utimes (Session.path s) ~access:at ~modif:at);
  let hide = List.map (Session.list ~dir) ~f:(fun s -> s.id, "<id>") in
  prigh ~hide t [ "sessions"; "list" ];
  write t ".prigh/config.json" {|{"archive_after_days": 0}|};
  prigh ~hide t [ "sessions"; "list" ];
  [%expect
    {|
    $ prigh sessions list
    <time>  <id>    1 msgs  /proj  recent work
    archived:
    <time>  <id>    1 msgs  /proj  archived by hand
    <time>  <id>    1 msgs  /proj  idle for a month
    $ prigh sessions list
    <time>  <id>    1 msgs  /proj  recent work
    <time>  <id>    1 msgs  /proj  idle for a month
    archived:
    <time>  <id>    1 msgs  /proj  archived by hand
    |}]
;;

let%expect_test "run without a login says how to log in, as a command" =
  with_sandbox
  @@ fun t ->
  prigh t [ "run"; "-quiet"; "hi" ];
  [%expect
    {|
    $ prigh run -quiet hi
    error: not logged in to DeepSeek: use prigh login deepseek or set DEEPSEEK_API_KEY
    [exit 1]
    |}]
;;

let%expect_test "-session takes the id prigh sessions list shows" =
  with_sandbox
  @@ fun t ->
  prigh t [ "run"; "-faux"; "-quiet"; "first" ];
  let id =
    match Session.list ~dir:(Filename.concat t.dir ".prigh/sessions") with
    | [ s ] -> s.id
    | _ -> assert false
  in
  let hide = [ String.prefix id 6, "<id-prefix>" ] in
  prigh ~hide t [ "run"; "-faux"; "-session"; String.prefix id 6; "second" ];
  print_s
    [%sexp
      (List.map
         (Session.list ~dir:(Filename.concat t.dir ".prigh/sessions"))
         ~f:(fun s -> s.message_count)
       : int list)];
  prigh t [ "run"; "-faux"; "-session"; "nosuch"; "hi" ];
  prigh t [ "run"; "-faux"; "-session"; "/no/such.jsonl"; "hi" ];
  prigh ~hide t [ "sessions"; "delete"; String.prefix id 6; "nosuch" ];
  [%expect
    {|
    $ prigh run -faux -quiet first
    faux reply
    $ prigh run -faux -session <id-prefix> second
    faux reply
    [deepseek-flash] tokens: in=20 (cached 0) out=10 cost=$0.0000 session=$DIR/.prigh/sessions/<stamp>_<id>.jsonl
    (4)
    $ prigh run -faux -session nosuch hi
    -session nosuch: no session "nosuch"
    prigh sessions list shows the saved sessions (in $DIR/.prigh/sessions)
    [exit 2]
    $ prigh run -faux -session /no/such.jsonl hi
    -session /no/such.jsonl: no such file
    prigh sessions list shows the saved sessions (in $DIR/.prigh/sessions)
    [exit 2]
    $ prigh sessions delete <id-prefix> nosuch
    no session "nosuch"
    Nothing deleted; prigh sessions list shows the saved sessions
    [exit 1]
    |}]
;;

let%expect_test "broken files say which file and what to do" =
  with_sandbox
  @@ fun t ->
  write t ".prigh/config.json" {|{"default_model": "deepseek-flash",|};
  prigh t [ "run"; "-faux"; "-quiet"; "hi" ];
  write
    t
    ".prigh/config.json"
    {|{"confirm_tools": "yes", "default_modl": "deepseek-flash"}|};
  prigh t [ "run"; "-faux"; "-quiet"; "hi" ];
  write t ".prigh/config.json" {|{"default_model": "deepseek-flsh"}|};
  prigh t [ "run"; "-faux"; "-quiet"; "hi" ];
  Core_unix.unlink (Filename.concat t.dir ".prigh/config.json");
  write t ".config/prigh/auth.json" "{oops";
  prigh t [ "auth" ];
  write t ".config/prigh/auth.json" {|{"deepseek": {"type": "password"}}|};
  prigh t [ "run"; "-quiet"; "hi" ];
  [%expect
    {|
    $ prigh run -faux -quiet hi
    faux reply
    $DIR/.prigh/config.json is not valid JSON (json > object: char '}'); its settings and custom providers are ignored until it is fixed
    $ prigh run -faux -quiet hi
    faux reply
    config.confirm_tools must be a boolean (in $DIR/.prigh/config.json); prigh ignores the file's settings until it is fixed
    unknown setting "default_modl" in $DIR/.prigh/config.json is ignored; did you mean "default_model"?
    $ prigh run -faux -quiet hi
    faux reply
    default_model "deepseek-flsh" (in $DIR/.prigh/config.json) is not a known model, so new sessions start on another; did you mean deepseek/deepseek-flash? (/change_default saves the current model as the default)
    $ prigh auth
    $DIR/.config/prigh/auth.json is not valid JSON (json > object: char '}'): fix it, or move it aside and log in again
    [exit 1]
    $ prigh run -quiet hi
    error: auth: $DIR/.config/prigh/auth.json: the deepseek credential is unreadable (credential: unknown type "password"): log in to deepseek again
    [exit 1]
    |}]
;;

(* Starts [prigh serve ARGS] and returns it with the port it announced. *)
let serve t args =
  let p = spawn t ("serve" :: "-faux" :: args) in
  let err = Core_unix.in_channel_of_descr p.stderr in
  let line = Option.value_exn (In_channel.input_line err) in
  let port =
    String.rsplit2_exn line ~on:':' |> snd |> String.strip |> Int.of_string
  in
  p, port
;;

let stop (p : Core_unix.Process_info.t) =
  Signal_unix.send_i Signal.term (`Pid p.pid);
  ignore (Core_unix.waitpid p.pid : Core_unix.Exit_or_signal.t)
;;

let%expect_test "serve: a taken port, a refused tool host" =
  with_sandbox
  @@ fun t ->
  let server, port = serve t [ "-listen"; "127.0.0.1:0"; "-token"; "good" ] in
  let address = sprintf "127.0.0.1:%d" port in
  let prigh = prigh ~hide:[ address, "127.0.0.1:PORT" ] in
  prigh t [ "serve"; "-faux"; "-listen"; address ];
  prigh t [ "serve"; "-faux"; "-web"; address ];
  prigh t [ "tool-host"; "-connect"; address ];
  prigh t [ "tool-host"; "-connect"; address; "-token"; "bad" ];
  prigh t [ "tool-host"; "-connect"; address; "-token"; "bad"; "-user"; "me" ];
  stop server;
  [%expect
    {|
    $ prigh serve -faux -listen 127.0.0.1:PORT
    prigh: cannot listen on tcp:127.0.0.1:PORT for -listen: address already in use
    Another program (perhaps another prigh serve) has that port: stop it, or pass -listen another port (0 picks a free one).
    [exit 2]
    $ prigh serve -faux -web 127.0.0.1:PORT
    prigh: cannot listen on tcp:127.0.0.1:PORT for -web: address already in use
    Another program (perhaps another prigh serve) has that port: stop it, or pass -web another port (0 picks a free one).
    [exit 2]
    $ prigh tool-host -connect 127.0.0.1:PORT
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    No token was given: pass the backend's with -token SECRET (or $PRIGH_TOKEN).
    [exit 1]
    $ prigh tool-host -connect 127.0.0.1:PORT -token bad
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    Check -token (or $PRIGH_TOKEN) against the backend's; if it serves several users (-tokens), also give -user NAME (or $PRIGH_USER).
    [exit 1]
    $ prigh tool-host -connect 127.0.0.1:PORT -token bad -user me
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    Check -user and -token (or $PRIGH_USER and $PRIGH_TOKEN) against the backend's -tokens NAME=TOKEN (or -host-tokens) entries.
    [exit 1]
    |}]
;;

let%expect_test "run: confirmations and questions are refused, not waited for" =
  with_sandbox
  @@ fun t ->
  write t ".prigh/config.json" {|{"approval_mode": "ask"}|};
  write
    t
    "script.json"
    {|[
  {"text": "", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "echo hi"}}]},
  {"text": "", "tool_calls": [{"id": "q1", "name": "ask_user", "arguments": {"questions": [{"question": "Which?"}]}}]},
  {"text": "done"}
]|};
  prigh
    t
    [ "run"
    ; "-faux-script"
    ; Filename.concat t.dir "script.json"
    ; "-cwd"
    ; t.dir
    ; "go"
    ];
  [%expect
    {|
    $ prigh run -faux-script $DIR/script.json -cwd $DIR go
    done
    [tool] bash {"command":"echo hi"}
    [tool] bash echo hi needs confirmation: denied (headless)
    [tool] error: [denied by user] Their reason: prigh run is headless, so nobody can confirm it; the user can allow it with approval_mode or permissions in ~/.prigh/config.json
    [tool] ask_user {"questions":[{"question":"Which?"}]}
    [tool] error: [cannot ask: prigh run is headless, so nobody can answer; decide yourself and say what you assumed]
    [deepseek-flash] tokens: in=0 (cached 0) out=0 cost=$0.0000 session=$DIR/.prigh/sessions/<stamp>_<id>.jsonl
    |}]
;;

let%expect_test
    "run: a turn that ends with a job running is woken by its report"
  =
  with_sandbox
  @@ fun t ->
  write
    t
    "script.json"
    {|[
  {"text": "", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "sleep 0.5; echo built", "background": true}}]},
  {"text": "started the build"},
  {"text": "the build finished"}
]|};
  (* The job needs bash, wherever it is. *)
  prigh
    ~path:(Sys.getenv_exn "PATH")
    t
    [ "run"
    ; "-faux-script"
    ; Filename.concat t.dir "script.json"
    ; "-cwd"
    ; t.dir
    ; "go"
    ];
  [%expect
    {|
    $ prigh run -faux-script $DIR/script.json -cwd $DIR go
    started the build
    the build finished
    [tool] bash {"command":"sleep 0.5; echo built","background":true}
    [tool] started job j1: sleep 0.5; echo built; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    [deepseek-flash] tokens: in=0 (cached 0) out=0 cost=$0.0000 session=$DIR/.prigh/sessions/<stamp>_<id>.jsonl
    |}]
;;

let%expect_test "compaction-eval: a developer's tool, left out of the help" =
  with_sandbox
  @@ fun t ->
  let dir = Filename.concat t.dir ".prigh/sessions" in
  let s = Session.create ~dir ~cwd:"/proj" () in
  let big = String.make 9000 'x' in
  List.iter
    ([ Message.user "Fix it. Don't touch the lexer." ]
     @ List.concat_map (List.range 0 6) ~f:(fun i ->
       let id = sprintf "c%d" i in
       [ Message.Assistant
           { content =
               [ Tool_call
                   { id
                   ; name = "read"
                   ; arguments = sprintf {|{"path":"f%d.ml"}|} i
                   }
               ]
           ; stop_reason = Tool_use
           ; usage = Usage.zero
           ; model = "m"
           }
       ; Message.Tool_result
           { tool_call_id = id
           ; tool_name = "read"
           ; text = big
           ; is_error = false
           ; images = []
           }
       ]))
    ~f:(fun m -> ignore (Session.append_message s m : Session.Entry.t));
  let before = In_channel.read_all (Session.path s) in
  let p = spawn t [ "-help" ] in
  Core_unix.close p.stdin;
  let help = In_channel.input_all (Core_unix.in_channel_of_descr p.stdout) in
  ignore (Core_unix.waitpid p.pid : (unit, _) Result.t);
  printf
    "in the help: %b\n"
    (String.is_substring help ~substring:"compaction-eval");
  prigh t [ "compaction-eval" ];
  prigh t [ "compaction-eval"; "-all"; "-strategies"; "none,trim" ];
  prigh t [ "compaction-eval"; "nope" ];
  let prefix = String.prefix (Session.id s) 6 in
  prigh
    ~hide:[ prefix, "<prefix>" ]
    t
    [ "compaction-eval"
    ; prefix
    ; "-window"
    ; "20000"
    ; "-keep-turns"
    ; "2"
    ; "-overhead-tokens"
    ; "0"
    ];
  printf
    "file unchanged: %b\n"
    (String.equal before (In_channel.read_all (Session.path s)));
  [%expect {|
    in the help: false
    $ prigh compaction-eval
    name sessions (ids, as prigh sessions list shows, or files), or pass -all
    [exit 2]
    $ prigh compaction-eval -all -strategies none,trim
    -strategies: unknown strategy "trim": one of none, mask, summarise, mask+summarise
    [exit 2]
    $ prigh compaction-eval nope
    nope: no session "nope"
    prigh sessions list shows the saved sessions (in $DIR/.prigh/sessions)
    [exit 2]
    $ prigh compaction-eval <prefix> -window 20000 -keep-turns 2 -overhead-tokens 0
    session <id> "Fix it. Don't touch the lexer."
      deepseek/deepseek-flash (default), window 20.0k, summarise over 16.0k: 13 messages, recorded input 0
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               11.3k    0    33.9k    11.3k    16.4k     0     0     0  13.5k       1/0/1     6/6       -       -
      mask                9027    0    27.2k    13.6k    18.4k     1     1     0   6850       1/0/1     6/6       -       -
      summarise          11.3k    0    33.9k    11.3k    16.4k     0     0     0  13.5k       1/0/1     6/6       -       -
      mask+summarise      9027    0    27.2k    13.6k    18.4k     1     1     0   6850       1/0/1     6/6       -       -

    aggregate over 1 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               11.3k    0    33.9k    11.3k    16.4k     0     0     0  13.5k       1/0/1     6/6       -       -
      mask                9027    0    27.2k    13.6k    18.4k     1     1     0   6850       1/0/1     6/6       -       -
      summarise          11.3k    0    33.9k    11.3k    16.4k     0     0     0  13.5k       1/0/1     6/6       -       -
      mask+summarise      9027    0    27.2k    13.6k    18.4k     1     1     0   6850       1/0/1     6/6       -       -
    file unchanged: true
    |}]
;;
