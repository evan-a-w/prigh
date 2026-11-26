open! Core
open! Prigh
open Tool_test_helpers
open! Test_cli

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
  (* The saved connection: -remote uses it, and -connect to its address
     takes its token. *)
  prigh t [ "tool-host"; "-remote" ];
  write
    t
    ".prigh/tui-connection.sexp"
    (sprintf "((connect %s) (token stale))" address);
  prigh t [ "tool-host"; "-remote" ];
  prigh t [ "tool-host"; "-connect"; address ];
  write
    t
    ".prigh/tui-connection.sexp"
    (sprintf "((connect %s) (token good))" address);
  let host = spawn t [ "tool-host"; "-remote"; "-name"; "box" ] in
  let err = Core_unix.in_channel_of_descr host.stderr in
  let line = Option.value_exn (In_channel.input_line err) in
  print_endline
    (String.substr_replace_all line ~pattern:address ~with_:"127.0.0.1:PORT"
     |> String.split ~on:' '
     |> List.take_while ~f:(Fn.non (String.equal "as"))
     |> String.concat ~sep:" ");
  stop host;
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
    No token was given, and none is saved for 127.0.0.1:PORT: pass the backend's with -token SECRET (or $PRIGH_TOKEN), or sign in once with prigh-tui -connect 127.0.0.1:PORT -token SECRET (or prigh web -connect 127.0.0.1:PORT -token SECRET), which saves it.
    [exit 1]
    $ prigh tool-host -connect 127.0.0.1:PORT -token bad
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    Check -token (or $PRIGH_TOKEN) against the backend's; if it serves several users (-tokens), also give -user NAME (or $PRIGH_USER).
    [exit 1]
    $ prigh tool-host -connect 127.0.0.1:PORT -token bad -user me
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    Check -user and -token (or $PRIGH_USER and $PRIGH_TOKEN) against the backend's -tokens NAME=TOKEN (or -host-tokens) entries.
    [exit 1]
    $ prigh tool-host -remote
    prigh tool-host: no backend to connect to: pass -connect HOST:PORT (or a prigh-web URL such as wss://host/ws) and -token SECRET, set $PRIGH_CONNECT, or save a connection first with prigh-tui -connect HOST:PORT -token SECRET (or prigh web -connect HOST:PORT -token SECRET).
    [exit 2]
    $ prigh tool-host -remote
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    The credentials are the ones saved for 127.0.0.1:PORT in $DIR/.prigh/tui-connection.sexp: sign in again with /signin in prigh-tui (or prigh-tui -connect 127.0.0.1:PORT -token SECRET [-user NAME]), or pass -token SECRET (and -user NAME).
    [exit 1]
    $ prigh tool-host -connect 127.0.0.1:PORT
    prigh tool-host: 127.0.0.1:PORT refused the login: unauthorised: bad user name or password
    The credentials are the ones saved for 127.0.0.1:PORT in $DIR/.prigh/tui-connection.sexp: sign in again with /signin in prigh-tui (or prigh-tui -connect 127.0.0.1:PORT -token SECRET [-user NAME]), or pass -token SECRET (and -user NAME).
    [exit 1]
    prigh tool-host: connected to 127.0.0.1:PORT
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
                   ; extra = None
                   }
               ]
           ; stop_reason = Tool_use
           ; usage = Usage.zero
           ; model = "m"
           ; account = None
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
  (* The threshold honours compact_at_tokens, and -compact-at over it. *)
  Out_channel.write_all
    (Filename.concat t.dir ".prigh/config.json")
    ~data:{|{"compact_at_tokens": 10000}|};
  List.iter
    [ []; [ "-compact-at"; "12000" ]; [ "-compact-at"; "0" ] ]
    ~f:(fun flags ->
      prigh
        ~hide:[ prefix, "<prefix>" ]
        t
        ([ "compaction-eval"
         ; prefix
         ; "-window"
         ; "20000"
         ; "-strategies"
         ; "none"
         ]
         @ flags));
  [%expect
    {|
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
    $ prigh compaction-eval <prefix> -window 20000 -strategies none
    session <id> "Fix it. Don't touch the lexer."
      deepseek/deepseek-flash (default), window 20.0k, summarise over 10.0k: 13 messages, recorded input 0
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               15.4k    0    58.4k    15.4k    23.5k     0     0     0  17.6k       1/0/1     6/6       -       -

    aggregate over 1 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               15.4k    0    58.4k    15.4k    23.5k     0     0     0  17.6k       1/0/1     6/6       -       -
    $ prigh compaction-eval <prefix> -window 20000 -strategies none -compact-at 12000
    session <id> "Fix it. Don't touch the lexer."
      deepseek/deepseek-flash (default), window 20.0k, summarise over 12.0k: 13 messages, recorded input 0
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               15.4k    0    58.4k    15.4k    23.5k     0     0     0  17.6k       1/0/1     6/6       -       -

    aggregate over 1 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               15.4k    0    58.4k    15.4k    23.5k     0     0     0  17.6k       1/0/1     6/6       -       -
    $ prigh compaction-eval <prefix> -window 20000 -strategies none -compact-at 0
    session <id> "Fix it. Don't touch the lexer."
      deepseek/deepseek-flash (default), window 20.0k, summarise over 16.0k: 13 messages, recorded input 0
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               15.4k    0    58.4k    15.4k    23.5k     0     0     0  17.6k       1/0/1     6/6       -       -

    aggregate over 1 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               15.4k    0    58.4k    15.4k    23.5k     0     0     0  17.6k       1/0/1     6/6       -       -
    |}]
;;
