open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let sessions_dir t = Filename.concat t.dir "sessions"

(* 2026-01-01 00:00 UTC. *)
let day n = 1767225600. +. (Float.of_int n *. 86400.)

let assistant ?(calls = []) text =
  Message.Assistant
    { content =
        (if String.is_empty text then [] else [ Content.Text text ])
        @ List.map calls ~f:(fun (id, name, arguments) ->
          Content.Tool_call { id; name; arguments })
    ; stop_reason = (if List.is_empty calls then End_turn else Tool_use)
    ; usage = Usage.zero
    ; model = "m"
    }
;;

let result id text =
  Message.Tool_result
    { tool_call_id = id
    ; tool_name = "bash"
    ; text
    ; is_error = false
    ; images = []
    }
;;

(* A saved session last written on day [n], its messages a minute apart. *)
let session t ~day:n ?name ?(cwd = "/work/app") ?model messages =
  let s = Session.create ~dir:(sessions_dir t) ~cwd () in
  Option.iter name ~f:(fun name ->
    ignore (Session.set_name s ~name : Session.Entry.t));
  Option.iter model ~f:(fun model ->
    ignore (Session.set_model s ~model ~thinking:Off : Session.Entry.t));
  List.iteri messages ~f:(fun i message ->
    ignore
      (Session.append_message s ~at:(day n +. (60. *. Float.of_int i)) message
       : Session.Entry.t));
  Core_unix.utimes (Session.path s) ~access:(day n) ~modif:(day n);
  s
;;

let three t =
  let parser =
    session
      t
      ~day:1
      ~name:"Fix the parser"
      [ Message.user "the tokenizer drops the last token"
      ; assistant
          ~calls:[ "c1", "bash", {|{"command": "dune build @runtest"}|} ]
          "Looking at the Lexer module."
      ; result "c1" "1 test failed: unterminated string"
      ; assistant "Fixed: the lexer now flushes at end of input."
      ]
  in
  let deploy =
    session
      t
      ~day:2
      ~cwd:"/work/infra"
      [ Message.user "deploy the staging cluster"
      ; assistant "Deployed with the Zebra rollout strategy; the lexer is fine."
      ]
  in
  let notes =
    session
      t
      ~day:3
      ~name:"Release notes"
      [ Message.user "write release notes"; assistant "Done." ]
  in
  parser, deploy, notes
;;

let tool ?current t =
  Tool_sessions.create
    ~search:(Tool_sessions.local_search ~dir:(fun () -> sessions_dir t))
    ~sessions_dir:(fun () -> sessions_dir t)
    ~current:(fun () -> current)
    ()
;;

let args fields =
  Jsonaf.to_string
    (`Object
        (List.map fields ~f:(fun (key, value) -> key, Jsonaf.of_string value)))
;;

let search ?current ?limit ?session t query =
  run
    t
    (tool ?current t)
    (args
       ([ "action", {|"search"|}; "query", sprintf "%S" query ]
        @ Option.value_map limit ~default:[] ~f:(fun n ->
          [ "limit", Int.to_string n ])
        @ Option.value_map session ~default:[] ~f:(fun s ->
          [ "session", sprintf "%S" (Session.id s) ])))
;;

let%expect_test "search: names and what was said, the best first" =
  with_sandbox
  @@ fun t ->
  let parser, _, _ = three t in
  search t "PARSER";
  [%expect
    {|
    <id>  2026-01-02  "Fix the parser"  /work/app  4 messages
    (for message indices to read from: search with session, or outline)
    |}];
  (* Directories are not searched. *)
  search t "infra";
  [%expect
    {| No session matches: every word must match what was said in a session or its name, so try fewer or other words. |}];
  (* Said in two sessions: the most recent first, each with a snippet. *)
  search t "lexer";
  [%expect
    {|
    <id>  2026-01-03  "deploy the staging cluster"  /work/infra  2 messages
      …strategy; the lexer is fine.
    <id>  2026-01-02  "Fix the parser"  /work/app  4 messages
      Looking at the Lexer module.
    (for message indices to read from: search with session, or outline)
    |}];
  (* A name's match; every word must match. *)
  search t "notes";
  search t "release lexer";
  [%expect
    {|
    <id>  2026-01-04  "Release notes"  /work/app  2 messages
      write release notes
    (for message indices to read from: search with session, or outline)
    No session matches: every word must match what was said in a session or its name, so try fewer or other words.
    |}];
  (* Tool results are not searched ("unterminated"); a typo still finds. *)
  search t "parser unterminated flushes";
  [%expect
    {| No session matches: every word must match what was said in a session or its name, so try fewer or other words. |}];
  search ~current:(Session.id parser) t "tokenizr";
  [%expect
    {|
    <id>  2026-01-02  "Fix the parser"  /work/app  4 messages  (this session)
      the tokenizer drops the last token
    (for message indices to read from: search with session, or outline)
    |}];
  search t "   ";
  [%expect {| ERROR: query is required: the words to look for |}]
;;

let%expect_test "search: limit" =
  with_sandbox
  @@ fun t ->
  ignore (three t : Session.t * Session.t * Session.t);
  ignore
    (session t ~day:4 [ Message.user "say \"quoted\" things\nabout the lexer" ]
     : Session.t);
  search t ~limit:1 "lexer";
  [%expect
    {|
    <id>  2026-01-05  "say \"quoted\" things about the lexer"  /work/app  1 messages
      …things about the lexer
    (more sessions match: add words or raise limit)
    (for message indices to read from: search with session, or outline)
    |}];
  search t ~limit:2 "the";
  [%expect
    {|
    <id>  2026-01-02  "Fix the parser"  /work/app  4 messages
      the tokenizer drops the last token
    <id>  2026-01-05  "say \"quoted\" things about the lexer"  /work/app  1 messages
      …things about the lexer
    (more sessions match: add words or raise limit)
    (for message indices to read from: search with session, or outline)
    |}];
  search t {|"quoted" zebra|};
  [%expect
    {| No session matches: every word must match what was said in a session or its name, so try fewer or other words. |}];
  search t {|"quoted" things|};
  [%expect
    {|
    <id>  2026-01-05  "say \"quoted\" things about the lexer"  /work/app  1 messages
      say "quoted" things about the lexer
    (for message indices to read from: search with session, or outline)
    |}]
;;

let%expect_test "search inside one session" =
  with_sandbox
  @@ fun t ->
  let parser, _, notes = three t in
  search t ~session:parser "lexer string";
  [%expect
    {|
    <id>  2026-01-02  "Fix the parser"  /work/app  4 messages
      [1] assistant: Looking at the Lexer module. bash {"command": "dune build @runtest"}
      [2] result bash: 1 test failed: unterminated string
      [3] assistant: Fixed: the lexer now flushes at end of input.
    |}];
  search t ~session:notes "lexer";
  [%expect
    {|
    <id>  2026-01-04  "Release notes"  /work/app  2 messages
    No message has any of these words.
    |}];
  let long =
    session
      t
      ~day:5
      (List.init 14 ~f:(fun i ->
         Message.user
           (sprintf
              "%s needle %d %s"
              (String.make 200 'y')
              i
              (String.make 200 'z'))))
  in
  search t ~session:long "needle";
  [%expect
    {|
    <id>  2026-01-06  "yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy\226\128\166"  /work/app  14 messages
      [0] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 0 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
      [1] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 1 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
      [2] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 2 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
      [3] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 3 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
      [4] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 4 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
      [5] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 5 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
      [6] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 6 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
      [7] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 7 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
      [8] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 8 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
      [9] user: …yyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyyy needle 9 zzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzzz…
    (4 more matching messages: add words)
    |}]
;;

let%expect_test "sessions are named by id or unique prefix, never by path" =
  with_sandbox
  @@ fun t ->
  let parser, _, _ = three t in
  let outline session =
    run
      t
      (tool t)
      (args [ "action", {|"outline"|}; "session", sprintf "%S" session ])
  in
  let id = Session.id parser in
  outline (String.prefix id 12);
  [%expect
    {|
    <id>  2026-01-02  "Fix the parser"  /work/app  4 messages
    [0] user 2026-01-02 00:00: the tokenizer drops the last token
    |}];
  outline "zz";
  [%expect
    {| ERROR: no session "zz"; action search of the sessions tool finds sessions and their ids |}];
  outline (Session.path parser);
  [%expect
    {| ERROR: "$DIR/sessions/<stamp>_<id>.jsonl" is a path: give the session's id (action search of the sessions tool finds it) |}];
  outline "../sessions/x";
  [%expect
    {| ERROR: "../sessions/x" is a path: give the session's id (action search of the sessions tool finds it) |}];
  run t (tool t) {|{"action": "outline"}|};
  [%expect
    {| ERROR: session is required: the id of one that action search shows |}];
  run t (tool t) {|{"action": "delete", "session": "x"}|};
  [%expect {| ERROR: unknown action "delete": search, outline or read |}];
  (* Seventeen sessions: two of their ids start with the same character. *)
  let ids =
    List.init 17 ~f:(fun i ->
      Session.id (session t ~day:(10 + i) [ Message.user "hi" ]))
  in
  let shared =
    List.find_map_exn ids ~f:(fun id ->
      let prefix = String.prefix id 1 in
      Option.some_if
        (List.count ids ~f:(String.is_prefix ~prefix) > 1
         && not (String.is_prefix (Session.id parser) ~prefix))
        prefix)
  in
  let context = Tool.Context.create ~env:t.env ~cwd:t.dir () in
  let result =
    Tool.execute
      (tool t)
      context
      (Jsonaf.of_string
         (args [ "action", {|"read"|}; "session", sprintf "%S" shared ]))
  in
  print_endline
    (String.substr_replace_all
       (mask t result.text)
       ~pattern:(sprintf "%S" shared)
       ~with_:"<prefix>"
     |> String.substr_replace_all ~pattern:", <id>" ~with_:"");
  [%expect
    {| session <prefix> is ambiguous: <id>; action search of the sessions tool finds sessions and their ids |}]
;;

let%expect_test "outline: the prompts, the last ones first" =
  with_sandbox
  @@ fun t ->
  let parser, _, _ = three t in
  let outline ?before session =
    run
      t
      (tool t)
      (args
         ([ "action", {|"outline"|}
          ; "session", sprintf "%S" (Session.id session)
          ]
          @ Option.value_map before ~default:[] ~f:(fun n ->
            [ "before", Int.to_string n ])))
  in
  outline parser;
  [%expect
    {|
    <id>  2026-01-02  "Fix the parser"  /work/app  4 messages
    [0] user 2026-01-02 00:00: the tokenizer drops the last token
    |}];
  let long =
    session
      t
      ~day:5
      ~name:"Long"
      (List.concat
         (List.init 45 ~f:(fun i ->
            [ Message.user (sprintf "prompt %d\nwith a second line" i)
            ; assistant (sprintf "reply %d" i)
            ])))
  in
  outline long;
  [%expect
    {|
    <id>  2026-01-06  "Long"  /work/app  90 messages
    (5 earlier prompts: outline with before=10)
    [10] user 2026-01-06 00:10: prompt 5 with a second line
    [12] user 2026-01-06 00:12: prompt 6 with a second line
    [14] user 2026-01-06 00:14: prompt 7 with a second line
    [16] user 2026-01-06 00:16: prompt 8 with a second line
    [18] user 2026-01-06 00:18: prompt 9 with a second line
    [20] user 2026-01-06 00:20: prompt 10 with a second line
    [22] user 2026-01-06 00:22: prompt 11 with a second line
    [24] user 2026-01-06 00:24: prompt 12 with a second line
    [26] user 2026-01-06 00:26: prompt 13 with a second line
    [28] user 2026-01-06 00:28: prompt 14 with a second line
    [30] user 2026-01-06 00:30: prompt 15 with a second line
    [32] user 2026-01-06 00:32: prompt 16 with a second line
    [34] user 2026-01-06 00:34: prompt 17 with a second line
    [36] user 2026-01-06 00:36: prompt 18 with a second line
    [38] user 2026-01-06 00:38: prompt 19 with a second line
    [40] user 2026-01-06 00:40: prompt 20 with a second line
    [42] user 2026-01-06 00:42: prompt 21 with a second line
    [44] user 2026-01-06 00:44: prompt 22 with a second line
    [46] user 2026-01-06 00:46: prompt 23 with a second line
    [48] user 2026-01-06 00:48: prompt 24 with a second line
    [50] user 2026-01-06 00:50: prompt 25 with a second line
    [52] user 2026-01-06 00:52: prompt 26 with a second line
    [54] user 2026-01-06 00:54: prompt 27 with a second line
    [56] user 2026-01-06 00:56: prompt 28 with a second line
    [58] user 2026-01-06 00:58: prompt 29 with a second line
    [60] user 2026-01-06 01:00: prompt 30 with a second line
    [62] user 2026-01-06 01:02: prompt 31 with a second line
    [64] user 2026-01-06 01:04: prompt 32 with a second line
    [66] user 2026-01-06 01:06: prompt 33 with a second line
    [68] user 2026-01-06 01:08: prompt 34 with a second line
    [70] user 2026-01-06 01:10: prompt 35 with a second line
    [72] user 2026-01-06 01:12: prompt 36 with a second line
    [74] user 2026-01-06 01:14: prompt 37 with a second line
    [76] user 2026-01-06 01:16: prompt 38 with a second line
    [78] user 2026-01-06 01:18: prompt 39 with a second line
    [80] user 2026-01-06 01:20: prompt 40 with a second line
    [82] user 2026-01-06 01:22: prompt 41 with a second line
    [84] user 2026-01-06 01:24: prompt 42 with a second line
    [86] user 2026-01-06 01:26: prompt 43 with a second line
    [88] user 2026-01-06 01:28: prompt 44 with a second line
    |}];
  outline ~before:10 long;
  [%expect
    {|
    <id>  2026-01-06  "Long"  /work/app  90 messages
    [0] user 2026-01-06 00:00: prompt 0 with a second line
    [2] user 2026-01-06 00:02: prompt 1 with a second line
    [4] user 2026-01-06 00:04: prompt 2 with a second line
    [6] user 2026-01-06 00:06: prompt 3 with a second line
    [8] user 2026-01-06 00:08: prompt 4 with a second line
    |}]
;;

let%expect_test "read: a few messages, cut, with the outline's indices" =
  with_sandbox
  @@ fun t ->
  let parser, _, _ = three t in
  let read ?from ?limit session =
    run
      t
      (tool t)
      (args
         ([ "action", {|"read"|}; "session", sprintf "%S" (Session.id session) ]
          @ Option.value_map from ~default:[] ~f:(fun n ->
            [ "from", Int.to_string n ])
          @ Option.value_map limit ~default:[] ~f:(fun n ->
            [ "limit", Int.to_string n ])))
  in
  read parser;
  [%expect
    {|
    <id>  2026-01-02  "Fix the parser"  /work/app  4 messages
    messages 0 to 3 of 4
    [0] user (2026-01-02 00:00): the tokenizer drops the last token
    [1] assistant:
    Looking at the Lexer module.
      -> bash dune build @runtest
    [2] result bash: 1 test failed: unterminated string
    [3] assistant: Fixed: the lexer now flushes at end of input.
    |}];
  read ~from:1 ~limit:2 parser;
  [%expect
    {|
    <id>  2026-01-02  "Fix the parser"  /work/app  4 messages
    messages 1 to 2 of 4
    [1] assistant:
    Looking at the Lexer module.
      -> bash dune build @runtest
    [2] result bash: 1 test failed: unterminated string
    (more: read with from=3)
    |}];
  read ~from:4 parser;
  [%expect
    {| ERROR: from=4 is not a message of this session: it has 4, 0 to 3 |}];
  let big =
    session
      t
      ~day:5
      ~name:"Big"
      ([ Message.user ("start " ^ String.make 2000 'u')
       ; assistant ~calls:[ "c1", "bash", {|{"command": "make"}|} ] ""
       ; result "c1" (String.make 1000 'r')
       ]
       @ List.init 20 ~f:(fun i ->
         assistant (sprintf "reply %d %s" i (String.make 1400 'z'))))
  in
  let lines () =
    String.split_lines [%expect.output]
    |> List.iter ~f:(fun line ->
      print_endline
        (if String.length line > 60
         then
           sprintf "%s… (%d bytes)" (String.prefix line 40) (String.length line)
         else line))
  in
  read ~from:0 ~limit:3 big;
  lines ();
  [%expect
    {|
    <id>  2026-01-06  "Big"  /work/app  23 messages
    messages 0 to 2 of 23
    [0] user (2026-01-06 00:00): start uuuuu… (1545 bytes)
    [1] assistant:
      -> bash make
    [2] result bash: rrrrrrrrrrrrrrrrrrrrrrr… (333 bytes)
    (more: read with from=3)
    |}];
  (* Thirty are asked for; the result's budget ends it sooner. *)
  read ~from:3 ~limit:99 big;
  lines ();
  [%expect
    {|
    <id>  2026-01-06  "Big"  /work/app  23 messages
    messages 3 to 10 of 23
    [3] assistant: reply 0 zzzzzzzzzzzzzzzzz… (1423 bytes)
    [4] assistant: reply 1 zzzzzzzzzzzzzzzzz… (1423 bytes)
    [5] assistant: reply 2 zzzzzzzzzzzzzzzzz… (1423 bytes)
    [6] assistant: reply 3 zzzzzzzzzzzzzzzzz… (1423 bytes)
    [7] assistant: reply 4 zzzzzzzzzzzzzzzzz… (1423 bytes)
    [8] assistant: reply 5 zzzzzzzzzzzzzzzzz… (1423 bytes)
    [9] assistant: reply 6 zzzzzzzzzzzzzzzzz… (1423 bytes)
    [10] assistant: reply 7 zzzzzzzzzzzzzzzz… (1424 bytes)
    (more: read with from=11)
    |}];
  read ~from:11 ~limit:99 big;
  lines ();
  [%expect
    {|
    <id>  2026-01-06  "Big"  /work/app  23 messages
    messages 11 to 18 of 23
    [11] assistant: reply 8 zzzzzzzzzzzzzzzz… (1424 bytes)
    [12] assistant: reply 9 zzzzzzzzzzzzzzzz… (1424 bytes)
    [13] assistant: reply 10 zzzzzzzzzzzzzzz… (1425 bytes)
    [14] assistant: reply 11 zzzzzzzzzzzzzzz… (1425 bytes)
    [15] assistant: reply 12 zzzzzzzzzzzzzzz… (1425 bytes)
    [16] assistant: reply 13 zzzzzzzzzzzzzzz… (1425 bytes)
    [17] assistant: reply 14 zzzzzzzzzzzzzzz… (1425 bytes)
    [18] assistant: reply 15 zzzzzzzzzzzzzzz… (1425 bytes)
    (more: read with from=19)
    |}];
  let empty = session t ~day:6 ~name:"Empty" [] in
  read empty;
  [%expect
    {|
    <id>  2026-01-07  "Empty"  /work/app  0 messages
    It has no messages.
    |}]
;;
