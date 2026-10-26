open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let assistant content : Message.t =
  Assistant
    { content
    ; usage = { Usage.zero with input = 100; cache_read = 900 }
    ; stop_reason =
        (if
           List.exists content ~f:(function
             | Content.Tool_call _ -> true
             | Text _ | Thinking _ | Server_tool _ -> false)
         then Tool_use
         else End_turn)
    ; model = "anthropic/claude-opus-4-5"
    }
;;

let call id name arguments =
  assistant [ Text "working"; Tool_call { id; name; arguments } ]
;;

let result ?(is_error = false) id name text : Message.t =
  Tool_result
    { tool_call_id = id; tool_name = name; text; is_error; images = [] }
;;

let file i = sprintf "src/part_%d.ml" i

let contents i =
  String.concat (List.init 400 ~f:(fun l -> sprintf "let v%d_%d = %d\n" i l l))
;;

(* A user's task: a file read, then [turns] rounds of reading, editing and
   building, with a build error every third round and a long message from
   the user half-way. *)
let messages ~turns =
  [ Message.user
      "Fix the parser. Don't touch the lexer, and make sure the tests pass."
  ; call "c0" "read" {|{"path":"README.md"}|}
  ; result "c0" "read" "# The parser"
  ]
  @ List.concat_map
      (List.range 1 (turns + 1))
      ~f:(fun i ->
        let id k = sprintf "c%d_%s" i k in
        [ call (id "read") "read" (sprintf {|{"path":"%s"}|} (file i))
        ; result (id "read") "read" (contents i)
        ; call
            (id "edit")
            "edit"
            (sprintf {|{"path":"%s","old":"let v","new":"let w"}|} (file i))
        ; result (id "edit") "edit" "Edited."
        ; call (id "bash") "bash" {|{"command":"dune build"}|}
        ; (if i % 3 = 0
           then
             result
               ~is_error:true
               (id "bash")
               "bash"
               (sprintf "Error: Unbound value w%d_0 in %s\nexit 1" i (file i))
           else result (id "bash") "bash" "built")
        ]
        @
        if i = turns / 2
        then
          [ assistant [ Text "Half way there." ]
          ; Message.user
              ("Keep the public API stable. You must not rename exported \
                values. "
               ^ String.concat
                   (List.init 120 ~f:(fun _ -> "Some more detail here. ")))
          ]
        else [])
  @ [ assistant [ Text "Done: the parser is fixed." ] ]
;;

let make_session t ~turns =
  let session = Session.create ~dir:t.dir ~cwd:"/w" () in
  ignore
    (Session.set_model session ~model:"anthropic/claude-opus-4-5" ~thinking:Off
     : Session.Entry.t);
  List.iter (messages ~turns) ~f:(fun m ->
    ignore (Session.append_message session m : Session.Entry.t));
  session
;;

let config =
  { Compaction_eval.Config.default with
    window = Some 20_000
  ; threshold_tokens = Some 14_000
  ; keep_recent_tokens = 3_000
  ; keep_turns = 4
  ; summary_tokens = 200
  ; overhead_tokens = 500
  }
;;

let%expect_test "offline: each strategy on a session" =
  with_sandbox
  @@ fun t ->
  let session = make_session t ~turns:18 in
  let report =
    Compaction_eval.report [ Compaction_eval.session config session ]
  in
  print_string (mask t (Compaction_eval.Report.to_table report));
  [%expect
    {|
    session <id> "Fix the parser. Don't touch the lexer, and make sure the tes"
      anthropic/claude-opus-4-5, window 20.0k, summarise over 14.0k: 114 messages, recorded input 57.0k
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               32.4k   21   940.5k    32.4k   131.3k     0     0     0  32.4k       2/0/2     1/1   18/18     6/6
      mask                9879    0   353.2k    39.2k    80.4k     4     4     0   7485       2/0/2     1/1   18/18     6/6
      summarise          14.0k    0   448.8k    33.7k    83.7k     4     0     2   6904       2/0/2     1/1   18/18     2/6
      mask+summarise      9879    0   353.2k    39.2k    80.4k     4     4     0   7485       2/0/2     1/1   18/18     6/6

    aggregate over 1 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               32.4k   21   940.5k    32.4k   131.3k     0     0     0  32.4k       2/0/2     1/1   18/18     6/6
      mask                9879    0   353.2k    39.2k    80.4k     4     4     0   7485       2/0/2     1/1   18/18     6/6
      summarise          14.0k    0   448.8k    33.7k    83.7k     4     0     2   6904       2/0/2     1/1   18/18     2/6
      mask+summarise      9879    0   353.2k    39.2k    80.4k     4     4     0   7485       2/0/2     1/1   18/18     6/6
    |}]
;;

let%expect_test "offline: the defaults, and the aggregate over sessions" =
  with_sandbox
  @@ fun t ->
  let sessions = [ make_session t ~turns:4; make_session t ~turns:30 ] in
  let report =
    Compaction_eval.report
      (List.map
         sessions
         ~f:
           (Compaction_eval.session
              { config with window = Some 30_000; threshold_tokens = None }))
  in
  print_string (mask t (Compaction_eval.Report.to_table report));
  [%expect
    {|
    session <id> "Fix the parser. Don't touch the lexer, and make sure the tes"
      anthropic/claude-opus-4-5, window 30.0k, summarise over 24.0k: 30 messages, recorded input 15.0k
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none                7959    0    66.5k     7959    15.8k     0     0     0   7965       2/0/2     1/1     4/4     1/1
      mask                7959    0    66.5k     7959    15.8k     0     0     0   7965       2/0/2     1/1     4/4     1/1
      summarise           7959    0    66.5k     7959    15.8k     0     0     0   7965       2/0/2     1/1     4/4     1/1
      mask+summarise      7959    0    66.5k     7959    15.8k     0     0     0   7965       2/0/2     1/1     4/4     1/1

    session <id> "Fix the parser. Don't touch the lexer, and make sure the tes"
      anthropic/claude-opus-4-5, window 30.0k, summarise over 24.0k: 186 messages, recorded input 93.0k
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               53.8k   42    2.52M    53.8k   314.1k     0     0     0  53.8k       2/0/2     1/1   30/30   10/10
      mask               15.0k    0   831.6k    63.1k   155.7k     4     4     0   9936       2/0/2     1/1   30/30   10/10
      summarise          22.9k    0    1.11M    55.2k   174.0k     4     0     2  10.5k       2/0/2     1/1   30/30    2/10
      mask+summarise     15.0k    0   831.6k    63.1k   155.7k     4     4     0   9936       2/0/2     1/1   30/30   10/10

    aggregate over 2 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               53.8k   42    2.59M    61.8k   329.9k     0     0     0  61.8k       4/0/4     2/2   34/34   11/11
      mask               15.0k    0   898.1k    71.0k   171.5k     4     4     0  17.9k       4/0/4     2/2   34/34   11/11
      summarise          22.9k    0    1.17M    63.2k   189.8k     4     0     2  18.5k       4/0/4     2/2   34/34    3/11
      mask+summarise     15.0k    0   898.1k    71.0k   171.5k     4     4     0  17.9k       4/0/4     2/2   34/34   11/11
    |}];
  (* One result as the -sexp output shows it. *)
  let mask_only =
    List.find_exn report.aggregate ~f:(fun r ->
      Compaction_eval.Strategy.equal r.strategy Mask)
  in
  print_s ([%sexp_of: Compaction_eval.Result.t] mask_only);
  [%expect
    {|
    ((strategy mask) (requests 108) (peak_tokens 14983) (over_window 0)
     (input_tokens 898112) (uncached_tokens 71045) (cached_cost_tokens 171513)
     (cache_invalidations 4) (mask_batches 4) (compactions 0)
     (final_tokens 17901) (users_verbatim 4) (users_shortened 0) (users_total 4)
     (files_read ((hit 2) (total 2))) (files_modified ((hit 34) (total 34)))
     (errors ((hit 11) (total 11))) (live ()))
    |}]
;;

let%expect_test
    "the recorded conversation is replayed, not its compacted context"
  =
  with_sandbox
  @@ fun t ->
  let session = make_session t ~turns:10 in
  let ids = List.filter_map (Session.context_ids session) ~f:Fn.id in
  ignore
    (Session.append_compaction
       session
       ~summary:"short"
       ~kept_from:(List.nth_exn ids 40)
     : Session.Entry.t);
  ignore
    (Session.append_masked session ~through:(List.nth_exn ids 50)
     : Session.Entry.t);
  printf "context: %d messages\n" (List.length (Session.messages session));
  let report =
    Compaction_eval.session { config with strategies = [ None_ ] } session
  in
  printf "replayed: %d messages\n" report.messages;
  print_string
    (mask
       t
       (Compaction_eval.Report.to_table (Compaction_eval.report [ report ])));
  [%expect
    {|
    context: 27 messages
    replayed: 66 messages
    session <id> "Fix the parser. Don't touch the lexer, and make sure the tes"
      anthropic/claude-opus-4-5, window 20.0k, summarise over 14.0k: 66 messages, recorded input 33.0k
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               18.2k    0   314.4k    18.2k    52.3k     0     0     0  18.2k       2/0/2     1/1   10/10     3/3

    aggregate over 1 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               18.2k    0   314.4k    18.2k    52.3k     0     0     0  18.2k       2/0/2     1/1   10/10     3/3
    |}]
;;

let%expect_test "strategies by name" =
  List.iter
    [ "none"; "mask"; "summarize"; "mask+summarise"; "both"; "trim" ]
    ~f:(fun s ->
      print_s
        ([%sexp_of: Compaction_eval.Strategy.t Or_error.t]
           (Compaction_eval.Strategy.of_string s)));
  [%expect
    {|
    (Ok none)
    (Ok mask)
    (Ok summarise)
    (Ok mask+summarise)
    (Ok mask+summarise)
    (Error
     "unknown strategy \"trim\": one of none, mask, summarise, mask+summarise")
    |}]
;;

(* As long as the offline stub, so that the live run compacts where the
   offline one does. *)
let summary_text = "[stub summary] " ^ String.make (800 - 15) '.'

let%expect_test "live: a model writes the summaries and answers the probes" =
  with_sandbox
  @@ fun t ->
  let session = make_session t ~turns:18 in
  let offline = Compaction_eval.session config session in
  let usage = { Usage.input = 1000; output = 200; cache_read = 0 } in
  let answer =
    "1. src/part_17.ml, src/part_18.ml\n\
     2. Don't touch the lexer; make sure the tests pass.\n\
     3. Error: Unbound value w18_0 in src/part_18.ml; we renamed it back."
  in
  let replies =
    List.concat_map offline.results ~f:(fun r ->
      List.init r.compactions ~f:(fun _ ->
        { (Reply.text summary_text) with usage })
      @ [ { (Reply.text answer) with usage } ])
  in
  let requests = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        requests
        := (match List.last r.messages with
            | Some (User u)
              when String.is_substring u.text ~substring:"compaction-eval" ->
              "probe"
            | Some _ | None -> "summary")
           :: !requests)
      replies
  in
  let live =
    { Compaction_eval.Live.Setup.provider
    ; model = Model.default
    ; cancel = Cancellation.never
    }
  in
  let report = Compaction_eval.session ~live config session in
  print_endline (String.concat ~sep:" " (List.rev !requests));
  print_string
    (mask
       t
       (Compaction_eval.Report.to_table (Compaction_eval.report [ report ])));
  List.iter report.results ~f:(fun r ->
    print_s
      [%sexp
        (Compaction_eval.Strategy.to_string r.strategy : string)
      , (r.compactions : int)
      , (Option.map r.live ~f:(fun l -> l.summaries, l.usage)
         : (int * Usage.t) option)]);
  [%expect
    {|
    probe probe summary summary probe probe
    session <id> "Fix the parser. Don't touch the lexer, and make sure the tes"
      anthropic/claude-opus-4-5, window 20.0k, summarise over 14.0k: 114 messages, recorded input 57.0k
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               32.4k   21   940.5k    32.4k   131.3k     0     0     0  32.4k       2/0/2     1/1   18/18     6/6
                      live: 0 summaries (0 failed), $0.0005; 2/18 written files, 1/2 constraints, 1/1 last error
      mask                9879    0   353.2k    39.2k    80.4k     4     4     0   7485       2/0/2     1/1   18/18     6/6
                      live: 0 summaries (0 failed), $0.0005; 2/18 written files, 1/2 constraints, 1/1 last error
      summarise          14.0k    0   448.8k    33.7k    83.7k     4     0     2   6904       2/0/2     1/1   18/18     2/6
                      live: 2 summaries (0 failed), $0.0016; 2/18 written files, 1/2 constraints, 1/1 last error
      mask+summarise      9879    0   353.2k    39.2k    80.4k     4     4     0   7485       2/0/2     1/1   18/18     6/6
                      live: 0 summaries (0 failed), $0.0005; 2/18 written files, 1/2 constraints, 1/1 last error

    aggregate over 1 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               32.4k   21   940.5k    32.4k   131.3k     0     0     0  32.4k       2/0/2     1/1   18/18     6/6
                      live: 0 summaries (0 failed), $0.0005; 2/18 written files, 1/2 constraints, 1/1 last error
      mask                9879    0   353.2k    39.2k    80.4k     4     4     0   7485       2/0/2     1/1   18/18     6/6
                      live: 0 summaries (0 failed), $0.0005; 2/18 written files, 1/2 constraints, 1/1 last error
      summarise          14.0k    0   448.8k    33.7k    83.7k     4     0     2   6904       2/0/2     1/1   18/18     2/6
                      live: 2 summaries (0 failed), $0.0016; 2/18 written files, 1/2 constraints, 1/1 last error
      mask+summarise      9879    0   353.2k    39.2k    80.4k     4     4     0   7485       2/0/2     1/1   18/18     6/6
                      live: 0 summaries (0 failed), $0.0005; 2/18 written files, 1/2 constraints, 1/1 last error
    (none 0 ((0 ((input 1000) (output 200) (cache_read 0)))))
    (mask 0 ((0 ((input 1000) (output 200) (cache_read 0)))))
    (summarise 2 ((2 ((input 3000) (output 600) (cache_read 0)))))
    (mask+summarise 0 ((0 ((input 1000) (output 200) (cache_read 0)))))
    |}]
;;

let%expect_test "live: a final context too large for the model is not probed" =
  with_sandbox
  @@ fun t ->
  let session = make_session t ~turns:18 in
  let provider = Faux_provider.create [] in
  let live =
    { Compaction_eval.Live.Setup.provider
    ; model = { Model.default with context_window = 8_000 }
    ; cancel = Cancellation.never
    }
  in
  let report =
    Compaction_eval.session ~live { config with strategies = [ None_ ] } session
  in
  print_string
    (mask
       t
       (Compaction_eval.Report.to_table (Compaction_eval.report [ report ])));
  [%expect
    {|
    session <id> "Fix the parser. Don't touch the lexer, and make sure the tes"
      anthropic/claude-opus-4-5, window 20.0k, summarise over 14.0k: 114 messages, recorded input 57.0k
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               32.4k   21   940.5k    32.4k   131.3k     0     0     0  32.4k       2/0/2     1/1   18/18     6/6
                      live: 0 summaries (0 failed), $0.0000; n/a written files, n/a constraints, n/a last error

    aggregate over 1 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      none               32.4k   21   940.5k    32.4k   131.3k     0     0     0  32.4k       2/0/2     1/1   18/18     6/6
                      live: 0 summaries (0 failed), $0.0000; n/a written files, n/a constraints, n/a last error
    |}]
;;

let%expect_test "mask+summarise: summarising when masking frees too little" =
  with_sandbox
  @@ fun t ->
  let session = make_session t ~turns:30 in
  let report =
    Compaction_eval.session
      { config with keep_turns = 30; strategies = [ Mask; Mask_and_summarise ] }
      session
  in
  print_string
    (mask
       t
       (Compaction_eval.Report.to_table (Compaction_eval.report [ report ])));
  [%expect
    {|
    session <id> "Fix the parser. Don't touch the lexer, and make sure the tes"
      anthropic/claude-opus-4-5, window 20.0k, summarise over 14.0k: 186 messages, recorded input 93.0k
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      mask               20.3k   14    1.46M   378.5k   580.8k    20    20     0  20.3k       2/0/2     1/1   30/30   10/10
      mask+summarise     14.0k    0   751.2k    56.6k   140.2k     8     0     4   6952       2/0/2     1/1   30/30    2/10

    aggregate over 1 sessions
      strategy            peak over    input uncached  cached$ inval masks  summ  final  user v/s/n    read written  errors
      mask               20.3k   14    1.46M   378.5k   580.8k    20    20     0  20.3k       2/0/2     1/1   30/30   10/10
      mask+summarise     14.0k    0   751.2k    56.6k   140.2k     8     0     4   6952       2/0/2     1/1   30/30    2/10
    |}]
;;
