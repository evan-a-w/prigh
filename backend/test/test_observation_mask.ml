open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let is_masked text =
  String.is_substring text ~substring:"omitted to save context"
;;

let shape (m : Message.t) =
  match m with
  | User u when Session.is_summary_message m ->
    sprintf "S(%s)" (List.nth_exn (String.split_lines u.text) 1)
  | User u -> sprintf "U(%s)" (String.prefix u.text 20)
  | Assistant a ->
    (match Message.Assistant.tool_calls a with
     | [] -> sprintf "A(%s)" (Message.Assistant.text a)
     | calls ->
       sprintf
         "A[%s]"
         (String.concat ~sep:"," (List.map calls ~f:(fun c -> c.id))))
  | Tool_result r when is_masked r.text -> sprintf "M(%s)" r.tool_call_id
  | Tool_result r -> sprintf "T(%s,%d)" r.tool_call_id (String.length r.text)
;;

let shapes messages = String.concat ~sep:" " (List.map messages ~f:shape)

let call id ~name ~arguments =
  Message.Assistant
    { content = [ Text "working"; Tool_call { id; name; arguments } ]
    ; usage = Usage.zero
    ; stop_reason = Tool_use
    ; model = "m"
    }
;;

let result ?(is_error = false) ?(images = []) ?(name = "bash") id text =
  Message.Tool_result
    { tool_call_id = id; tool_name = name; text; is_error; images }
;;

let png = { Image.mime_type = "image/png"; data = "AAAA" }

let print_message (m : Message.t) =
  match m with
  | User u -> printf "user: %s\n" (String.prefix u.text 40)
  | Assistant a ->
    List.iter a.content ~f:(function
      | Text s -> printf "assistant text: %s\n" s
      | Tool_call c ->
        printf
          "call %s %s: %s (valid JSON: %b)\n"
          c.id
          c.name
          (String.prefix c.arguments 120)
          (Result.is_ok (Content.Tool_call.parse_arguments c))
      | Thinking _ | Server_tool _ -> ())
  | Tool_result r ->
    printf
      "result %s%s (%d images): %s\n"
      r.tool_call_id
      (if r.is_error then " error" else "")
      (List.length r.images)
      (String.prefix (String.tr r.text ~target:'\n' ~replacement:'|') 120)
;;

let%expect_test "what masking replaces" =
  let big = String.concat (List.init 412 ~f:(fun i -> sprintf "line %d\n" i)) in
  let content = String.make 5000 'c' in
  let messages =
    [ Message.user (String.make 5000 'u')
    ; call "c1" ~name:"bash" ~arguments:{|{"command":"seq 412"}|}
    ; result "c1" big
    ; result "c2" "small output"
    ; result ~is_error:true "c3" big
    ; result ~images:[ png; png ] ~name:"read" "c4" "an image"
    ; result ~images:[ png ] ~name:"read" "c5" big
    ; call
        "c6"
        ~name:"write"
        ~arguments:
          (sprintf {|{"path":"a.ml","content":%S}|} (content ^ "\nend"))
    ; call
        "c7"
        ~name:"apply_patch"
        ~arguments:
          (sprintf
             {|{"input":%S,"options":[%S,"short"]}|}
             (String.make 3000 'p')
             (String.make 1500 'q'))
    ; (* Large, but no single field is: kept as it is. *)
      call
        "c8"
        ~name:"multi"
        ~arguments:
          (sprintf
             "{%s}"
             (String.concat
                ~sep:", "
                (List.init 10 ~f:(fun i ->
                   sprintf {|"f%d": %S|} i (String.make 300 'x')))))
    ; call "c9" ~name:"bash" ~arguments:(String.make 3000 '{')
    ]
  in
  List.iter messages ~f:(fun m ->
    let masked = Observation_mask.mask m in
    print_message masked;
    (* Masking again changes nothing. *)
    if not (Message.equal (Observation_mask.mask masked) masked)
    then print_endline "  (not idempotent!)");
  [%expect
    {|
    user: uuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuuu
    assistant text: working
    call c1 bash: {"command":"seq 412"} (valid JSON: true)
    result c1 (0 images): [bash output omitted to save context: 412 lines, 4 KB; run the tool again if you need it]
    result c2 (0 images): small output
    result c3 error (0 images): line 0|line 1|line 2|line 3|line 4|line 5|line 6|line 7|line 8|line 9|line 10|line 11|line 12|line 13|line 14|line 15|li
    result c4 (0 images): an image|[2 images omitted to save context]
    result c5 (0 images): [read output omitted to save context: 412 lines, 4 KB, 1 image; run the tool again if you need it]
    assistant text: working
    call c6 write: {"path":"a.ml","content":"[omitted to save context: 2 lines, 5 KB]"} (valid JSON: true)
    assistant text: working
    call c7 apply_patch: {"input":"[omitted to save context: 1 lines, 3 KB]","options":["[omitted to save context: 1 lines, 1 KB]","short"]} (valid JSON: true)
    assistant text: working
    call c8 multi: {"f0": "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx (valid JSON: true)
    assistant text: working
    call c9 bash: {{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{{ (valid JSON: false)
    |}]
;;

let turn i =
  [ call (sprintf "c%d" i) ~name:"bash" ~arguments:"{}"
  ; result (sprintf "c%d" i) (String.make 4000 'x')
  ]
;;

let%expect_test "a plan masks what comes before the last turns" =
  let show ?keep_turns messages =
    print_s
      [%sexp
        (Observation_mask.plan ?keep_turns messages
         : Observation_mask.Plan.t option)]
  in
  let run = Message.user "task" :: List.concat_map (List.range 1 6) ~f:turn in
  print_endline (shapes run);
  show ~keep_turns:2 run;
  show ~keep_turns:5 run;
  (* Not enough turns to mask anything. *)
  show ~keep_turns:6 run;
  show run;
  (* Already masked: nothing more. *)
  let masked = Observation_mask.apply ~through:6 run in
  print_endline (shapes masked);
  show ~keep_turns:2 masked;
  show ~keep_turns:1 masked;
  [%expect
    {|
    U(task) A[c1] T(c1,4000) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000) A[c5] T(c5,4000)
    (((through 6) (masked 3) (tokens_saved 2937)))
    ()
    ()
    ()
    U(task) A[c1] M(c1) A[c2] M(c2) A[c3] M(c3) A[c4] T(c4,4000) A[c5] T(c5,4000)
    ()
    (((through 8) (masked 1) (tokens_saved 979)))
    |}]
;;

let%expect_test "when masking is worth it: relative to the compaction threshold"
  =
  let plan tokens_saved =
    { Observation_mask.Plan.through = 0; masked = 1; tokens_saved }
  in
  let show ~window ~cap cases =
    let model = { Model.default with context_window = window } in
    let settings = { Compaction_settings.window_only with max_tokens = cap } in
    printf
      "window %d, cap %s: compacts past %d\n"
      window
      (Option.value_map cap ~default:"none" ~f:Int.to_string)
      (Compaction_settings.threshold settings model);
    List.iter cases ~f:(fun (input_tokens, overflowed, saved) ->
      printf
        "  %6d tokens, overflowed %-5b, saves %5d: %b\n"
        input_tokens
        overflowed
        saved
        (Compaction_settings.worth_masking
           settings
           model
           ~input_tokens
           ~overflowed
           (plan saved)))
  in
  (* 80% of the window: masks past half of it when that frees a tenth, past
     the threshold too (only an overflow masks whatever it frees). *)
  show
    ~window:100_000
    ~cap:None
    [ 40_000, false, 50_000
    ; 50_000, false, 10_000
    ; 50_001, false, 10_000
    ; 60_000, false, 9_999
    ; 60_000, false, 10_000
    ; 90_000, false, 9_999
    ; 90_000, false, 10_000
    ; 60_000, true, 5
    ; 0, true, 1
    ];
  (* A 1M window compacted at the 200k cap masks past 125k when that frees
     25k, as a 250k window would; the window alone would wait for 500k. *)
  show
    ~window:1_000_000
    ~cap:(Some 200_000)
    [ 125_000, false, 25_000
    ; 125_001, false, 24_999
    ; 125_001, false, 25_000
    ; 400_000, false, 30_000
    ; 400_000, false, 24_999
    ];
  show ~window:1_000_000 ~cap:None [ 400_000, false, 30_000 ];
  [%expect
    {|
    window 100000, cap none: compacts past 80000
       40000 tokens, overflowed false, saves 50000: false
       50000 tokens, overflowed false, saves 10000: false
       50001 tokens, overflowed false, saves 10000: true
       60000 tokens, overflowed false, saves  9999: false
       60000 tokens, overflowed false, saves 10000: true
       90000 tokens, overflowed false, saves  9999: false
       90000 tokens, overflowed false, saves 10000: true
       60000 tokens, overflowed true , saves     5: true
           0 tokens, overflowed true , saves     1: true
    window 1000000, cap 200000: compacts past 200000
      125000 tokens, overflowed false, saves 25000: false
      125001 tokens, overflowed false, saves 24999: false
      125001 tokens, overflowed false, saves 25000: true
      400000 tokens, overflowed false, saves 30000: true
      400000 tokens, overflowed false, saves 24999: false
    window 1000000, cap none: compacts past 800000
      400000 tokens, overflowed false, saves 30000: false
    |}]
;;

(* 40k tokens: masks past 20k when it frees 4k, summarises past 32k. *)
let model = { Model.default with context_window = 40_000 }

(* Reports the request's estimated size as its input tokens, as a real
   provider would. *)
let measured (provider : Provider.t) =
  { provider with
    stream =
      (fun request ~cancel ~on_event ->
        let a = provider.stream request ~cancel ~on_event in
        { a with
          usage =
            { a.usage with input = Message.estimate_tokens request.messages }
        })
  }
;;

let is_ask (m : Message.t) =
  match m with
  | User u -> String.is_prefix u.text ~prefix:"[prigh: compaction]"
  | Assistant _ | Tool_result _ -> false
;;

(* A summary is asked for with the agent's own messages and an ask at the
   end, or (too long, or after an overflow) a transcript of its own. *)
let print_request (r : Provider.Request.t) =
  let masked =
    List.filter_map r.messages ~f:(function
      | Tool_result t when is_masked t.text -> Some t.tool_call_id
      | _ -> None)
    |> String.concat ~sep:" "
  in
  match r.system, List.last r.messages with
  | Some system, _ when String.is_prefix system ~prefix:"You summarise" ->
    print_endline "summarise a transcript"
  | _, Some m when is_ask m ->
    printf
      "summarise: %d messages + ask, masked [%s]\n"
      (List.length r.messages - 1)
      masked
  | _ ->
    printf
      "request: %d messages, ~%d tokens, masked [%s]\n"
      (List.length r.messages)
      (Message.estimate_tokens r.messages)
      masked
;;

let filler_call ?(size = 8000) i =
  Reply.tool_call
    ~id:(sprintf "c%d" i)
    ~name:"filler"
    ~arguments:(sprintf {|{"size":%d}|} size)
    ()
;;

let run_loop
      ?(model = model)
      ?(compaction = Compaction_settings.window_only)
      ?(context = [])
      replies
  =
  with_sandbox
  @@ fun t ->
  let provider =
    measured (Faux_provider.create ~on_request:print_request replies)
  in
  let added =
    Agent_loop.run
      ~env:t.env
      ~provider
      ~config:
        { Agent_loop.Config.model
        ; thinking = Off
        ; system = None
        ; tools = [ filler ]
        ; max_turns = None
        ; max_tokens = None
        ; retries = 0
        ; cache_key = None
        ; compaction
        }
      ~cwd:t.dir
      ~emit:(function
        | Masked { masked; tokens_saved } ->
          printf "masked %d messages, saving ~%d tokens\n" masked tokens_saved
        | Compacted { summary; _ } -> printf "compacted: %s\n" summary
        | Turn_end { assistant = { stop_reason = Error e; _ }; _ } ->
          printf "turn failed: %s\n" e
        | _ -> ())
      ~context
      ~prompts:[ Message.user "the task" ]
      ()
  in
  printf "added: %d messages\n" (List.length added)
;;

let%expect_test "a long run masks in batches and never summarises" =
  run_loop (List.map (List.range 1 18) ~f:filler_call @ [ Reply.text "done" ]);
  [%expect
    {|
    request: 1 messages, ~2 tokens, masked []
    request: 3 messages, ~2006 tokens, masked []
    request: 5 messages, ~4011 tokens, masked []
    request: 7 messages, ~6016 tokens, masked []
    request: 9 messages, ~8021 tokens, masked []
    request: 11 messages, ~10025 tokens, masked []
    request: 13 messages, ~12030 tokens, masked []
    request: 15 messages, ~14035 tokens, masked []
    request: 17 messages, ~16040 tokens, masked []
    request: 19 messages, ~18044 tokens, masked []
    request: 21 messages, ~20049 tokens, masked []
    request: 23 messages, ~22054 tokens, masked []
    request: 25 messages, ~24059 tokens, masked []
    masked 3 messages, saving ~5934 tokens
    request: 27 messages, ~20130 tokens, masked [c1 c2 c3]
    request: 29 messages, ~22135 tokens, masked [c1 c2 c3]
    request: 31 messages, ~24140 tokens, masked [c1 c2 c3]
    masked 3 messages, saving ~5934 tokens
    request: 33 messages, ~20211 tokens, masked [c1 c2 c3 c4 c5 c6]
    request: 35 messages, ~22216 tokens, masked [c1 c2 c3 c4 c5 c6]
    added: 36 messages
    |}]
;;

let%expect_test "with masking off, the same run summarises" =
  run_loop
    ~compaction:
      { Compaction_settings.window_only with mask_observations = false }
    (List.map (List.range 1 17) ~f:filler_call
     @ [ Reply.text "SUMMARY"; Reply.text "done" ]);
  [%expect
    {|
    request: 1 messages, ~2 tokens, masked []
    request: 3 messages, ~2006 tokens, masked []
    request: 5 messages, ~4011 tokens, masked []
    request: 7 messages, ~6016 tokens, masked []
    request: 9 messages, ~8021 tokens, masked []
    request: 11 messages, ~10025 tokens, masked []
    request: 13 messages, ~12030 tokens, masked []
    request: 15 messages, ~14035 tokens, masked []
    request: 17 messages, ~16040 tokens, masked []
    request: 19 messages, ~18044 tokens, masked []
    request: 21 messages, ~20049 tokens, masked []
    request: 23 messages, ~22054 tokens, masked []
    request: 25 messages, ~24059 tokens, masked []
    request: 27 messages, ~26063 tokens, masked []
    request: 29 messages, ~28068 tokens, masked []
    request: 31 messages, ~30073 tokens, masked []
    summarise: 27 messages + ask, masked []
    compacted: SUMMARY
    request: 8 messages, ~6027 tokens, masked []
    added: 34 messages
    |}]
;;

(* Over the threshold with nothing it can summarise (the user's message is
   kept):
   masking waits until a batch frees as much as it would under the
   threshold, rather than masking each turn's output as it ages, which
   would invalidate the provider's cache on every request. *)
let%expect_test "over the threshold, masking still waits for a large batch" =
  run_loop
    ~context:[ Message.user ("old " ^ String.make 112_000 'u') ]
    (List.map (List.range 1 25) ~f:(filler_call ~size:3000)
     @ [ Reply.text "done" ]);
  [%expect
    {|
    request: 2 messages, ~28003 tokens, masked []
    request: 4 messages, ~28757 tokens, masked []
    request: 6 messages, ~29512 tokens, masked []
    request: 8 messages, ~30267 tokens, masked []
    request: 10 messages, ~31022 tokens, masked []
    request: 12 messages, ~31776 tokens, masked []
    request: 14 messages, ~32531 tokens, masked []
    request: 16 messages, ~33286 tokens, masked []
    request: 18 messages, ~34041 tokens, masked []
    request: 20 messages, ~34795 tokens, masked []
    request: 22 messages, ~35550 tokens, masked []
    request: 24 messages, ~36305 tokens, masked []
    request: 26 messages, ~37060 tokens, masked []
    request: 28 messages, ~37814 tokens, masked []
    request: 30 messages, ~38569 tokens, masked []
    request: 32 messages, ~39324 tokens, masked []
    masked 6 messages, saving ~4368 tokens
    request: 34 messages, ~35712 tokens, masked [c1 c2 c3 c4 c5 c6]
    request: 36 messages, ~36467 tokens, masked [c1 c2 c3 c4 c5 c6]
    request: 38 messages, ~37222 tokens, masked [c1 c2 c3 c4 c5 c6]
    request: 40 messages, ~37976 tokens, masked [c1 c2 c3 c4 c5 c6]
    request: 42 messages, ~38731 tokens, masked [c1 c2 c3 c4 c5 c6]
    request: 44 messages, ~39486 tokens, masked [c1 c2 c3 c4 c5 c6]
    masked 6 messages, saving ~4368 tokens
    request: 46 messages, ~35874 tokens, masked [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]
    request: 48 messages, ~36629 tokens, masked [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]
    request: 50 messages, ~37384 tokens, masked [c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12]
    added: 50 messages
    |}]
;;

let%expect_test "a capped window masks as early, relative to the cap" =
  (* 400k tokens, compacted at the 32k cap: masks as the 40k window does. *)
  let model = { model with context_window = 400_000 } in
  let replies =
    List.map (List.range 1 18) ~f:filler_call @ [ Reply.text "done" ]
  in
  run_loop
    ~model
    ~compaction:
      { Compaction_settings.window_only with max_tokens = Some 32_000 }
    replies;
  [%expect
    {|
    request: 1 messages, ~2 tokens, masked []
    request: 3 messages, ~2006 tokens, masked []
    request: 5 messages, ~4011 tokens, masked []
    request: 7 messages, ~6016 tokens, masked []
    request: 9 messages, ~8021 tokens, masked []
    request: 11 messages, ~10025 tokens, masked []
    request: 13 messages, ~12030 tokens, masked []
    request: 15 messages, ~14035 tokens, masked []
    request: 17 messages, ~16040 tokens, masked []
    request: 19 messages, ~18044 tokens, masked []
    request: 21 messages, ~20049 tokens, masked []
    request: 23 messages, ~22054 tokens, masked []
    request: 25 messages, ~24059 tokens, masked []
    masked 3 messages, saving ~5934 tokens
    request: 27 messages, ~20130 tokens, masked [c1 c2 c3]
    request: 29 messages, ~22135 tokens, masked [c1 c2 c3]
    request: 31 messages, ~24140 tokens, masked [c1 c2 c3]
    masked 3 messages, saving ~5934 tokens
    request: 33 messages, ~20211 tokens, masked [c1 c2 c3 c4 c5 c6]
    request: 35 messages, ~22216 tokens, masked [c1 c2 c3 c4 c5 c6]
    added: 36 messages
    |}];
  (* Without the cap, 36k tokens are far from 320k: nothing is masked. *)
  run_loop ~model replies;
  [%expect
    {|
    request: 1 messages, ~2 tokens, masked []
    request: 3 messages, ~2006 tokens, masked []
    request: 5 messages, ~4011 tokens, masked []
    request: 7 messages, ~6016 tokens, masked []
    request: 9 messages, ~8021 tokens, masked []
    request: 11 messages, ~10025 tokens, masked []
    request: 13 messages, ~12030 tokens, masked []
    request: 15 messages, ~14035 tokens, masked []
    request: 17 messages, ~16040 tokens, masked []
    request: 19 messages, ~18044 tokens, masked []
    request: 21 messages, ~20049 tokens, masked []
    request: 23 messages, ~22054 tokens, masked []
    request: 25 messages, ~24059 tokens, masked []
    request: 27 messages, ~26063 tokens, masked []
    request: 29 messages, ~28068 tokens, masked []
    request: 31 messages, ~30073 tokens, masked []
    request: 33 messages, ~32078 tokens, masked []
    request: 35 messages, ~34082 tokens, masked []
    added: 36 messages
    |}]
;;

(* Small enough not to mask between turns, too big for the provider. *)
let overflowing =
  Message.user "old" :: List.concat_map (List.range 1 13) ~f:turn
;;

let%expect_test "an overflow masks, and sends the request again" =
  run_loop ~context:overflowing [ Reply.context_overflow (); Reply.text "done" ];
  [%expect
    {|
    request: 26 messages, ~12041 tokens, masked []
    masked 2 messages, saving ~1958 tokens
    request: 26 messages, ~10085 tokens, masked [c1 c2]
    added: 2 messages
    |}]
;;

let%expect_test "an overflow masks first, then summarises" =
  let context = overflowing in
  run_loop
    ~context
    [ Reply.context_overflow ()
    ; Reply.context_overflow ()
    ; Reply.text "SUMMARY"
    ; Reply.text "done"
    ];
  [%expect
    {|
    request: 26 messages, ~12041 tokens, masked []
    masked 2 messages, saving ~1958 tokens
    request: 26 messages, ~10085 tokens, masked [c1 c2]
    summarise a transcript
    compacted: SUMMARY
    request: 17 messages, ~7036 tokens, masked []
    added: 2 messages
    |}]
;;

let%expect_test "a session records masking: resumed, rewound, forked, compacted"
  =
  with_sandbox
  @@ fun t ->
  let dir = Filename.concat t.dir "sessions" in
  let s = Session.create ~dir ~cwd:t.dir () in
  let append m = Session.append_message s m in
  let (_ : Session.Entry.t) = append (Message.user "task") in
  let ids =
    List.map (List.range 1 5) ~f:(fun i ->
      match turn i with
      | [ a; r ] ->
        let a = append a in
        let r = append r in
        a.id, r.id
      | _ -> assert false)
  in
  let show label s =
    printf "%s: %s\n" label (shapes (Session.messages s));
    printf
      "  shown: %s\n"
      (shapes
         (List.map (Session.timed_messages s) ~f:(fun (m : Timed_message.t) ->
            m.message)))
  in
  let before = Option.value_exn (Session.head s) in
  let masked = Session.append_masked s ~through:(snd (List.nth_exn ids 1)) in
  show "masked" s;
  let line = List.last_exn (In_channel.read_lines (Session.path s)) in
  print_endline
    (String.substr_replace_all
       (String.drop_prefix
          line
          (String.substr_index_exn line ~pattern:"\"payload"))
       ~pattern:(snd (List.nth_exn ids 1))
       ~with_:"<c2's result>");
  show "resumed" (Or_error.ok_exn (Session.load (Session.path s)));
  show "forked" (Or_error.ok_exn (Session.fork s ~dir));
  Or_error.ok_exn (Session.rewind s ~to_:before);
  show "rewound before it" s;
  Or_error.ok_exn (Session.rewind s ~to_:masked.id);
  show "back on it" s;
  (* A compaction keeps the kept messages masked; a later masking goes
     further. *)
  let (_ : Session.Entry.t) =
    Session.append_compaction
      s
      ~summary:"S"
      ~kept_from:(fst (List.nth_exn ids 1))
  in
  show "compacted" s;
  let (_ : Session.Entry.t) =
    Session.append_masked s ~through:(snd (List.nth_exn ids 2))
  in
  show "masked again" s;
  show "resumed" (Or_error.ok_exn (Session.load (Session.path s)));
  [%expect
    {|
    masked: U(task) A[c1] M(c1) A[c2] M(c2) A[c3] T(c3,4000) A[c4] T(c4,4000)
      shown: U(task) A[c1] T(c1,4000) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000)
    "payload":["Masked",{"through":"<c2's result>"}]}]
    resumed: U(task) A[c1] M(c1) A[c2] M(c2) A[c3] T(c3,4000) A[c4] T(c4,4000)
      shown: U(task) A[c1] T(c1,4000) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000)
    forked: U(task) A[c1] M(c1) A[c2] M(c2) A[c3] T(c3,4000) A[c4] T(c4,4000)
      shown: U(task) A[c1] T(c1,4000) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000)
    rewound before it: U(task) A[c1] T(c1,4000) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000)
      shown: U(task) A[c1] T(c1,4000) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000)
    back on it: U(task) A[c1] M(c1) A[c2] M(c2) A[c3] T(c3,4000) A[c4] T(c4,4000)
      shown: U(task) A[c1] T(c1,4000) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000)
    compacted: S(S) A[c2] M(c2) A[c3] T(c3,4000) A[c4] T(c4,4000)
      shown: S(S) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000)
    masked again: S(S) A[c2] M(c2) A[c3] M(c3) A[c4] T(c4,4000)
      shown: S(S) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000)
    resumed: S(S) A[c2] M(c2) A[c3] M(c3) A[c4] T(c4,4000)
      shown: S(S) A[c2] T(c2,4000) A[c3] T(c3,4000) A[c4] T(c4,4000)
    |}]
;;

let%expect_test
    "an agent masks between turns and after its run, not summarising"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let last_request = ref [] in
  let provider =
    measured
      (Faux_provider.create
         ~on_request:(fun r ->
           print_request r;
           last_request := r.messages)
         (List.map (List.range 1 16) ~f:filler_call
          @ [ Reply.text "done"; Reply.text "SUMMARY" ]))
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~model
      ~tools:[ filler ]
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(function
    | Loop (Masked { masked; tokens_saved }) ->
      printf "masked %d messages, saving ~%d tokens\n" masked tokens_saved
    | Compacted { summary } -> printf "compacted: %s\n" summary
    | Notice n -> printf "notice: %s\n" n
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "the task");
  Agent.wait_idle agent;
  let session = Agent.session agent in
  (* The last request is what the session has, up to what the run masked
     after it. *)
  printf
    "last request: %s\nsession now:  %s\n"
    (shapes !last_request)
    (shapes (Session.messages session));
  let resumed = Or_error.ok_exn (Session.load (Session.path session)) in
  printf
    "resumed the same: %b\n"
    ([%equal: Message.t list]
       (Session.messages resumed)
       (Session.messages session));
  printf "masks: %d\n" (Agent.session_stats agent).masks;
  (* The summary is asked for with the context the agent sends, masked: the
     provider's cache serves it. *)
  let before = Session.messages session in
  (match Agent.compact agent with
   | Ok _ -> ()
   | Error e -> print_s [%sexp (e : Error.t)]);
  printf
    "the summary request is the start of the agent's: %b\n"
    (List.is_prefix
       before
       ~prefix:(List.drop_last_exn !last_request)
       ~equal:[%equal: Message.t]);
  printf "after: %s\n" (shapes (Session.messages session));
  [%expect
    {|
    request: 1 messages, ~2 tokens, masked []
    request: 3 messages, ~2006 tokens, masked []
    request: 5 messages, ~4011 tokens, masked []
    request: 7 messages, ~6016 tokens, masked []
    request: 9 messages, ~8021 tokens, masked []
    request: 11 messages, ~10025 tokens, masked []
    request: 13 messages, ~12030 tokens, masked []
    request: 15 messages, ~14035 tokens, masked []
    request: 17 messages, ~16040 tokens, masked []
    request: 19 messages, ~18044 tokens, masked []
    request: 21 messages, ~20049 tokens, masked []
    request: 23 messages, ~22054 tokens, masked []
    request: 25 messages, ~24059 tokens, masked []
    masked 3 messages, saving ~5934 tokens
    request: 27 messages, ~20130 tokens, masked [c1 c2 c3]
    request: 29 messages, ~22135 tokens, masked [c1 c2 c3]
    request: 31 messages, ~24140 tokens, masked [c1 c2 c3]
    masked 3 messages, saving ~5934 tokens
    last request: U(the task) A[c1] M(c1) A[c2] M(c2) A[c3] M(c3) A[c4] T(c4,8000) A[c5] T(c5,8000) A[c6] T(c6,8000) A[c7] T(c7,8000) A[c8] T(c8,8000) A[c9] T(c9,8000) A[c10] T(c10,8000) A[c11] T(c11,8000) A[c12] T(c12,8000) A[c13] T(c13,8000) A[c14] T(c14,8000) A[c15] T(c15,8000)
    session now:  U(the task) A[c1] M(c1) A[c2] M(c2) A[c3] M(c3) A[c4] M(c4) A[c5] M(c5) A[c6] M(c6) A[c7] T(c7,8000) A[c8] T(c8,8000) A[c9] T(c9,8000) A[c10] T(c10,8000) A[c11] T(c11,8000) A[c12] T(c12,8000) A[c13] T(c13,8000) A[c14] T(c14,8000) A[c15] T(c15,8000) A(done)
    resumed the same: true
    masks: 2
    summarise: 25 messages + ask, masked [c1 c2 c3 c4 c5 c6]
    compacted: SUMMARY
    the summary request is the start of the agent's: true
    after: U(the task) S(SUMMARY) A[c13] T(c13,8000) A[c14] T(c14,8000) A[c15] T(c15,8000) A(done)
    |}]
;;

let%expect_test "masking and a compaction with pinned messages, either order" =
  with_sandbox
  @@ fun t ->
  let s = Session.create ~dir:t.dir ~cwd:"/w" () in
  let append m = Session.append_message s m in
  let show label =
    printf
      "%s: %s\n"
      label
      (String.concat
         ~sep:" "
         (List.map (Session.messages s) ~f:(fun m ->
            match m with
            | User u when not (Session.is_summary_message m) ->
              sprintf "%s(%d)" (shape m) (String.length u.text)
            | Assistant a ->
              List.find_map (Message.Assistant.tool_calls a) ~f:(fun c ->
                Option.some_if (is_masked c.arguments) (c.id ^ "-args-masked"))
              |> Option.value_map
                   ~default:(shape m)
                   ~f:(sprintf "%s %s" (shape m))
            | _ -> shape m)))
  in
  let (_ : Session.Entry.t) = append (Message.user "task") in
  (* A large patch: masking drops its body, the files still list it. *)
  let patch =
    "*** Begin Patch\n*** Add File: src/big.ml\n"
    ^ String.concat (List.init 100 ~f:(fun _ -> "+let x = 1 (* padding *)\n"))
    ^ "*** End Patch"
  in
  let (_ : Session.Entry.t) =
    append
      (call
         "p1"
         ~name:"apply_patch"
         ~arguments:(Jsonaf.to_string (`Object [ "input", `String patch ])))
  in
  let (_ : Session.Entry.t) = append (result ~name:"apply_patch" "p1" "done") in
  let turns range = List.map range ~f:(fun i -> List.map (turn i) ~f:append) in
  let (_ : Session.Entry.t list list) = turns (List.range 1 3) in
  let (_ : Session.Entry.t) =
    append (Message.user ("then " ^ String.make 5000 'y'))
  in
  let c3 = turns [ 3 ] in
  let (_ : Session.Entry.t) = append (Message.user "and z") in
  let later = turns (List.range 4 7) in
  (* Masked up to c3's result, after the long user message: masking leaves
     it as it is (and, pinned below, shortened). *)
  let (_ : Session.Entry.t) =
    Session.append_masked s ~through:(List.last_exn (List.hd_exn c3)).id
  in
  show "masked";
  let provider =
    Faux_provider.create ~on_request:print_request [ Reply.text "S" ]
  in
  (match
     Compaction.compact
       ~env:t.env
       ~provider
       ~model
       ~prompt:
         { Compaction.Prompt.system = None
         ; tools = []
         ; thinking = Off
         ; cache_key = None
         }
       ~keep_recent_tokens:2500
       s
   with
   | Ok outcome ->
     print_endline
       (List.drop_while (String.split_lines outcome.summary) ~f:(fun l ->
          not (String.is_prefix l ~prefix:"Files"))
        |> String.concat ~sep:"\n")
   | Error e -> print_s [%sexp (e : Error.t)]);
  show "compacted";
  let (_ : Session.Entry.t) =
    Session.append_masked s ~through:(List.last_exn (List.nth_exn later 1)).id
  in
  show "masked again";
  let resumed = Or_error.ok_exn (Session.load (Session.path s)) in
  printf
    "resumed the same: %b\n"
    ([%equal: Message.t list] (Session.messages resumed) (Session.messages s));
  [%expect
    {|
    masked: U(task)(4) A[p1] p1-args-masked T(p1,4) A[c1] M(c1) A[c2] M(c2) U(then yyyyyyyyyyyyyyy)(5005) A[c3] M(c3) U(and z)(5) A[c4] T(c4,4000) A[c5] T(c5,4000) A[c6] T(c6,4000)
    summarise: 13 messages + ask, masked [c1 c2 c3]
    Files modified: /w/src/big.ml
    compacted: U(task)(4) U(then yyyyyyyyyyyyyyy)(2030) U(and z)(5) S(S) A[c5] T(c5,4000) A[c6] T(c6,4000)
    masked again: U(task)(4) U(then yyyyyyyyyyyyyyy)(2030) U(and z)(5) S(S) A[c5] M(c5) A[c6] T(c6,4000)
    resumed the same: true
    |}]
;;
