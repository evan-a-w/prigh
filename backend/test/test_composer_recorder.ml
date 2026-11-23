open! Core
open! Prigh
module E = Adapter_event
module Json = Jsonaf

(* Feeds [events] to a recorder and prints what it emits (as
   [Test_agent_loop.show_event] does, without the deltas of a stream when
   [deltas] is false), its notices, and the messages the run added. *)
let run ?(deltas = true) ?(approvals = []) ?model events =
  let recorder =
    Composer_recorder.create
      ~adapter:"codex"
      ~model
      ~emit:(fun e ->
        match e with
        | Message_update _ when not deltas -> ()
        | e -> print_endline (Test_agent_loop.show_event e))
      ~notice:(fun n -> print_endline ("notice: " ^ n))
      ~prompts:[ Message.user "do it" ]
  in
  List.iter approvals ~f:(fun (tool_id, reply) ->
    Composer_recorder.note_approval recorder ~tool_id reply);
  List.iter events ~f:(Composer_recorder.feed recorder);
  let added = Composer_recorder.finish recorder in
  print_endline "--- messages";
  List.iter added ~f:(fun m -> print_s [%sexp (m : Message.t)]);
  print_s
    [%message
      ""
        ~stop:(Composer_recorder.stop recorder : E.Stop.t option)
        ~thread:(Composer_recorder.thread_id recorder : string option)
        ~cost_usd:(Composer_recorder.cost_usd recorder : float)
        ~context:(Composer_recorder.context recorder : int option * int option)]
;;

let usage ?cost_usd input output =
  E.Usage
    { usage = { Usage.input; output; cache_read = 0 }
    ; cost_usd
    ; context_tokens = Some (input + output)
    ; context_window = Some 200_000
    }
;;

let tool ?parent ?(kind = E.Tool_kind.Command) ~native_name id input =
  E.Tool_start { id; kind; native_name; title = native_name; input; parent }
;;

let done_ = E.Turn_end { turn_id = Some "t1"; stop = Done }

let%expect_test "a text turn" =
  run
    ~model:"gpt-5.5"
    [ Thread_started { thread_id = "th1"; model = None }
    ; Turn_started { turn_id = "t1" }
    ; Text_delta { item = "m1"; text = "Hel" }
    ; Text_delta { item = "m1"; text = "lo" }
    ; Text_done { item = "m1"; text = "Hello!" }
    ; usage ~cost_usd:0.01 10 5
    ; done_
    ];
  [%expect
    {|
    agent_start
    message_start user "do it"
    message_end
    turn_start
    message_start assistant
      update (Text_delta Hel)
      update (Text_delta lo)
      update (Text_delta !)
    message_end assistant "Hello!"
    turn_end stop=End_turn tool_results=0
    agent_end (2 new messages)
    --- messages
    (User ((text "do it")))
    (Assistant
     ((content ((Text Hello!))) (stop_reason End_turn)
      (usage ((input 10) (output 5) (cache_read 0))) (model codex/gpt-5.5)))
    ((stop (Done)) (thread (th1)) (cost_usd 0.01) (context ((15) (200000))))
    |}]
;;

let%expect_test "thinking, then a command with streamed output, then text" =
  run
    [ Thread_started { thread_id = "th1"; model = Some "gpt-5.5" }
    ; Turn_started { turn_id = "t1" }
    ; Thinking_delta { item = "r1"; text = "Let me look." }
    ; Text_delta { item = "m1"; text = "Listing." }
    ; tool
        ~native_name:"commandExecution"
        "c1"
        (`Object [ "command", `String "ls" ])
    ; Tool_update { id = "c1"; output = "a\n" }
    ; Tool_update { id = "c1"; output = "b\n" }
    ; Tool_end { id = "c1"; ok = true; output = ""; changes = []; images = [] }
    ; Text_delta { item = "m2"; text = "Two files." }
    ; usage 30 8
    ; done_
    ];
  [%expect
    {|
    agent_start
    message_start user "do it"
    message_end
    turn_start
    message_start assistant
      update (Thinking_delta"Let me look.")
      update (Text_delta Listing.)
      update (Tool_call_start(index 0)(id c1)(name bash))
      update (Tool_call_delta(index 0)(arguments"{\"command\":\"ls\"}"))
    message_end assistant "Listing."
    tool_start bash {"command":"ls"}
      tool_output "a\n"
      tool_output "b\n"
    tool_end "a\nb\n"
    message_start tool_result c1
    message_end
    turn_end stop=Tool_use tool_results=1
    turn_start
    message_start assistant
      update (Text_delta"Two files.")
    message_end assistant "Two files."
    turn_end stop=End_turn tool_results=0
    agent_end (4 new messages)
    --- messages
    (User ((text "do it")))
    (Assistant
     ((content
       ((Thinking ((text "Let me look.") (signature ()))) (Text Listing.)
        (Tool_call ((id c1) (name bash) (arguments "{\"command\":\"ls\"}")))))
      (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0)))
      (model codex/gpt-5.5)))
    (Tool_result
     ((tool_call_id c1) (tool_name bash) (text  "a\
                                               \nb\
                                               \n") (is_error false)))
    (Assistant
     ((content ((Text "Two files."))) (stop_reason End_turn)
      (usage ((input 30) (output 8) (cache_read 0))) (model codex/gpt-5.5)))
    ((stop (Done)) (thread (th1)) (cost_usd 0) (context ((38) (200000))))
    |}]
;;

let diff = "--- a/hello.txt\n+++ b/hello.txt\n@@ -1 +1 @@\n-hi\n+hello\n"

let%expect_test "file changes: one edit, then several files" =
  run
    ~deltas:false
    [ tool
        ~kind:File_change
        ~native_name:"Edit"
        "e1"
        (`Object
            [ "file_path", `String "hello.txt"
            ; "old_string", `String "hi"
            ; "new_string", `String "hello"
            ])
    ; Tool_end
        { id = "e1"
        ; ok = true
        ; output = "The file was updated."
        ; changes =
            [ { path = "hello.txt"; kind = Update { move_to = None }; diff } ]
        ; images = []
        }
    ; tool
        ~kind:File_change
        ~native_name:"fileChange"
        "f1"
        (`Object
            [ ( "changes"
              , `Array
                  [ `Object
                      [ "path", `String "hello.txt"
                      ; ( "kind"
                        , `Object
                            [ "type", `String "update"; "move_path", `Null ] )
                      ; "diff", `String diff
                      ]
                  ; `Object
                      [ "path", `String "new.txt"
                      ; "kind", `Object [ "type", `String "add" ]
                      ; "diff", `String "fresh\n"
                      ]
                  ; `Object
                      [ "path", `String "old.txt"
                      ; "kind", `Object [ "type", `String "delete" ]
                      ; "diff", `String ""
                      ]
                  ] )
            ])
    ; Tool_end
        { id = "f1"
        ; ok = true
        ; output = ""
        ; changes =
            [ { path = "hello.txt"; kind = Update { move_to = None }; diff }
            ; { path = "new.txt"; kind = Add; diff = "" }
            ; { path = "old.txt"; kind = Delete; diff = "" }
            ]
        ; images = []
        }
    ; done_
    ];
  [%expect
    {|
    agent_start
    message_start user "do it"
    message_end
    turn_start
    message_start assistant
    message_end assistant ""
    tool_start edit {"path":"hello.txt","edits":[{"old_text":"hi","new_text":"hello"}]}
    tool_end "--- a/hello.txt\n+++ b/hello.txt\n@@ -1 +1 @@\n-hi\n+hello\n"
    message_start tool_result e1
    message_end
    turn_end stop=Tool_use tool_results=1
    turn_start
    message_start assistant
    message_end assistant ""
    tool_start apply_patch {"input":"*** Begin Patch\n*** Update File: hello.txt\n@@\n-hi\n+hello\n*** Add File: new.txt\n+fresh\n*** Delete File: old.txt\n*** End Patch\n"}
    tool_end "Success. Updated the following files:\nM hello.txt\nA new.txt\nD old.txt\n\n--- a/hello.txt\n+++ b/hello.txt\n@@ -1 +1 @@\n-hi\n+hello\n"
    message_start tool_result f1
    message_end
    turn_end stop=Tool_use tool_results=1
    agent_end (5 new messages)
    --- messages
    (User ((text "do it")))
    (Assistant
     ((content
       ((Tool_call
         ((id e1) (name edit)
          (arguments
           "{\"path\":\"hello.txt\",\"edits\":[{\"old_text\":\"hi\",\"new_text\":\"hello\"}]}")))))
      (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0)))
      (model codex/default)))
    (Tool_result
     ((tool_call_id e1) (tool_name edit)
      (text  "--- a/hello.txt\
            \n+++ b/hello.txt\
            \n@@ -1 +1 @@\
            \n-hi\
            \n+hello\
            \n")
      (is_error false)))
    (Assistant
     ((content
       ((Tool_call
         ((id f1) (name apply_patch)
          (arguments
           "{\"input\":\"*** Begin Patch\\n*** Update File: hello.txt\\n@@\\n-hi\\n+hello\\n*** Add File: new.txt\\n+fresh\\n*** Delete File: old.txt\\n*** End Patch\\n\"}")))))
      (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0)))
      (model codex/default)))
    (Tool_result
     ((tool_call_id f1) (tool_name apply_patch)
      (text
        "Success. Updated the following files:\
       \nM hello.txt\
       \nA new.txt\
       \nD old.txt\
       \n\
       \n--- a/hello.txt\
       \n+++ b/hello.txt\
       \n@@ -1 +1 @@\
       \n-hi\
       \n+hello\
       \n")
      (is_error false)))
    ((stop (Done)) (thread ()) (cost_usd 0) (context (() ())))
    |}]
;;

let%expect_test "canonical tools" =
  let show kind native_name input =
    let t =
      Canonical_tool.of_start ~kind ~native_name ~input:(Json.of_string input)
    in
    printf "%-16s -> %s %s\n" native_name t.name (Json.to_string t.arguments)
  in
  show Command "Bash" {|{"command":"ls -la","description":"list"}|};
  show Command "commandExecution" {|{"command":["bash","-lc","ls"]}|};
  show Read "Read" {|{"file_path":"/a/b.ml","offset":3,"limit":10}|};
  show Search "Grep" {|{"pattern":"foo","path":"src"}|};
  show Search "Glob" {|{"pattern":"**/*.ml"}|};
  show Web_search "WebSearch" {|{"query":"ocaml"}|};
  show Web_search "WebFetch" {|{"url":"https://x.org","prompt":"p"}|};
  show (Mcp { server = "prigh"; tool = "ask user" }) "mcp" {|{"q":1}|};
  show
    Todo
    "TodoWrite"
    {|{"todos":[{"content":"a","status":"completed"},{"content":"b","status":"in_progress"}]}|};
  show Subagent "Task" {|{"description":"look","prompt":"Find it"}|};
  show File_change "Write" {|{"file_path":"x.txt","content":"hi\n"}|};
  show (Other "exitPlanMode") "ExitPlanMode" {|{"plan":"do"}|};
  [%expect
    {|
    Bash             -> bash {"command":"ls -la"}
    commandExecution -> bash {"command":"ls"}
    Read             -> read {"path":"/a/b.ml","offset":3,"limit":10}
    Grep             -> grep {"pattern":"foo","path":"src"}
    Glob             -> find {"pattern":"**/*.ml"}
    WebSearch        -> web_search {"query":"ocaml"}
    WebFetch         -> web_fetch {"url":"https://x.org"}
    mcp              -> mcp__prigh__ask_user {"q":1}
    TodoWrite        -> update_plan {"plan":[{"step":"a","status":"completed"},{"step":"b","status":"in_progress"}]}
    Task             -> subagent {"task":"Find it","name":"look"}
    Write            -> write {"path":"x.txt","content":"hi\n"}
    ExitPlanMode     -> ExitPlanMode {"plan":"do"}
    |}]
;;

let%expect_test "a plan is an update_plan call" =
  run
    ~deltas:false
    [ Plan
        { explanation = None
        ; steps =
            [ { step = "read"; status = Completed }
            ; { step = "write"; status = In_progress }
            ]
        }
    ; Text_delta { item = "m1"; text = "Working." }
    ; done_
    ];
  [%expect
    {|
    agent_start
    message_start user "do it"
    message_end
    turn_start
    message_start assistant
    message_end assistant ""
    tool_start update_plan {"plan":[{"step":"read","status":"completed"},{"step":"write","status":"in_progress"}]}
    tool_end "Plan updated (1 of 2 steps completed)"
    message_start tool_result plan-1
    message_end
    turn_end stop=Tool_use tool_results=1
    turn_start
    message_start assistant
    message_end assistant "Working."
    turn_end stop=End_turn tool_results=0
    agent_end (4 new messages)
    --- messages
    (User ((text "do it")))
    (Assistant
     ((content
       ((Tool_call
         ((id plan-1) (name update_plan)
          (arguments
           "{\"plan\":[{\"step\":\"read\",\"status\":\"completed\"},{\"step\":\"write\",\"status\":\"in_progress\"}]}")))))
      (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0)))
      (model codex/default)))
    (Tool_result
     ((tool_call_id plan-1) (tool_name update_plan)
      (text "Plan updated (1 of 2 steps completed)") (is_error false)))
    (Assistant
     ((content ((Text Working.))) (stop_reason End_turn)
      (usage ((input 0) (output 0) (cache_read 0))) (model codex/default)))
    ((stop (Done)) (thread ()) (cost_usd 0) (context (() ())))
    |}]
;;

let%expect_test "a usage limit fails the turn" =
  run
    ~deltas:false
    [ Text_delta { item = "m1"; text = "Start" }
    ; usage 5 1
    ; Turn_end
        { turn_id = None
        ; stop =
            Failed
              { kind = Usage_limit { resets_at = None }
              ; message = "You've hit your usage limit."
              }
        }
    ];
  [%expect
    {|
    agent_start
    message_start user "do it"
    message_end
    turn_start
    message_start assistant
    message_end assistant "Start"
    turn_end stop=(Error"You've hit your usage limit. (usage limit reached)") tool_results=0
    agent_end (2 new messages)
    --- messages
    (User ((text "do it")))
    (Assistant
     ((content ((Text Start)))
      (stop_reason (Error "You've hit your usage limit. (usage limit reached)"))
      (usage ((input 5) (output 1) (cache_read 0))) (model codex/default)))
    ((stop
      ((Failed (kind (Usage_limit (resets_at ())))
        (message "You've hit your usage limit."))))
     (thread ()) (cost_usd 0) (context ((6) (200000))))
    |}];
  print_endline
    (Composer_recorder.error_text
       (Auth_required { hint = "run `codex login` on laptop" })
       ~message:"Not signed in");
  print_endline
    (Composer_recorder.error_text
       (Crashed { stderr_tail = "panic: oops\n" })
       ~message:"codex exited (code 101)");
  [%expect
    {|
    Not signed in: run `codex login` on laptop
    codex exited (code 101)
    panic: oops
    |}]
;;

let%expect_test "interrupted mid-command, and a denied call" =
  run
    ~deltas:false
    ~approvals:[ "c2", Deny (Some "not that") ]
    [ tool ~native_name:"Bash" "c2" (`Object [ "command", `String "rm -rf /" ])
    ; Tool_end
        { id = "c2"; ok = false; output = "denied"; changes = []; images = [] }
    ; tool ~native_name:"Bash" "c1" (`Object [ "command", `String "sleep 100" ])
    ; Tool_update { id = "c1"; output = "zzz" }
    ];
  [%expect
    {|
    agent_start
    message_start user "do it"
    message_end
    turn_start
    message_start assistant
    message_end assistant ""
    tool_start bash {"command":"rm -rf /"}
    tool_end ERROR "[denied by user] Their reason: not that"
    message_start tool_result c2
    message_end
    turn_end stop=Tool_use tool_results=1
    turn_start
    message_start assistant
    message_end assistant ""
    tool_start bash {"command":"sleep 100"}
      tool_output "zzz"
    tool_end ERROR "zzz\n[cancelled]"
    message_start tool_result c1
    message_end
    turn_end stop=Tool_use tool_results=1
    agent_end (5 new messages)
    --- messages
    (User ((text "do it")))
    (Assistant
     ((content
       ((Tool_call
         ((id c2) (name bash) (arguments "{\"command\":\"rm -rf /\"}")))))
      (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0)))
      (model codex/default)))
    (Tool_result
     ((tool_call_id c2) (tool_name bash)
      (text "[denied by user] Their reason: not that") (is_error true)))
    (Assistant
     ((content
       ((Tool_call
         ((id c1) (name bash) (arguments "{\"command\":\"sleep 100\"}")))))
      (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0)))
      (model codex/default)))
    (Tool_result
     ((tool_call_id c1) (tool_name bash) (text  "zzz\
                                               \n[cancelled]")
      (is_error true)))
    ((stop (Interrupted)) (thread ()) (cost_usd 0) (context (() ())))
    |}]
;;

let%expect_test "a subagent's calls are nested under it" =
  run
    ~deltas:false
    [ Text_delta { item = "m1"; text = "Delegating." }
    ; tool
        ~kind:Subagent
        ~native_name:"Task"
        "s1"
        (`Object
            [ "description", `String "explore"; "prompt", `String "Find X" ])
    ; tool
        ~parent:"s1"
        ~kind:Read
        ~native_name:"Read"
        "r1"
        (`Object [ "file_path", `String "x.ml" ])
    ; Tool_end
        { id = "r1"
        ; ok = true
        ; output = "let x = 1"
        ; changes = []
        ; images = []
        }
    ; tool
        ~parent:"s1"
        ~native_name:"Bash"
        "c1"
        (`Object [ "command", `String "make" ])
    ; Tool_end
        { id = "s1"
        ; ok = true
        ; output = "X is in x.ml"
        ; changes = []
        ; images = []
        }
    ; Text_delta { item = "m2"; text = "Found it." }
    ; done_
    ];
  [%expect
    {|
    agent_start
    message_start user "do it"
    message_end
    turn_start
    message_start assistant
    message_end assistant "Delegating."
    tool_start subagent {"task":"Find X","name":"explore"}
    subagent_start s1/s1 task="Find X" model=codex/default tools=[]
    subagent s1/s1: turn_start
    subagent s1/s1: tool_start read {"path":"x.ml"}
    subagent s1/s1: tool_end "let x = 1"
    subagent s1/s1: tool_start bash {"command":"make"}
    subagent s1/s1: tool_end ERROR "[cancelled]"
    subagent_end s1/s1 turns=1 in=0 out=0 cache=0 cost=0.
    tool_end "X is in x.ml"
    message_start tool_result s1
    message_end
    turn_end stop=Tool_use tool_results=1
    turn_start
    message_start assistant
    message_end assistant "Found it."
    turn_end stop=End_turn tool_results=0
    agent_end (4 new messages)
    --- messages
    (User ((text "do it")))
    (Assistant
     ((content
       ((Text Delegating.)
        (Tool_call
         ((id s1) (name subagent)
          (arguments "{\"task\":\"Find X\",\"name\":\"explore\"}")))))
      (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0)))
      (model codex/default)))
    (Tool_result
     ((tool_call_id s1) (tool_name subagent) (text "X is in x.ml")
      (is_error false)))
    (Assistant
     ((content ((Text "Found it."))) (stop_reason End_turn)
      (usage ((input 0) (output 0) (cache_read 0))) (model codex/default)))
    ((stop (Done)) (thread ()) (cost_usd 0) (context (() ())))
    |}]
;;
