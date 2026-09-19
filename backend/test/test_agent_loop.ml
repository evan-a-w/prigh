open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let config ?(tools = Tools.all) ?max_turns () =
  { Agent_loop.Config.model = Model.default
  ; thinking = Off
  ; system = Some "You are a test."
  ; tools
  ; max_turns
  ; max_tokens = None
  ; retries = 2
  }
;;

(* Compact rendering of the event stream. *)
let show_event (e : Agent_event.t) =
  match e with
  | Agent_start -> "agent_start"
  | Agent_end added -> sprintf "agent_end (%d new messages)" (List.length added)
  | Turn_start -> "turn_start"
  | Turn_end { assistant; tool_results } ->
    sprintf
      "turn_end stop=%s tool_results=%d"
      (Sexp.to_string [%sexp (assistant.stop_reason : Stop_reason.t)])
      (List.length tool_results)
  | Message_start (User u) -> sprintf "message_start user %S" u.text
  | Message_start (Assistant _) -> "message_start assistant"
  | Message_start (Tool_result r) ->
    sprintf "message_start tool_result %s" r.tool_call_id
  | Message_update { delta; _ } ->
    sprintf "  update %s" (Sexp.to_string [%sexp (delta : Assistant_event.t)])
  | Message_end (Assistant a) ->
    sprintf "message_end assistant %S" (Message.Assistant.text a)
  | Message_end (User _ | Tool_result _) -> "message_end"
  | Tool_start call -> sprintf "tool_start %s %s" call.name call.arguments
  | Tool_output { chunk; _ } -> sprintf "  tool_output %S" chunk
  | Tool_end { result; _ } ->
    sprintf
      "tool_end %s%S"
      (if result.is_error then "ERROR " else "")
      result.text
;;

let run ?cancel ?steer ?max_turns ?(context = []) t provider prompt =
  let events = ref [] in
  let added =
    Agent_loop.run
      ~env:t.env
      ~provider
      ~config:(config ?max_turns ())
      ~cwd:t.dir
      ?cancel
      ?steer
      ~emit:(fun e -> events := e :: !events)
      ~retry_delay:(fun ~attempt ->
        events
        := Agent_event.Tool_output
             { call_id = "retry"; chunk = sprintf "attempt %d" attempt }
           :: !events)
      ~context
      ~prompts:[ Message.user prompt ]
      ()
  in
  List.iter (List.rev !events) ~f:(fun e ->
    print_endline (mask t (show_event e)));
  added
;;

let%expect_test "plain reply" =
  with_sandbox
  @@ fun t ->
  let provider = Faux_provider.create [ Reply.text "Hello!" ] in
  let added = run t provider "hi" in
  [%expect
    {|
    agent_start
    message_start user "hi"
    message_end
    turn_start
    message_start assistant
      update (Text_delta Hello!)
    message_end assistant "Hello!"
    turn_end stop=End_turn tool_results=0
    agent_end (2 new messages)
    |}];
  print_s [%sexp (added : Message.t list)];
  [%expect
    {|
    ((User ((text hi)))
     (Assistant
      ((content ((Text Hello!))) (stop_reason End_turn)
       (usage ((input 10) (output 5) (cache_read 0))) (model deepseek-flash))))
    |}]
;;

let%expect_test
    "tool round trip, then final answer; requests carry the growing context"
  =
  with_sandbox
  @@ fun t ->
  write t "notes.txt" "remember the milk\n";
  let requests = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r -> requests := List.length r.messages :: !requests)
      [ Reply.tool_call
          ~text:"Let me read."
          ~id:"c1"
          ~name:"read"
          ~arguments:{|{"path":"notes.txt"}|}
          ()
      ; Reply.text "It says: remember the milk"
      ]
  in
  let added = run t provider "what do my notes say?" in
  [%expect
    {|
    agent_start
    message_start user "what do my notes say?"
    message_end
    turn_start
    message_start assistant
      update (Text_delta"Let me read.")
      update (Tool_call_start(index 0)(id c1)(name read))
      update (Tool_call_delta(index 0)(arguments"{\"path\":\"notes.txt\"}"))
    message_end assistant "Let me read."
    tool_start read {"path":"notes.txt"}
    tool_end "remember the milk\n"
    message_start tool_result c1
    message_end
    turn_end stop=Tool_use tool_results=1
    turn_start
    message_start assistant
      update (Text_delta"It says: remember the milk")
    message_end assistant "It says: remember the milk"
    turn_end stop=End_turn tool_results=0
    agent_end (4 new messages)
    |}];
  print_s [%sexp (List.rev !requests : int list)];
  print_s
    [%sexp
      (List.map added ~f:(function
         | Message.User _ -> "user"
         | Assistant _ -> "assistant"
         | Tool_result r -> "tool_result:" ^ r.tool_name)
       : string list)];
  [%expect
    {|
    (1 3)
    (user assistant tool_result:read assistant)
    |}]
;;

let%expect_test
    "multiple tool calls in one turn run sequentially; bash output streams"
  =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      [ Reply.tool_calls
          [ "c1", "bash", {|{"command":"echo one"}|}
          ; "c2", "write", {|{"path":"out.txt","content":"x"}|}
          ; "c3", "bash", {|{"command":"cat out.txt"}|}
          ]
      ; Reply.text "done"
      ]
  in
  let (_ : Message.t list) = run t provider "go" in
  [%expect
    {|
    agent_start
    message_start user "go"
    message_end
    turn_start
    message_start assistant
      update (Tool_call_start(index 0)(id c1)(name bash))
      update (Tool_call_delta(index 0)(arguments"{\"command\":\"echo one\"}"))
      update (Tool_call_start(index 1)(id c2)(name write))
      update (Tool_call_delta(index 1)(arguments"{\"path\":\"out.txt\",\"content\":\"x\"}"))
      update (Tool_call_start(index 2)(id c3)(name bash))
      update (Tool_call_delta(index 2)(arguments"{\"command\":\"cat out.txt\"}"))
    message_end assistant ""
    tool_start bash {"command":"echo one"}
      tool_output "one\n"
    tool_end "one\n"
    message_start tool_result c1
    message_end
    tool_start write {"path":"out.txt","content":"x"}
    tool_end "Created $DIR/out.txt (1 bytes)"
    message_start tool_result c2
    message_end
    tool_start bash {"command":"cat out.txt"}
      tool_output "x"
    tool_end "x"
    message_start tool_result c3
    message_end
    turn_end stop=Tool_use tool_results=3
    turn_start
    message_start assistant
      update (Text_delta done)
    message_end assistant "done"
    turn_end stop=End_turn tool_results=0
    agent_end (6 new messages)
    |}]
;;

let%expect_test
    "unknown tool, bad arguments and tool errors become error results and the \
     loop continues"
  =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      [ Reply.tool_calls
          [ "c1", "teleport", "{}"
          ; "c2", "read", "not json"
          ; "c3", "read", {|{"path":"missing"}|}
          ; "c4", "bash", {|{"command":"exit 2"}|}
          ]
      ; Reply.text "ok"
      ]
  in
  let added = run t provider "go" in
  let output = [%expect.output] in
  print_string
    (String.concat_lines
       (List.filter
          (String.split_lines output)
          ~f:(String.is_prefix ~prefix:"tool_end")));
  [%expect
    {|
    tool_end ERROR "unknown tool \"teleport\""
    tool_end ERROR "invalid arguments: json: unexpected string: 'not'"
    tool_end ERROR "file not found: $DIR/missing"
    tool_end ERROR "[exit code 2]"
    |}];
  print_s
    [%sexp
      (List.filter_map added ~f:(function
         | Message.Tool_result r -> Some (r.tool_call_id, r.is_error)
         | _ -> None)
       : (string * bool) list)];
  [%expect {| ((c1 true) (c2 true) (c3 true) (c4 true)) |}]
;;

let%expect_test
    "provider error and length stop the loop; error keeps context valid"
  =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      [ Reply.text ~stop_reason:(Error "HTTP 400: boom") "partial" ]
  in
  let (_ : Message.t list) = run t provider "go" in
  [%expect
    {|
    agent_start
    message_start user "go"
    message_end
    turn_start
    message_start assistant
      update (Text_delta partial)
    message_end assistant "partial"
    turn_end stop=(Error"HTTP 400: boom") tool_results=0
    agent_end (2 new messages)
    |}];
  let provider =
    Faux_provider.create [ Reply.text ~stop_reason:Length "cut off" ]
  in
  let (_ : Message.t list) = run t provider "go" in
  let output = [%expect.output] in
  print_endline (List.nth_exn (String.split_lines output) 7);
  [%expect {| turn_end stop=Length tool_results=0 |}];
  let provider = Faux_provider.create [] in
  let added = run t provider "go" in
  print_s [%sexp (List.last_exn added : Message.t)];
  let output = [%expect.output] in
  print_endline (List.last_exn (String.split_lines output));
  [%expect
    {| (usage ((input 0) (output 0) (cache_read 0))) (model deepseek-flash))) |}]
;;

let%expect_test
    "abort mid-stream: partial message kept, pending tool calls get cancelled \
     results"
  =
  with_sandbox
  @@ fun t ->
  let cancel = Cancellation.create () in
  let count = ref 0 in
  let provider =
    Faux_provider.create
      ~delay_between_events:(fun () ->
        incr count;
        if !count = 3 then Cancellation.cancel cancel)
      [ Reply.tool_calls
          ~text:"I will run something"
          [ "c1", "bash", {|{"command":"sleep 5"}|} ]
      ; Reply.text "never"
      ]
  in
  let added = run t ~cancel provider "go" in
  [%expect
    {|
    agent_start
    message_start user "go"
    message_end
    turn_start
    message_start assistant
      update (Text_delta"I will run something")
      update (Tool_call_start(index 0)(id c1)(name bash))
    message_end assistant "I will run something"
    message_start tool_result c1
    message_end
    turn_end stop=Aborted tool_results=1
    agent_end (3 new messages)
    |}];
  print_s [%sexp (List.last_exn added : Message.t)];
  [%expect
    {|
    (Tool_result
     ((tool_call_id c1) (tool_name bash) (text [cancelled]) (is_error true)))
    |}]
;;

let%expect_test "abort during a tool: tool is killed, remaining calls skipped" =
  with_sandbox
  @@ fun t ->
  let cancel = Cancellation.create () in
  let provider =
    Faux_provider.create
      [ Reply.tool_calls
          [ "c1", "bash", {|{"command":"echo started; sleep 5"}|}
          ; "c2", "bash", {|{"command":"echo never"}|}
          ]
      ; Reply.text "never"
      ]
  in
  Eio.Fiber.both
    (fun () -> ignore (run t ~cancel provider "go" : Message.t list))
    (fun () ->
       Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.2;
       Cancellation.cancel cancel);
  let output = [%expect.output] in
  print_string
    (String.concat_lines
       (List.filter (String.split_lines output) ~f:(fun l ->
          String.is_prefix l ~prefix:"tool_"
          || String.is_prefix l ~prefix:"turn_end"
          || String.is_prefix l ~prefix:"agent_end")));
  [%expect
    {|
    tool_start bash {"command":"echo started; sleep 5"}
    tool_end ERROR "started\n[cancelled]"
    turn_end stop=Tool_use tool_results=2
    agent_end (4 new messages)
    |}]
;;

let%expect_test "max_turns" =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      (List.init 5 ~f:(fun i ->
         Reply.tool_call ~id:(sprintf "c%d" i) ~name:"ls" ~arguments:"{}" ()))
  in
  let added = run t ~max_turns:2 provider "loop forever" in
  let output = [%expect.output] in
  print_s
    [%sexp
      (List.count
         (String.split_lines output)
         ~f:(String.is_prefix ~prefix:"turn_start")
       : int)];
  print_s [%sexp (List.length added : int)];
  [%expect
    {|
    2
    5
    |}]
;;

let%expect_test "steering messages are injected after tool results" =
  with_sandbox
  @@ fun t ->
  let queued = ref [ Message.user "actually, stop and summarise" ] in
  let steer () =
    let m = !queued in
    queued := [];
    m
  in
  let requests = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        requests
        := List.map r.messages ~f:(function
             | Message.User u -> "user:" ^ u.text
             | Assistant _ -> "assistant"
             | Tool_result _ -> "tool_result")
           :: !requests)
      [ Reply.tool_call ~id:"c1" ~name:"ls" ~arguments:"{}" ()
      ; Reply.text "Summary: nothing."
      ]
  in
  let (_ : Message.t list) = run t ~steer provider "explore" in
  print_s [%sexp (List.rev !requests : string list list)];
  [%expect
    {|
    agent_start
    message_start user "explore"
    message_end
    turn_start
    message_start assistant
      update (Tool_call_start(index 0)(id c1)(name ls))
      update (Tool_call_delta(index 0)(arguments {}))
    message_end assistant ""
    tool_start ls {}
    tool_end "(empty directory)\n"
    message_start tool_result c1
    message_end
    turn_end stop=Tool_use tool_results=1
    message_start user "actually, stop and summarise"
    message_end
    turn_start
    message_start assistant
      update (Text_delta"Summary: nothing.")
    message_end assistant "Summary: nothing."
    turn_end stop=End_turn tool_results=0
    agent_end (5 new messages)
    ((user:explore)
     (user:explore assistant tool_result "user:actually, stop and summarise"))
    |}]
;;

let%expect_test "existing context is sent but not returned" =
  with_sandbox
  @@ fun t ->
  let context =
    [ Message.user "earlier"
    ; Assistant
        { content = [ Text "ok" ]
        ; stop_reason = End_turn
        ; usage = Usage.zero
        ; model = "m"
        }
    ]
  in
  let sizes = ref [] in
  let provider =
    Faux_provider.create
      ~on_request:(fun r -> sizes := List.length r.messages :: !sizes)
      [ Reply.text "later" ]
  in
  let added = run t ~context provider "now" in
  let (_ : string) = [%expect.output] in
  print_s [%sexp (!sizes : int list), (List.length added : int)];
  [%expect {| ((3) 2) |}]
;;

let%expect_test "transient provider errors are retried; permanent ones are not" =
  with_sandbox
  @@ fun t ->
  let provider =
    Faux_provider.create
      [ Reply.text ~stop_reason:(Error "HTTP 429: slow down") ""
      ; Reply.text ~stop_reason:(Error "connection failed: x") ""
      ; Reply.text "finally"
      ]
  in
  let (_ : Message.t list) = run t provider "go" in
  [%expect
    {|
    agent_start
    message_start user "go"
    message_end
    turn_start
    message_start assistant
      update (Text_delta"")
      tool_output "attempt 1"
    message_start assistant
      update (Text_delta"")
      tool_output "attempt 2"
    message_start assistant
      update (Text_delta finally)
    message_end assistant "finally"
    turn_end stop=End_turn tool_results=0
    agent_end (2 new messages)
    |}];
  let provider =
    Faux_provider.create
      (List.init 4 ~f:(fun i ->
         Reply.text ~stop_reason:(Error (sprintf "HTTP 503: down %d" i)) ""))
  in
  let (_ : Message.t list) = run t provider "go" in
  let turn_ends output =
    print_string
      (String.concat_lines
         (List.filter
            (String.split_lines output)
            ~f:(String.is_prefix ~prefix:"turn_end")))
  in
  turn_ends [%expect.output];
  [%expect {| turn_end stop=(Error"HTTP 503: down 2") tool_results=0 |}];
  let provider =
    Faux_provider.create
      [ Reply.text ~stop_reason:(Error "HTTP 401: nope") ""
      ; Reply.text "unused"
      ]
  in
  let (_ : Message.t list) = run t provider "go" in
  turn_ends [%expect.output];
  [%expect {| turn_end stop=(Error"HTTP 401: nope") tool_results=0 |}]
;;
