open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf
module H = Test_rpc.H

let call = Test_rpc.call

let assistant ?(text = "") calls =
  Message.Assistant
    { content =
        (if String.is_empty text then [] else [ Content.Text text ])
        @ List.map calls ~f:(fun (id, name) ->
          Content.Tool_call { id; name; arguments = "{}" })
    ; stop_reason = Tool_use
    ; usage = Usage.zero
    ; model = "m"
    }
;;

let result id =
  Message.Tool_result
    { tool_call_id = id
    ; tool_name = "ls"
    ; text = "out-" ^ id
    ; is_error = false
    ; images = []
    }
;;

let summarize_message = function
  | Message.User u ->
    "user: " ^ String.prefix (String.tr u.text ~target:'\n' ~replacement:' ') 70
  | Tool_result r -> sprintf "tool_result %s: %S" r.tool_call_id r.text
  | Assistant a ->
    let calls =
      List.map (Message.Assistant.tool_calls a) ~f:(fun c ->
        sprintf " [call %s %s]" c.id c.name)
    in
    "assistant: " ^ Message.Assistant.text a ^ String.concat calls
;;

let print_messages messages =
  List.iter messages ~f:(fun m -> print_endline (summarize_message m))
;;

let%expect_test "sanitize pairs every tool call with one result" =
  print_messages
    (Btw.sanitize
       [ Message.user "go"
       ; assistant ~text:"two calls" [ "c1", "ls"; "c2", "ls" ]
       ; result "c2"
       ; result "stray"
       ; Message.user "steer"
       ; assistant [ "c3", "ls"; "c4", "bash" ]
       ; result "c3"
       ]);
  [%expect
    {|
    user: go
    assistant: two calls [call c1 ls] [call c2 ls]
    tool_result c1: "[still running: no result yet]"
    tool_result c2: "out-c2"
    user: steer
    assistant:  [call c3 ls] [call c4 bash]
    tool_result c3: "out-c3"
    tool_result c4: "[still running: no result yet]"
    |}];
  print_messages
    (Btw.sanitize [ result "orphan"; Message.user "hi"; assistant [] ]);
  [%expect {| user: hi |}]
;;

let summarize_request (r : Provider.Request.t) =
  printf
    "model=%s thinking=%s tools=%d system=%s\n"
    r.model.id
    (Sexp.to_string [%sexp (r.thinking : Thinking.t)])
    (List.length r.tools)
    (match r.system with
     | Some s when String.is_prefix s ~prefix:"You are prigh" ->
       "<prigh system prompt>"
     | Some s -> String.prefix s 40
     | None -> "none");
  print_messages r.messages
;;

let with_btw_server replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let provider =
    Faux_provider.create ~on_request:(Queue.enqueue requests) replies
  in
  let agent, h = Test_rpc.make_server t ~sw ~provider in
  f t agent h requests
;;

let session_file agent =
  let path = Session.path (Agent.session agent) in
  if Sys_unix.file_exists_exn path then In_channel.read_all path else "<none>"
;;

let print_file_lines t contents =
  String.split_lines contents
  |> List.iter ~f:(fun line -> print_endline (String.prefix (mask t line) 70))
;;

let events_named name (sent : Json.t Queue.t) =
  Queue.to_list sent
  |> List.filter ~f:(fun json ->
    match Json.member "event" json with
    | Some (`String e) -> String.equal e name
    | _ -> false)
;;

let print_btw_deltas t sent =
  List.iter (events_named "btw_delta" sent) ~f:(fun json ->
    print_endline (mask t (Json.to_string json)))
;;

let%expect_test "btw while idle answers without touching the session" =
  with_btw_server
    [ Reply.text "Hello there."
    ; { Reply.events = [ Text_delta "It "; Text_delta "said hello." ]
      ; stop_reason = End_turn
      ; usage = { input = 100; output = 10; cache_read = 0 }
      }
    ]
  @@ fun t agent h requests ->
  call t h ~params:{|{"text": "say hello"}|} "prompt";
  Agent.wait_idle agent;
  let before = session_file agent in
  let count () = List.length (Agent.messages agent) in
  printf "messages before: %d\n" (count ());
  print_file_lines t before;
  Queue.clear requests;
  Queue.clear h.sent;
  call t h ~params:{|{"question": "what did you say?"}|} "btw";
  print_btw_deltas t h.sent;
  printf "messages after: %d\n" (count ());
  printf
    "session file unchanged: %b\n"
    (String.equal before (session_file agent));
  Queue.iter requests ~f:summarize_request;
  let state = Agent.state agent in
  printf
    "state: message_count=%d usage=%s\n"
    state.message_count
    (Sexp.to_string [%sexp (state.usage : Usage.t)]);
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    messages before: 2
    ["Header",{"id":"<id>","cwd":"$DIR","created_at":"<time>"}]
    ["Entry",{"id":"<id>","parent":null,"payload":["System_prompt",{"text"
    ["Entry",{"id":"<id>","parent":"<id>","payload":["Message",["User",{"t
    ["Entry",{"id":"<id>","parent":"<id>","payload":["Message",["Assistant
    {"type":"response","id":"r1","ok":true,"result":{"btw_id":"btw-1","text":"It said hello.","usage":{"input":100,"output":10,"cache_read":0},"cost_usd":4.2e-05}}
    {"type":"event","event":"btw_delta","btw_id":"btw-1","delta":"It "}
    {"type":"event","event":"btw_delta","btw_id":"btw-1","delta":"said hello."}
    messages after: 2
    session file unchanged: true
    model=deepseek-flash thinking=Off tools=0 system=<prigh system prompt>
    user: say hello
    assistant: Hello there.
    user: <btw>The user asks a side question (/btw) while you may be in the midd
    state: message_count=2 usage=((input 110)(output 15)(cache_read 0))
    |}];
  (* The question itself is the tail of the final message. *)
  let last = List.last_exn (Queue.last_exn requests).messages in
  (match last with
   | User u -> print_endline (List.last_exn (String.split_lines u.text))
   | _ -> ());
  [%expect {| what did you say? |}];
  call t h ~params:{|{"question": "   "}|} "btw";
  call t h "btw";
  call t h ~params:{|{"question": "again?"}|} "btw";
  [%expect
    {|
    {"type":"response","id":"r1","ok":false,"error":"missing question"}
    {"type":"response","id":"r1","ok":false,"error":"missing param \"question\""}
    {"type":"response","id":"r1","ok":false,"error":"faux provider: no scripted reply"}
    |}]
;;

let%expect_test
    "btw in a fresh session builds the system prompt without recording it"
  =
  with_btw_server [ Reply.text "Nothing yet." ]
  @@ fun t agent h requests ->
  call t h ~params:{|{"question": "anything?", "btw_id": "mine"}|} "btw";
  Queue.iter requests ~f:summarize_request;
  printf
    "recorded system prompt: %b, file: %s\n"
    (Option.is_some (Session.system_prompt (Agent.session agent)))
    (session_file agent);
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"btw_id":"mine","text":"Nothing yet.","usage":{"input":10,"output":5,"cache_read":0},"cost_usd":9e-06}}
    model=deepseek-flash thinking=Off tools=0 system=<prigh system prompt>
    user: <btw>The user asks a side question (/btw) while you may be in the midd
    recorded system prompt: false, file: <none>
    |}]
;;

let wait_until ~what f =
  let rec go n =
    if f ()
    then ()
    else if n = 0
    then failwithf "timed out waiting for %s" what ()
    else (
      Eio.Fiber.yield ();
      go (n - 1))
  in
  go 10_000
;;

(* A turn blocked on confirming its second tool call: the first call's result
   is in the session, the second has none yet. *)
let%expect_test
    "btw during a run sees a sanitized snapshot and leaves the run alone"
  =
  with_btw_server
    [ Reply.tool_calls
        ~text:"Listing, then echoing."
        [ "c1", "ls", "{}"; "c2", "bash", {|{"command":"echo hi"}|} ]
    ; Reply.text "I am listing files and about to echo."
    ; Reply.text "All done."
    ]
  @@ fun t agent h requests ->
  call
    t
    h
    ~params:{|{"config": {"scoped_models": [], "confirm_tools": true}}|}
    "set_config";
  let other_sent = Queue.create () in
  let other = Rpc_server.connect h.server ~send:(Queue.enqueue other_sent) in
  ignore
    (Rpc_server.handle
       h.server
       other
       (Json.of_string
          (sprintf
             {|{"id": 1, "method": "hello", "params": %s}|}
             (Test_rpc.hello_params ~session:agent {|"name": "other"|})))
     : Json.t);
  Queue.clear h.sent;
  call t h ~params:{|{"text": "list and echo"}|} "prompt";
  wait_until ~what:"tool_confirm" (fun () ->
    not (List.is_empty (events_named "tool_confirm" h.sent)));
  Queue.clear requests;
  Queue.clear other_sent;
  printf "running: %b\n" (Agent.is_running agent);
  call t h ~params:{|{"question": "what are you doing?"}|} "btw";
  Queue.iter requests ~f:summarize_request;
  printf
    "other client saw btw_delta: %b\n"
    (not (List.is_empty (events_named "btw_delta" other_sent)));
  printf "still running: %b\n" (Agent.is_running agent);
  call t h ~params:{|{"call_id": "c2", "allow": true}|} "tool_confirm_respond";
  Agent.wait_idle agent;
  print_messages (Agent.messages agent);
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"scoped_models":[],"confirm_tools":true,"default_model":null,"default_thinking":null}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    running: true
    {"type":"response","id":"r1","ok":true,"result":{"btw_id":"btw-1","text":"I am listing files and about to echo.","usage":{"input":10,"output":5,"cache_read":0},"cost_usd":9e-06}}
    model=deepseek-flash thinking=Off tools=0 system=<prigh system prompt>
    user: list and echo
    assistant: Listing, then echoing. [call c1 ls] [call c2 bash]
    tool_result c1: ".prigh/\nsessions/\n"
    tool_result c2: "[still running: no result yet]"
    user: <btw>The user asks a side question (/btw) while you may be in the midd
    other client saw btw_delta: false
    still running: true
    {"type":"response","id":"r1","ok":true,"result":{}}
    user: list and echo
    assistant: Listing, then echoing. [call c1 ls] [call c2 bash]
    tool_result c1: ".prigh/\nsessions/\n"
    tool_result c2: "hi\n"
    assistant: All done.
    |}]
;;

let%expect_test "btw_cancel and disconnect cancel an in-flight btw" =
  let slow =
    { Reply.events =
        List.init 50 ~f:(fun i -> Assistant_event.Text_delta (sprintf "%d " i))
    ; stop_reason = End_turn
    ; usage = Usage.zero
    }
  in
  with_btw_server [ slow; slow ]
  @@ fun t _agent h _requests ->
  Eio.Fiber.both
    (fun () -> call t h ~params:{|{"question": "q", "btw_id": "a"}|} "btw")
    (fun () ->
       wait_until ~what:"a delta" (fun () ->
         not (List.is_empty (events_named "btw_delta" h.sent)));
       call t h ~params:{|{"question": "dup", "btw_id": "a"}|} "btw";
       call t h ~params:{|{"btw_id": "a"}|} "btw_cancel");
  call t h ~params:{|{"btw_id": "a"}|} "btw_cancel";
  [%expect
    {|
    {"type":"response","id":"r1","ok":false,"error":"btw \"a\" is already running"}
    {"type":"response","id":"r1","ok":true,"result":{"cancelled":true}}
    {"type":"response","id":"r1","ok":false,"error":"cancelled"}
    {"type":"response","id":"r1","ok":true,"result":{"cancelled":false}}
    |}];
  Queue.clear h.sent;
  Eio.Fiber.both
    (fun () -> call t h ~params:{|{"question": "q", "btw_id": "b"}|} "btw")
    (fun () ->
       wait_until ~what:"b delta" (fun () ->
         not (List.is_empty (events_named "btw_delta" h.sent)));
       Rpc_server.disconnect h.server h.client);
  [%expect {| {"type":"response","id":"r1","ok":false,"error":"cancelled"} |}]
;;
