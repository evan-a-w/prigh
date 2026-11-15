open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

let update ?(partial = "p") delta =
  sprintf
    {|{"type":"event","event":"message_update","partial":"%s","delta":%s}|}
    partial
    delta
;;

let text s = sprintf {|{"type":"text_delta","text":"%s"}|} s
let thinking s = sprintf {|{"type":"thinking_delta","text":"%s"}|} s

let args index s =
  sprintf {|{"type":"tool_call_delta","index":%d,"arguments":"%s"}|} index s
;;

let subagent agent_id inner =
  sprintf
    {|{"type":"event","event":"subagent","call_id":"c","agent_id":"%s","inner":%s}|}
    agent_id
    inner
;;

(* Sends [lines] while nothing is written, then writes what waits. *)
let written lines =
  with_sandbox
  @@ fun _ ->
  let outbox = Outbox.create () in
  let coalescer = Update_coalescer.create outbox in
  List.iter lines ~f:(fun line ->
    Update_coalescer.send coalescer (Json.of_string line));
  Outbox.finish outbox;
  Outbox.run outbox ~write:print_endline
;;

let%expect_test "waiting updates of one message merge; others keep their order" =
  written
    [ update ~partial:"t" (thinking "a")
    ; update ~partial:"tb" (thinking "b")
    ; update ~partial:"tbx" (text "x")
    ; update ~partial:"tbxy" (text "y")
    ; {|{"type":"event","event":"tool_start"}|}
    ; update ~partial:"1" (text "z")
    ; update ~partial:"2" (args 0 "{\\\"a")
    ; update ~partial:"3" (args 0 "\\\":1}")
    ; update ~partial:"4" (args 1 "{")
    ; update ~partial:"5" {|{"type":"thinking_signature"}|}
    ; update ~partial:"6" {|{"type":"thinking_signature"}|}
    ];
  [%expect
    {|
    {"type":"event","event":"message_update","partial":"tb","delta":{"type":"thinking_delta","text":"ab"}}
    {"type":"event","event":"message_update","partial":"tbxy","delta":{"type":"text_delta","text":"xy"}}
    {"type":"event","event":"tool_start"}
    {"type":"event","event":"message_update","partial":"1","delta":{"type":"text_delta","text":"z"}}
    {"type":"event","event":"message_update","partial":"3","delta":{"type":"tool_call_delta","index":0,"arguments":"{\"a\":1}"}}
    {"type":"event","event":"message_update","partial":"4","delta":{"type":"tool_call_delta","index":1,"arguments":"{"}}
    {"type":"event","event":"message_update","partial":"5","delta":{"type":"thinking_signature"}}
    {"type":"event","event":"message_update","partial":"6","delta":{"type":"thinking_signature"}}
    |}]
;;

let%expect_test "subagents' streams merge each on their own" =
  written
    [ update (text "main ")
    ; subagent "a1" (update (text "one "))
    ; subagent "a2" (update (text "two "))
    ; subagent "a1" (subagent "a3" (update (text "nested ")))
    ; update (text "again")
    ; subagent "a1" (update (text "again"))
    ; subagent "a1" (subagent "a3" (update (text "again")))
    ; subagent "a2" (update (text "again"))
    ];
  [%expect
    {|
    {"type":"event","event":"message_update","partial":"p","delta":{"type":"text_delta","text":"main again"}}
    {"type":"event","event":"subagent","call_id":"c","agent_id":"a1","inner":{"type":"event","event":"message_update","partial":"p","delta":{"type":"text_delta","text":"one again"}}}
    {"type":"event","event":"subagent","call_id":"c","agent_id":"a2","inner":{"type":"event","event":"message_update","partial":"p","delta":{"type":"text_delta","text":"two again"}}}
    {"type":"event","event":"subagent","call_id":"c","agent_id":"a1","inner":{"type":"event","event":"subagent","call_id":"c","agent_id":"a3","inner":{"type":"event","event":"message_update","partial":"p","delta":{"type":"text_delta","text":"nested again"}}}}
    |}]
;;

let%expect_test "an update already written is not merged into" =
  with_sandbox
  @@ fun _ ->
  let outbox = Outbox.create () in
  let coalescer = Update_coalescer.create outbox in
  let send line = Update_coalescer.send coalescer (Json.of_string line) in
  let written = ref [] in
  send (update (text "a"));
  Outbox.finish outbox;
  Outbox.run outbox ~write:(fun line ->
    written := line :: !written;
    send (update (text "b")));
  List.iter (List.rev !written) ~f:print_endline;
  [%expect
    {| {"type":"event","event":"message_update","partial":"p","delta":{"type":"text_delta","text":"a"}} |}]
;;

let numbered ?partial ~stream ~seq ~block delta =
  sprintf
    {|{"type":"event","event":"message_update","stream":%d,"seq":%d%s,"delta":%s}|}
    stream
    seq
    (Option.value_map partial ~default:"" ~f:(sprintf {|,"partial":"%s"|}))
    (String.chop_suffix_exn delta ~suffix:"}" ^ sprintf {|,"block":%d}|} block)
;;

let%expect_test "waiting deltas merge into one covering their pieces" =
  written
    [ numbered ~stream:1 ~seq:1 ~block:0 (thinking "a")
    ; numbered ~stream:1 ~seq:2 ~block:0 (thinking "b")
    ; numbered ~stream:1 ~seq:3 ~block:1 (text "x")
    ; numbered ~stream:1 ~seq:4 ~block:1 (text "y")
    ; numbered ~stream:1 ~seq:5 ~block:1 (text "z")
    ; (* A new text block (after a tool call) is not joined to the old one. *)
      numbered ~stream:1 ~seq:6 ~block:3 (text "w")
    ; (* Nor is a resync, which has the whole message, to a delta. *)
      numbered ~partial:"all" ~stream:2 ~seq:7 ~block:0 (text "r")
    ; numbered ~partial:"all" ~stream:2 ~seq:8 ~block:0 (text "s")
    ; numbered ~stream:2 ~seq:9 ~block:0 (text "t")
    ; numbered ~stream:2 ~seq:10 ~block:0 (text "u")
    ; numbered ~stream:3 ~seq:1 ~block:0 (text "new")
    ];
  [%expect
    {|
    {"type":"event","event":"message_update","stream":1,"from":1,"seq":2,"delta":{"type":"thinking_delta","text":"ab","block":0}}
    {"type":"event","event":"message_update","stream":1,"from":3,"seq":5,"delta":{"type":"text_delta","text":"xyz","block":1}}
    {"type":"event","event":"message_update","stream":1,"seq":6,"delta":{"type":"text_delta","text":"w","block":3}}
    {"type":"event","event":"message_update","stream":2,"from":7,"seq":8,"partial":"all","delta":{"type":"text_delta","text":"rs","block":0}}
    {"type":"event","event":"message_update","stream":2,"from":9,"seq":10,"delta":{"type":"text_delta","text":"tu","block":0}}
    {"type":"event","event":"message_update","stream":3,"seq":1,"delta":{"type":"text_delta","text":"new","block":0}}
    |}]
;;
