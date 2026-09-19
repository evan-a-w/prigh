open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let script =
  {|[
  {"text": "hello"},
  {"text": "let me look", "tool_calls": [{"id": "c1", "name": "bash", "arguments": {"command": "ls"}}]},
  {"thinking": "hmm", "text": "done", "stop_reason": "end_turn"},
  {"text": "oops", "stop_reason": "error", "error": "boom"},
  {"text": "cut", "stop_reason": "length"}
]|}
;;

let%expect_test "Reply.of_json parses a scripted reply" =
  let replies =
    match Json.of_string script with
    | `Array items -> List.map items ~f:Reply.of_json
    | _ -> []
  in
  print_s [%sexp (Or_error.all replies : Reply.t list Or_error.t)];
  [%expect
    {|
    (Ok
     (((events ((Text_delta hello))) (stop_reason End_turn)
       (usage ((input 0) (output 0) (cache_read 0))))
      ((events
        ((Text_delta "let me look")
         (Tool_call_start (index 0) (id c1) (name bash))
         (Tool_call_delta (index 0) (arguments "{\"command\":\"ls\"}"))))
       (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0))))
      ((events ((Thinking_delta hmm) (Text_delta done))) (stop_reason End_turn)
       (usage ((input 0) (output 0) (cache_read 0))))
      ((events ((Text_delta oops))) (stop_reason (Error boom))
       (usage ((input 0) (output 0) (cache_read 0))))
      ((events ((Text_delta cut))) (stop_reason Length)
       (usage ((input 0) (output 0) (cache_read 0))))))
    |}]
;;

let%expect_test "Reply.of_json: chunks, usage, thinking, errors" =
  let parse s = Reply.of_json (Json.of_string s) in
  print_s
    [%sexp
      (parse
         {|{"text": "abcdef", "chunks": 3, "usage": {"input": 1, "output": 2, "cache_read": 3}}|}
       : Reply.t Or_error.t)];
  print_s
    [%sexp
      (parse {|{"thinking": "think", "text": "xy", "chunks": 2}|}
       : Reply.t Or_error.t)];
  print_s
    [%sexp
      (parse {|{"text": "x", "stop_reason": "bogus"}|} : Reply.t Or_error.t)];
  print_s
    [%sexp
      (parse {|{"stop_reason": "error", "error": "no"}|} : Reply.t Or_error.t)];
  print_s
    [%sexp
      (parse {|{"tool_calls": [{"id": "c", "name": "bash"}]}|}
       : Reply.t Or_error.t)];
  [%expect
    {|
    (Ok
     ((events ((Text_delta ab) (Text_delta cd) (Text_delta ef)))
      (stop_reason End_turn) (usage ((input 1) (output 2) (cache_read 3)))))
    (Ok
     ((events ((Thinking_delta think) (Text_delta x) (Text_delta y)))
      (stop_reason End_turn) (usage ((input 0) (output 0) (cache_read 0)))))
    (Error "unknown stop_reason \"bogus\"")
    (Ok
     ((events ()) (stop_reason (Error no))
      (usage ((input 0) (output 0) (cache_read 0)))))
    (Ok
     ((events
       ((Tool_call_start (index 0) (id c) (name bash))
        (Tool_call_delta (index 0) (arguments {}))))
      (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0)))))
    |}]
;;

let%expect_test "of_script_file reads an array of replies" =
  with_sandbox
  @@ fun t ->
  write t "script.json" script;
  let show result =
    print_endline
      (mask t (Sexp.to_string_hum [%sexp (result : Reply.t list Or_error.t)]))
  in
  show (Faux_provider.of_script_file (Filename.concat t.dir "script.json"));
  write t "bad.json" {|{"text": "not an array"}|};
  show (Faux_provider.of_script_file (Filename.concat t.dir "bad.json"));
  show (Faux_provider.of_script_file (Filename.concat t.dir "nope.json"));
  [%expect
    {|
    (Ok
     (((events ((Text_delta hello))) (stop_reason End_turn)
       (usage ((input 0) (output 0) (cache_read 0))))
      ((events
        ((Text_delta "let me look")
         (Tool_call_start (index 0) (id c1) (name bash))
         (Tool_call_delta (index 0) (arguments "{\"command\":\"ls\"}"))))
       (stop_reason Tool_use) (usage ((input 0) (output 0) (cache_read 0))))
      ((events ((Thinking_delta hmm) (Text_delta done))) (stop_reason End_turn)
       (usage ((input 0) (output 0) (cache_read 0))))
      ((events ((Text_delta oops))) (stop_reason (Error boom))
       (usage ((input 0) (output 0) (cache_read 0))))
      ((events ((Text_delta cut))) (stop_reason Length)
       (usage ((input 0) (output 0) (cache_read 0))))))
    (Error "faux script must be a JSON array of replies")
    (Error
     (Sys_error
      "$DIR/nope.json: No such file or directory"))
    |}]
;;
