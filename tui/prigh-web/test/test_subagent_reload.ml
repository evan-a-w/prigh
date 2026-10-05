open! Core

(* [get_messages] only has a subagent's report; its transcript comes from
   [get_subagent], so it survives a reload. *)
let%expect_test "a reloaded subagent call fetches its transcript" =
  let h = Harness.create () in
  Harness.act
    h
    (Reply
       ( Messages "s1"
       , Ok
           (Jsonaf.of_string
              {|[
    {"role":"user","text":"delegate"},
    {"role":"assistant","content":[{"type":"tool_call","id":"s1","name":"subagent","arguments":"{\"task\":\"count files\"}"},{"type":"tool_call","id":"s2","name":"subagent","arguments":"{\"task\":\"old one\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"},
    {"role":"tool_result","tool_call_id":"s1","tool_name":"subagent","text":"3 files","is_error":false},
    {"role":"tool_result","tool_call_id":"s2","tool_name":"subagent","text":"done","is_error":false}
  ]|})
       ));
  Harness.reply
    h
    "get_subagent"
    {|{"subagent":{"id":"a1","call_id":"s1","parent":null,"task":"count files","model":"deepseek-flash","state":"complete","turns":2,"result":{"text":"3 files","is_error":false}},
       "messages":[{"role":"user","text":"count files"},
                   {"role":"assistant","content":[{"type":"text","text":"There are 3 files."}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}]}|};
  (* From before the backend restarted. *)
  Harness.act h (Reply (Subagent "s2", Error "unknown subagent \"s2\""));
  print_s
    [%sexp
      (List.map [ "s1"; "s2" ] ~f:(fun id ->
         ( id
         , Option.bind
             (Prigh_web.Chat.tool (Harness.model h).chat id)
             ~f:(fun t ->
               Option.map t.subagent ~f:(fun s ->
                 s.agent_id, List.length (Prigh_web.Chat.entries s.chat))) ))
       : (string * (string * int) option) list)];
  print_s [%sexp (List.length (Harness.model h).toasts : int)];
  [%expect
    {|
    Scroll_to_bottom
    (Rpc (method_ get_subagent) (params ((id s1))) (tag (Subagent s1)))
    (Rpc (method_ get_subagent) (params ((id s2))) (tag (Subagent s2)))
    ((s1 ((a1 2))) (s2 ()))
    0
    |}]
;;
