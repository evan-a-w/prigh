open! Core
module H = Harness
module E = Agent_events

(* A long session shows only its newest page: the [subagent] calls that
   started its subagents can be in older pages, not loaded. *)

let usage = {|"usage":{"input":0,"output":0,"cache_read":0}|}

let call_message ~id task =
  sprintf
    {|{"role":"assistant","content":[{"type":"tool_call","id":"%s","name":"subagent","arguments":"{\"task\":\"%s\"}"}],"stop_reason":{"type":"tool_use"},%s,"model":"m"}|}
    id
    task
    usage
;;

let call_result ~id text =
  sprintf
    {|{"role":"tool_result","tool_call_id":"%s","tool_name":"subagent","text":"%s","is_error":false}|}
    id
    text
;;

let text_message text =
  sprintf
    {|{"role":"assistant","content":[{"type":"text","text":"%s"}],"stop_reason":{"type":"end_turn"},%s,"model":"m"}|}
    text
    usage
;;

let user text = sprintf {|{"role":"user","text":"%s"}|} text
let array messages = "[" ^ String.concat ~sep:"," messages ^ "]"

let listed
      ?(parent = "null")
      ?(state = "complete")
      ?(result = "null")
      ~id
      ~call
      task
  =
  sprintf
    {|{"id":"%s","call_id":"%s","parent":%s,"task":"%s","model":"deepseek-flash","state":"%s","started_at_ms":1791187200000,"updated_at_ms":0,"ended_at_ms":null,"turns":1,"tool_calls":0,"current_tool":null,"current_tool_started_at_ms":null,"message_count":2,"stale":false,"result":%s}|}
    id
    call
    parent
    task
    state
    result
;;

let detail ?(parent = "null") ?(result = "null") ~id ~call ~task messages =
  sprintf
    {|{"subagent":{"id":"%s","call_id":"%s","parent":%s,"task":"%s","model":"deepseek-flash","state":"complete","turns":1,"result":%s},"messages":%s}|}
    id
    call
    parent
    task
    result
    (array messages)
;;

(* The newest page: two exchanges after 40 older messages, among them the
   calls [s1] (agent [a1], with [a1/n1] under it) and [s2] (agent [a2]). *)
let paged () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act
      h
      (Reply
         ( H.newest "s1"
         , Ok
             (Jsonaf.of_string
              @@ H.page
                   ~older:40
                   ~before:"e40"
                   (array [ user "and now?"; text_message "Still going." ])) )));
  H.reply
    h
    "list_subagents"
    (sprintf
       "[%s]"
       (String.concat
          ~sep:","
          [ listed
              ~id:"a1"
              ~call:"s1"
              ~result:{|{"text":"3 files","is_error":false}|}
              "Count the files"
          ; listed
              ~id:"a1/n1"
              ~call:"n1"
              ~parent:{|"a1"|}
              ~result:{|{"text":"42","is_error":false}|}
              "Count the modules"
          ; listed ~id:"a2" ~call:"s2" ~state:"running" "Watch the build"
          ]));
  h
;;

let a1_detail =
  detail
    ~id:"a1"
    ~call:"s1"
    ~task:"Count the files"
    ~result:{|{"text":"3 files","is_error":false}|}
    [ user "Count the files"
    ; call_message ~id:"n1" "Count the modules"
    ; call_result ~id:"n1" "42"
    ; text_message "There are 3 files."
    ]
;;

let n1_detail =
  detail
    ~id:"a1/n1"
    ~call:"n1"
    ~parent:{|"a1"|}
    ~task:"Count the modules"
    ~result:{|{"text":"42","is_error":false}|}
    [ user "Count the modules"; text_message "42 modules." ]
;;

let show h =
  H.text h ~selector:".agents-transcript";
  H.text h ~selector:".agents-note"
;;

let%expect_test "a subagent whose call is in a page not loaded" =
  let h = paged () in
  H.act h (Select_item (Agent "a1"));
  show h;
  H.reply h "get_subagent" a1_detail;
  show h;
  [%expect
    {|
    (Rpc (method_ get_subagent) (params ((id a1) (elide true)))
     (tag (Subagent a1)))
    Loading the transcript…
    Count the files
    ✓ subagent Count the modules
    Report 42
    42
    There are 3 files.
    |}];
  (* Its own subagent, fetched before or after it. *)
  H.act h (Select_item (Agent "a1/n1"));
  H.reply h "get_subagent" n1_detail;
  show h;
  [%expect
    {|
    (Rpc (method_ get_subagent) (params ((id a1/n1) (elide true)))
     (tag (Subagent a1/n1)))
    Count the modules
    42 modules.
    |}]
;;

let%expect_test "a subagent not loaded follows its events" =
  let h = paged () in
  H.quiet h (fun () -> H.act h (Select_item (Agent "a2")));
  H.reply
    h
    "get_subagent"
    (detail
       ~id:"a2"
       ~call:"s2"
       ~task:"Watch the build"
       [ user "Watch the build" ]);
  H.event h (E.inner ~call:"s2" ~agent:"a2" {|{"event":"turn_start"}|});
  H.event
    h
    (E.inner ~call:"s2" ~agent:"a2" (E.text_reply "The build is green."));
  show h;
  [%expect
    {|
    Watch the build
    The build is green.
    |}];
  H.quiet h (fun () ->
    H.event h (E.end_ ~call:"s2" ~agent:"a2" ~turns:1 "green"));
  let s =
    Option.value_exn (Prigh_web.Chat.find_subagent (H.model h).chat "a2")
  in
  print_s
    [%sexp
      { turns : int = s.turns
      ; cost_usd : float option = s.cost_usd
      ; result : bool = Option.is_some s.result
      }];
  [%expect {| ((turns 1) (cost_usd (0.012)) (result true)) |}]
;;

let%expect_test "a transcript that does not come says so, with a retry" =
  let h = paged () in
  H.act h (Select_item (Agent "a1"));
  H.fail h "get_subagent" "unknown subagent \"a1\"";
  show h;
  H.click h ~selector:".agents-note button";
  show h;
  [%expect
    {|
    (Rpc (method_ get_subagent) (params ((id a1) (elide true)))
     (tag (Subagent a1)))
    Couldn't load the transcript: unknown subagent "a1". (Retry)
    (Select_item (Agent a1))
    (Rpc (method_ get_subagent) (params ((id a1) (elide true)))
     (tag (Subagent a1)))
    Loading the transcript…
    |}];
  (* No answer at all: not a spinner for ever. *)
  let at seconds = Time_ns.add H.now (Time_ns.Span.of_sec seconds) in
  H.act h (Clock (at 1.));
  H.act h (Clock (at 15.));
  show h;
  H.act h (Clock (at 21.));
  show h;
  [%expect
    {|
    Loading the transcript…
    Couldn't load the transcript: no answer in 20s. (Retry)
    |}];
  (* A late answer still counts. *)
  H.reply h "get_subagent" a1_detail;
  H.text h ~selector:".agents-transcript .user";
  [%expect {| Count the files |}]
;;

let%expect_test "the older page with the call: the transcript moves onto it" =
  let h = paged () in
  H.quiet h (fun () -> H.act h (Select_item (Agent "a1/n1")));
  (* The nested one first: it waits beside the chat for its parent. *)
  H.act h (Reply (Subagent "a1/n1", Ok (Jsonaf.of_string n1_detail)));
  H.reply h "get_subagent" a1_detail;
  H.act h Load_older;
  H.reply
    h
    "get_messages_page"
    (H.page
       (array
          [ user "count"
          ; call_message ~id:"s1" "Count the files"
          ; call_result ~id:"s1" "3 files"
          ; call_message ~id:"s2" "Watch the build"
          ]));
  let chat = (H.model h).chat in
  print_s
    [%sexp
      (Option.bind (Prigh_web.Chat.tool chat "s1") ~f:(fun t ->
         Option.map t.subagent ~f:(fun s ->
           ( s.agent_id
           , List.length (Prigh_web.Chat.entries s.chat)
           , Option.map (Prigh_web.Chat.tool s.chat "n1") ~f:(fun n ->
               Option.map n.subagent ~f:(fun n -> n.agent_id)) )))
       : (string * int * string option option) option)];
  (* Kept once: on the call, not beside it as well. *)
  print_s
    [%sexp
      (String.is_substring
         (Sexp.to_string [%sexp (chat : Prigh_web.Chat.t)])
         ~substring:"detached"
       : bool)];
  show h;
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (before e40) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before (e40)))))
    Keep_scroll_position
    (Rpc (method_ get_subagent) (params ((id s2) (elide true)))
     (tag (Subagent s2)))
    ((a1 3 ((a1/n1))))
    false
    Count the modules
    42 modules.
    |}]
;;

let%expect_test "a transcript the panel did not ask for is not kept" =
  (* E.g. asked for in the session shown before: its ids mean other
     agents here. *)
  let h = paged () in
  H.act h (Reply (Subagent "a1", Ok (Jsonaf.of_string a1_detail)));
  print_s
    [%sexp
      (Option.is_some (Prigh_web.Chat.find_subagent (H.model h).chat "a1")
       : bool)];
  [%expect {| false |}]
;;
