open! Core
module H = Harness

(* What the agent is doing, in the status line and the reply it streams: a
   slow model, a thinking one and a failing one look different. *)

let assistant content =
  sprintf
    {|{"role":"assistant","content":[%s],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"claude-opus-5-5"}|}
    (String.concat ~sep:"," content)
;;

let event fields = sprintf {|{"type":"event",%s}|} fields

let update delta content =
  event
    (sprintf
       {|"event":"message_update","partial":%s,"delta":%s|}
       (assistant content)
       delta)
;;

let show h =
  H.text h ~selector:".context-ring-anchor .sr-only";
  H.show h ~selector:".msg.assistant.streaming";
  print_endline "---"
;;

let%expect_test
    "waiting, thinking without text, a retry and its attempt, then writing"
  =
  let h = H.create () in
  H.event
    h
    (event
       (sprintf
          {|"event":"state","state":%s|}
          (H.state_json ~fields:[ "running", `True ] ())));
  H.event h (event {|"event":"agent_start"|});
  H.event h (event {|"event":"turn_start"|});
  H.event
    h
    (event (sprintf {|"event":"message_start","message":%s|} (assistant [])));
  show h;
  (* Newer models' thinking has no text to show: a block starts, nothing
     more comes until it ends. *)
  H.event
    h
    (update
       {|{"type":"thinking_delta","text":""}|}
       [ {|{"type":"thinking","text":""}|} ]);
  show h;
  H.event
    h
    (event
       {|"event":"retry","attempt":2,"max_attempts":6,"error":"overloaded_error: Overloaded","delay_ms":2000|});
  show h;
  H.event
    h
    (event (sprintf {|"event":"message_start","message":%s|} (assistant [])));
  show h;
  H.event
    h
    (update
       {|{"type":"text_delta","text":"Hello"}|}
       [ {|{"type":"text","text":"Hello"}|} ]);
  H.text h ~selector:".context-ring-anchor .sr-only";
  [%expect
    {|
    Waiting for the model
    <div data-message-entry="0" class="assistant msg streaming">
      <div class="pending">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="label"> Waiting for the model… </span>
        <span class="stop-hint">
          <kbd data-slot="kbd"
               class="[&_svg:not([class*='size-'])]:size-3 bg-muted dark:in-data-[slot=tooltip-content]:bg-background/10 font-medium font-sans gap-1 h-5 in-data-[slot=tooltip-content]:bg-background/20 in-data-[slot=tooltip-content]:text-background inline-flex items-center justify-center min-w-5 pointer-events-none px-1 rounded-sm select-none text-muted-foreground text-xs w-fit"> Esc </kbd>
           to stop
        </span>
      </div>
    </div>
    ---
    Thinking
    <div data-message-entry="0" class="assistant msg streaming">
      <div class="live thinking">
        <div class="thinking-head">
          <span aria-hidden="true" class="lamp"> </span>
          <span class="label"> Thinking… </span>
        </div>
      </div>
    </div>
    ---
    Retrying in 2s (attempt 2/6): overloaded_error: Overloaded
    <div data-message-entry="0" class="assistant msg streaming">
      <div class="pending">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="label"> Retrying in 2s (attempt 2 of 6)… </span>
        <span class="stop-hint">
          <kbd data-slot="kbd"
               class="[&_svg:not([class*='size-'])]:size-3 bg-muted dark:in-data-[slot=tooltip-content]:bg-background/10 font-medium font-sans gap-1 h-5 in-data-[slot=tooltip-content]:bg-background/20 in-data-[slot=tooltip-content]:text-background inline-flex items-center justify-center min-w-5 pointer-events-none px-1 rounded-sm select-none text-muted-foreground text-xs w-fit"> Esc </kbd>
           to stop
        </span>
        <div class="detail"> after: overloaded_error: Overloaded </div>
      </div>
    </div>
    ---
    Waiting for the model (attempt 2/6, after: overloaded_error: Overloaded)
    <div data-message-entry="0" class="assistant msg streaming">
      <div class="pending">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="label"> Waiting for the model (attempt 2 of 6)… </span>
        <span class="stop-hint">
          <kbd data-slot="kbd"
               class="[&_svg:not([class*='size-'])]:size-3 bg-muted dark:in-data-[slot=tooltip-content]:bg-background/10 font-medium font-sans gap-1 h-5 in-data-[slot=tooltip-content]:bg-background/20 in-data-[slot=tooltip-content]:text-background inline-flex items-center justify-center min-w-5 pointer-events-none px-1 rounded-sm select-none text-muted-foreground text-xs w-fit"> Esc </kbd>
           to stop
        </span>
        <div class="detail"> after: overloaded_error: Overloaded </div>
      </div>
    </div>
    ---
    Writing
    |}]
;;

(* A composer session's turn, on an agent CLI: it starts, is waited for
   (with the model's look, naming the CLI), streams, asks to run a call,
   runs it and is waited for again; nothing says "the model". *)
let%expect_test "an agent CLI's turn: starting, waited for, asking, done" =
  let h = H.create () in
  let ring () =
    H.text h ~selector:".context-ring-anchor .sr-only";
    print_endline "---"
  in
  H.event
    h
    (event
       (sprintf
          {|"event":"state","state":%s|}
          (H.state_json ~fields:[ "running", `True ] ())));
  H.event h (event {|"event":"agent_start"|});
  H.event
    h
    (event {|"event":"message_start","message":{"role":"user","text":"ls"}|});
  H.event h (event {|"event":"agent_waiting","agent":"Codex","starting":true|});
  H.show h ~selector:".msg.assistant.streaming";
  ring ();
  H.event h (event {|"event":"agent_waiting","agent":"Codex","starting":false|});
  H.text h ~selector:".msg.assistant.streaming .label";
  ring ();
  H.event h (event {|"event":"turn_start"|});
  H.event h (event {|"event":"agent_waiting","agent":"Codex","starting":false|});
  H.event
    h
    (event (sprintf {|"event":"message_start","message":%s|} (assistant [])));
  H.text h ~selector:".msg.assistant.streaming .label";
  ring ();
  H.event
    h
    (update
       {|{"type":"text_delta","text":"Listing."}|}
       [ {|{"type":"text","text":"Listing."}|} ]);
  ring ();
  H.event
    h
    (event
       (sprintf
          {|"event":"message_end","message":%s|}
          (assistant
             [ {|{"type":"text","text":"Listing."}|}
             ; {|{"type":"tool_call","id":"c1","name":"bash","arguments":"{}"}|}
             ])));
  H.event
    h
    (event
       {|"event":"tool_start","call":{"id":"c1","name":"bash","arguments":"{}"}|});
  ring ();
  H.event
    h
    (event
       {|"event":"tool_confirm","call_id":"c1","agent_id":null,"name":"bash","summary":"ls","rules":[]|});
  ring ();
  H.event
    h
    (event {|"event":"tool_confirm_done","call_id":"c1","agent_id":null|});
  ring ();
  H.event
    h
    (event
       {|"event":"tool_end","call":{"id":"c1","name":"bash","arguments":"{}"},"result":{"tool_call_id":"c1","tool_name":"bash","text":"a","is_error":false}|});
  ring ();
  H.event h (event {|"event":"agent_waiting","agent":"Codex","starting":false|});
  H.text h ~selector:".msg.assistant.streaming .label";
  ring ();
  H.event h (event {|"event":"agent_end","messages":[]|});
  H.event
    h
    (event
       (sprintf
          {|"event":"state","state":%s|}
          (H.state_json ~fields:[ "running", `False ] ())));
  H.text h ~selector:".msg.assistant.streaming .label";
  ring ();
  [%expect
    {|
    <div @key=waiting class="assistant msg streaming">
      <div class="pending">
        <span aria-hidden="true" class="lamp"> </span>
        <span class="label"> Starting Codex… </span>
        <span class="stop-hint">
          <kbd data-slot="kbd"
               class="[&_svg:not([class*='size-'])]:size-3 bg-muted dark:in-data-[slot=tooltip-content]:bg-background/10 font-medium font-sans gap-1 h-5 in-data-[slot=tooltip-content]:bg-background/20 in-data-[slot=tooltip-content]:text-background inline-flex items-center justify-center min-w-5 pointer-events-none px-1 rounded-sm select-none text-muted-foreground text-xs w-fit"> Esc </kbd>
           to stop
        </span>
      </div>
    </div>
    Starting Codex
    ---
    Waiting for Codex…
    Waiting for Codex
    ---
    Waiting for Codex…
    Waiting for Codex
    ---
    Writing
    ---
    Running tools
    ---
    (Focus confirm)
    Waiting for your approval
    ---
    (Focus editor)
    (Expire_toast (id 0) (after_ms 2740))
    Running tools
    ---
    Running tools
    ---
    Waiting for Codex…
    Waiting for Codex
    ---
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    Ready
    ---
    |}]
;;
