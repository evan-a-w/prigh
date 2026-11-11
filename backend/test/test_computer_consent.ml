open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

(* An agent on tool hosts [desk] (and [lap]) that run the computer tool
   ([Host_ops.execute] with a {!Computer_fake}'s helper, as a host started
   with -computer does), with a subagent that has it too. [answer] answers
   each consent ([None]: leaves it unanswered); [on_event] and
   [on_request] (given the request's number) run first. What matters is
   printed in order: the consents asked and withdrawn, the results and
   whether prigh controls the screen. *)
let with_agent
      ?config
      ?(sub_replies = [])
      ?sleep_until
      ?(answer =
        fun (_ : Agent.Pending_confirm.t) -> Some Agent.Confirm_answer.Allow)
      ?(on_event =
        fun ~sw:(_ : Eio.Switch.t) (_ : Agent.t) (_ : Agent.Event.t) -> ())
      ?(on_request = fun (_ : Agent.t) (_ : int) -> ())
      replies
      f
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Option.iter config ~f:(fun json -> write t ".prigh/config.json" json);
  let fake = Computer_fake.create () in
  let helper =
    Computer_helper.create (Computer_fake.info fake) ~start:(fun () ->
      Computer_fake.transport fake)
  in
  let subagent =
    Tool_subagent.create
      ~provider:(Faux_provider.create sub_replies)
      ~current_model:(fun () -> Model.default)
      ~current_thinking:(fun () -> Off)
      ~home:t.dir
      ()
  in
  let agent_ref = ref None in
  let requests = ref 0 in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ?sleep_until
      ~provider:
        (Faux_provider.create
           ~on_request:(fun _ ->
             incr requests;
             on_request (Option.value_exn !agent_ref) !requests)
           replies)
      ~tools:(Tools.all @ [ subagent ])
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~backend_host:false
      ~cwd:t.dir
      ()
  in
  agent_ref := Some agent;
  let control = ref None in
  Agent.subscribe agent ~f:(fun e ->
    on_event ~sw agent e;
    match e with
    | Tool_exec { exec_id; name; arguments; cwd; _ } ->
      Eio.Fiber.fork ~sw (fun () ->
        let result =
          Host_ops.execute
            ~mcp:None
            ~computer:helper
            ~env:t.env
            ~cancel:Cancellation.never
            ~on_output:ignore
            ~cwd
            ~name
            ~arguments
            ()
        in
        ignore (Agent.tool_exec_result agent ~exec_id result : unit Or_error.t))
    | Loop (Tool_confirm { call_id; agent_id; name; summary; rules; kind }) ->
      printf
        "confirm %s%s %s: %s %s\n"
        (Option.value_map agent_id ~default:"" ~f:(fun a -> a ^ "/"))
        call_id
        name
        summary
        (Sexp.to_string [%sexp (kind : Confirm_kind.t)]);
      (match
         answer
           { Agent.Pending_confirm.call_id
           ; agent_id
           ; name
           ; summary
           ; rules
           ; kind
           }
       with
       | None -> print_endline "  (unanswered)"
       | Some answer ->
         printf
           "  -> %s\n"
           (Sexp.to_string [%sexp (answer : Agent.Confirm_answer.t)]);
         Or_error.ok_exn (Agent.respond_confirm ?agent_id agent ~call_id answer))
    | Loop (Tool_confirm_done { call_id; agent_id }) ->
      printf
        "confirm done %s%s\n"
        (Option.value_map agent_id ~default:"" ~f:(fun a -> a ^ "/"))
        call_id
    | Loop (Tool_end { call; result }) ->
      printf
        "%s %s -> %s%s\n"
        call.name
        call.id
        (if result.is_error then "ERROR " else "")
        (List.hd_exn (String.split_lines (String.strip result.text ^ "\n")))
    | Loop (Subagent { event = Tool_end { call; result }; agent_id; _ }) ->
      printf
        "  [%s] %s %s -> %s%s\n"
        agent_id
        call.name
        call.id
        (if result.is_error then "ERROR " else "")
        (List.hd_exn (String.split_lines (String.strip result.text ^ "\n")))
    | State_changed s ->
      if not (Option.equal String.equal s.computer_control !control)
      then (
        control := s.computer_control;
        printf "control: %s\n" (Option.value s.computer_control ~default:"none"))
    | Notice text -> printf "notice: %s\n" (mask t text)
    | _ -> ());
  let host id name : Agent.Host.t =
    { id
    ; name
    ; cwd = t.dir
    ; session_id = None
    ; session_name = None
    ; home = Some t.dir
    ; exec_input = false
    ; git_location = false
    ; computer = Some (Computer_fake.info fake)
    }
  in
  Agent.set_hosts agent [ host "h" "desk"; host "h2" "lap" ];
  f t agent fake
;;

let prompt agent text =
  printf "> %s\n" text;
  Or_error.ok_exn (Agent.prompt agent text);
  Agent.wait_idle agent
;;

let click id =
  Reply.tool_call
    ~id
    ~name:"computer"
    ~arguments:{|{"action":"click","x":10,"y":20}|}
    ()
;;

let%expect_test
    "the first action asks (also in auto mode); while allowed the next ones do \
     not; the next run asks again"
  =
  with_agent
    [ click "c1"; click "c2"; Reply.text "done"; click "c3"; Reply.text "done" ]
  @@ fun _ agent fake ->
  print_endline (Approval_mode.to_string (Agent.state agent).approval_mode);
  prompt agent "open the menu";
  prompt agent "again";
  List.iter (Computer_fake.log fake) ~f:print_endline;
  [%expect
    {|
    notice: tools now run on desk in $DIR
    auto
    > open the menu
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c1
    control: desk
    computer c1 -> click (10, 20) → (20, 40) on the screen
    computer c2 -> click (10, 20) → (20, 40) on the screen
    control: none
    > again
    confirm c3 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c3
    control: desk
    computer c3 -> click (10, 20) → (20, 40) on the screen
    control: none
    move 20,40
    click left×1
    move 20,40
    click left×1
    move 20,40
    click left×1
    |}]
;;

let%expect_test "ask mode: the consent stands for its question too" =
  with_agent [ click "c1"; click "c2"; Reply.text "done" ]
  @@ fun _ agent _ ->
  Agent.set_mode agent ~approval:Ask;
  prompt agent "go";
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c1
    control: desk
    computer c1 -> click (10, 20) → (20, 40) on the screen
    computer c2 -> click (10, 20) → (20, 40) on the screen
    control: none
    |}]
;;

let%expect_test
    "the user moving the mouse ends control: the next action asks again"
  =
  with_agent [ click "c1"; click "c2"; click "c3"; Reply.text "done" ]
  @@ fun _ agent fake ->
  let moved = ref false in
  Agent.subscribe agent ~f:(function
    | Loop (Tool_end { call = { id = "c1"; _ }; _ }) when not !moved ->
      moved := true;
      Computer_fake.user_move fake ~x:5 ~y:5
    | _ -> ());
  prompt agent "go";
  prompt agent "carry on";
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c1
    control: desk
    computer c1 -> click (10, 20) → (20, 40) on the screen
    control: none
    computer c2 -> ERROR [interrupted] you moved the mouse: stopped (computer use paused until the next prompt)
    notice: computer use stopped: you moved the mouse or pressed a key on the controlled screen. Send a message to go on.
    > carry on
    confirm c3 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c3
    control: desk
    computer c3 -> click (10, 20) → (20, 40) on the screen
    control: none
    |}]
;;

let%expect_test "Stop (aborting the run) ends control; the next run asks again" =
  with_agent
    ~on_event:(fun ~sw agent -> function
       | Loop (Tool_end { call = { id = "c1"; _ }; _ }) ->
         Eio.Fiber.fork ~sw (fun () -> ignore (Agent.abort agent : string list))
       | _ -> ())
    [ click "c1"; click "c2"; Reply.text "done"; click "c3"; Reply.text "done" ]
  @@ fun _ agent _ ->
  prompt agent "go";
  prompt agent "again";
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c1
    control: desk
    computer c1 -> click (10, 20) → (20, 40) on the screen
    control: none
    > again
    confirm c2 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c2
    control: desk
    computer c2 -> click (10, 20) → (20, 40) on the screen
    control: none
    |}]
;;

let%expect_test "denied: the model is told not to retry unless asked" =
  let answers =
    Queue.of_list
      [ Agent.Confirm_answer.Deny None; Deny (Some "I'm using the screen") ]
  in
  with_agent
    ~answer:(fun _ -> Some (Queue.dequeue_exn answers))
    [ click "c1"; Reply.text "ok"; click "c2"; Reply.text "ok" ]
  @@ fun _ agent _ ->
  prompt agent "go";
  prompt agent "go";
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      -> (Deny())
    confirm done c1
    computer c1 -> ERROR [computer use declined] The user did not allow prigh to control desk's screen. Do not use the computer tool again unless the user asks you to.
    > go
    confirm c2 computer: first: click (10, 20) (Computer_use(host desk))
      -> (Deny("I'm using the screen"))
    confirm done c2
    computer c2 -> ERROR [computer use declined] The user did not allow prigh to control desk's screen. Do not use the computer tool again unless the user asks you to. Their reason: I'm using the screen
    |}]
;;

let%expect_test "unanswered within ask_user's timeout: not allowed" =
  with_agent
    ~config:{|{"ask_user_timeout_seconds": 90}|}
    ~sleep_until:(fun _ -> ())
    ~answer:(fun _ -> None)
    [ click "c1"; Reply.text "ok" ]
  @@ fun _ agent _ ->
  prompt agent "go";
  print_s [%sexp (List.length (Agent.pending_confirms agent) : int)];
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      (unanswered)
    confirm done c1
    notice: computer use on desk was not allowed within 1m 30s: the agent was told not to use it again unless you ask
    computer c1 -> ERROR [computer use not allowed] Nobody allowed prigh to control desk's screen within 1m 30s (the user may be away). Do not use the computer tool again unless the user asks you to.
    0
    |}]
;;

let%expect_test "nobody to ask: refused at once" =
  with_agent [ click "c1"; Reply.text "ok" ]
  @@ fun _ agent fake ->
  Agent.set_unattended agent (fun () ->
    Some "no prigh frontend is connected to this session");
  prompt agent "go";
  List.iter (Computer_fake.log fake) ~f:print_endline;
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    computer c1 -> ERROR [computer use not allowed] no prigh frontend is connected to this session, so nobody can allow prigh to control desk's screen. Do not use the computer tool again unless the user asks you to.
    |}]
;;

let%expect_test "aborting withdraws the question" =
  with_agent
    ~answer:(fun _ -> None)
    ~on_event:(fun ~sw agent -> function
       | Loop (Tool_confirm _) ->
         Eio.Fiber.fork ~sw (fun () -> ignore (Agent.abort agent : string list))
       | _ -> ())
    [ click "c1"; Reply.text "ok" ]
  @@ fun _ agent fake ->
  prompt agent "go";
  print_s [%sexp (List.length (Agent.pending_confirms agent) : int)];
  List.iter (Computer_fake.log fake) ~f:print_endline;
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      (unanswered)
    confirm done c1
    computer c1 -> ERROR [cancelled]
    0
    |}]
;;

let%expect_test
    "a subagent asks through the same dialog; the consent covers the run"
  =
  with_agent
    ~sub_replies:[ click "s1"; click "s2"; Reply.text "clicked" ]
    [ Reply.tool_call
        ~id:"a1"
        ~name:"subagent"
        ~arguments:{|{"task":"click the menu","background":false}|}
        ()
    ; click "c1"
    ; Reply.text "done"
    ]
  @@ fun _ agent _ ->
  prompt agent "go";
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    subagent a1 -> started agent a1 (click the menu); its result will be delivered to you when it finishes: keep working or end your turn meanwhile
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm a1/s1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c1
    control: desk
    confirm done a1/s1
    computer c1 -> click (10, 20) → (20, 40) on the screen
    control: none
      [a1] computer s1 -> click (10, 20) → (20, 40) on the screen
    confirm a1/s2 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done a1/s2
    control: desk
      [a1] computer s2 -> click (10, 20) → (20, 40) on the screen
    control: none
    |}]
;;

let%expect_test "switching to another host ends control: it asks about that one"
  =
  with_agent
    ~on_request:(fun agent n ->
      if n = 2
      then (
        print_endline "(the session moves to lap)";
        ignore
          (Or_error.ok_exn (Agent.set_active_host agent "h2" ~cwd:None)
           : Dir_change.Outcome.t)))
    [ click "c1"; click "c2"; Reply.text "done" ]
  @@ fun _ agent _ ->
  prompt agent "go";
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    confirm c1 computer: first: click (10, 20) (Computer_use(host desk))
      -> Allow
    confirm done c1
    control: desk
    computer c1 -> click (10, 20) → (20, 40) on the screen
    (the session moves to lap)
    notice: tools now run on lap in $DIR
    control: none
    confirm c2 computer: first: click (10, 20) (Computer_use(host lap))
      -> Allow
    confirm done c2
    control: lap
    computer c2 -> click (10, 20) → (20, 40) on the screen
    control: none
    |}]
;;

let%expect_test "a deny rule still refuses without asking" =
  with_agent
    ~config:{|{"permissions": {"deny": ["computer"]}}|}
    [ click "c1"; Reply.text "ok" ]
  @@ fun _ agent _ ->
  prompt agent "go";
  [%expect
    {|
    notice: tools now run on desk in $DIR
    > go
    computer c1 -> ERROR [denied by the rule computer in permissions.deny; do not try to get around it: ask the user]
    |}]
;;

let%expect_test "consent: granted per host, revoked once" =
  let c = Computer_consent.create () in
  let show () =
    print_s
      [%message
        ""
          ~granted:(Computer_consent.granted c : string option)
          ~desk:(Computer_consent.is_granted c ~host:"desk" : bool)
          ~lap:(Computer_consent.is_granted c ~host:"lap" : bool)]
  in
  show ();
  Computer_consent.grant c ~host:"desk";
  show ();
  print_s [%sexp (Computer_consent.revoke c : bool)];
  print_s [%sexp (Computer_consent.revoke c : bool)];
  show ();
  [%expect
    {|
    ((granted ()) (desk false) (lap false))
    ((granted (desk)) (desk true) (lap false))
    true
    false
    ((granted ()) (desk false) (lap false))
    |}]
;;
