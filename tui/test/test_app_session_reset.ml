open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* What belongs to the session shown: the dialog, the confirmations and
   questions waiting behind it, the queue and the transcript. *)
let session_state (h : H.t) =
  let m = h.model in
  print_s
    [%sexp
      { mode = (Mode.name m.mode : string)
      ; editor = (Editor.text m.editor : string)
      ; pending_confirms =
          (List.map m.pending_confirms ~f:(fun c -> c.call_id) : string list)
      ; pending_questions =
          (List.map m.pending_questions ~f:(fun q -> q.call_id) : string list)
      ; queued = (List.map m.queued_messages ~f:Queued_message.id : string list)
      ; agents = (List.length m.agents : int)
      }]
;;

let confirm call_id summary : P.Event.t =
  Tool_confirm { call_id; agent_id = None; name = "bash"; summary; rules = [] }
;;

let question call_id : P.Event.t =
  Ask_user
    { call_id
    ; questions =
        [ { question = "Proceed?"; header = None; choices = []; multiple = false }
        ]
    }
;;

let pending_b =
  {|{"steer_texts":["steer in b"],"follow_up_texts":["later in b"],
     "confirms":[{"call_id":"b1","agent_id":null,"name":"bash","summary":"make in b","rules":[]}],
     "questions":[]}|}
;;

let%expect_test
    "switching sessions drops the old session's confirmations, questions and \
     queue, and fetches the new one's"
  =
  let h = Test_app.connected ~height:16 () in
  H.event h (Queue_update { steer = [ "steer in a" ]; follow_up = [] });
  H.keys h "/sessions";
  H.enter h;
  H.reply h Sessions_picker sessions_json;
  (* They arrive while the picker owns the keyboard, so they wait. *)
  H.event h (confirm "a1" "rm -rf build");
  H.event h (question "qa");
  session_state h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))
    ((mode   picker)
     (editor "")
     (pending_confirms  (a1))
     (pending_questions (qa))
     (queued            (steer:0))
     (agents 0))
    |}];
  H.key h (Key.plain Down);
  H.enter h;
  H.reply h Reload_messages {|{}|};
  session_state h;
  [%expect
    {|
    (Rpc
      (method_ switch_session)
      (params ((path /home/u/.prigh/sessions/2.jsonl)))
      (tag Reload_messages))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    ((mode   editing)
     (editor "")
     (pending_confirms  ())
     (pending_questions ())
     (queued            ())
     (agents 0))
    |}];
  H.reply ~quiet:true h Initial_state (state_json ~cwd:"/other" ());
  H.reply ~quiet:true h Initial_messages {|[{"role":"user","text":"in b"}]|};
  H.reply h Pending pending_b;
  session_state h;
  H.show h;
  [%expect
    {|
    ((mode   confirm)
     (editor "")
     (pending_confirms  ())
     (pending_questions ())
     (queued (steer:0 follow_up:0))
     (agents 0))

      prigh in /other · /help · Esc aborts · Ctrl+C twice quits

    ▌ in b
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Run bash: make in b?                                     │
    │ y  allow once                                            │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    queued (2) · Alt+Up: edit the last · /queue: edit or remove
      steer      steer in b
      follow-up  later in b
    ? ▏
    …deepseek-flash  ctx:0.1%/1.0M  queued:2  confirm: y/n/r
    |}];
  (* The answer goes to the new session's call. *)
  H.keys h "y";
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id b1)
        (allow   true)))
      (tag Show_error))
    |}]
;;

let%expect_test
    "switching user closes the reason being typed, puts the draft back and \
     fetches the new session's pending"
  =
  let h = Test_app.connected () in
  H.keys h "/setusr a";
  H.enter h;
  H.keys h "my draft";
  H.event h (confirm "a1" "make");
  H.keys h "r";
  H.keys h "because";
  session_state h;
  [%expect
    {|
    (Rpc (method_ set_user) (params ((user a))) (tag User_switched))
    ((mode   text_prompt)
     (editor because)
     (pending_confirms  ())
     (pending_questions ())
     (queued            ())
     (agents 0))
    |}];
  H.reply h User_switched {|{"client_id":"c2","namespace":"me","user":"a"}|};
  session_state h;
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    ((mode   editing)
     (editor "my draft")
     (pending_confirms  ())
     (pending_questions ())
     (queued            ())
     (agents 0))
    |}]
;;

let%expect_test
    "reconnecting fetches what is pending: the dialog closed by the lost \
     connection and the queue come back"
  =
  let h = Test_app.connected () in
  H.event h (Queue_update { steer = []; follow_up = [ "afterwards" ] });
  H.event h (question "q1");
  H.event h (confirm "c1" "make");
  session_state h;
  [%expect
    {|
    ((mode   question)
     (editor "")
     (pending_confirms (c1))
     (pending_questions ())
     (queued (follow_up:0))
     (agents 0))
    |}];
  H.step h Backend_closed;
  session_state h;
  [%expect
    {|
    (Reconnect
      (generation 1)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    ((mode   editing)
     (editor "")
     (pending_confirms  ())
     (pending_questions ())
     (queued (follow_up:0))
     (agents 0))
    |}];
  H.reply h (Reconnect 1) {|{"client_id":"client-2"}|};
  session_state h;
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    ((mode   editing)
     (editor "")
     (pending_confirms  ())
     (pending_questions ())
     (queued            ())
     (agents 0))
    |}];
  H.reply
    h
    Pending
    {|{"steer_texts":[],"follow_up_texts":["afterwards"],
       "confirms":[{"call_id":"c1","agent_id":null,"name":"bash","summary":"make","rules":[]}],
       "questions":[{"call_id":"q1","questions":[{"question":"Proceed?"}]}]}|};
  session_state h;
  [%expect
    {|
    ((mode   confirm)
     (editor "")
     (pending_confirms ())
     (pending_questions (q1))
     (queued            (follow_up:0))
     (agents 0))
    |}]
;;

let%expect_test "a reload closes the queue picker, which lists the old queue" =
  let h = Test_app.connected () in
  H.event h (Queue_update { steer = [ "s" ]; follow_up = [] });
  H.keys h "/clone";
  H.enter h;
  H.keys h "/queue";
  H.enter h;
  H.mode h;
  H.reply h (Reload_messages_notice "cloned session") {|{}|};
  session_state h;
  [%expect
    {|
    (Rpc
      (method_ clone)
      (params ())
      (tag (Reload_messages_notice "cloned session")))
    picker
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    ((mode   editing)
     (editor "")
     (pending_confirms  ())
     (pending_questions ())
     (queued            ())
     (agents 0))
    |}]
;;
