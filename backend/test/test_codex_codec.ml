open! Core
open! Prigh
open! Import
module C = Codex_codec

(* Replays of Codex app-server sessions recorded by T3 Code
   (github.com/pingdotgg/t3code, MIT; codex 0.156.1), trimmed of
   notifications prigh ignores and of bulky thread snapshots. Each
   [expect_outbound] request is made again through the codec's builders (the
   ids are the codec's own: Codex's responses are renumbered to match), each
   reply T3 gave to a request of Codex's is given again through the codec,
   and every [emit_inbound] line is handed to [handle_line]. *)

let settings ?(approval = Approval_mode.Auto) ?model () =
  { C.Settings.cwd = "/work"
  ; model
  ; thinking = On None
  ; approval
  ; plan = false
  ; efforts = None
  ; sandbox = true
  }
;;

let str name json = Option.bind (Json.member name json) ~f:Json.string

let approval_of_policy json : Approval_mode.t =
  match Json.member "approvalPolicy" json with
  | Some (`String "untrusted") -> Ask
  | Some (`String "on-request") -> Auto_edit
  | _ -> Auto
;;

let inputs params =
  Option.value_map
    (Option.bind (Json.member "input" params) ~f:Json.list)
    ~default:[]
    ~f:
      (List.filter_map ~f:(fun i ->
         Option.map (str "text" i) ~f:(fun t -> C.Input.Text t)))
;;

let id_of_line line =
  Option.bind
    (Result.ok (Json.parse line))
    ~f:(fun json -> Option.bind (Json.member "id" json) ~f:Json.int)
;;

let method_of_line line =
  Option.bind (Result.ok (Json.parse line)) ~f:(str "method")
;;

(* Coalesces runs of deltas for the same item into one line. *)
module Printer = struct
  type t = { mutable pending : (string * string * Buffer.t) option }

  let create () = { pending = None }

  let flush t =
    Option.iter t.pending ~f:(fun (kind, id, buffer) ->
      print_s [%message kind id ~_:(Buffer.contents buffer : string)]);
    t.pending <- None
  ;;

  let delta t kind id text =
    match t.pending with
    | Some (k, i, buffer) when String.equal k kind && String.equal i id ->
      Buffer.add_string buffer text
    | _ ->
      flush t;
      let buffer = Buffer.create 64 in
      Buffer.add_string buffer text;
      t.pending <- Some (kind, id, buffer)
  ;;

  let output ?(full_sends = false) t (output : C.Output.t) =
    match output with
    | Event (Text_delta { item; text }) -> delta t "Text_delta" item text
    | Event (Thinking_delta { item; text }) ->
      delta t "Thinking_delta" item text
    | Event (Tool_update { id; output }) -> delta t "Tool_update" id output
    | Send line ->
      flush t;
      (match method_of_line line with
       | Some m when not full_sends -> print_endline ("-> " ^ m)
       | _ -> print_endline ("-> " ^ line))
    | Event event ->
      flush t;
      print_s [%sexp (event : Adapter_event.t)]
    | Response response ->
      flush t;
      print_s [%message "response" (response : C.Response.t)]
    | Tool_call call ->
      flush t;
      print_s [%message "tool call" (call : C.Tool_call.t)]
    | Token_refresh { request_id; previous_account_id } ->
      flush t;
      print_s
        [%message
          "token refresh"
            (request_id : string)
            (previous_account_id : string option)]
  ;;
end

let replay name =
  let lines = In_channel.read_lines [%string "fixtures/codex/%{name}.ndjson"] in
  let printer = Printer.create () in
  let codec = ref (C.create ()) in
  (* Codex's ids for our requests, to ours; None for requests we don't make. *)
  let ids = Int.Table.create () in
  let our_line transcript_id line =
    Option.iter (id_of_line line) ~f:(fun ours ->
      Hashtbl.set ids ~key:transcript_id ~data:(Some ours));
    Printer.output printer (Send line)
  in
  let skip transcript_id = Hashtbl.set ids ~key:transcript_id ~data:None in
  let handle line =
    List.iter (C.handle_line !codec line) ~f:(Printer.output printer)
  in
  List.iter lines ~f:(fun line ->
    let record = Json.parse line |> Result.ok |> Option.value_exn in
    let frame = Option.value (Json.member "frame" record) ~default:`Null in
    let params =
      Option.value (Json.member "params" frame) ~default:(`Object [])
    in
    let transcript_id = Option.bind (Json.member "id" frame) ~f:Json.int in
    match str "type" record with
    | Some "runtime_exit" ->
      Printer.flush printer;
      print_endline "--- Codex exits; a new process ---";
      codec := C.create ();
      Hashtbl.clear ids
    | Some "expect_outbound" ->
      let tid = Option.value transcript_id ~default:(-1) in
      let settings () =
        settings
          ~approval:(approval_of_policy params)
          ?model:(str "model" params)
          ()
      in
      (match str "method" frame, Json.member "result" frame with
       | Some "initialize", _ ->
         our_line
           tid
           (C.initialize !codec ~client_name:"prigh" ~client_version:"test")
       | Some "initialized", _ -> ()
       | Some "thread/start", _ ->
         our_line
           tid
           (C.start_thread !codec (settings ()) C.Thread_options.empty)
       | Some "thread/resume", _ ->
         our_line
           tid
           (C.resume_thread
              !codec
              ~thread_id:(Option.value_exn (str "threadId" params))
              (settings ())
              C.Thread_options.empty)
       | Some "turn/start", _ ->
         our_line
           tid
           (Or_error.ok_exn (C.start_turn !codec (settings ()) (inputs params)))
       | Some "turn/steer", _ ->
         our_line tid (Or_error.ok_exn (C.steer !codec (inputs params)))
       | Some "turn/interrupt", _ ->
         (match C.interrupt !codec with
          | `Send line -> our_line tid line
          | `Deferred -> print_endline "(interrupt deferred)"
          | `Idle -> print_endline "(interrupt: idle)")
       | Some "thread/revert", _ ->
         our_line
           tid
           (Or_error.ok_exn
              (C.revert
                 !codec
                 ~before_turn_id:(Option.value_exn (str "beforeTurnId" params))))
       | Some _, _ -> skip tid
       | None, Some result ->
         let request_id = Int.to_string tid in
         let reply =
           match str "decision" result, Json.member "answers" result with
           | Some decision, _ ->
             C.reply_approval
               !codec
               ~request_id
               (match decision with
                | "accept" -> Allow
                | "acceptForSession" -> Allow_always
                | "decline" -> Deny None
                | _ -> Cancel)
           | None, Some (`Object answers) ->
             C.answer_question
               !codec
               ~request_id
               (Answered
                  (List.map answers ~f:(fun (_, a) ->
                     Option.bind (Json.member "answers" a) ~f:Json.list
                     |> Option.value ~default:[]
                     |> List.filter_map ~f:Json.string)))
           | None, _ -> None
         in
         (match reply with
          | Some line -> Printer.output ~full_sends:true printer (Send line)
          | None -> print_endline "(no request to reply to)")
       | None, None -> ())
    | Some "emit_inbound" ->
      (match transcript_id, str "method" frame with
       | Some tid, None ->
         (match Hashtbl.find ids tid with
          | Some (Some ours) ->
            let frame =
              match frame with
              | `Object fields ->
                `Object
                  (List.map fields ~f:(fun (k, v) ->
                     if String.equal k "id"
                     then k, `Number (Int.to_string ours)
                     else k, v))
              | json -> json
            in
            handle (Json.to_string frame)
          | Some None -> ()
          | None ->
            print_s [%message "response to an unknown request" (tid : int)])
       | _ -> handle (Json.to_string frame))
    | _ -> ());
  Printer.flush printer
;;

let%expect_test "simple: one thread, one turn" =
  replay "simple";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5eb-f0c0-7481-a417-5e8871805cbc)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5eb-f0c0-7481-a417-5e8871805cbc) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5eb-f176-71d0-8e1c-a459700443de))))
    (Turn_started (turn_id 01a0d5eb-f176-71d0-8e1c-a459700443de))
    (Text_delta msg_08fda0295ef7a774016ab5bd70c40887d080d27158f71dce50
     "fixture simple ok")
    (Text_done (item msg_08fda0295ef7a774016ab5bd70c40887d080d27158f71dce50)
     (text "fixture simple ok"))
    (Usage
     ((usage ((input 22730) (output 7) (cache_read 6912))) (cost_usd ())
      (context_tokens (22737)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5eb-f176-71d0-8e1c-a459700443de)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "command output streams, then the command ends" =
  replay "command_output_streaming";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "T3 Code/0.157.0 (Mac OS 27.0.0; arm64) unknown (T3 Code; 0.0.45)")
       (version (0.157.0)))))
    -> thread/start
    (Thread_started (thread_id 01a10004-0a27-7421-852d-0f7f99ace2d6)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a10004-0a27-7421-852d-0f7f99ace2d6) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a10004-0b1b-78f0-a41c-b253bded078e))))
    (Turn_started (turn_id 01a10004-0b1b-78f0-a41c-b253bded078e))
    (Text_delta msg_0038d12d01b50ff0016ac08420563087d2af7fb28a3cee1956
     "I\226\128\153ll run the command in the foreground and wait for it to finish.")
    (Text_done (item msg_0038d12d01b50ff0016ac08420563087d2af7fb28a3cee1956)
     (text
      "I\226\128\153ll run the command in the foreground and wait for it to finish."))
    (Tool_start (id exec-1a2dcd5e-1591-48a8-bb8e-47cb40883d2c) (kind Command)
     (native_name commandExecution)
     (title
      "for i in 1 2 3 4; do printf '\\033[32mstream line %s\\033[0m\\n' $i; sleep 1; done; printf 'progress 50%%\\rprogress 100%%\\n'")
     (input
      (Object
       ((command
         (String
          "for i in 1 2 3 4; do printf '\\033[32mstream line %s\\033[0m\\n' $i; sleep 1; done; printf 'progress 50%%\\rprogress 100%%\\n'"))
        (cwd (String /home/replay-user/t3code/apps/server)))))
     (parent ()))
    (Tool_update exec-1a2dcd5e-1591-48a8-bb8e-47cb40883d2c
      "\027[32mstream line 2\027[0m\
     \n\027[32mstream line 3\027[0m\
     \n\027[32mstream line 4\027[0m\
     \nprogress 50%\rprogress 100%\
     \n")
    (Tool_end (id exec-1a2dcd5e-1591-48a8-bb8e-47cb40883d2c) (ok true)
     (output
       "\027[32mstream line 2\027[0m\
      \n\027[32mstream line 3\027[0m\
      \n\027[32mstream line 4\027[0m\
      \nprogress 50%\rprogress 100%\
      \n")
     (changes ()) (images ()))
    (Usage
     ((usage ((input 17653) (output 142) (cache_read 3840))) (cost_usd ())
      (context_tokens (17795)) (context_window (258400))))
    (Text_delta msg_0038d12d01b50ff0016ac0842a975c87d2bf566b8ae1f7e5f3
     "command output fixture complete")
    (Text_done (item msg_0038d12d01b50ff0016ac0842a975c87d2bf566b8ae1f7e5f3)
     (text "command output fixture complete"))
    (Usage
     ((usage ((input 17879) (output 8) (cache_read 17152))) (cost_usd ())
      (context_tokens (17887)) (context_window (258400))))
    (Turn_end (turn_id (01a10004-0b1b-78f0-a41c-b253bded078e)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "a command approval, accepted" =
  replay "tool_call_read_only_on_request";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d604-1f8d-7522-8fe1-371c6626896e)
     (model (gpt-6-sol)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d604-1f8d-7522-8fe1-371c6626896e) (model (gpt-6-sol)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d604-2095-7fb0-a7e6-3f63d1949bbe))))
    (Turn_started (turn_id 01a0d604-2095-7fb0-a7e6-3f63d1949bbe))
    (Thinking_delta rs_0cdfbd98c0a84c4d016ab5c3a277d087d0b61ce58052cecbf6
      "**Waiting for write access**\
     \n\
     \n**Attempting the file edit**")
    (Thinking_done (item rs_0cdfbd98c0a84c4d016ab5c3a277d087d0b61ce58052cecbf6)
     (text  "**Waiting for write access**\
           \n\
           \n**Attempting the file edit**"))
    (Text_delta msg_0cdfbd98c0a84c4d016ab5c3a991d487d09ee1173b054eb34d
     "I\226\128\153ll write the exact text to the file with a local shell command.")
    (Text_done (item msg_0cdfbd98c0a84c4d016ab5c3a991d487d09ee1173b054eb34d)
     (text
      "I\226\128\153ll write the exact text to the file with a local shell command."))
    (Tool_start (id exec-02c3fc6e-7a54-4c15-afda-bc59a03b138a) (kind Command)
     (native_name commandExecution)
     (title
      "printf '%s' 'codex app-server approval fixture' > .codex-probe-write-action.txt")
     (input
      (Object
       ((command
         (String
          "printf '%s' 'codex app-server approval fixture' > .codex-probe-write-action.txt"))
        (cwd (String /home/replay-user/t3code/packages/effect-codex-app-server)))))
     (parent ()))
    (Approval_request (request_id 0)
     (tool_id (exec-02c3fc6e-7a54-4c15-afda-bc59a03b138a)) (kind Command)
     (summary
      "printf '%s' 'codex app-server approval fixture' > .codex-probe-write-action.txt (May I create or overwrite .codex-probe-write-action.txt with the exact text you requested?)")
     (input
      (Object
       ((command
         (String
          "printf '%s' 'codex app-server approval fixture' > .codex-probe-write-action.txt"))
        (cwd (String /home/replay-user/t3code/packages/effect-codex-app-server))
        (reason
         (String
          "May I create or overwrite .codex-probe-write-action.txt with the exact text you requested?")))))
     (offers_always true))
    -> {"id":0,"result":{"decision":"accept"}}
    (Tool_end (id exec-02c3fc6e-7a54-4c15-afda-bc59a03b138a) (ok true)
     (output "") (changes ()) (images ()))
    (Usage
     ((usage ((input 22438) (output 288) (cache_read 22272))) (cost_usd ())
      (context_tokens (22726)) (context_window (258400))))
    (Text_delta msg_0cdfbd98c0a84c4d016ab5c3ac835087d08df330990ac01d5d
     "Created or overwrote `.codex-probe-write-action.txt` with exactly `codex app-server approval fixture`.")
    (Text_done (item msg_0cdfbd98c0a84c4d016ab5c3ac835087d08df330990ac01d5d)
     (text
      "Created or overwrote `.codex-probe-write-action.txt` with exactly `codex app-server approval fixture`."))
    (Usage
     ((usage ((input 22783) (output 27) (cache_read 22272))) (cost_usd ())
      (context_tokens (22810)) (context_window (258400))))
    (Turn_end (turn_id (01a0d604-2095-7fb0-a7e6-3f63d1949bbe)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "a file change approval under granular approval" =
  replay "tool_call_restricted_granular";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d604-629d-7162-b667-18c96f6fdf56)
     (model (gpt-5.6-terra)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d604-629d-7162-b667-18c96f6fdf56) (model (gpt-5.6-terra)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d604-6358-72f1-bb89-7361cce0045c))))
    (Turn_started (turn_id 01a0d604-6358-72f1-bb89-7361cce0045c))
    (Thinking_delta rs_08bfce1a588b6283016ab5c3b2f48487d08c265fbb08c31486
     "**Planning patch application despite sandbox restrictions**")
    (Thinking_done (item rs_08bfce1a588b6283016ab5c3b2f48487d08c265fbb08c31486)
     (text "**Planning patch application despite sandbox restrictions**"))
    (Text_delta msg_08bfce1a588b6283016ab5c3b41e5c87d0badb0d609ab7ddf4
     "I\226\128\153ll create the requested fixture file locally with that exact single-line content.")
    (Text_done (item msg_08bfce1a588b6283016ab5c3b41e5c87d0badb0d609ab7ddf4)
     (text
      "I\226\128\153ll create the requested fixture file locally with that exact single-line content."))
    (Tool_start (id exec-114e8eef-018a-444d-8542-aa0232ff1f38) (kind File_change)
     (native_name fileChange)
     (title
      "Add /home/replay-user/t3code/packages/effect-codex-app-server/.codex-probe-write-action.txt")
     (input
      (Object
       ((changes
         (Array
          ((Object
            ((path
              (String
               /home/replay-user/t3code/packages/effect-codex-app-server/.codex-probe-write-action.txt))
             (kind (Object ((type (String add)))))))))))))
     (parent ()))
    (Approval_request (request_id 0)
     (tool_id (exec-114e8eef-018a-444d-8542-aa0232ff1f38)) (kind File_change)
     (summary
      "Add /home/replay-user/t3code/packages/effect-codex-app-server/.codex-probe-write-action.txt")
     (input
      (Object
       ((changes
         (Array
          ((Object
            ((path
              (String
               /home/replay-user/t3code/packages/effect-codex-app-server/.codex-probe-write-action.txt))
             (kind (Object ((type (String add)))))
             (diff (String "codex app-server approval fixture.\n"))))))))))
     (offers_always true))
    -> {"id":0,"result":{"decision":"accept"}}
    (Tool_end (id exec-114e8eef-018a-444d-8542-aa0232ff1f38) (ok true)
     (output "")
     (changes
      (((path
         /home/replay-user/t3code/packages/effect-codex-app-server/.codex-probe-write-action.txt)
        (kind Add)
        (diff
          "--- /dev/null\
         \n+++ /home/replay-user/t3code/packages/effect-codex-app-server/.codex-probe-write-action.txt\
         \n@@ -0,0 +1,1 @@\
         \n+codex app-server approval fixture.\
         \n"))))
     (images ()))
    (Usage
     ((usage ((input 21328) (output 124) (cache_read 6912))) (cost_usd ())
      (context_tokens (21452)) (context_window (258400))))
    (Text_delta msg_08bfce1a588b6283016ab5c3b7ef0c87d096aba0299c914020
      "Created `.codex-probe-write-action.txt` with exactly:\
     \n\
     \n```text\
     \ncodex app-server approval fixture.\
     \n```")
    (Text_done (item msg_08bfce1a588b6283016ab5c3b7ef0c87d096aba0299c914020)
     (text
       "Created `.codex-probe-write-action.txt` with exactly:\
      \n\
      \n```text\
      \ncodex app-server approval fixture.\
      \n```"))
    (Usage
     ((usage ((input 21479) (output 28) (cache_read 20224))) (cost_usd ())
      (context_tokens (21507)) (context_window (258400))))
    (Turn_end (turn_id (01a0d604-6358-72f1-bb89-7361cce0045c)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "request_user_input, answered" =
  replay "plan_questions";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5ef-3a6a-75d0-9942-61b29ab82328)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5ef-3a6a-75d0-9942-61b29ab82328) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ef-3b5a-7bb2-9cd2-da112b6b158f))))
    (Turn_started (turn_id 01a0d5ef-3b5a-7bb2-9cd2-da112b6b158f))
    (User_question (request_id 0)
     (questions
      (((question "Should this fixture prefer strict schemas or UI flexibility?")
        (header (Fixture))
        (choices
         (((label "Strict schemas (Recommended)")
           (description
            ("Keep fixture data tightly validated, even if UI variations are harder to represent.")))
          ((label "UI flexibility")
           (description
            ("Allow fixture shapes that better support varied UI states, with looser validation.")))))
        (multiple false)))))
    -> {"id":0,"result":{"answers":{"schema_preference":{"answers":["Strict schemas (Recommended)"]}}}}
    (Usage
     ((usage ((input 21320) (output 108) (cache_read 6912))) (cost_usd ())
      (context_tokens (21428)) (context_window (258400))))
    (Text_delta msg_028f9564f7cc52f0016ab5be4c609887d09ffdd45f53ffdc49
     "plan questions fixture complete")
    (Text_done (item msg_028f9564f7cc52f0016ab5be4c609887d09ffdd45f53ffdc49)
     (text "plan questions fixture complete"))
    (Usage
     ((usage ((input 21457) (output 8) (cache_read 20224))) (cost_usd ())
      (context_tokens (21465)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ef-3b5a-7bb2-9cd2-da112b6b158f)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "steering the running turn" =
  replay "message_steering";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5ed-65ea-7d42-b8e9-d06f045cfa86)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5ed-65ea-7d42-b8e9-d06f045cfa86) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ed-670e-7032-9a87-707c736491a6))))
    (Turn_started (turn_id 01a0d5ed-670e-7032-9a87-707c736491a6))
    -> turn/steer
    (response
     (response (Steered (turn_id 01a0d5ed-670e-7032-9a87-707c736491a6))))
    (Text_delta msg_082e3c417c674aca016ab5bdd0a78c87d0bfa0eb88fa813e1c
     "steering fixture initial response")
    (Text_done (item msg_082e3c417c674aca016ab5bdd0a78c87d0bfa0eb88fa813e1c)
     (text "steering fixture initial response"))
    (Usage
     ((usage ((input 21418) (output 9) (cache_read 6912))) (cost_usd ())
      (context_tokens (21427)) (context_window (258400))))
    (Text_delta msg_082e3c417c674aca016ab5bdd2530487d08314cbb7d690f5d2
     "steering fixture observed")
    (Text_done (item msg_082e3c417c674aca016ab5bdd2530487d08314cbb7d690f5d2)
     (text "steering fixture observed"))
    (Usage
     ((usage ((input 21442) (output 8) (cache_read 21248))) (cost_usd ())
      (context_tokens (21450)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ed-670e-7032-9a87-707c736491a6)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "interrupting a turn" =
  replay "turn_interrupt";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5ed-3804-72d0-9a3f-929885a11f32)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5ed-3804-72d0-9a3f-929885a11f32) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ed-38b3-7a11-9203-bd9650a02764))))
    (Turn_started (turn_id 01a0d5ed-38b3-7a11-9203-bd9650a02764))
    -> turn/interrupt
    (response (response (Done Turn_interrupt)))
    (Turn_end (turn_id (01a0d5ed-38b3-7a11-9203-bd9650a02764))
     (stop Interrupted))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "interrupting a turn mid command" =
  replay "turn_interrupt_mid_tool";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d60e-f7cc-7a50-bdff-90174e2c3b56)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d60e-f7cc-7a50-bdff-90174e2c3b56) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d60e-f90f-7f23-b6ba-9ca7ee20b5cd))))
    (Turn_started (turn_id 01a0d60e-f90f-7f23-b6ba-9ca7ee20b5cd))
    (Thinking_delta rs_02867d76e88a025b016ab5c669424c87d09ca784eb308b0650
     "**Running exact command**")
    (Thinking_done (item rs_02867d76e88a025b016ab5c669424c87d09ca784eb308b0650)
     (text "**Running exact command**"))
    (Tool_start (id exec-0ce96e49-aa1c-4bae-b4f8-687a7473017c) (kind Command)
     (native_name commandExecution)
     (title
      "node -e \"console.log('interrupt fixture tool started'); setTimeout(() => {}, 30000)\"")
     (input
      (Object
       ((command
         (String
          "node -e \"console.log('interrupt fixture tool started'); setTimeout(() => {}, 30000)\""))
        (cwd (String /home/replay-user/t3code/packages/effect-codex-app-server)))))
     (parent ()))
    -> turn/interrupt
    (Usage
     ((usage ((input 17855) (output 98) (cache_read 11008))) (cost_usd ())
      (context_tokens (17953)) (context_window (258400))))
    (response (response (Done Turn_interrupt)))
    (Turn_end (turn_id (01a0d60e-f90f-7f23-b6ba-9ca7ee20b5cd))
     (stop Interrupted))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "a second turn after the first" =
  replay "queued_turn";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5ed-3b1d-7b03-a1a3-075d3fa4bc67)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5ed-3b1d-7b03-a1a3-075d3fa4bc67) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ed-3c38-77a3-ab7d-7b1af28c714e))))
    (Turn_started (turn_id 01a0d5ed-3c38-77a3-ab7d-7b1af28c714e))
    (Text_delta msg_0e736caa8e522655016ab5bdc6199487d097782bddce2cbb65
     "first fixture turn complete")
    (Text_done (item msg_0e736caa8e522655016ab5bdc6199487d097782bddce2cbb65)
     (text "first fixture turn complete"))
    (Usage
     ((usage ((input 21418) (output 8) (cache_read 11008))) (cost_usd ())
      (context_tokens (21426)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ed-3c38-77a3-ab7d-7b1af28c714e)) (stop Done))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ed-4f65-7182-8985-cb9fba86acad))))
    (Turn_started (turn_id 01a0d5ed-4f65-7182-8985-cb9fba86acad))
    (Text_delta msg_0e736caa8e522655016ab5bdc815b887d0aad2681e25df89ff
     "second fixture turn complete")
    (Text_done (item msg_0e736caa8e522655016ab5bdc815b887d0aad2681e25df89ff)
     (text "second fixture turn complete"))
    (Usage
     ((usage ((input 25777) (output 8) (cache_read 21248))) (cost_usd ())
      (context_tokens (25785)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ed-4f65-7182-8985-cb9fba86acad)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "update_plan" =
  replay "todo_list";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d691-5ba5-77b1-bf0d-1bf7be8e9abe)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d691-5ba5-77b1-bf0d-1bf7be8e9abe) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d691-5c9c-7873-a191-5a599fd58099))))
    (Turn_started (turn_id 01a0d691-5c9c-7873-a191-5a599fd58099))
    (Plan
     ((explanation ())
      (steps
       (((step "Inspect package.json") (status In_progress))
        ((step "Inspect tsconfig.json") (status Pending))
        ((step "Report completion") (status Pending))))))
    (Tool_start (id exec-251a740a-c3ff-48c5-9be0-feffdbad806c) (kind Command)
     (native_name commandExecution)
     (title "/bin/bash -lc 'cat package.json && cat tsconfig.json'")
     (input
      (Object
       ((command
         (String "/bin/bash -lc 'cat package.json && cat tsconfig.json'"))
        (cwd (String /home/replay-user/t3code/packages/effect-codex-app-server)))))
     (parent ()))
    (Tool_end (id exec-251a740a-c3ff-48c5-9be0-feffdbad806c) (ok true)
     (output
       "{\
      \n  \"name\": \"effect-codex-app-server\",\
      \n  \"private\": true,\
      \n  \"type\": \"module\",\
      \n  \"exports\": {\
      \n    \"./client\": {\
      \n      \"types\": \"./src/client.ts\",\
      \n      \"import\": \"./src/client.ts\"\
      \n    },\
      \n    \"./schema\": {\
      \n      \"types\": \"./src/schema.ts\",\
      \n      \"import\": \"./src/schema.ts\"\
      \n    },\
      \n    \"./rpc\": {\
      \n      \"types\": \"./src/rpc.ts\",\
      \n      \"import\": \"./src/rpc.ts\"\
      \n    },\
      \n    \"./protocol\": {\
      \n      \"types\": \"./src/protocol.ts\",\
      \n      \"import\": \"./src/protocol.ts\"\
      \n    },\
      \n    \"./replay\": {\
      \n      \"types\": \"./src/replay.ts\",\
      \n      \"import\": \"./src/replay.ts\"\
      \n    },\
      \n    \"./errors\": {\
      \n      \"types\": \"./src/errors.ts\",\
      \n      \"import\": \"./src/errors.ts\"\
      \n    }\
      \n  },\
      \n  \"scripts\": {\
      \n    \"typecheck\": \"tsc --noEmit\",\
      \n    \"test\": \"vp test run\",\
      \n    \"generate\": \"node scripts/generate.ts\",\
      \n    \"probe\": \"node test/examples/codex-app-server-probe.ts\"\
      \n  },\
      \n  \"dependencies\": {\
      \n    \"effect\": \"catalog:\"\
      \n  },\
      \n  \"devDependencies\": {\
      \n    \"@effect/openapi-generator\": \"catalog:\",\
      \n    \"@effect/platform-node\": \"catalog:\",\
      \n    \"@effect/vitest\": \"catalog:\",\
      \n    \"@types/node\": \"catalog:\",\
      \n    \"vite-plus\": \"catalog:\"\
      \n  }\
      \n}\
      \n{\
      \n  \"extends\": \"../../tsconfig.base.json\",\
      \n  \"compilerOptions\": {\
      \n    \"types\": [\"node\"]\
      \n  },\
      \n  \"include\": [\"src\", \"scripts\", \"test\"]\
      \n}\
      \n")
     (changes ()) (images ()))
    (Usage
     ((usage ((input 22888) (output 92) (cache_read 1792))) (cost_usd ())
      (context_tokens (22980)) (context_window (258400))))
    (Plan
     ((explanation ())
      (steps
       (((step "Inspect package.json") (status Completed))
        ((step "Inspect tsconfig.json") (status Completed))
        ((step "Report completion") (status Completed))))))
    (Usage
     ((usage ((input 23356) (output 61) (cache_read 22272))) (cost_usd ())
      (context_tokens (23417)) (context_window (258400))))
    (Text_delta msg_05ada939d0eb95f2016ab5e7d040ac87d0b3189e7502ca7c6a
     "todo list fixture complete")
    (Text_done (item msg_05ada939d0eb95f2016ab5e7d040ac87d0b3189e7502ca7c6a)
     (text "todo list fixture complete"))
    (Usage
     ((usage ((input 23441) (output 8) (cache_read 22272))) (cost_usd ())
      (context_tokens (23449)) (context_window (258400))))
    (Turn_end (turn_id (01a0d691-5c9c-7873-a191-5a599fd58099)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "web search" =
  replay "web_search";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5ee-87e4-7461-9ab7-5c316b503107)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5ee-87e4-7461-9ab7-5c316b503107) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ee-8888-7d30-8f2b-68fe28f521bc))))
    (Turn_started (turn_id 01a0d5ee-8888-7d30-8f2b-68fe28f521bc))
    (Text_delta msg_03bceefdcc0be4af016ab5be1abc7887d09187d9bc35241854
     "I\226\128\153ll check current ticket pricing sources on the web, then return the exact requested phrase.")
    (Text_done (item msg_03bceefdcc0be4af016ab5be1abc7887d09187d9bc35241854)
     (text
      "I\226\128\153ll check current ticket pricing sources on the web, then return the exact requested phrase."))
    (Tool_start (id exec-f0a0c7db-02b9-449e-9981-9d588f7509ad) (kind Web_search)
     (native_name webSearch) (title "FIFA World Cup 2026 ticket prices official")
     (input
      (Object ((query (String "FIFA World Cup 2026 ticket prices official")))))
     (parent ()))
    (Tool_end (id exec-f0a0c7db-02b9-449e-9981-9d588f7509ad) (ok true)
     (output
       "10. How does FIFA determine ticket prices for the FIFA World Cup 2026\226\132\162? \226\128\147 FIFA World Cup 2026\226\132\162\
      \nhttps://gpcustomersupportfwc2026.tickets.fifa.com/hc/en-gb/articles/30201776314397-10-How-does-FIFA-determine-ticket-prices-for-the-FIFA-World-Cup-2026\
      \n\
      \n10. How does FIFA determine ticket prices for the FIFA World Cup 2026\226\132\162? \226\128\147 FIFA World Cup 2026\226\132\162\
      \nhttps://gpcustomersupportfwc2026.tickets.fifa.com/hc/en-gb/articles/30201776314397-9-How-does-FIFA-determine-ticket-prices-for-the-FIFA-World-Cup-26\
      \n\
      \nNew ticket pricing tier introduced for fans of qualified teams at FIFA World Cup 2026\226\132\162\
      \nhttps://tickets.fifa.com/organisation/news/world-cup-2026-new-ticket-pricing-tier-fans-qualified-teams\
      \n\
      \n1. What are the FIFA World Cup 2026\226\132\162 ticket products available to the general public? \226\128\147 FIFA World Cup 2026\226\132\162\
      \nhttps://gpcustomersupportfwc2026.tickets.fifa.com/hc/en-gb/articles/30067198563869-1-What-are-the-FIFA-World-Cup-2026-ticket-products-available-to-the-general-public\
      \n\
      \nTICKETS \227\131\129\227\130\177\227\131\131\227\131\136\230\131\133\229\160\177\239\189\156FIFA World Cup 2026 TM\239\189\156SAMURAI BLUE\239\189\156JFA\239\189\156\229\133\172\231\155\138\232\178\161\229\155\163\230\179\149\228\186\186\230\151\165\230\156\172\227\130\181\227\131\131\227\130\171\227\131\188\229\141\148\228\188\154\
      \nhttps://www.jfa.jp/samuraiblue/worldcup_2026/canadamexicousa2026/tickets/\
      \n\
      \nPayment and Ticket Prices \226\128\147 FIFA World Cup 2026\226\132\162 Play-Off Tournament\
      \nhttps://pot2026customersupport.tickets.fifa.com/hc/en-gb/sections/34174147686429-Payment-and-Ticket-Prices\
      \n\
      \nRight to Tickets - FIFA Collect\
      \nhttps://collect.fifa.com/right-to-ticket?match=0\
      \n\
      \nFIFA World Cup 2026\226\132\162 ticket demand breaks all records\
      \nhttps://tickets.fifa.com/organisation/media-releases/world-cup-2026-ticket-demand-breaks-all-records\
      \n\
      \nFIFA WORLD CUP 2026\226\132\162\
      \nhttps://www.ftf.pf/cms/wp-content/uploads/2026/01/FWC2026-Advanced-Request-GS-Pricing-USD.pdf\
      \n\
      \nOfficial FIFA World Cup 26 Tickets & Hospitality | On Location\
      \nhttps://https-www-fifa.com/tickets_shop\
      \n\
      \nOne week to go before first phase of FIFA World Cup\226\132\162 ticket sales open\
      \nhttps://tickets.fifa.com/organisation/media-releases/one-week-to-go-first-phase-world-cup-ticket-sales-open\
      \n\
      \nRandom Selection Draw Ticketing Phase Now Open for FIFA World Cup 2026\226\132\162\
      \nhttps://tickets.fifa.com/organisation/news/random-selection-draw-ticketing-phase-now-open-for-fifa-world-cup-2026-tm\
      \n\
      \nWaiting Room\
      \nhttps://access.tickets.fifa.com/pkpcontroller/wp/FWC26SHOP/index_en.html?queue=11-FWC26-shop\
      \n\
      \nMicrosoft Word - 2025.05.07 - Pou+Pallone FIFA Ticket Pricing Letter\
      \nhttps://pou.house.gov/sites/evo-subsites/pou.house.gov/files/evo-media-document/pou-pallone-fifa-ticket-pricing-letter-5-7-2026.pdf\
      \n\
      \n03.11.26 Letter to FIFA re World Cup Ticket Pricing\
      \nhttps://mcgarvey.house.gov/imo/media/doc/031126lettertofifareworldcupticketpricing.pdf\
      \n\
      \nTicketing FAQ\
      \nhttps://arsa-fwc26.shop.secutix.com/custom/images/documents/Aramco_FWC26_TICKETING_FAQ.pdf\
      \n\
      \nFifa ordered to explain World Cup ticket pricing | BBC News - YouTube\
      \nhttps://www.youtube.com/watch?v=lUDFmed5Nvk\
      \n\
      \nFIFA unveils $60 ticket tier amid backlash over 2026 World Cup prices | REUTERS - YouTube\
      \nhttps://www.youtube.com/watch?v=ypzT8vVoLnU\
      \n\
      \nFAQS2\
      \nhttps://gsu.qualtrics.com/jfe/file/v2/aa0c498fa3ea89a54686510524a3f61b823ed48c003f03c23cc55f1357f5bfad/0103ac0c9c18050a29b1779762e79a2942e366f1ae40586e2499690902baef29/F_32W7ISlYns9TuCt?staged=1\
      \n\
      \n2026 FIFA World Cup\
      \nhttps://en.wikipedia.org/wiki/2026_FIFA_World_Cup\
      \n\
      \nFIFA's Infantino defends World Cup ticket prices, says fans 'should chill' about ref denied US entry\
      \nhttps://apnews.com/article/7725f0e7df91eeefcbf598bdd9e72f94\
      \n\
      \nFIFA raises top ticket price for World Cup final to $10,990 during glitch-hampered sales reopening\
      \nhttps://apnews.com/article/e4bb8a9eb9aa285f55caa4b9405fb182\
      \n\
      \nThe biggest takeaways from the 2026 World Cup\
      \nhttps://apnews.com/article/e45fd30932bb9885812805cafa99ca33\
      \n\
      \nMexican president says FIFA should reflect on high World Cup ticket prices\
      \nhttps://apnews.com/article/74b3d0d08b248d838c846f522ea89807\
      \n\
      \nThe priciest World Cup games to get into\
      \nhttps://www.axios.com/2026/07/17/cost-world-cup-tickets-get-in\
      \n\
      \nWorld Cup 2026 Ticket Price Tracker\
      \nhttps://www.reddit.com/r/FIFACollect/comments/1pdym8u/world_cup_2026_ticket_price_tracker/\
      \n\
      \nFu\195\159ball-Weltmeisterschaft 2026\
      \nhttps://de.wikipedia.org/wiki/Fu%C3%9Fball-Weltmeisterschaft_2026")
     (changes ()) (images ()))
    (Usage
     ((usage ((input 21428) (output 71) (cache_read 21248))) (cost_usd ())
      (context_tokens (21499)) (context_window (258400))))
    (Text_delta msg_03bceefdcc0be4af016ab5be1f494887d080c69010f9873eec
     "web search fixture complete")
    (Text_done (item msg_03bceefdcc0be4af016ab5be1f494887d080c69010f9873eec)
     (text "web search fixture complete"))
    (Usage
     ((usage ((input 25420) (output 8) (cache_read 21248))) (cost_usd ())
      (context_tokens (25428)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ee-8888-7d30-8f2b-68fe28f521bc)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "a subagent's calls are shown under its spawn" =
  replay "subagent_v2";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5ff-49e8-7741-9ca7-b12aa375019a)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5ff-49e8-7741-9ca7-b12aa375019a) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ff-4b00-7641-bba1-06faa0ef23f1))))
    (Turn_started (turn_id 01a0d5ff-4b00-7641-bba1-06faa0ef23f1))
    (Tool_start (id call_opQacJbIfXWqOZb5gT9hd9Mv) (kind Subagent)
     (native_name subAgentActivity) (title /root/say_hello)
     (input (Object ((agentPath (String /root/say_hello))))) (parent ()))
    (Usage
     ((usage ((input 22750) (output 41) (cache_read 11008))) (cost_usd ())
      (context_tokens (22791)) (context_window (258400))))
    (Tool_start (id call_90ICrpbuqsWj4Q3wJB3q10HO) (kind (Other wait))
     (native_name collabAgentToolCall) (title wait)
     (input (Object ((tool (String wait))))) (parent ()))
    (Tool_update call_opQacJbIfXWqOZb5gT9hd9Mv "Hello.\n")
    (Tool_end (id call_opQacJbIfXWqOZb5gT9hd9Mv) (ok true) (output Hello.)
     (changes ()) (images ()))
    (Tool_end (id call_90ICrpbuqsWj4Q3wJB3q10HO) (ok true) (output "")
     (changes ()) (images ()))
    (Usage
     ((usage ((input 22814) (output 22) (cache_read 22272))) (cost_usd ())
      (context_tokens (22836)) (context_window (258400))))
    (Text_delta msg_034802ac4db0e789016ab5c26b3d8c87d085ee92cd13fc7288 Hello.)
    (Text_done (item msg_034802ac4db0e789016ab5c26b3d8c87d085ee92cd13fc7288)
     (text Hello.))
    (Usage
     ((usage ((input 22901) (output 6) (cache_read 22272))) (cost_usd ())
      (context_tokens (22907)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ff-4b00-7641-bba1-06faa0ef23f1)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "a subagent's approval" =
  replay "subagent_v2_approval";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0da41-a28d-7271-b6f6-59123b5bb248)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0da41-a28d-7271-b6f6-59123b5bb248) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0da41-a31f-7741-bda3-ebc5ad36c38f))))
    (Turn_started (turn_id 01a0da41-a31f-7741-bda3-ebc5ad36c38f))
    (Tool_start (id call_KrtWrCMa1IBTJ4JEr5CvdJMs) (kind Subagent)
     (native_name subAgentActivity) (title /root/fixture_writer)
     (input (Object ((agentPath (String /root/fixture_writer))))) (parent ()))
    (Usage
     ((usage ((input 15998) (output 109) (cache_read 0))) (cost_usd ())
      (context_tokens (16107)) (context_window (258400))))
    (Thinking_delta rs_03696b8961937e5c016ab6d989e28087d0ba611ea28e22e961
     "**Waiting for agent**")
    (Thinking_done (item rs_03696b8961937e5c016ab6d989e28087d0ba611ea28e22e961)
     (text "**Waiting for agent**"))
    (Tool_start (id call_XpVU6N1lxEZ9sxJ0yPNh4dl5) (kind (Other wait))
     (native_name collabAgentToolCall) (title wait)
     (input (Object ((tool (String wait))))) (parent ()))
    (Tool_start (id exec-27a5374a-5e64-4a9b-88ff-8e1f77aa9745) (kind Command)
     (native_name commandExecution)
     (title "printf 'subagent approval fixture' > subagent-approval.txt")
     (input
      (Object
       ((command
         (String "printf 'subagent approval fixture' > subagent-approval.txt"))
        (cwd (String /home/replay-user/tmp/codex-subagent-approval-rec/repo)))))
     (parent (call_KrtWrCMa1IBTJ4JEr5CvdJMs)))
    (Approval_request (request_id 0)
     (tool_id (exec-27a5374a-5e64-4a9b-88ff-8e1f77aa9745)) (kind Command)
     (summary "printf 'subagent approval fixture' > subagent-approval.txt")
     (input
      (Object
       ((command
         (String "printf 'subagent approval fixture' > subagent-approval.txt"))
        (cwd (String /home/replay-user/tmp/codex-subagent-approval-rec/repo)))))
     (offers_always true))
    -> {"id":0,"result":{"decision":"accept"}}
    (Tool_end (id exec-27a5374a-5e64-4a9b-88ff-8e1f77aa9745) (ok true)
     (output "") (changes ()) (images ()))
    (Tool_update call_KrtWrCMa1IBTJ4JEr5CvdJMs "Written.\n")
    (Tool_end (id call_KrtWrCMa1IBTJ4JEr5CvdJMs) (ok true) (output Written.)
     (changes ()) (images ()))
    (Tool_end (id call_XpVU6N1lxEZ9sxJ0yPNh4dl5) (ok true) (output "")
     (changes ()) (images ()))
    (Usage
     ((usage ((input 16129) (output 32) (cache_read 15104))) (cost_usd ())
      (context_tokens (16161)) (context_window (258400))))
    (Text_delta msg_03696b8961937e5c016ab6d995012487d099704aa73dea56e6 Written.)
    (Text_done (item msg_03696b8961937e5c016ab6d995012487d099704aa73dea56e6)
     (text Written.))
    (Usage
     ((usage ((input 16224) (output 6) (cache_read 15104))) (cost_usd ())
      (context_tokens (16230)) (context_window (258400))))
    (Turn_end (turn_id (01a0da41-a31f-7741-bda3-ebc5ad36c38f)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "reverting the last turn" =
  replay "thread_rollback";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5ef-9a99-7bf2-a24c-197fc5cc32ff)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5ef-9a99-7bf2-a24c-197fc5cc32ff) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ef-9b58-7980-9e7a-2463dd068a8e))))
    (Turn_started (turn_id 01a0d5ef-9b58-7980-9e7a-2463dd068a8e))
    (Text_delta msg_044082e5a0068297016ab5be60f20c87d0aed9858a5d1f00b2
     "rollback fixture first turn complete")
    (Text_done (item msg_044082e5a0068297016ab5be60f20c87d0aed9858a5d1f00b2)
     (text "rollback fixture first turn complete"))
    (Usage
     ((usage ((input 21419) (output 9) (cache_read 21248))) (cost_usd ())
      (context_tokens (21428)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ef-9b58-7980-9e7a-2463dd068a8e)) (stop Done))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ef-ac4e-7471-a4a8-6776e39e6d29))))
    (Turn_started (turn_id 01a0d5ef-ac4e-7471-a4a8-6776e39e6d29))
    (Text_delta msg_044082e5a0068297016ab5be62fefc87d0bbf3fdbed6561ac8
     "rollback fixture second turn complete")
    (Text_done (item msg_044082e5a0068297016ab5be62fefc87d0bbf3fdbed6561ac8)
     (text "rollback fixture second turn complete"))
    (Usage
     ((usage ((input 25780) (output 9) (cache_read 21248))) (cost_usd ())
      (context_tokens (25789)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ef-ac4e-7471-a4a8-6776e39e6d29)) (stop Done))
    -> thread/revert
    (response (response (Done Revert)))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ef-b4f4-7aa2-b4c2-f5f494881401))))
    (Turn_started (turn_id 01a0d5ef-b4f4-7aa2-b4c2-f5f494881401))
    (Text_delta msg_044082e5a0068297016ab5be6663e487d0b7a5dcf749ae239e
      "User: Respond with exactly: rollback fixture first turn complete  \
     \nAssistant: rollback fixture first turn complete  \
     \nUser: Repeat the conversation verbatim.")
    (Text_done (item msg_044082e5a0068297016ab5be6663e487d0b7a5dcf749ae239e)
     (text
       "User: Respond with exactly: rollback fixture first turn complete  \
      \nAssistant: rollback fixture first turn complete  \
      \nUser: Repeat the conversation verbatim."))
    (Usage
     ((usage ((input 25777) (output 32) (cache_read 25344))) (cost_usd ())
      (context_tokens (25809)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ef-b4f4-7aa2-b4c2-f5f494881401)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "resuming a thread in a new process" =
  replay "provider_thread_resume";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5ef-62bd-7c20-861b-0528ff7d86bd)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5ef-62bd-7c20-861b-0528ff7d86bd) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ef-637e-7ba2-bf9a-5534a6f98a83))))
    (Turn_started (turn_id 01a0d5ef-637e-7ba2-bf9a-5534a6f98a83))
    (Text_delta msg_0b6c69e800b2ec4c016ab5be52efd887d092b6695452f82b22
     "provider thread resume fixture first turn complete")
    (Text_done (item msg_0b6c69e800b2ec4c016ab5be52efd887d092b6695452f82b22)
     (text "provider thread resume fixture first turn complete"))
    (Usage
     ((usage ((input 21421) (output 11) (cache_read 21248))) (cost_usd ())
      (context_tokens (21432)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ef-637e-7ba2-bf9a-5534a6f98a83)) (stop Done))
    --- Codex exits; a new process ---
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/resume
    (Thread_started (thread_id 01a0d5ef-62bd-7c20-861b-0528ff7d86bd)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_resume)
       (thread_id 01a0d5ef-62bd-7c20-861b-0528ff7d86bd) (model (gpt-6-luna)))))
    -> turn/start
    (Usage
     ((usage ((input 21421) (output 11) (cache_read 21248))) (cost_usd ())
      (context_tokens (21432)) (context_window (258400))))
    (response
     (response (Turn_accepted (turn_id 01a0d5ef-77e2-7e22-a198-4864f687b8b5))))
    (Turn_started (turn_id 01a0d5ef-77e2-7e22-a198-4864f687b8b5))
    (Text_delta msg_0b6c69e800b2ec4c016ab5be59d99087d0ae72c7dc026e0be4
      "provider thread resume fixture first turn complete\
     \nprovider thread resume fixture second turn complete")
    (Text_done (item msg_0b6c69e800b2ec4c016ab5be59d99087d0ae72c7dc026e0be4)
     (text
       "provider thread resume fixture first turn complete\
      \nprovider thread resume fixture second turn complete"))
    (Usage
     ((usage ((input 21476) (output 39) (cache_read 21248))) (cost_usd ())
      (context_tokens (21515)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ef-77e2-7e22-a198-4864f687b8b5)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

let%expect_test "Codex reroutes the model" =
  replay "codex_model_reroute";
  [%expect
    {|
    -> initialize
    -> initialized
    (response
     (response
      (Initialized
       (user_agent
        "t3code_desktop/0.156.1 (Debian 13.0.0; x86_64) unknown (t3code_desktop; 0.1.0)")
       (version (0.156.1)))))
    -> thread/start
    (Thread_started (thread_id 01a0d5ed-187d-70c2-b4b6-b6881a68de09)
     (model (gpt-6-luna)))
    (response
     (response
      (Thread (kind Thread_start)
       (thread_id 01a0d5ed-187d-70c2-b4b6-b6881a68de09) (model (gpt-6-luna)))))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ed-1957-7a22-8399-d0172af342fd))))
    (Turn_started (turn_id 01a0d5ed-1957-7a22-8399-d0172af342fd))
    (Notice
     "Codex switched from gpt-6-luna to gpt-6-astra (highRiskCyberActivity)")
    (Text_delta msg_06900ecb02a780a6016ab5bdbcd76887d0ba52ca8fee9da2e1
     "first fixture turn complete")
    (Text_done (item msg_06900ecb02a780a6016ab5bdbcd76887d0ba52ca8fee9da2e1)
     (text "first fixture turn complete"))
    (Usage
     ((usage ((input 21418) (output 8) (cache_read 6912))) (cost_usd ())
      (context_tokens (21426)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ed-1957-7a22-8399-d0172af342fd)) (stop Done))
    -> turn/start
    (response
     (response (Turn_accepted (turn_id 01a0d5ed-2b29-7353-a6d5-7745731173b7))))
    (Turn_started (turn_id 01a0d5ed-2b29-7353-a6d5-7745731173b7))
    (Text_delta msg_06900ecb02a780a6016ab5bdbf08a087d08c1dafcd3f15697a
     "second fixture turn complete")
    (Text_done (item msg_06900ecb02a780a6016ab5bdbf08a087d08c1dafcd3f15697a)
     (text "second fixture turn complete"))
    (Usage
     ((usage ((input 25777) (output 8) (cache_read 21248))) (cost_usd ())
      (context_tokens (25785)) (context_window (258400))))
    (Turn_end (turn_id (01a0d5ed-2b29-7353-a6d5-7745731173b7)) (stop Done))
    --- Codex exits; a new process ---
    |}]
;;

(* Hand-written exchanges, for what the recordings do not show. *)

let thread = "th-1"

let feed codec lines =
  let printer = Printer.create () in
  List.iter lines ~f:(fun line ->
    List.iter
      (C.handle_line codec line)
      ~f:(Printer.output ~full_sends:true printer));
  Printer.flush printer
;;

let print_line line = print_endline ("-> " ^ line)

(* A codec with a thread, its requests 0 (initialize) and 1 (thread/start)
   answered. *)
let started ?(settings = settings ()) () =
  let codec = C.create () in
  ignore
    (C.initialize codec ~client_name:"prigh" ~client_version:"test" : string);
  ignore (C.start_thread codec settings C.Thread_options.empty : string);
  ignore
    (C.handle_line
       codec
       {|{"id":0,"result":{"userAgent":"prigh/0.166.0 (Linux)"}}|}
     : C.Output.t list);
  ignore
    (C.handle_line
       codec
       [%string
         {|{"id":1,"result":{"thread":{"id":"%{thread}"},"model":"gpt-5.5"}}|}]
     : C.Output.t list);
  codec
;;

let notify method_ params =
  Json.to_string (`Object [ "method", `String method_; "params", params ])
;;

let turn_event method_ ?(status = "inProgress") ?error turn =
  notify
    method_
    (`Object
        [ "threadId", `String thread
        ; ( "turn"
          , `Object
              [ "id", `String turn
              ; "status", `String status
              ; "error", Option.value error ~default:`Null
              ] )
        ])
;;

let%expect_test "the version is in initialize's user agent" =
  List.iter
    [ "prigh/0.166.0 (Linux 6.1; x86_64) xterm"
    ; "T3 Code/0.157.0 (Mac OS 27.0.0; arm64)"
    ; "codex"
    ; "codex/"
    ]
    ~f:(fun ua ->
      print_s
        [%sexp (ua : string), (C.version_of_user_agent ua : string option)]);
  [%expect
    {|
    ("prigh/0.166.0 (Linux 6.1; x86_64) xterm" (0.166.0))
    ("T3 Code/0.157.0 (Mac OS 27.0.0; arm64)" (0.157.0))
    (codex ())
    (codex/ ())
    |}]
;;

let%expect_test "what a thread and a turn are started with" =
  let codec = C.create () in
  print_line
    (C.start_thread
       codec
       (settings ~approval:Ask ~model:"gpt-5.5" ())
       { developer_instructions = Some "Be brief."
       ; mcp_servers =
           [ ( "prigh"
             , `Object
                 [ "command", `String "prigh"
                 ; "args", `Array [ `String "mcp" ]
                 ] )
           ]
       ; dynamic_tools =
           [ { name = "ask_user"
             ; description = "Ask the user"
             ; input_schema = `Object [ "type", `String "object" ]
             }
           ]
       ; config = [ "features.x", `True ]
       });
  print_s
    [%sexp (C.start_turn codec (settings ()) [ Text "hi" ] : string Or_error.t)];
  feed
    codec
    [ {|{"id":0,"result":{"thread":{"id":"th-1"},"model":"gpt-5.5"}}|} ];
  List.iter
    [ Approval_mode.Ask, Thinking.Off
    ; Auto_edit, On (Some Low)
    ; Smart, On None
    ; Auto, On (Some Max)
    ]
    ~f:(fun (approval, thinking) ->
      let codec = started () in
      print_line
        (Or_error.ok_exn
           (C.start_turn
              codec
              { (settings ~approval ()) with
                thinking
              ; plan = Poly.equal approval Ask
              }
              [ Text "look"
              ; Image { mime_type = "image/png"; data = "iVBO" }
              ; Local_image "/tmp/a.png"
              ])));
  [%expect
    {|
    -> {"id":0,"method":"thread/start","params":{"cwd":"/work","model":"gpt-5.5","approvalPolicy":"untrusted","approvalsReviewer":"user","sandbox":"read-only","developerInstructions":"Be brief.","config":{"tools.update_plan.enabled":true,"mcp_servers.prigh":{"command":"prigh","args":["mcp"]},"features.x":true},"dynamicTools":[{"type":"function","name":"ask_user","description":"Ask the user","inputSchema":{"type":"object"}}]}}
    (Error "Codex has no thread yet")
    (Thread_started (thread_id th-1) (model (gpt-5.5)))
    (response
     (response (Thread (kind Thread_start) (thread_id th-1) (model (gpt-5.5)))))
    -> {"id":2,"method":"turn/start","params":{"threadId":"th-1","input":[{"type":"text","text":"look"},{"type":"image","url":"data:image/png;base64,iVBO"},{"type":"localImage","path":"/tmp/a.png"}],"cwd":"/work","approvalPolicy":"untrusted","approvalsReviewer":"user","sandboxPolicy":{"type":"readOnly"},"effort":"none","summary":"auto","collaborationMode":{"mode":"plan","settings":{"model":"gpt-5.5","reasoning_effort":"none","developer_instructions":null}}}}
    -> {"id":2,"method":"turn/start","params":{"threadId":"th-1","input":[{"type":"text","text":"look"},{"type":"image","url":"data:image/png;base64,iVBO"},{"type":"localImage","path":"/tmp/a.png"}],"cwd":"/work","approvalPolicy":"untrusted","approvalsReviewer":"user","sandboxPolicy":{"type":"workspaceWrite"},"effort":"low","summary":"auto","collaborationMode":{"mode":"default","settings":{"model":"gpt-5.5","reasoning_effort":"low","developer_instructions":null}}}}
    -> {"id":2,"method":"turn/start","params":{"threadId":"th-1","input":[{"type":"text","text":"look"},{"type":"image","url":"data:image/png;base64,iVBO"},{"type":"localImage","path":"/tmp/a.png"}],"cwd":"/work","approvalPolicy":"untrusted","approvalsReviewer":"user","sandboxPolicy":{"type":"workspaceWrite"},"summary":"auto","collaborationMode":{"mode":"default","settings":{"model":"gpt-5.5","reasoning_effort":null,"developer_instructions":null}}}}
    -> {"id":2,"method":"turn/start","params":{"threadId":"th-1","input":[{"type":"text","text":"look"},{"type":"image","url":"data:image/png;base64,iVBO"},{"type":"localImage","path":"/tmp/a.png"}],"cwd":"/work","approvalPolicy":"untrusted","approvalsReviewer":"user","sandboxPolicy":{"type":"dangerFullAccess"},"effort":"xhigh","summary":"auto","collaborationMode":{"mode":"default","settings":{"model":"gpt-5.5","reasoning_effort":"xhigh","developer_instructions":null}}}}
    |}]
;;

let%expect_test "notifications before turn/start's response; one turn at a time"
  =
  let codec = started () in
  let start =
    Or_error.ok_exn (C.start_turn codec (settings ()) [ Text "go" ])
  in
  print_line start;
  print_s
    [%sexp
      (C.start_turn codec (settings ()) [ Text "again" ] : string Or_error.t)];
  print_s [%sexp (C.steer codec [ Text "more" ] : string Or_error.t)];
  feed
    codec
    [ turn_event "turn/started" "tu-1"
    ; notify
        "item/agentMessage/delta"
        (`Object
            [ "threadId", `String thread
            ; "itemId", `String "m1"
            ; "delta", `String "Hel"
            ])
    ; {|{"id":2,"result":{"turn":{"id":"tu-1","status":"inProgress","startedAt":null}}}|}
    ];
  print_s [%sexp (C.turn_id codec : string option)];
  print_line (Or_error.ok_exn (C.steer codec [ Text "more" ]));
  feed
    codec
    [ turn_event "turn/completed" ~status:"completed" "tu-1"
    ; (* a repeat, and a turn of a thread we don't follow *)
      turn_event "turn/completed" ~status:"completed" "tu-1"
    ; notify
        "turn/completed"
        (`Object
            [ "threadId", `String "other"
            ; ( "turn"
              , `Object [ "id", `String "x"; "status", `String "completed" ] )
            ])
    ];
  print_s [%sexp (C.turn_id codec : string option)];
  [%expect
    {|
    -> {"id":2,"method":"turn/start","params":{"threadId":"th-1","input":[{"type":"text","text":"go"}],"cwd":"/work","approvalPolicy":"untrusted","approvalsReviewer":"user","sandboxPolicy":{"type":"dangerFullAccess"},"summary":"auto","collaborationMode":{"mode":"default","settings":{"model":"gpt-5.5","reasoning_effort":null,"developer_instructions":null}}}}
    (Error "a Codex turn is already running")
    (Error "the Codex turn has not started yet")
    (Turn_started (turn_id tu-1))
    (Text_delta m1 Hel)
    (response (response (Turn_accepted (turn_id tu-1))))
    (tu-1)
    -> {"id":3,"method":"turn/steer","params":{"threadId":"th-1","input":[{"type":"text","text":"more"}],"expectedTurnId":"tu-1"}}
    (Turn_end (turn_id (tu-1)) (stop Done))
    ()
    |}]
;;

let%expect_test
    "an interrupt before Codex starts the turn waits for turn/started"
  =
  let codec = started () in
  print_s [%sexp (C.interrupt codec : [ `Send of string | `Deferred | `Idle ])];
  ignore (C.start_turn codec (settings ()) [ Text "go" ] : string Or_error.t);
  print_s [%sexp (C.interrupt codec : [ `Send of string | `Deferred | `Idle ])];
  feed
    codec
    [ {|{"id":2,"result":{"turn":{"id":"tu-1","status":"inProgress","startedAt":null}}}|}
    ];
  print_s [%sexp (C.interrupt codec : [ `Send of string | `Deferred | `Idle ])];
  feed
    codec
    [ turn_event "turn/started" "tu-1"
    ; {|{"id":3,"result":{}}|}
    ; (* Codex says completed, but we asked: still an interruption *)
      turn_event "turn/completed" ~status:"completed" "tu-1"
    ];
  [%expect
    {|
    Idle
    Deferred
    (response (response (Turn_accepted (turn_id tu-1))))
    Deferred
    (Turn_started (turn_id tu-1))
    -> {"id":3,"method":"turn/interrupt","params":{"threadId":"th-1","turnId":"tu-1"}}
    (response (response (Done Turn_interrupt)))
    (Turn_end (turn_id (tu-1)) (stop Interrupted))
    |}]
;;

let%expect_test "errors: retries, the usage limit, a refused turn" =
  let codec = started () in
  ignore (C.start_turn codec (settings ()) [ Text "go" ] : string Or_error.t);
  let error ?(will_retry = false) info message =
    notify
      "error"
      (`Object
          [ "threadId", `String thread
          ; "turnId", `String "tu-1"
          ; ("willRetry", if will_retry then `True else `False)
          ; ( "error"
            , `Object
                [ "message", `String message
                ; "codexErrorInfo", info
                ; "additionalDetails", `Null
                ] )
          ])
  in
  feed
    codec
    [ turn_event "turn/started" "tu-1"
    ; notify
        "account/rateLimits/updated"
        (Json.parse
           {|{"rateLimits":{"primary":{"usedPercent":100.0,"windowDurationMins":300,"resetsAt":1790000000},"secondary":{"usedPercent":40.0,"resetsAt":1790500000}}}|}
         |> Result.ok
         |> Option.value_exn)
    ; error
        ~will_retry:true
        (Json.parse {|{"responseStreamDisconnected":{"httpStatusCode":502}}|}
         |> Result.ok
         |> Option.value_exn)
        "stream disconnected"
    ; error (`String "usageLimitExceeded") "You've hit your usage limit."
    ; turn_event "turn/completed" ~status:"failed" "tu-1"
    ];
  ignore (C.start_turn codec (settings ()) [ Text "again" ] : string Or_error.t);
  feed
    codec
    [ {|{"id":3,"error":{"code":-32600,"message":"thread not loaded"}}|}
    ; (* a failed turn whose error only comes with turn/completed *)
      turn_event
        "turn/completed"
        ~status:"failed"
        ~error:
          (`Object
              [ "message", `String "Unauthorized"
              ; "codexErrorInfo", `String "unauthorized"
              ])
        "tu-0"
    ];
  print_s [%sexp (C.turn_id codec : string option)];
  ignore (C.start_turn codec (settings ()) [ Text "third" ] : string Or_error.t);
  feed
    codec
    [ turn_event "turn/started" "tu-2"
    ; turn_event
        "turn/completed"
        ~status:"failed"
        ~error:
          (`Object
              [ "message", `String "Unauthorized"
              ; "codexErrorInfo", `String "unauthorized"
              ])
        "tu-2"
    ];
  [%expect
    {|
    (Turn_started (turn_id tu-1))
    (Retrying (attempt ()) (message "stream disconnected"))
    (Turn_end (turn_id (tu-1))
     (stop
      (Failed (kind (Usage_limit (resets_at (1790000000))))
       (message "You've hit your usage limit."))))
    (response
     (response
      (Failed (kind Turn_start) (code -32600) (message "thread not loaded"))))
    (Turn_end (turn_id ())
     (stop
      (Failed (kind Other) (message "Codex refused the turn: thread not loaded"))))
    ()
    (Turn_started (turn_id tu-2))
    (Turn_end (turn_id (tu-2))
     (stop
      (Failed
       (kind
        (Auth_required
         (hint "Codex is not logged in: run `codex login` where it runs")))
       (message Unauthorized))))
    |}]
;;

let%expect_test "Codex's requests: answered, withdrawn, declined, unknown" =
  let codec = started () in
  ignore (C.start_turn codec (settings ()) [ Text "go" ] : string Or_error.t);
  feed
    codec
    [ turn_event "turn/started" "tu-1"
    ; notify
        "item/started"
        (Json.parse
           {|{"threadId":"th-1","item":{"type":"fileChange","id":"fc-1","status":"inProgress","changes":[{"path":"src/a.ml","kind":{"type":"update","move_path":"src/b.ml"},"diff":"@@ -1 +1 @@\n-a\n+b\n"},{"path":"old.txt","kind":{"type":"delete"},"diff":"gone\n"}]}}|}
         |> Result.ok
         |> Option.value_exn)
    ; {|{"id":7,"method":"item/fileChange/requestApproval","params":{"threadId":"th-1","turnId":"tu-1","itemId":"fc-1","reason":null}}|}
    ; {|{"id":"q","method":"item/commandExecution/requestApproval","params":{"threadId":"th-1","turnId":"tu-1","itemId":"c-1","command":"rm -rf build","cwd":"/work","availableDecisions":["accept","acceptForSession","decline","cancel"]}}|}
    ; {|{"id":8,"method":"item/tool/requestUserInput","params":{"threadId":"th-1","turnId":"tu-1","itemId":"u","questions":[{"id":"a","header":"H","question":"Which?","isOther":true,"isSecret":false,"options":[{"label":"x","description":"the x"}]},{"id":"b","header":"","question":"Why?","isOther":true,"isSecret":false,"options":null}]}}|}
    ; {|{"id":9,"method":"item/tool/call","params":{"threadId":"th-1","turnId":"tu-1","callId":"call-1","namespace":null,"tool":"ask_user","arguments":{"q":1}}}|}
    ; {|{"id":10,"method":"mcpServer/elicitation/request","params":{"threadId":"th-1","serverName":"github"}}|}
    ; {|{"id":11,"method":"item/somethingNew","params":{}}|}
    ];
  let show = Option.iter ~f:print_line in
  show (C.reply_approval codec ~request_id:"7" Allow_always);
  show (C.reply_approval codec ~request_id:"q" (Deny (Some "no")));
  print_s [%sexp (C.reply_approval codec ~request_id:"q" Allow : string option)];
  show
    (C.answer_question
       codec
       ~request_id:"8"
       (Answered [ [ "x" ]; [ "because" ] ]));
  show (C.reply_tool_call codec ~request_id:"9" ~output:"yes" ~success:true);
  feed
    codec
    [ {|{"id":12,"method":"item/commandExecution/requestApproval","params":{"threadId":"th-1","itemId":"c-2","command":"ls"}}|}
    ; {|{"method":"serverRequest/resolved","params":{"threadId":"th-1","requestId":12}}|}
    ; {|{"method":"serverRequest/resolved","params":{"threadId":"th-1","requestId":12}}|}
    ; notify
        "item/completed"
        (Json.parse
           {|{"threadId":"th-1","item":{"type":"fileChange","id":"fc-1","status":"completed","changes":[{"path":"src/a.ml","kind":{"type":"update","move_path":"src/b.ml"},"diff":"@@ -1 +1 @@\n-a\n+b\n"},{"path":"old.txt","kind":{"type":"delete"},"diff":"gone\n"}]}}|}
         |> Result.ok
         |> Option.value_exn)
    ; "not json"
    ; ""
    ];
  print_s
    [%sexp (C.reply_approval codec ~request_id:"12" Allow : string option)];
  [%expect
    {|
    (Turn_started (turn_id tu-1))
    (Tool_start (id fc-1) (kind File_change) (native_name fileChange)
     (title "Move src/a.ml to src/b.ml, Delete old.txt")
     (input
      (Object
       ((changes
         (Array
          ((Object
            ((path (String src/a.ml))
             (kind
              (Object ((type (String update)) (move_path (String src/b.ml)))))))
           (Object
            ((path (String old.txt)) (kind (Object ((type (String delete)))))))))))))
     (parent ()))
    (Approval_request (request_id 7) (tool_id (fc-1)) (kind File_change)
     (summary "Move src/a.ml to src/b.ml, Delete old.txt")
     (input
      (Object
       ((changes
         (Array
          ((Object
            ((path (String src/a.ml))
             (kind
              (Object ((type (String update)) (move_path (String src/b.ml)))))
             (diff (String  "@@ -1 +1 @@\
                           \n-a\
                           \n+b\
                           \n"))))
           (Object
            ((path (String old.txt)) (kind (Object ((type (String delete)))))
             (diff (String "gone\n"))))))))))
     (offers_always true))
    (Approval_request (request_id q) (tool_id (c-1)) (kind Command)
     (summary "rm -rf build")
     (input (Object ((command (String "rm -rf build")) (cwd (String /work)))))
     (offers_always true))
    (User_question (request_id 8)
     (questions
      (((question Which?) (header (H))
        (choices (((label x) (description ("the x"))))) (multiple false))
       ((question Why?) (header ("")) (choices ()) (multiple false)))))
    ("tool call"
     (call
      ((request_id 9) (call_id call-1) (tool ask_user)
       (arguments (Object ((q (Number 1))))))))
    -> {"id":10,"result":{"action":"cancel"}}
    (Notice
     "Codex's github asked for input, which prigh cannot show yet: cancelled")
    -> {"id":11,"error":{"code":-32601,"message":"prigh does not handle item/somethingNew"}}
    -> {"id":7,"result":{"decision":"acceptForSession"}}
    -> {"id":"q","result":{"decision":"decline"}}
    ()
    -> {"id":8,"result":{"answers":{"a":{"answers":["x"]},"b":{"answers":["because"]}}}}
    -> {"id":9,"result":{"contentItems":[{"type":"inputText","text":"yes"}],"success":true}}
    (Approval_request (request_id 12) (tool_id (c-2)) (kind Command) (summary ls)
     (input (Object ((command (String ls))))) (offers_always true))
    (Request_withdrawn (request_id 12))
    (Tool_end (id fc-1) (ok true) (output "")
     (changes
      (((path src/a.ml) (kind (Update (move_to (src/b.ml))))
        (diff  "--- a/src/a.ml\
              \n+++ b/src/b.ml\
              \n@@ -1 +1 @@\
              \n-a\
              \n+b\
              \n"))
       ((path old.txt) (kind Delete)
        (diff  "--- a/old.txt\
              \n+++ /dev/null\
              \n@@ -1,1 +0,0 @@\
              \n-gone\
              \n"))))
     (images ()))
    (Notice "Codex wrote something that is not JSON: not json")
    ()
    |}]
;;

let%expect_test "MCP tool-call approvals and network access are asked as such" =
  let codec = started () in
  ignore (C.start_turn codec (settings ()) [ Text "go" ] : string Or_error.t);
  let form =
    {|"requestedSchema":{"type":"object","properties":{"decision":{"type":"string","oneOf":[{"const":"allow_once","title":"Allow"},{"const":"allow_session","title":"Allow for this session"},{"const":"deny","title":"Deny"}]},"remember":{"type":"boolean","title":"Always allow"}},"required":["decision"]}|}
  in
  feed
    codec
    [ turn_event "turn/started" "tu-1"
    ; sprintf
        {|{"id":20,"method":"mcpServer/elicitation/request","params":{"threadId":"th-1","turnId":"tu-1","serverName":"github","mode":"form","message":"Allow github to create_issue?","_meta":{"codex_approval_kind":"mcp_tool_call","tool_name":"create_issue"},%s}}|}
        form
    ; sprintf
        {|{"id":21,"method":"mcpServer/elicitation/request","params":{"threadId":"th-1","turnId":"tu-1","serverName":"github","mode":"form","message":"Allow?","_meta":{"codex_approval_kind":"mcp_tool_call"},%s}}|}
        form
    ; sprintf
        {|{"id":22,"method":"mcpServer/elicitation/request","params":{"threadId":"th-1","turnId":"tu-1","serverName":"github","mode":"form","message":"Allow?","_meta":{"codex_approval_kind":"mcp_tool_call"},%s}}|}
        form
    ; (* prigh's own tools: prigh asks when the call reaches it. *)
      sprintf
        {|{"id":23,"method":"mcpServer/elicitation/request","params":{"threadId":"th-1","turnId":"tu-1","serverName":"prigh","mode":"form","message":"Allow?","_meta":{"codex_approval_kind":"mcp_tool_call"},%s}}|}
        form
    ; {|{"id":24,"method":"mcpServer/elicitation/request","params":{"threadId":"th-1","turnId":"tu-1","serverName":"web","mode":"url","message":"Sign in","url":"https://example.com","elicitationId":"e","_meta":{"codex_approval_kind":"mcp_tool_call"}}}|}
    ; {|{"id":25,"method":"item/commandExecution/requestApproval","params":{"threadId":"th-1","turnId":"tu-1","itemId":"c-9","command":"curl https://example.com","cwd":"/work","networkApprovalContext":{"host":"example.com","protocol":"https"}}}|}
    ; {|{"id":26,"method":"item/commandExecution/requestApproval","params":{"threadId":"th-1","turnId":"tu-1","itemId":"c-10","command":"y","kind":"writeStdin"}}|}
    ];
  let show = Option.iter ~f:print_line in
  show (C.reply_approval codec ~request_id:"20" Allow);
  show (C.reply_approval codec ~request_id:"21" Allow_always);
  show (C.reply_approval codec ~request_id:"22" (Deny None));
  [%expect
    {|
    (Turn_started (turn_id tu-1))
    (Approval_request (request_id 20) (tool_id ())
     (kind (Mcp (server github) (tool create_issue)))
     (summary "Allow github to create_issue?")
     (input
      (Object
       ((codex_approval_kind (String mcp_tool_call))
        (tool_name (String create_issue)))))
     (offers_always true))
    (Approval_request (request_id 21) (tool_id ())
     (kind (Mcp (server github) (tool tool))) (summary Allow?)
     (input (Object ((codex_approval_kind (String mcp_tool_call)))))
     (offers_always true))
    (Approval_request (request_id 22) (tool_id ())
     (kind (Mcp (server github) (tool tool))) (summary Allow?)
     (input (Object ((codex_approval_kind (String mcp_tool_call)))))
     (offers_always true))
    -> {"id":23,"result":{"action":"accept","content":{"decision":"allow_once","remember":false}}}
    -> {"id":24,"result":{"action":"cancel"}}
    (Notice
     "Codex's web asked for input, which prigh cannot show yet: cancelled")
    (Approval_request (request_id 25) (tool_id ()) (kind (Other network))
     (summary
      "Network access to example.com (https) for: curl https://example.com")
     (input
      (Object
       ((command (String "curl https://example.com")) (cwd (String /work))
        (network
         (Object ((host (String example.com)) (protocol (String https))))))))
     (offers_always true))
    (Approval_request (request_id 26) (tool_id (c-10)) (kind Command)
     (summary "Send input to the running command: y")
     (input (Object ((command (String y)) (kind (String writeStdin)))))
     (offers_always true))
    -> {"id":20,"result":{"action":"accept","content":{"decision":"allow_once","remember":false}}}
    -> {"id":21,"result":{"action":"accept","_meta":{"persist":"session"},"content":{"decision":"allow_session","remember":true}}}
    -> {"id":22,"result":{"action":"decline"}}
    |}]
;;

let%expect_test "models and the account" =
  let codec = C.create () in
  print_line (C.list_models codec ~cursor:None);
  print_line (C.list_models codec ~cursor:(Some "c2"));
  print_line (C.read_account codec);
  print_line (C.read_account codec);
  print_line (C.read_account codec);
  print_line (C.read_account codec);
  print_s [%sexp (C.compact codec : string Or_error.t)];
  feed
    codec
    [ {|{"id":0,"result":{"data":[{"id":"gpt-5.5","model":"gpt-5.5","displayName":"GPT-5.5","description":"Frontier","hidden":false,"isDefault":true,"supportedReasoningEfforts":[{"reasoningEffort":"low","description":""},{"reasoningEffort":"high","description":""}],"defaultReasoningEffort":"medium","inputModalities":["text","image"]}],"nextCursor":"c2"}}|}
    ; {|{"id":1,"result":{"data":[{"id":"mini","model":"gpt-5.5-mini","displayName":"Mini","description":"","hidden":true,"isDefault":false,"supportedReasoningEfforts":[],"defaultReasoningEffort":"low","inputModalities":["text"]}],"nextCursor":null}}|}
    ; {|{"id":2,"result":{"account":null,"requiresOpenaiAuth":true}}|}
    ; {|{"id":3,"result":{"account":{"type":"chatgpt","email":"l@example.com","planType":"pro"},"requiresOpenaiAuth":true}}|}
    ; {|{"id":4,"result":{"account":{"type":"apiKey"},"requiresOpenaiAuth":true}}|}
    ; {|{"id":5,"result":{"account":null,"requiresOpenaiAuth":false}}|}
    ; {|{"id":99,"result":{}}|}
    ];
  [%expect
    {|
    -> {"id":0,"method":"model/list","params":{}}
    -> {"id":1,"method":"model/list","params":{"cursor":"c2"}}
    -> {"id":2,"method":"account/read","params":{"refreshToken":false}}
    -> {"id":3,"method":"account/read","params":{"refreshToken":false}}
    -> {"id":4,"method":"account/read","params":{"refreshToken":false}}
    -> {"id":5,"method":"account/read","params":{"refreshToken":false}}
    (Error "Codex has no thread yet")
    (response
     (response
      (Models
       (models
        (((id gpt-5.5) (display_name GPT-5.5) (description Frontier)
          (is_default true) (hidden false) (efforts (low high))
          (default_effort (medium)) (images true))))
       (next_cursor (c2)))))
    (response
     (response
      (Models
       (models
        (((id gpt-5.5-mini) (display_name Mini) (description "")
          (is_default false) (hidden true) (efforts ()) (default_effort (low))
          (images false))))
       (next_cursor ()))))
    (response (response (Account Logged_out)))
    (response
     (response (Account (Chatgpt (email (l@example.com)) (plan (pro))))))
    (response (response (Account Api_key)))
    (response (response (Account Not_needed)))
    |}]
;;

let%expect_test "prigh's ChatGPT login: handed, refused, refreshed" =
  let codec = C.create () in
  let tokens access : C.Chatgpt_tokens.t =
    { access_token = access; account_id = "acct-1"; plan = Some "plus" }
  in
  print_line (C.login_tokens codec (tokens "tok-1"));
  print_line (C.login_tokens codec { (tokens "tok-2") with plan = None });
  print_s [%sexp (tokens "secret" : C.Chatgpt_tokens.t)];
  feed
    codec
    [ {|{"id":0,"result":{"type":"chatgptAuthTokens"}}|}
    ; {|{"id":1,"error":{"code":-32600,"message":"external auth is not supported"}}|}
    ; {|{"method":"account/chatgptAuthTokens/refresh","id":0,"params":{"reason":"unauthorized","previousAccountId":"acct-1"}}|}
    ; {|{"method":"account/chatgptAuthTokens/refresh","id":1,"params":{"reason":"unauthorized"}}|}
    ];
  print_s
    [%sexp
      (C.reply_token_refresh codec ~request_id:"0" (Ok (tokens "tok-3"))
       : string option)];
  print_s
    [%sexp
      (C.reply_token_refresh
         codec
         ~request_id:"1"
         (Or_error.error_string "login gone")
       : string option)];
  (* Answered once; not an approval. *)
  print_s
    [%sexp
      (C.reply_token_refresh codec ~request_id:"0" (Ok (tokens "tok-4"))
       : string option)];
  print_s [%sexp (C.reply_approval codec ~request_id:"0" Allow : string option)];
  [%expect
    {|
    -> {"id":0,"method":"account/login/start","params":{"type":"chatgptAuthTokens","accessToken":"tok-1","chatgptAccountId":"acct-1","chatgptPlanType":"plus"}}
    -> {"id":1,"method":"account/login/start","params":{"type":"chatgptAuthTokens","accessToken":"tok-2","chatgptAccountId":"acct-1","chatgptPlanType":null}}
    ((access_token <redacted>) (account_id acct-1) (plan (plus)))
    (response (response Logged_in))
    (response
     (response
      (Failed (kind Login) (code -32600)
       (message "external auth is not supported"))))
    ("token refresh" (request_id 0) (previous_account_id (acct-1)))
    ("token refresh" (request_id 1) (previous_account_id ()))
    ("{\"id\":0,\"result\":{\"accessToken\":\"tok-3\",\"chatgptAccountId\":\"acct-1\",\"chatgptPlanType\":\"plus\"}}")
    ("{\"id\":1,\"error\":{\"code\":-32603,\"message\":\"login gone\"}}")
    ()
    ()
    |}]
;;

let%expect_test "a fork becomes the codec's thread; compaction" =
  let codec = started () in
  print_line
    (C.fork_thread
       codec
       ~thread_id:thread
       ~last_turn_id:(Some "tu-1")
       (settings ())
       C.Thread_options.empty);
  feed
    codec
    [ {|{"id":2,"result":{"thread":{"id":"th-2"},"model":"gpt-5.5"}}|} ];
  print_line (Or_error.ok_exn (C.compact codec));
  feed
    codec
    [ {|{"id":3,"result":{}}|}
    ; notify
        "item/completed"
        (`Object
            [ "threadId", `String "th-2"
            ; ( "item"
              , `Object
                  [ "type", `String "contextCompaction"; "id", `String "cc" ] )
            ])
    ; (* the old thread is no longer ours *)
      notify
        "item/completed"
        (`Object
            [ "threadId", `String thread
            ; ( "item"
              , `Object
                  [ "type", `String "contextCompaction"; "id", `String "cc" ] )
            ])
    ];
  print_s [%sexp (C.thread_id codec : string option)];
  [%expect
    {|
    -> {"id":2,"method":"thread/fork","params":{"threadId":"th-1","lastTurnId":"tu-1","excludeTurns":true,"cwd":"/work","approvalPolicy":"untrusted","approvalsReviewer":"user","sandbox":"danger-full-access","config":{"tools.update_plan.enabled":true}}}
    (Thread_started (thread_id th-2) (model (gpt-5.5)))
    (response
     (response (Thread (kind Thread_fork) (thread_id th-2) (model (gpt-5.5)))))
    -> {"id":3,"method":"thread/compact/start","params":{"threadId":"th-2"}}
    (response (response (Done Compact)))
    (Compacted (tokens_before ()) (tokens_after ()))
    (th-2)
    |}]
;;

let%expect_test "the effort sent is one the model supports" =
  List.iter
    [ Thinking.Off, Some [ "none"; "low"; "high" ]
    ; Off, Some [ "minimal"; "low"; "medium" ]
    ; On (Some Medium), Some [ "low"; "high" ]
    ; On (Some Max), Some [ "low"; "medium"; "high" ]
    ; On (Some Minimal), Some [ "low"; "medium" ]
    ; On None, Some [ "low" ]
    ; Off, None
    ; On (Some Xhigh), Some []
    ]
    ~f:(fun (thinking, efforts) ->
      let codec = started () in
      let line =
        Or_error.ok_exn
          (C.start_turn
             codec
             { (settings ()) with thinking; efforts }
             [ Text "x" ])
      in
      let effort =
        match Json.member "params" (Json.of_string line) with
        | Some params ->
          (match Json.member "effort" params with
           | Some (`String e) -> Some e
           | _ -> None)
        | None -> None
      in
      print_s
        [%sexp
          (thinking : Thinking.t)
        , (efforts : string list option)
        , "->"
        , (effort : string option)]);
  [%expect
    {|
    (Off ((none low high)) -> (none))
    (Off ((minimal low medium)) -> (minimal))
    ((On (Medium)) ((low high)) -> (low))
    ((On (Max)) ((low medium high)) -> (high))
    ((On (Minimal)) ((low medium)) -> (low))
    ((On ()) ((low)) -> ())
    (Off () -> (none))
    ((On (Xhigh)) (()) -> (xhigh))
    |}]
;;

let%expect_test "thread/name/updated emits Session_title for main thread only" =
  let codec = started () in
  let thread = Option.value_exn (C.thread_id codec) in
  let show label params =
    printf "%s:\n" label;
    feed codec [ notify "thread/name/updated" (`Object params) ]
  in
  show
    "threadName"
    [ "threadId", `String thread; "threadName", `String "  Fix parser bug  " ];
  show "name" [ "threadId", `String thread; "name", `String "Refine tests" ];
  show "blank" [ "threadId", `String thread; "threadName", `String "   " ];
  show
    "other thread"
    [ "threadId", `String "other-thread"; "threadName", `String "Ignored" ];
  [%expect
    {|
    threadName:
    (Session_title "Fix parser bug")
    name:
    (Session_title "Refine tests")
    blank:
    other thread:
    |}]
;;
