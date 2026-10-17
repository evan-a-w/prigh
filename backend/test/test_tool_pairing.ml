open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply

let call id = { Content.Tool_call.id; name = "bash"; arguments = "{}" }

let assistant ids =
  Message.Assistant
    { content = List.map ids ~f:(fun id -> Content.Tool_call (call id))
    ; stop_reason = Tool_use
    ; usage = Usage.zero
    ; model = "m"
    }
;;

let result id =
  Message.Tool_result
    { tool_call_id = id
    ; tool_name = "bash"
    ; text = "ok " ^ id
    ; is_error = false
    ; images = []
    }
;;

let show messages =
  List.iter messages ~f:(function
    | Message.User u -> printf "user %s\n" u.text
    | Assistant a ->
      printf
        "assistant calls [%s]\n"
        (String.concat
           ~sep:" "
           (List.map (Message.Assistant.tool_calls a) ~f:(fun c -> c.id)))
    | Tool_result r -> printf "  result %s: %s\n" r.tool_call_id r.text)
;;

let%expect_test "repair answers every call right after it" =
  let well_formed =
    [ Message.user "go"; assistant [ "a"; "b" ]; result "a"; result "b" ]
  in
  print_s
    [%sexp
      (List.equal Message.equal (Tool_pairing.repair well_formed) well_formed
       : bool)];
  [%expect {| true |}];
  (* The backend restarted while "b" ran; the user wrote afterwards. *)
  show
    (Tool_pairing.repair
       [ Message.user "go"
       ; assistant [ "a"; "b" ]
       ; result "a"
       ; Message.user "hello?"
       ]);
  [%expect
    {|
    user go
    assistant calls [a b]
      result a: ok a
      result b: [interrupted: prigh stopped before this tool call returned (e.g. the backend restarted), so whether it ran, and what it did, is unknown: check before relying on it]
    user hello?
    |}];
  (* A result that came after a message moves up; one answering nothing,
     or answering a call of an earlier turn, is dropped. Ids may repeat
     across turns. *)
  show
    (Tool_pairing.repair
       [ result "stray"
       ; assistant [ "a" ]
       ; Message.user "note"
       ; result "a"
       ; result "zzz"
       ; assistant [ "a" ]
       ; result "a"
       ; assistant []
       ]);
  [%expect
    {|
    assistant calls [a]
      result a: ok a
    user note
    assistant calls [a]
      result a: ok a
    assistant calls []
    |}];
  print_s
    [%sexp
      (List.map
         (Tool_pairing.unanswered [ assistant [ "a"; "b"; "c" ]; result "b" ])
         ~f:(fun c -> c.id)
       : string list)
    , (Tool_pairing.unanswered [ assistant [ "a" ]; Message.user "x" ]
       : Content.Tool_call.t list)];
  [%expect {| ((a c) ()) |}]
;;

let%expect_test "the Anthropic request is repaired" =
  let body =
    Anthropic.For_testing.request_body
      ~oauth:false
      { Provider.Request.model =
          Option.value_exn (Model.find "claude-sonnet-4-5")
      ; system = None
      ; messages =
          [ Message.user "go"; assistant [ "toolu_1" ]; Message.user "again" ]
      ; tools = []
      ; thinking = Off
      ; max_tokens = Some 100
      ; cache_key = None
      }
  in
  print_endline
    (Jsonaf.to_string_hum (Option.value_exn (Jsonaf.member "messages" body)));
  [%expect
    {|
    [
      {
        "role": "user",
        "content": [
          {
            "type": "text",
            "text": "go"
          }
        ]
      },
      {
        "role": "assistant",
        "content": [
          {
            "type": "tool_use",
            "id": "toolu_1",
            "name": "bash",
            "input": {}
          }
        ]
      },
      {
        "role": "user",
        "content": [
          {
            "type": "tool_result",
            "tool_use_id": "toolu_1",
            "content": "[interrupted: prigh stopped before this tool call returned (e.g. the backend restarted), so whether it ran, and what it did, is unknown: check before relying on it]",
            "is_error": true
          },
          {
            "type": "text",
            "text": "again",
            "cache_control": {
              "type": "ephemeral"
            }
          }
        ]
      }
    ]
    |}]
;;

let%expect_test
    "after a restart mid-tool, the next run answers the call first and the \
     request is well-formed"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let dir = Filename.concat t.dir "sessions" in
  let session = Session.create ~dir ~cwd:t.dir () in
  List.iter
    [ Message.user "build it"; assistant [ "c1" ] ]
    ~f:(fun m -> ignore (Session.append_message session m : Session.Entry.t));
  let provider =
    Faux_provider.create
      ~on_request:(fun r ->
        print_endline "request:";
        show r.messages)
      [ Reply.text "carrying on" ]
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:dir
      ~home:t.dir
      ~session
      ~cwd:t.dir
      ()
  in
  Or_error.ok_exn (Agent.prompt agent "did it work?");
  Agent.wait_idle agent;
  print_endline "session:";
  show (Agent.messages agent);
  [%expect
    {|
    request:
    user build it
    assistant calls [c1]
      result c1: [interrupted: prigh stopped before this tool call returned (e.g. the backend restarted), so whether it ran, and what it did, is unknown: check before relying on it]
    user did it work?
    session:
    user build it
    assistant calls [c1]
      result c1: [interrupted: prigh stopped before this tool call returned (e.g. the backend restarted), so whether it ran, and what it did, is unknown: check before relying on it]
    user did it work?
    assistant calls []
    |}]
;;
