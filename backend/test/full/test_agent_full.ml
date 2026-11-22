open! Core
open! Prigh
open Tool_test_helpers
open! Test_agent

let%expect_test "Anthropic requests keep a stable, cached prefix across turns" =
  let anthropic = Option.value_exn (Model.default_for Anthropic) in
  let requests = Queue.create () in
  (* As if Claude had written the replies, so that their thinking is sent
     back. *)
  let by_claude : Message.t -> Message.t = function
    | Assistant a -> Assistant { a with model = Model.key anthropic }
    | (User _ | Tool_result _) as m -> m
  in
  let on_request (r : Provider.Request.t) =
    Queue.enqueue
      requests
      (Anthropic.For_testing.request_body
         ~oauth:true
         { r with
           model = anthropic
         ; thinking = On None
         ; messages = List.map r.messages ~f:by_claude
         })
  in
  let thinking_tool_calls calls =
    let reply = Reply.tool_calls ~text:"Looking." calls in
    { reply with
      events =
        [ Assistant_event.Thinking_delta "Plan."; Thinking_signature "SIG" ]
        @ reply.events
    }
  in
  let echo i = sprintf "c%d" i, "bash", sprintf {|{"command":"echo %d"}|} i in
  with_agent
    ~on_request
    [ thinking_tool_calls [ echo 1; echo 2 ]
    ; Reply.text "first done"
    ; (* more blocks than the provider looks back from one breakpoint *)
      thinking_tool_calls (List.init 12 ~f:echo)
    ; Reply.text "second done"
    ]
  @@ fun _t agent _dump ->
  Or_error.ok_exn (Agent.prompt agent "one");
  Agent.wait_idle agent;
  Or_error.ok_exn (Agent.prompt agent "two");
  Agent.wait_idle agent;
  let member name json = Option.value_exn (Json.member name json) in
  let cached block = Option.is_some (Json.member "cache_control" block) in
  let uncached = function
    | `Object fields ->
      `Object
        (List.filter fields ~f:(fun (name, _) ->
           not (String.equal name "cache_control")))
    | other -> other
  in
  let blocks body =
    List.concat_map
      (Json.list_exn (member "messages" body))
      ~f:(fun m ->
        let role = Json.string_exn (member "role" m) in
        List.map (Json.list_exn (member "content" m)) ~f:(fun b -> role, b))
  in
  let show (role, block) =
    let type_ = Json.string_exn (member "type" block) in
    sprintf "%s:%s%s" role type_ (if cached block then "*" else "")
  in
  let same_blocks a b =
    List.equal
      (fun (r1, b1) (r2, b2) ->
         String.equal r1 r2
         && String.equal
              (Json.to_string (uncached b1))
              (Json.to_string (uncached b2)))
      a
      b
  in
  let fixed body =
    Json.to_string (member "tools" body) ^ Json.to_string (member "system" body)
  in
  let previous = ref None in
  Queue.iteri requests ~f:(fun i body ->
    let blocks = blocks body in
    let breakpoints =
      List.count blocks ~f:(fun (_, b) -> cached b)
      + List.count (Json.list_exn (member "system" body)) ~f:cached
      + List.count (Json.list_exn (member "tools" body)) ~f:cached
    in
    let continues =
      match !previous with
      | None -> "first"
      | Some (fixed_before, before) ->
        sprintf
          "tools and system unchanged: %b, extends the last request: %b"
          (String.equal fixed_before (fixed body))
          (List.length before <= List.length blocks
           && same_blocks before (List.take blocks (List.length before)))
    in
    printf
      "request %d (%d breakpoints; %s)\n  %s\n"
      (i + 1)
      breakpoints
      continues
      (String.concat ~sep:" " (List.map blocks ~f:show));
    previous := Some (fixed body, blocks));
  [%expect
    {|
    request 1 (2 breakpoints; first)
      user:text*
    request 2 (3 breakpoints; tools and system unchanged: true, extends the last request: true)
      user:text* assistant:thinking assistant:text assistant:tool_use assistant:tool_use user:tool_result user:tool_result*
    request 3 (3 breakpoints; tools and system unchanged: true, extends the last request: true)
      user:text assistant:thinking assistant:text assistant:tool_use assistant:tool_use user:tool_result user:tool_result* assistant:text user:text*
    request 4 (3 breakpoints; tools and system unchanged: true, extends the last request: true)
      user:text assistant:thinking assistant:text assistant:tool_use assistant:tool_use user:tool_result user:tool_result assistant:text user:text* assistant:thinking assistant:text assistant:tool_use assistant:tool_use assistant:tool_use assistant:tool_use assistant:tool_use assistant:tool_use assistant:tool_use assistant:tool_use assistant:tool_use assistant:tool_use assistant:tool_use assistant:tool_use user:tool_result user:tool_result user:tool_result user:tool_result user:tool_result user:tool_result user:tool_result user:tool_result user:tool_result user:tool_result user:tool_result user:tool_result*
    |}]
;;
