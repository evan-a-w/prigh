open! Core
open! Prigh

let model = Option.value_exn (Model.find "gemini/gemini-4-hydrogen")

let%expect_test "parallel function signatures, ids, and token usage round-trip" =
  let module T = Gemini.For_testing in
  let assistant =
    T.response
      ~model
      [ Jsonaf.of_string
          {|{"candidates":[{"content":{"parts":[{"text":"Planning","thought":true},
              {"functionCall":{"id":"first","name":"read","args":{"path":"a"}},"thoughtSignature":"opaque"},
              {"functionCall":{"name":"read","args":{"path":"b"}}}]},"finishReason":"STOP"}],
             "usageMetadata":{"promptTokenCount":100,"cachedContentTokenCount":80,
                              "candidatesTokenCount":5,"thoughtsTokenCount":7}}|}
      ]
  in
  print_s
    [%sexp (assistant.stop_reason : Stop_reason.t), (assistant.usage : Usage.t)];
  let result id =
    Message.Tool_result
      { tool_call_id = id
      ; tool_name = "read"
      ; text = "data"
      ; is_error = false
      ; images = []
      }
  in
  let request : Provider.Request.t =
    { model
    ; messages =
        [ Message.user "Open both files"
        ; Assistant assistant
        ; result "gemini_local_1"
        ; result "first"
        ]
    ; system = None
    ; tools = []
    ; thinking = On (Some Minimal)
    ; max_tokens = Some 256
    ; cache_key = None
    }
  in
  print_endline (Jsonaf.to_string (T.request_body request));
  [%expect
    {|
    (Tool_use ((input 100) (output 12) (cache_read 80)))
    {"contents":[{"role":"user","parts":[{"text":"Open both files"}]},{"role":"model","parts":[{"functionCall":{"name":"read","args":{"path":"a"},"id":"first"},"thoughtSignature":"opaque"},{"functionCall":{"name":"read","args":{"path":"b"}}}]},{"role":"user","parts":[{"functionResponse":{"name":"read","response":{"output":"data"},"id":"first"}},{"functionResponse":{"name":"read","response":{"output":"data"}}}]}],"generationConfig":{"thinkingConfig":{"includeThoughts":true,"thinkingLevel":"LOW"},"maxOutputTokens":256}}
    |}]
;;

let%expect_test
    "signed streamed text (including a final empty part) retains its boundaries"
  =
  let module T = Gemini.For_testing in
  let chunk parts =
    Jsonaf.of_string
      (sprintf {|{"candidates":[{"content":{"parts":[%s]}}]}|} parts)
  in
  let assistant =
    T.response
      ~model
      [ chunk {|{"thought":true,"text":"idea","thoughtSignature":"A"}|}
      ; chunk {|{"text":"plain"},{"text":"signed","thoughtSignature":"B"}|}
      ; chunk {|{"text":"","thoughtSignature":"C"}|}
      ; Jsonaf.of_string {|{"candidates":[{"finishReason":"STOP"}]}|}
      ]
  in
  let request : Provider.Request.t =
    { model
    ; messages = [ Message.user "hi"; Assistant assistant ]
    ; system = None
    ; tools = []
    ; thinking = Off
    ; max_tokens = None
    ; cache_key = None
    }
  in
  Jsonaf.member "contents" (T.request_body request)
  |> Option.value_exn
  |> Jsonaf.to_string
  |> print_endline;
  [%expect
    {| [{"role":"user","parts":[{"text":"hi"}]},{"role":"model","parts":[{"text":"idea","thought":true,"thoughtSignature":"A"},{"text":"plain"},{"text":"signed","thoughtSignature":"B"},{"text":"","thoughtSignature":"C"}]}] |}]
;;

let%expect_test "stream errors do not look like a completed turn" =
  let reason chunks =
    let a =
      Gemini.For_testing.response ~model (List.map chunks ~f:Jsonaf.of_string)
    in
    print_s [%sexp (a.stop_reason : Stop_reason.t)]
  in
  reason [ {|{"candidates":[{"content":{"parts":[{"text":"partial"}]}}]}|} ];
  reason [ {|{"promptFeedback":{"blockReason":"SAFETY"}}|} ];
  reason [ {|{"candidates":[{"finishReason":"MAX_TOKENS"}]}|} ];
  [%expect
    {|
    (Error "Gemini stream ended without a finish reason")
    (Error "Gemini blocked the prompt: SAFETY")
    Length
    |}]
;;
