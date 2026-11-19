open! Core
open! Prigh

let model = Result.ok_or_failwith (Model.find "gemini/gemini-4-hydrogen")

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
        ; result "first"
        ; result "gemini_local_1"
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
