open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* Dictation's speech to text: the multipart request, where it goes for
   each provider, and what goes wrong. *)

let visible s =
  String.concat_map s ~f:(function
    | '\r' -> "\\r"
    | '\n' -> "\n"
    | c when Char.is_print c -> String.of_char c
    | c -> sprintf "\\x%02x" (Char.to_int c))
;;

let custom ~name json =
  fst (ok_exn (Custom_provider.of_json ~name (Json.of_string json)))
;;

let providers =
  [ custom
      ~name:"aiproxy"
      {|{"base_url": "http://localhost:3000/v1", "api": "chat",
         "headers": {"X-Team": "infra"}}|}
  ; custom
      ~name:"gateway"
      {|{"base_url": "http://localhost:4000/v1", "api": "anthropic"}|}
  ]
;;

(* Answers each request with the next of [replies], printing it. *)
let fake_http replies : Transcription.Http.t =
  let replies = Queue.of_list replies in
  fun ?cancel:_ ~url ~headers ~body () ->
    printf "POST %s\n" url;
    List.iter headers ~f:(fun (k, v) -> printf "  %s: %s\n" k v);
    printf "%s\n" (visible body);
    match Queue.dequeue replies with
    | Some reply -> reply
    | None -> Or_error.error_string "no more replies"
;;

let run ?(env_vars = []) ?(store = fun _ -> ()) ~model ?(http = []) audio =
  with_sandbox
  @@ fun t ->
  let auth = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  store auth;
  let result =
    Transcription.transcribe
      ~env:t.env
      ~getenv:(fun name -> List.Assoc.find env_vars ~equal:String.equal name)
      ~boundary:"BOUNDARY"
      ~http:(fake_http http)
      ~store:auth
      ~providers
      ~model
      audio
  in
  print_s [%sexp (result : string Or_error.t)]
;;

let webm = { Transcription.Audio.mime_type = "audio/webm"; data = "WEBM" }

let%expect_test "the multipart body: model, response_format, then the file" =
  print_endline
    (visible
       (Transcription.multipart_body
          ~boundary:"b0undary"
          ~model:"gpt-4o-mini-transcribe"
          ~filename:"speech.webm"
          { mime_type = "audio/webm;codecs=opus"; data = "RAW\x00BYTES" }));
  [%expect
    {|
    --b0undary\r
    Content-Disposition: form-data; name="model"\r
    \r
    gpt-4o-mini-transcribe\r
    --b0undary\r
    Content-Disposition: form-data; name="response_format"\r
    \r
    json\r
    --b0undary\r
    Content-Disposition: form-data; name="file"; filename="speech.webm"\r
    Content-Type: audio/webm\r
    \r
    RAW\x00BYTES\r
    --b0undary--\r
    |}]
;;

let%expect_test "file names by mime type" =
  List.iter
    [ "audio/webm"
    ; "audio/webm;codecs=opus"
    ; "video/webm"
    ; "audio/ogg; codecs=opus"
    ; "audio/mp4"
    ; "audio/x-m4a"
    ; "audio/mpeg"
    ; "audio/wav"
    ; "audio/x-wav"
    ; "AUDIO/WAV"
    ; "audio/flac"
    ; ""
    ]
    ~f:(fun mime_type ->
      printf
        "%-24s %s\n"
        mime_type
        (match Transcription.Audio.filename ~mime_type with
         | Ok name -> name
         | Error e -> Error.to_string_hum e));
  [%expect
    {|
    audio/webm               speech.webm
    audio/webm;codecs=opus   speech.webm
    video/webm               speech.webm
    audio/ogg; codecs=opus   speech.ogg
    audio/mp4                speech.m4a
    audio/x-m4a              speech.m4a
    audio/mpeg               speech.mp3
    audio/wav                speech.wav
    audio/x-wav              speech.wav
    AUDIO/WAV                speech.wav
    audio/flac               cannot transcribe audio/flac audio: record webm, ogg, mp4, mpeg or wav
                             cannot transcribe untyped audio: record webm, ogg, mp4, mpeg or wav
    |}]
;;

let%expect_test "openai: the API key from the environment, the text back" =
  run
    ~env_vars:[ "OPENAI_API_KEY", "sk-env" ]
    ~model:"openai/gpt-4o-mini-transcribe"
    ~http:[ Ok (200, {|{"text": " Hello there. \n"}|}) ]
    webm;
  [%expect
    {|
    POST https://api.openai.com/v1/audio/transcriptions
      Content-Type: multipart/form-data; boundary=BOUNDARY
      Authorization: Bearer sk-env
    --BOUNDARY\r
    Content-Disposition: form-data; name="model"\r
    \r
    gpt-4o-mini-transcribe\r
    --BOUNDARY\r
    Content-Disposition: form-data; name="response_format"\r
    \r
    json\r
    --BOUNDARY\r
    Content-Disposition: form-data; name="file"; filename="speech.webm"\r
    Content-Type: audio/webm\r
    \r
    WEBM\r
    --BOUNDARY--\r

    (Ok "Hello there.")
    |}]
;;

let%expect_test "openai: a stored key wins over the environment" =
  run
    ~env_vars:[ "OPENAI_API_KEY", "sk-env" ]
    ~store:(fun auth ->
      ok_exn (Auth_store.set auth Openai (Api_key "sk-stored")))
    ~model:"openai/whisper-1"
    ~http:[ Ok (200, {|{"text": "Hi"}|}) ]
    { mime_type = "audio/ogg"; data = "x" };
  [%expect
    {|
    POST https://api.openai.com/v1/audio/transcriptions
      Content-Type: multipart/form-data; boundary=BOUNDARY
      Authorization: Bearer sk-stored
    --BOUNDARY\r
    Content-Disposition: form-data; name="model"\r
    \r
    whisper-1\r
    --BOUNDARY\r
    Content-Disposition: form-data; name="response_format"\r
    \r
    json\r
    --BOUNDARY\r
    Content-Disposition: form-data; name="file"; filename="speech.ogg"\r
    Content-Type: audio/ogg\r
    \r
    x\r
    --BOUNDARY--\r

    (Ok Hi)
    |}]
;;

let%expect_test "a custom provider: its base URL, headers and key" =
  run
    ~store:(fun auth ->
      ok_exn (Auth_store.set auth (Custom "aiproxy") (Api_key "sk-proxy")))
    ~model:"aiproxy/whisper-large"
    ~http:[ Ok (200, {|{"text": "Proxied"}|}) ]
    { mime_type = "audio/mp4"; data = "m" };
  [%expect
    {|
    POST http://localhost:3000/v1/audio/transcriptions
      Content-Type: multipart/form-data; boundary=BOUNDARY
      Authorization: Bearer sk-proxy
      X-Team: infra
    --BOUNDARY\r
    Content-Disposition: form-data; name="model"\r
    \r
    whisper-large\r
    --BOUNDARY\r
    Content-Disposition: form-data; name="response_format"\r
    \r
    json\r
    --BOUNDARY\r
    Content-Disposition: form-data; name="file"; filename="speech.m4a"\r
    Content-Type: audio/mp4\r
    \r
    m\r
    --BOUNDARY--\r

    (Ok Proxied)
    |}];
  (* Without a key (a local server): no authorisation. *)
  with_sandbox (fun t ->
    let auth = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
    print_s
      [%sexp
        (Transcription.target
           ~env:t.env
           ~getenv:(fun _ -> None)
           ~store:auth
           ~providers
           "aiproxy/whisper"
         : Transcription.Target.t Or_error.t)]);
  [%expect
    {|
    (Ok
     ((url http://localhost:3000/v1/audio/transcriptions) (headers (X-Team))
      (model whisper)))
    |}]
;;

let%expect_test "no OpenAI key, and providers that cannot transcribe" =
  List.iter
    [ "openai/gpt-4o-mini-transcribe"
    ; "anthropic/claude-sonnet-5"
    ; "deepseek/deepseek-chat"
    ; "openai-codex/gpt-5"
    ; "gateway/whisper"
    ; "nosuch/whisper"
    ; "whisper-1"
    ; "openai/"
    ]
    ~f:(fun model ->
      print_endline model;
      run ~model webm);
  [%expect
    {|
    openai/gpt-4o-mini-transcribe
    (Error
     "No transcription provider: sign in to openai with an API key (/login openai) or choose another dictation model (/dictation model)")
    anthropic/claude-sonnet-5
    (Error
     "Anthropic cannot transcribe: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe)")
    deepseek/deepseek-chat
    (Error
     "DeepSeek cannot transcribe: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe)")
    openai-codex/gpt-5
    (Error
     "OpenAI Codex (ChatGPT) cannot transcribe: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe)")
    gateway/whisper
    (Error
     "gateway (an Anthropic API) cannot transcribe: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe)")
    nosuch/whisper
    (Error
     "No transcription provider: there is no provider \"nosuch\" (add an OpenAI-compatible one with /login custom) or choose another dictation model (/dictation model)")
    whisper-1
    (Error
     "dictation model \"whisper-1\" is not provider/id: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe)")
    openai/
    (Error
     "dictation model \"openai/\" is not provider/id: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe)")
    |}]
;;

let%expect_test "too large, empty, or not audio: refused before any request" =
  let key = [ "OPENAI_API_KEY", "sk-env" ] in
  let model = "openai/whisper-1" in
  run
    ~env_vars:key
    ~model
    { mime_type = "audio/webm"
    ; data = String.make (Transcription.Audio.max_bytes + 1) 'a'
    };
  run ~env_vars:key ~model { mime_type = "audio/webm"; data = "" };
  run ~env_vars:key ~model { mime_type = "text/plain"; data = "hi" };
  [%expect
    {|
    (Error
     "the recording is 25.0 MB, over the 25 MB a transcription takes: record a shorter one")
    (Error "the recording is empty: nothing to transcribe")
    (Error
     "cannot transcribe text/plain audio: record webm, ogg, mp4, mpeg or wav")
    |}]
;;

let%expect_test "HTTP errors, failed connections and odd responses" =
  List.iter
    [ Ok
        ( 401
        , {|{"error": {"message": "Incorrect API key provided", "type": "invalid_request_error"}}|}
        )
    ; Ok (404, {|{"error": {"message": "model not found"}}|})
    ; Ok (500, "upstream exploded")
    ; Ok (200, "<html>")
    ; Ok (200, {|{"transcript": "x"}|})
    ; Or_error.error_string "api.openai.com: connection refused"
    ]
    ~f:(fun reply ->
      with_sandbox
      @@ fun t ->
      let auth = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
      print_s
        [%sexp
          (Transcription.transcribe
             ~env:t.env
             ~getenv:(fun _ -> Some "sk-env")
             ~http:(fun ?cancel:_ ~url:_ ~headers:_ ~body:_ () -> reply)
             ~store:auth
             ~providers
             ~model:"openai/whisper-1"
             webm
           : string Or_error.t)]);
  [%expect
    {|
    (Error
     "transcription with openai/whisper-1 failed: HTTP 401: Incorrect API key provided (the server refused the API key: /login openai changes it, or choose another dictation model (/dictation model))")
    (Error
     "transcription with openai/whisper-1 failed: HTTP 404: model not found (is whisper-1 a transcription model? Choose another dictation model (/dictation model))")
    (Error
     "transcription with openai/whisper-1 failed: HTTP 500: upstream exploded")
    (Error
     "transcription with openai/whisper-1 failed: the response is not JSON: <html>")
    (Error
     "transcription with openai/whisper-1 failed: the response has no text: {\"transcript\": \"x\"}")
    (Error
     ("transcription with openai/whisper-1 failed"
      "api.openai.com: connection refused"))
    |}]
;;

let%expect_test "check_model: what set_config accepts" =
  List.iter
    [ "openai/whisper-1"; "aiproxy/whisper"; "gateway/whisper"; "anthropic/x" ]
    ~f:(fun model ->
      print_s
        [%sexp
          (model : string)
        , (Transcription.check_model ~providers model : unit Or_error.t)]);
  [%expect
    {|
    (openai/whisper-1 (Ok ()))
    (aiproxy/whisper (Ok ()))
    (gateway/whisper
     (Error
      "gateway (an Anthropic API) cannot transcribe: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe)"))
    (anthropic/x
     (Error
      "Anthropic cannot transcribe: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe)"))
    |}]
;;

let%expect_test "rpc: transcribe and set_config's dictation_model" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let store = Auth_store.create ~path:(Filename.concat t.dir "auth.json") in
  let key = ref None in
  let login =
    Login_manager.create
      ~env:t.env
      ~sw
      ~getenv:(function
        | "OPENAI_API_KEY" -> !key
        | _ -> None)
      ~store
      ()
  in
  let provider = Faux_provider.create [] in
  let sessions_dir = Filename.concat t.dir "sessions" in
  let new_agent ?session ~cwd () =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir
      ~home:t.dir
      ?session
      ~cwd
      ()
  in
  let transcription_http ?cancel:_ ~url ~headers:_ ~body () =
    let model =
      List.find_map (String.split body ~on:'\n') ~f:(fun line ->
        Option.some_if
          (String.is_prefix line ~prefix:"whisper"
           || String.is_prefix line ~prefix:"gpt-")
          (String.rstrip line))
      |> Option.value ~default:"?"
    in
    printf "(POST %s, model %s, %d bytes)\n" url model (String.length body);
    Ok (200, {|{"text": "dictated words"}|})
  in
  let server =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login
      ~transcription_http
      ~sessions_dir
      ~cwd:t.dir
      ~new_agent
      ()
  in
  let client = Rpc_server.connect server ~send:ignore in
  let call meth params =
    let response =
      Rpc_server.handle
        server
        client
        (Json.of_string
           (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
    in
    match Json.member "result" response, Json.member "error" response with
    | Some result, _ -> print_endline (Json.to_string result)
    | None, Some (`String e) -> printf "error: %s\n" e
    | _ -> print_endline (Json.to_string response)
  in
  let audio = Base64.encode_exn "OGGS" in
  let transcribe () =
    call "transcribe" (sprintf {|{"mime_type": "audio/ogg", "data": %S}|} audio)
  in
  transcribe ();
  [%expect
    {| error: No transcription provider: sign in to openai with an API key (/login openai) or choose another dictation model (/dictation model) |}];
  key := Some "sk-env";
  transcribe ();
  [%expect
    {|
    (POST https://api.openai.com/v1/audio/transcriptions, model gpt-4o-mini-transcribe, 407 bytes)
    {"text":"dictated words"}
    |}];
  call "set_config" {|{"config": {"dictation_model": "openai/whisper-1"}}|};
  call "set_config" {|{"config": {"dictation_model": "anthropic/claude"}}|};
  call "set_config" {|{"config": {"dictation_model": 3}}|};
  let config () =
    Json.member
      "dictation_model"
      (Config.to_json (ok_exn (Config.load ~home:t.dir)))
    |> Option.value_map ~default:"absent" ~f:Json.to_string
    |> print_endline
  in
  config ();
  [%expect
    {|
    {"scoped_models":[],"approval_mode":"auto","permissions":{"allow":[],"ask":[],"deny":[]},"default_model":null,"default_thinking":null,"fallback_models":[],"default_cwd":null,"worktrees_dir":"~/.prigh/worktrees","max_budget_usd":null,"goal_max_continuations":20,"resume_on_limit_reset":true,"ask_user_timeout_seconds":300,"archive_after_days":7,"notify":"bell","snippets":[],"auto_background_seconds":30,"generate_model":null,"mask_observations":true,"computer_screenshots":3,"computer_screenshots_max":8,"computer_native":true,"compact_at_tokens":200000,"compaction_model":null,"naming_model":null,"dictation_model":"openai/whisper-1","trace":"default"}
    error: dictation_model: Anthropic cannot transcribe: pick an OpenAI or OpenAI-compatible model with /dictation model (e.g. openai/gpt-4o-mini-transcribe)
    error: config.dictation_model must be a model's provider/id (or null: openai/gpt-4o-mini-transcribe)
    "openai/whisper-1"
    |}];
  transcribe ();
  [%expect
    {|
    (POST https://api.openai.com/v1/audio/transcriptions, model whisper-1, 394 bytes)
    {"text":"dictated words"}
    |}];
  call "set_config" {|{"config": {"dictation_model": null}}|};
  config ();
  [%expect
    {|
    {"scoped_models":[],"approval_mode":"auto","permissions":{"allow":[],"ask":[],"deny":[]},"default_model":null,"default_thinking":null,"fallback_models":[],"default_cwd":null,"worktrees_dir":"~/.prigh/worktrees","max_budget_usd":null,"goal_max_continuations":20,"resume_on_limit_reset":true,"ask_user_timeout_seconds":300,"archive_after_days":7,"notify":"bell","snippets":[],"auto_background_seconds":30,"generate_model":null,"mask_observations":true,"computer_screenshots":3,"computer_screenshots_max":8,"computer_native":true,"compact_at_tokens":200000,"compaction_model":null,"naming_model":null,"dictation_model":null,"trace":"default"}
    null
    |}];
  call "transcribe" {|{"mime_type": "audio/ogg", "data": "not base64!"}|};
  call "transcribe" {|{"data": "T0dHUw=="}|};
  call "transcribe" {|{"mime_type": "audio/flac", "data": "T0dHUw=="}|};
  call
    "transcribe"
    (sprintf
       {|{"mime_type": "audio/webm", "data": %S}|}
       (String.make ((Transcription.Audio.max_bytes / 3 * 4) + 8) 'A'));
  [%expect
    {|
    error: param "data" must be base64: Malformed input
    error: missing param "mime_type"
    error: cannot transcribe audio/flac audio: record webm, ogg, mp4, mpeg or wav
    error: the recording is 25.0 MB, over the 25 MB a transcription takes: record a shorter one
    |}]
;;
