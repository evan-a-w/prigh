open! Core
open! Prigh
open Tool_test_helpers
module Server = Fake_http_server
module Json = Jsonaf

let port_re = Re.compile (Re.Perl.re {|127\.0\.0\.1:[0-9]+|})
let mask t s = mask t s |> Re.replace_string port_re ~by:"127.0.0.1:PORT"
let store t = Auth_store.create ~path:(Filename.concat t.dir "auth.json")
let no_env _ = None
let write_config t json = write t ".prigh/config.json" json

let show_file t path =
  match Sys_unix.file_exists_exn (Filename.concat t.dir path) with
  | false -> printf "%s: (none)\n" path
  | true -> printf "%s:\n%s\n" path (mask t (read t path))
;;

let models_body ?(extra = []) ids =
  Json.to_string
    (`Object
        [ "object", `String "list"
        ; ( "data"
          , `Array
              (List.map ids ~f:(fun id ->
                 `Object
                   ([ "id", `String id; "object", `String "model" ]
                    @ Option.value
                        (List.Assoc.find extra ~equal:String.equal id)
                        ~default:[]))) )
        ])
;;

let sse chunks =
  String.concat (List.map chunks ~f:(fun c -> "data: " ^ c ^ "\n\n"))
  ^ "data: [DONE]\n\n"
;;

let sse_reply chunks : Server.Reply.t =
  { status = 200
  ; headers = [ "Content-Type", "text/event-stream" ]
  ; chunks = [ 0., sse chunks ]
  }
;;

let is_get_models (r : Server.Request.t) =
  String.is_prefix r.request_line ~prefix:"GET /v1/models"
;;

(* An OpenAI-compatible server: lists [models] (only with [key], if given)
   and answers chat completions from [replies] in order. *)
let start_server
      ~(t : Tool_test_helpers.t)
      ~sw
      ?key
      ?(models = [ "gpt-4o" ])
      ?(replies = [])
      ()
  =
  let replies = Queue.of_list replies in
  Server.start ~sw ~env:t.env ~handler:(fun r ->
    let authorised =
      match key with
      | None -> true
      | Some key ->
        Option.equal
          String.equal
          (Server.Request.header r "authorization")
          (Some ("Bearer " ^ key))
    in
    if not authorised
    then Server.Reply.simple 401 {|{"error":{"message":"Invalid token"}}|}
    else if is_get_models r
    then Server.Reply.simple 200 (models_body models)
    else (
      match Queue.dequeue replies with
      | Some reply -> sse_reply reply
      | None ->
        Server.Reply.simple 500 {|{"error":{"message":"no more replies"}}|}))
;;

let registry ?(getenv = no_env) t ~sw =
  Model_registry.create
    ~env:t.env
    ~sw
    ~auto_fetch:false
    ~home:t.dir
    ~store:(store t)
    ~getenv
    ()
;;

let show_models t registry =
  List.iter (Model_registry.models registry) ~f:(fun (m : Model.t) ->
    if Provider_id.is_custom m.provider
    then
      printf
        "%s  name=%s ctx=%d max=%d thinking=%b images=%b cost=%g/%g/%g\n"
        (Model.key m)
        m.name
        m.context_window
        m.max_output
        m.supports_thinking
        m.supports_images
        m.cost.input
        m.cost.output
        m.cost.cache_read);
  List.iter (Model_registry.problems registry) ~f:(fun p ->
    print_endline ("problem: " ^ mask t p))
;;

let%expect_test "names, base URLs and environment variables" =
  List.iter
    [ "aiproxy"
    ; " my-proxy_2 "
    ; "AI Proxy"
    ; "2fast"
    ; "anthropic"
    ; "custom"
    ; ""
    ; "a.b"
    ]
    ~f:(fun name ->
      print_s
        [%sexp
          (name : string)
        , (Custom_provider.validate_name name : string Or_error.t)]);
  List.iter
    [ "http://localhost:3000/v1"
    ; " http://localhost:3000/v1/ "
    ; "https://openrouter.ai/api/v1/chat/completions"
    ; "http://localhost:11434/v1/models/"
    ; "localhost:3000"
    ; "ftp://example.com"
    ; "http://"
    ; "http://h/v1?x=1"
    ]
    ~f:(fun url ->
      print_s
        [%sexp
          (url : string)
        , (Custom_provider.validate_base_url url : string Or_error.t)]);
  print_s
    [%sexp
      (List.map [ "aiproxy"; "my-proxy" ] ~f:Custom_provider.env_var
       : string list)];
  [%expect
    {|
    (aiproxy (Ok aiproxy))
    (" my-proxy_2 " (Ok my-proxy_2))
    ("AI Proxy"
     (Error
      "\"AI Proxy\" is not a valid name: use lowercase letters, digits, - and _, starting with a letter (e.g. ai-proxy)"))
    (2fast
     (Error
      "\"2fast\" is not a valid name: use lowercase letters, digits, - and _, starting with a letter (e.g. fast)"))
    (anthropic
     (Error
      "\"anthropic\" is a built-in provider: choose another name (e.g. my-anthropic)"))
    (custom
     (Error
      "\"custom\" is a built-in provider: choose another name (e.g. my-custom)"))
    ("" (Error "the name is empty: type a short name such as aiproxy"))
    (a.b
     (Error
      "\"a.b\" is not a valid name: use lowercase letters, digits, - and _, starting with a letter (e.g. a-b)"))
    (http://localhost:3000/v1 (Ok http://localhost:3000/v1))
    (" http://localhost:3000/v1/ " (Ok http://localhost:3000/v1))
    (https://openrouter.ai/api/v1/chat/completions
     (Ok https://openrouter.ai/api/v1))
    (http://localhost:11434/v1/models/ (Ok http://localhost:11434/v1))
    (localhost:3000
     (Error
      "\"localhost:3000\" is not an http(s) URL: type the full base URL, e.g. http://localhost:3000/v1"))
    (ftp://example.com
     (Error
      "\"ftp://example.com\" is not an http(s) URL: type the full base URL, e.g. http://localhost:3000/v1"))
    (http://
     (Error
      "\"http://\" is not an http(s) URL: type the full base URL, e.g. http://localhost:3000/v1"))
    (http://h/v1?x=1
     (Error
      "\"http://h/v1?x=1\" has a query or fragment: give only the base URL, e.g. http://localhost:3000/v1"))
    (AIPROXY_API_KEY MY_PROXY_API_KEY)
    |}]
;;

let%expect_test "config: valid entries load, bad ones are reported and skipped" =
  with_sandbox
  @@ fun t ->
  write_config
    t
    {|{
  "confirm_tools": true,
  "providers": {
    "aiproxy": {
      "base_url": "http://localhost:3000/v1/",
      "headers": { "X-Team": "infra" },
      "models": [
        { "id": "gpt-4o", "context_window": 128000, "max_output": 4096,
          "thinking": true, "images": false, "cost": { "input": 2.5, "output": 10 } },
        { "id": "broken", "context_window": "big" },
        { "name": "no id" },
        { "id": "typo", "context": 1000 }
      ]
    },
    "ollama": { "base_url": "http://localhost:11434/v1", "api": "chat", "extra": 1 },
    "gateway": { "base_url": "https://gw.example.com/v1", "api": "anthropic" },
    "nourl": { "api": "chat" },
    "badapi": { "base_url": "http://x/v1", "api": "grpc" },
    "Bad Name": { "base_url": "http://x/v1" },
    "openai": { "base_url": "http://x/v1" },
    "badheaders": { "base_url": "http://x/v1", "headers": { "X-N": 1 } }
  }
}|};
  let providers, problems = Custom_provider.load ~home:t.dir in
  print_s [%sexp (providers : Custom_provider.t list)];
  List.iter problems ~f:(fun p -> print_endline (mask t p));
  [%expect
    {|
    (((name aiproxy) (base_url http://localhost:3000/v1) (api Chat)
      (headers ((X-Team infra)))
      (models
       (((id gpt-4o) (name ()) (context_window (128000)) (max_output (4096))
         (thinking (true)) (images (false))
         (cost (((input 2.5) (output 10) (cache_read 0)))))
        ((id typo) (name ()) (context_window ()) (max_output ()) (thinking ())
         (images ()) (cost ())))))
     ((name ollama) (base_url http://localhost:11434/v1) (api Chat) (headers ())
      (models ()))
     ((name gateway) (base_url https://gw.example.com/v1) (api Anthropic)
      (headers ()) (models ())))
    providers.aiproxy.models[1].context_window must be a positive integer (tokens); that entry is ignored (in $DIR/.prigh/config.json)
    providers.aiproxy.models[2] needs an "id" (the model id the server uses); that entry is ignored (in $DIR/.prigh/config.json)
    providers.aiproxy.models[3]: unknown field "context" ignored (known: id, name, context_window, max_output, thinking, images, cost) (in $DIR/.prigh/config.json)
    providers.ollama: unknown field "extra" ignored (known: base_url, api, headers, models) (in $DIR/.prigh/config.json)
    providers.nourl needs a "base_url" such as "http://localhost:3000/v1" (in $DIR/.prigh/config.json); that provider is skipped
    providers.badapi.api must be one of: chat, responses, anthropic (in $DIR/.prigh/config.json); that provider is skipped
    providers.Bad Name: "Bad Name" is not a valid name: use lowercase letters, digits, - and _, starting with a letter (e.g. bad-name) (in $DIR/.prigh/config.json); that provider is skipped
    providers.openai: "openai" is a built-in provider: choose another name (e.g. my-openai) (in $DIR/.prigh/config.json); that provider is skipped
    providers.badheaders.headers.X-N must be a string (in $DIR/.prigh/config.json); that provider is skipped
    |}];
  write_config t {|{"providers": []}|};
  print_endline
    (mask
       t
       (Sexp.to_string_hum
          [%sexp
            (Custom_provider.load ~home:t.dir
             : Custom_provider.t list * string list)]));
  write_config t {|{"providers": |};
  print_s [%sexp (snd (Custom_provider.load ~home:t.dir) : string list)];
  [%expect
    {|
    (()
     ("\"providers\" must be an object of provider names to definitions (in $DIR/.prigh/config.json)"))
    ("json > object: not enough input; custom providers are not loaded")
    |}]
;;

let%expect_test
    "saving a provider keeps the rest of config.json, and Config.save keeps \
     providers"
  =
  with_sandbox
  @@ fun t ->
  write_config t {|{"scoped_models": ["deepseek/deepseek-flash"], "mine": 1}|};
  let p : Custom_provider.t =
    { name = "ollama"
    ; base_url = "http://localhost:11434/v1"
    ; api = Chat
    ; headers = []
    ; models = []
    }
  in
  ok_exn (Custom_provider.save ~home:t.dir p);
  ok_exn
    (Custom_provider.save
       ~home:t.dir
       { p with name = "aiproxy"; api = Responses });
  ok_exn
    (Custom_provider.save
       ~home:t.dir
       { p with base_url = "http://gpu:11434/v1" });
  ok_exn
    (Config.save
       ~home:t.dir
       { (ok_exn (Config.load ~home:t.dir)) with confirm_tools = true });
  show_file t ".prigh/config.json";
  ok_exn (Custom_provider.remove ~home:t.dir "ollama");
  print_s
    [%sexp (fst (Custom_provider.load ~home:t.dir) : Custom_provider.t list)];
  [%expect
    {|
    .prigh/config.json:
    {
      "scoped_models": [
        "deepseek/deepseek-flash"
      ],
      "mine": 1,
      "providers": {
        "ollama": {
          "base_url": "http://gpu:11434/v1",
          "api": "chat"
        },
        "aiproxy": {
          "base_url": "http://localhost:11434/v1",
          "api": "responses"
        }
      },
      "confirm_tools": true,
      "default_model": null,
      "default_thinking": null,
      "fallback_models": [],
      "default_cwd": null
    }
    (((name aiproxy) (base_url http://localhost:11434/v1) (api Responses)
      (headers ()) (models ())))
    |}]
;;

let%expect_test
    "registry: the server's list, config overrides and defaults; failures"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let up =
    Server.start ~sw ~env:t.env ~handler:(fun r ->
      if is_get_models r
      then
        Server.Reply.simple
          200
          (models_body
             ~extra:
               [ ( "meta-llama/llama-3.1-8b"
                 , [ "context_length", `Number "131072" ] )
               ]
             [ "gpt-4o"; "meta-llama/llama-3.1-8b"; "gpt-4o" ])
      else Server.Reply.simple 404 "")
  in
  let down =
    Server.start ~sw ~env:t.env ~handler:(fun _ ->
      Server.Reply.simple 404 {|{"error":{"message":"Not Found"}}|})
  in
  write_config
    t
    (sprintf
       {|{"providers": {
  "aiproxy": {
    "base_url": "%s",
    "models": [
      { "id": "local-only", "name": "Local only", "thinking": true },
      { "id": "gpt-4o", "context_window": 64000, "images": false,
        "cost": { "input": 2.5, "output": 10, "cache_read": 1.25 } }
    ]
  },
  "down": { "base_url": "%s" }
}}|}
       (Server.url up "/v1")
       (Server.url down "/v1"));
  let registry = registry t ~sw in
  print_endline "before fetching:";
  show_models t registry;
  Model_registry.refresh registry ();
  print_endline "after:";
  show_models t registry;
  [%expect
    {|
    before fetching:
    aiproxy/local-only  name=Local only ctx=128000 max=16384 thinking=true images=true cost=0/0/0
    aiproxy/gpt-4o  name=gpt-4o ctx=64000 max=16384 thinking=false images=false cost=2.5/10/1.25
    after:
    aiproxy/local-only  name=Local only ctx=128000 max=16384 thinking=true images=true cost=0/0/0
    aiproxy/gpt-4o  name=gpt-4o ctx=64000 max=16384 thinking=false images=false cost=2.5/10/1.25
    aiproxy/meta-llama/llama-3.1-8b  name=meta-llama/llama-3.1-8b ctx=131072 max=16384 thinking=false images=true cost=0/0/0
    problem: down: no model list: GET http://127.0.0.1:PORT/v1/models: HTTP 404: Not Found (the base URL usually ends in /v1). Check that the server is running and the base URL and key are right (/login down to change them); models named in config.json still work.
    |}];
  let show_find s =
    print_s
      [%sexp
        (s : string)
      , (Option.map (Model_registry.find registry s) ~f:Model.key
         : string option)]
  in
  let show_resolve s =
    print_s
      [%sexp
        (s : string)
      , (Or_error.map (Model_registry.resolve registry s) ~f:Model.key
         : string Or_error.t)]
  in
  List.iter
    [ "aiproxy/meta-llama/llama-3.1-8b"
    ; "meta-llama/llama-3.1-8b"
    ; "aiproxy/not-listed"
    ; "down/anything"
    ; "nobody/model"
    ; "deepseek/deepseek-flash"
    ]
    ~f:show_find;
  List.iter
    [ "aiproxy/not-listed"
    ; "down/anything"
    ; "meta-llama"
    ; "Local only"
    ; "aiproxy/gpt-4o"
    ]
    ~f:show_resolve;
  [%expect
    {|
    (aiproxy/meta-llama/llama-3.1-8b (aiproxy/meta-llama/llama-3.1-8b))
    (meta-llama/llama-3.1-8b (aiproxy/meta-llama/llama-3.1-8b))
    (aiproxy/not-listed (aiproxy/not-listed))
    (down/anything (down/anything))
    (nobody/model ())
    (deepseek/deepseek-flash (deepseek/deepseek-flash))
    (aiproxy/not-listed
     (Error
      "unknown model \"aiproxy/not-listed\"; did you mean: aiproxy/local-only (Local only), aiproxy/gpt-4o (gpt-4o), aiproxy/meta-llama/llama-3.1-8b (meta-llama/llama-3.1-8b)"))
    (down/anything (Ok down/anything))
    (meta-llama (Ok aiproxy/meta-llama/llama-3.1-8b))
    ("Local only" (Ok aiproxy/local-only))
    (aiproxy/gpt-4o (Ok aiproxy/gpt-4o))
    |}];
  (* Removed by hand: its models go; announcements reach subscribers. *)
  Model_registry.subscribe registry ~f:(fun m ->
    print_endline ("announced: " ^ mask t m));
  write_config
    t
    (sprintf
       {|{"providers": {"aiproxy": {"base_url": "%s", "api": 7}}}|}
       (Server.url up "/v1"));
  Model_registry.reload registry;
  show_models t registry;
  [%expect
    {|
    announced: providers.aiproxy.api must be one of: chat, responses, anthropic (in $DIR/.prigh/config.json); that provider is skipped
    problem: providers.aiproxy.api must be one of: chat, responses, anthropic (in $DIR/.prigh/config.json); that provider is skipped
    |}]
;;

let%expect_test "auth: stored key, environment, keyless; status" =
  with_sandbox
  @@ fun t ->
  let store = store t in
  let aiproxy = Provider_id.Custom "aiproxy" in
  let resolve getenv =
    print_s
      [%sexp
        (Or_error.map
           (Provider_auth.resolve ~env:t.env ~getenv store aiproxy)
           ~f:
             (Option.map ~f:(fun (r : Provider_auth.Resolved.t) ->
                r.token, r.source))
         : (string * string) option Or_error.t)]
  in
  let env = function
    | "AIPROXY_API_KEY" -> Some "sk-env"
    | _ -> None
  in
  resolve no_env;
  resolve env;
  ok_exn (Auth_store.set store aiproxy (Api_key "sk-stored"));
  resolve env;
  write
    t
    "auth.json"
    {|{"aiproxy": {"type": "oauth", "access": "a", "refresh": "r", "expires": 1}}|};
  resolve env;
  print_s
    [%sexp
      (Namespace.without_provider_keys env "AIPROXY_API_KEY" : string option)
    , (Namespace.without_provider_keys env "HOME" : string option)];
  write
    t
    "auth.json"
    {|{"aiproxy": {"type": "api_key", "key": "sk"}, "google": {"weird": true}}|};
  let custom =
    [ { Custom_provider.name = "aiproxy"
      ; base_url = "http://localhost:3000/v1"
      ; api = Chat
      ; headers = []
      ; models = []
      }
    ; { name = "ollama"
      ; base_url = "http://localhost:11434/v1"
      ; api = Chat
      ; headers = []
      ; models = []
      }
    ]
  in
  (match Provider_auth.status ~getenv:no_env ~custom store with
   | Error e -> print_s [%sexp (e : Error.t)]
   | Ok statuses ->
     List.iter statuses ~f:(fun s ->
       print_endline (Json.to_string (Rpc_json.auth_status s))));
  [%expect
    {|
    (Ok ())
    (Ok ((sk-env AIPROXY_API_KEY)))
    (Ok ((sk-stored "stored api key")))
    (Error
     "auth.json's \"aiproxy\" entry is an OAuth login, not an API key (another tool may use that name): /logout aiproxy, then /login aiproxy")
    (() ())
    {"provider":"anthropic","name":"Anthropic","methods":[{"method":"oauth","label":"Anthropic (Claude Pro/Max)"},{"method":"api_key","label":"Anthropic API key"}],"configured":null,"expires_ms":null}
    {"provider":"openai","name":"OpenAI","methods":[{"method":"api_key","label":"OpenAI API key"}],"configured":null,"expires_ms":null}
    {"provider":"openai-codex","name":"OpenAI Codex (ChatGPT)","methods":[{"method":"oauth","label":"OpenAI (ChatGPT Plus/Pro)"}],"configured":null,"expires_ms":null}
    {"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"DeepSeek API key"}],"configured":null,"expires_ms":null}
    {"provider":"aiproxy","name":"aiproxy","methods":[{"method":"api_key","label":"aiproxy API key"}],"configured":{"method":"api_key","source":"stored api key"},"expires_ms":null,"custom":{"base_url":"http://localhost:3000/v1","api":"chat","api_label":"OpenAI chat completions (/chat/completions)"}}
    {"provider":"ollama","name":"ollama","methods":[{"method":"api_key","label":"ollama API key"}],"configured":{"method":"api_key","source":"no key"},"expires_ms":null,"custom":{"base_url":"http://localhost:11434/v1","api":"chat","api_label":"OpenAI chat completions (/chat/completions)"}}
    |}]
;;

(* ---- login flows ---------------------------------------------------------- *)

let show_prompt t (p : Auth_interaction.Prompt.t) =
  match p with
  | Text { message; placeholder; default } ->
    printf
      "? %s\n  [text, placeholder %S, prefilled %S]\n"
      (mask t message)
      placeholder
      (mask t default)
  | Secret { message; allow_empty } ->
    printf
      "? %s\n  [secret%s]\n"
      message
      (if allow_empty then ", empty allowed" else "")
  | Manual_code { message; _ } -> printf "? %s\n  [code]\n" message
  | Select { message; options } ->
    printf "? %s\n" (mask t message);
    List.iter options ~f:(fun (id, label) -> printf "  - %s: %s\n" id label)
;;

module Answer = struct
  type t =
    | Type of string
    | Esc
end

(* Plays the answers given to [drive] to the flow's prompts, printing what
   the user sees. *)
let script = ref []

let follow t login =
  Login_manager.subscribe login ~f:(fun (e : Login_manager.Event.t) ->
    let remaining = script in
    match e with
    | Prompt { id; prompt } ->
      show_prompt t prompt;
      (match !remaining with
       | [] | Answer.Esc :: _ ->
         print_endline "  <Esc>";
         remaining := List.drop !remaining 1;
         Login_manager.cancel login
       | Type a :: rest ->
         remaining := rest;
         printf "  > %s\n" (mask t a);
         ok_exn (Login_manager.respond login ~id a))
    | Progress m -> printf "%s\n" (mask t m)
    | Prompt_cancelled _ -> ()
    | Auth_url _ -> print_endline "auth url?!"
    | Done { provider; method_ } ->
      print_s
        [%message
          "done" (provider : Provider_id.t) (method_ : Provider_auth.Method.t)]
    | Failed { provider; error } ->
      print_s [%message "failed" provider (mask t error)]
    | Logged_out p -> print_s [%message "logged out" (p : Provider_id.t)])
;;

let drive answers = script := answers

let with_login ?(getenv = no_env) ?(scripted = true) f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let models = registry ~getenv t ~sw in
  let login =
    Login_manager.create ~env:t.env ~sw ~getenv ~models ~store:(store t) ()
  in
  if scripted then follow t login;
  f t ~sw models login
;;

let run_flow login ?name answers =
  drive answers;
  ok_exn (Login_manager.start_custom login ?name ());
  Login_manager.wait login
;;

let%expect_test
    "/login custom: validation errors re-ask, then the check and save"
  =
  with_login
  @@ fun t ~sw models login ->
  let server =
    start_server ~t ~sw ~key:"sk-test" ~models:[ "gpt-4o"; "qwen3" ] ()
  in
  write
    t
    "auth.json"
    {|{"google": {"type": "oauth", "access": "pi's", "refresh": "r", "expires": 1}}|};
  run_flow
    login
    [ Type "AI Proxy"
    ; Type "anthropic"
    ; Type "google"
    ; Type "aiproxy"
    ; Type "localhost:3000"
    ; Type (Server.url server "/v1/chat/completions")
    ; Type "chat"
    ; Type "sk-test"
    ];
  [%expect
    {|
    ? Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)
      [text, placeholder "aiproxy", prefilled ""]
      > AI Proxy
    ? "AI Proxy" is not a valid name: use lowercase letters, digits, - and _, starting with a letter (e.g. ai-proxy)
    Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)
      [text, placeholder "aiproxy", prefilled "AI Proxy"]
      > anthropic
    ? "anthropic" is a built-in provider: choose another name (e.g. my-anthropic)
    Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)
      [text, placeholder "aiproxy", prefilled "anthropic"]
      > google
    ? auth.json already has an entry named "google" (another tool, such as pi, may use it): choose another name
    Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)
      [text, placeholder "aiproxy", prefilled "google"]
      > aiproxy
    ? Base URL of aiproxy's API (the part before /chat/completions, usually ending in /v1)
      [text, placeholder "http://localhost:3000/v1", prefilled ""]
      > localhost:3000
    ? "localhost:3000" is not an http(s) URL: type the full base URL, e.g. http://localhost:3000/v1
    Base URL of aiproxy's API (the part before /chat/completions, usually ending in /v1)
      [text, placeholder "http://localhost:3000/v1", prefilled "localhost:3000"]
      > http://127.0.0.1:PORT/v1/chat/completions
    Using http://127.0.0.1:PORT/v1 (the endpoint paths are added per request)
    ? API style of aiproxy
      - chat: OpenAI chat completions (/chat/completions) - most servers
      - responses: OpenAI Responses (/responses)
      - anthropic: Anthropic messages (/messages)
      > chat
    ? API key for aiproxy (leave empty if the server needs none)
      [secret, empty allowed]
      > sk-test
    Checking http://127.0.0.1:PORT/v1/models ...
    Found 2 models: gpt-4o, qwen3
    (done (provider (Custom aiproxy)) (method_ Api_key))
    |}];
  show_file t ".prigh/config.json";
  show_file t "auth.json";
  show_models t models;
  [%expect
    {|
    .prigh/config.json:
    {
      "providers": {
        "aiproxy": {
          "base_url": "http://127.0.0.1:PORT/v1",
          "api": "chat"
        }
      }
    }
    auth.json:
    {
      "google": {
        "type": "oauth",
        "access": "pi's",
        "refresh": "r",
        "expires": 1
      },
      "aiproxy": {
        "type": "api_key",
        "key": "sk-test"
      }
    }

    aiproxy/gpt-4o  name=gpt-4o ctx=128000 max=16384 thinking=false images=true cost=0/0/0
    aiproxy/qwen3  name=qwen3 ctx=128000 max=16384 thinking=false images=true cost=0/0/0
    |}]
;;

let%expect_test
    "/login custom: the check fails; change the settings, then save anyway"
  =
  with_login
  @@ fun t ~sw models login ->
  let server = start_server ~t ~sw ~key:"sk-right" () in
  run_flow
    login
    [ Type "litellm"
    ; Type (Server.url server "/v1")
    ; Type "chat"
    ; Type "sk-wrong"
    ; Type "edit"
    ; Type ""
    ; Type "responses"
    ; Type ""
    ; Type "save"
    ];
  [%expect
    {|
    ? Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)
      [text, placeholder "aiproxy", prefilled ""]
      > litellm
    ? Base URL of litellm's API (the part before /chat/completions, usually ending in /v1)
      [text, placeholder "http://localhost:3000/v1", prefilled ""]
      > http://127.0.0.1:PORT/v1
    ? API style of litellm
      - chat: OpenAI chat completions (/chat/completions) - most servers
      - responses: OpenAI Responses (/responses)
      - anthropic: Anthropic messages (/messages)
      > chat
    ? API key for litellm (leave empty if the server needs none)
      [secret, empty allowed]
      > sk-wrong
    Checking http://127.0.0.1:PORT/v1/models ...
    ? Could not list litellm's models: GET http://127.0.0.1:PORT/v1/models: HTTP 401: Invalid token (the server refused the API key)
    Check the base URL (it usually ends in /v1), the API key, and that the server is running.
      - save: Save anyway
      - edit: Change the settings
      - cancel: Cancel (nothing is saved)
      > edit
    ? Base URL of litellm's API (the part before /chat/completions, usually ending in /v1)
      [text, placeholder "http://localhost:3000/v1", prefilled "http://127.0.0.1:PORT/v1"]
      >
    ? API style of litellm
      - chat: OpenAI chat completions (/chat/completions) - most servers
      - responses: OpenAI Responses (/responses)
      - anthropic: Anthropic messages (/messages)
      > responses
    ? API key for litellm (leave empty if the server needs none)
      [secret, empty allowed]
      >
    Checking http://127.0.0.1:PORT/v1/models ...
    ? Could not list litellm's models: GET http://127.0.0.1:PORT/v1/models: HTTP 401: Invalid token (the server wants an API key)
    Check the base URL (it usually ends in /v1), the API key, and that the server is running.
      - save: Save anyway
      - edit: Change the settings
      - cancel: Cancel (nothing is saved)
      > save
    (done (provider (Custom litellm)) (method_ Api_key))
    |}];
  show_file t ".prigh/config.json";
  show_file t "auth.json";
  show_models t models;
  [%expect
    {|
    .prigh/config.json:
    {
      "providers": {
        "litellm": {
          "base_url": "http://127.0.0.1:PORT/v1",
          "api": "responses"
        }
      }
    }
    auth.json: (none)
    |}]
;;

let%expect_test "/login custom: Esc and Cancel save nothing" =
  with_login
  @@ fun t ~sw _models login ->
  let server = start_server ~t ~sw ~key:"sk-right" () in
  run_flow login [ Type "aiproxy"; Type (Server.url server "/v1"); Esc ];
  run_flow
    login
    [ Type "aiproxy"
    ; Type (Server.url server "/v1")
    ; Type "chat"
    ; Type "nope"
    ; Type "cancel"
    ];
  show_file t ".prigh/config.json";
  show_file t "auth.json";
  [%expect
    {|
    ? Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)
      [text, placeholder "aiproxy", prefilled ""]
      > aiproxy
    ? Base URL of aiproxy's API (the part before /chat/completions, usually ending in /v1)
      [text, placeholder "http://localhost:3000/v1", prefilled ""]
      > http://127.0.0.1:PORT/v1
    ? API style of aiproxy
      - chat: OpenAI chat completions (/chat/completions) - most servers
      - responses: OpenAI Responses (/responses)
      - anthropic: Anthropic messages (/messages)
      <Esc>
    (failed custom "login cancelled")
    ? Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)
      [text, placeholder "aiproxy", prefilled ""]
      > aiproxy
    ? Base URL of aiproxy's API (the part before /chat/completions, usually ending in /v1)
      [text, placeholder "http://localhost:3000/v1", prefilled ""]
      > http://127.0.0.1:PORT/v1
    ? API style of aiproxy
      - chat: OpenAI chat completions (/chat/completions) - most servers
      - responses: OpenAI Responses (/responses)
      - anthropic: Anthropic messages (/messages)
      > chat
    ? API key for aiproxy (leave empty if the server needs none)
      [secret, empty allowed]
      > nope
    Checking http://127.0.0.1:PORT/v1/models ...
    ? Could not list aiproxy's models: GET http://127.0.0.1:PORT/v1/models: HTTP 401: Invalid token (the server refused the API key)
    Check the base URL (it usually ends in /v1), the API key, and that the server is running.
      - save: Save anyway
      - edit: Change the settings
      - cancel: Cancel (nothing is saved)
      > cancel
    (failed custom "login cancelled")
    .prigh/config.json: (none)
    auth.json: (none)
    |}]
;;

let%expect_test
    "/login <custom>: edits it, prefilled; keep, replace or remove the key"
  =
  with_login
  @@ fun t ~sw models login ->
  let server = start_server ~t ~sw ~models:[ "llama3" ] () in
  write_config
    t
    (sprintf
       {|{"providers": {"ollama": {"base_url": "%s", "api": "responses", "headers": {"X-A": "b"},
         "models": [{"id": "llama3", "context_window": 8192}]}}}|}
       (Server.url server "/v1"));
  ok_exn (Auth_store.set (store t) (Custom "ollama") (Api_key "sk-old"));
  Model_registry.reload models;
  run_flow login ~name:"ollama" [ Type ""; Type "chat"; Type "keep" ];
  print_s
    [%sexp
      (Auth_store.read (store t) (Custom "ollama")
       : Credential.t option Or_error.t)];
  run_flow login ~name:"ollama" [ Type ""; Type ""; Type "none" ];
  print_s
    [%sexp
      (Auth_store.read (store t) (Custom "ollama")
       : Credential.t option Or_error.t)];
  show_file t ".prigh/config.json";
  show_models t models;
  [%expect
    {|
    ? Base URL of ollama's API (the part before /chat/completions, usually ending in /v1)
      [text, placeholder "http://localhost:3000/v1", prefilled "http://127.0.0.1:PORT/v1"]
      >
    ? API style of ollama
      - responses: OpenAI Responses (/responses)
      - chat: OpenAI chat completions (/chat/completions) - most servers
      - anthropic: Anthropic messages (/messages)
      > chat
    ? ollama has a stored API key
      - keep: Keep the stored key
      - new: Enter a new key
      - none: Remove it (the server needs no key)
      > keep
    Checking http://127.0.0.1:PORT/v1/models ...
    Found 1 models: llama3
    (done (provider (Custom ollama)) (method_ Api_key))
    (Ok ((Api_key sk-old)))
    ? Base URL of ollama's API (the part before /chat/completions, usually ending in /v1)
      [text, placeholder "http://localhost:3000/v1", prefilled "http://127.0.0.1:PORT/v1"]
      >
    ? API style of ollama
      - chat: OpenAI chat completions (/chat/completions) - most servers
      - responses: OpenAI Responses (/responses)
      - anthropic: Anthropic messages (/messages)
      >
    ? ollama has a stored API key
      - keep: Keep the stored key
      - new: Enter a new key
      - none: Remove it (the server needs no key)
      > none
    Checking http://127.0.0.1:PORT/v1/models ...
    Found 1 models: llama3
    (done (provider (Custom ollama)) (method_ Api_key))
    (Ok ())
    .prigh/config.json:
    {
      "providers": {
        "ollama": {
          "base_url": "http://127.0.0.1:PORT/v1",
          "api": "chat",
          "headers": {
            "X-A": "b"
          },
          "models": [
            {
              "id": "llama3",
              "context_window": 8192
            }
          ]
        }
      }
    }
    ollama/llama3  name=llama3 ctx=8192 max=8192 thinking=false images=true cost=0/0/0
    |}];
  (* The server saw the extra header. *)
  print_s
    [%sexp
      (List.filter_map (Server.requests server) ~f:(fun r ->
         Server.Request.header r "x-a")
       : string list)];
  [%expect {| (b b) |}]
;;

let%expect_test "/logout <custom>: key only, everything, or nothing" =
  with_login
  @@ fun t ~sw:_ models login ->
  write_config
    t
    {|{"providers": {"aiproxy": {"base_url": "http://localhost:3000/v1"}}}|};
  Model_registry.reload models;
  let store = store t in
  let logout answers =
    drive answers;
    ok_exn (Login_manager.logout login (Custom "aiproxy"));
    Login_manager.wait login;
    print_s
      [%sexp
        (Auth_store.read store (Custom "aiproxy")
         : Credential.t option Or_error.t)
      , (List.map (Model_registry.providers models) ~f:(fun p -> p.name)
         : string list)]
  in
  ok_exn (Auth_store.set store (Custom "aiproxy") (Api_key "sk"));
  logout [ Esc ];
  logout [ Type "key" ];
  logout [ Type "keep" ];
  logout [ Type "all" ];
  [%expect
    {|
    ? Log out of aiproxy (http://localhost:3000/v1)
      - key: Remove the API key only (keep the provider)
      - all: Remove the API key and the provider
      <Esc>
    ((Ok ((Api_key sk))) (aiproxy))
    ? Log out of aiproxy (http://localhost:3000/v1)
      - key: Remove the API key only (keep the provider)
      - all: Remove the API key and the provider
      > key
    ("logged out" (p (Custom aiproxy)))
    ((Ok ()) (aiproxy))
    ? Log out of aiproxy (http://localhost:3000/v1)
      - all: Remove the provider from config.json
      - keep: Keep it
      > keep
    ((Ok ()) (aiproxy))
    ? Log out of aiproxy (http://localhost:3000/v1)
      - all: Remove the provider from config.json
      - keep: Keep it
      > all
    ("logged out" (p (Custom aiproxy)))
    ((Ok ()) ())
    |}];
  show_file t ".prigh/config.json";
  [%expect
    {|
    .prigh/config.json:
    {
      "providers": {}
    }
    |}]
;;

(* ---- the chat-completions wire format --------------------------------------- *)

let custom_model ?(thinking = true) ?(images = true) () : Model.t =
  Custom_provider.model
    { name = "aiproxy"
    ; base_url = "http://x/v1"
    ; api = Chat
    ; headers = []
    ; models =
        [ { id = "m"
          ; name = None
          ; context_window = None
          ; max_output = None
          ; thinking = Some thinking
          ; images = Some images
          ; cost = None
          }
        ]
    }
    "m"
;;

let image = Image_fixtures.placeholder "SHOT"

let conversation =
  [ Message.user ~images:[ image ] "what is this?"
  ; Assistant
      { content =
          [ Content.thinking "Two files to read."
          ; Text "Reading."
          ; Tool_call
              { id = "call_1"; name = "read"; arguments = {|{"path":"a.png"}|} }
          ; Tool_call
              { id = "call_2"; name = "read"; arguments = {|{"path":"b.txt"}|} }
          ]
      ; stop_reason = Tool_use
      ; usage = Usage.zero
      ; model = "aiproxy/m"
      }
  ; Tool_result
      { tool_call_id = "call_1"
      ; tool_name = "read"
      ; text = ""
      ; is_error = false
      ; images = [ image; image ]
      }
  ; Tool_result
      { tool_call_id = "call_2"
      ; tool_name = "read"
      ; text = "hello"
      ; is_error = false
      ; images = []
      }
  ; Message.user "thanks"
  ]
;;

let request ?(thinking = Thinking.On (Some High)) model =
  { Provider.Request.model
  ; system = Some "Be terse."
  ; messages = conversation
  ; tools = []
  ; thinking
  ; max_tokens = None
  }
;;

let%expect_test
    "chat completions: images in user messages and after tool results, tool \
     calls, reasoning effort"
  =
  let body r =
    print_endline
      (Json.to_string_hum
         (Openai_chat.For_testing.request_body
            ~quirks:Openai_chat.Quirks.generic
            r))
  in
  body (request (custom_model ()));
  [%expect
    {|
    {
      "model": "m",
      "messages": [
        {
          "role": "system",
          "content": "Be terse."
        },
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "what is this?"
            },
            {
              "type": "image_url",
              "image_url": {
                "url": "data:image/png;base64,SHOT"
              }
            }
          ]
        },
        {
          "role": "assistant",
          "content": "Reading.",
          "tool_calls": [
            {
              "id": "call_1",
              "type": "function",
              "function": {
                "name": "read",
                "arguments": "{\"path\":\"a.png\"}"
              }
            },
            {
              "id": "call_2",
              "type": "function",
              "function": {
                "name": "read",
                "arguments": "{\"path\":\"b.txt\"}"
              }
            }
          ]
        },
        {
          "role": "tool",
          "tool_call_id": "call_1",
          "content": "[2 images attached in the next message]"
        },
        {
          "role": "tool",
          "tool_call_id": "call_2",
          "content": "hello"
        },
        {
          "role": "user",
          "content": [
            {
              "type": "text",
              "text": "[2 images from the read result call_1]"
            },
            {
              "type": "image_url",
              "image_url": {
                "url": "data:image/png;base64,SHOT"
              }
            },
            {
              "type": "image_url",
              "image_url": {
                "url": "data:image/png;base64,SHOT"
              }
            }
          ]
        },
        {
          "role": "user",
          "content": "thanks"
        }
      ],
      "stream": true,
      "stream_options": {
        "include_usage": true
      },
      "reasoning_effort": "high"
    }
    |}];
  (* No images: notes instead; thinking off sends nothing; a trailing tool
     result's images still follow it. *)
  let r =
    request ~thinking:Off (custom_model ~thinking:true ~images:false ())
  in
  let messages body =
    match Json.member "messages" body with
    | Some (`Array m) -> m
    | _ -> []
  in
  let compact r =
    let body =
      Openai_chat.For_testing.request_body ~quirks:Openai_chat.Quirks.generic r
    in
    List.iter (messages body) ~f:(fun m -> print_endline (Json.to_string m));
    print_s
      [%sexp
        (Json.member "reasoning_effort" body |> Option.map ~f:Json.to_string
         : string option)]
  in
  compact r;
  [%expect
    {|
    {"role":"system","content":"Be terse."}
    {"role":"user","content":"what is this?\n[image/png image omitted: this model cannot see images]"}
    {"role":"assistant","content":"Reading.","tool_calls":[{"id":"call_1","type":"function","function":{"name":"read","arguments":"{\"path\":\"a.png\"}"}},{"id":"call_2","type":"function","function":{"name":"read","arguments":"{\"path\":\"b.txt\"}"}}]}
    {"role":"tool","tool_call_id":"call_1","content":"[image/png image omitted: this model cannot see images]\n[image/png image omitted: this model cannot see images]"}
    {"role":"tool","tool_call_id":"call_2","content":"hello"}
    {"role":"user","content":"thanks"}
    ()
    |}];
  compact
    { (request ~thinking:(On None) (custom_model ())) with
      messages = List.take conversation 3
    };
  [%expect
    {|
    {"role":"system","content":"Be terse."}
    {"role":"user","content":[{"type":"text","text":"what is this?"},{"type":"image_url","image_url":{"url":"data:image/png;base64,SHOT"}}]}
    {"role":"assistant","content":"Reading.","tool_calls":[{"id":"call_1","type":"function","function":{"name":"read","arguments":"{\"path\":\"a.png\"}"}},{"id":"call_2","type":"function","function":{"name":"read","arguments":"{\"path\":\"b.txt\"}"}}]}
    {"role":"tool","tool_call_id":"call_1","content":"[2 images attached in the next message]"}
    {"role":"user","content":[{"type":"text","text":"[2 images from the read result call_1]"},{"type":"image_url","image_url":{"url":"data:image/png;base64,SHOT"}},{"type":"image_url","image_url":{"url":"data:image/png;base64,SHOT"}}]}
    ("\"medium\"")
    |}]
;;

let%expect_test
    "chat completions stream: reasoning, repeated ids, missing indices, usage"
  =
  let chunks =
    [ {|{"choices":[{"index":0,"delta":{"role":"assistant","reasoning":"Think"}}]}|}
    ; {|{"choices":[{"index":0,"delta":{"content":"Hi"}}]}|}
    ; {|{"choices":[{"index":0,"delta":{"tool_calls":[{"id":"a","type":"function","function":{"name":"read","arguments":"{\"pa"}}]}}]}|}
    ; {|{"choices":[{"index":0,"delta":{"tool_calls":[{"id":"a","type":"function","function":{"name":"read","arguments":"th\":1}"}}]}}]}|}
    ; {|{"choices":[{"index":0,"delta":{"tool_calls":[{"function":{"name":"ls","arguments":"{}"}}]}}]}|}
    ; {|{"choices":[{"index":0,"delta":{},"finish_reason":"stop"}]}|}
    ; {|{"choices":[],"usage":{"prompt_tokens":50,"completion_tokens":7,"prompt_tokens_details":{"cached_tokens":20}}}|}
    ]
  in
  List.iter
    (Openai_chat.For_testing.parse_stream (List.map chunks ~f:Json.of_string))
    ~f:(fun c ->
      print_s [%sexp (c : Openai_chat.For_testing.Chunk.t Or_error.t)]);
  [%expect
    {|
    (Ok ((events ((Thinking_delta Think))) (finish_reason ()) (usage ())))
    (Ok ((events ((Text_delta Hi))) (finish_reason ()) (usage ())))
    (Ok
     ((events
       ((Tool_call_start (index 0) (id a) (name read))
        (Tool_call_delta (index 0) (arguments "{\"pa"))))
      (finish_reason ()) (usage ())))
    (Ok
     ((events ((Tool_call_delta (index 0) (arguments "th\":1}"))))
      (finish_reason ()) (usage ())))
    (Ok
     ((events
       ((Tool_call_start (index 1) (id call_1) (name ls))
        (Tool_call_delta (index 1) (arguments {}))))
      (finish_reason ()) (usage ())))
    (Ok ((events ()) (finish_reason (stop)) (usage ())))
    (Ok
     ((events ()) (finish_reason ())
      (usage (((input 50) (output 7) (cache_read 20))))))
    |}]
;;

(* ---- end to end ------------------------------------------------------------- *)

let tool_call_reply =
  [ {|{"choices":[{"index":0,"delta":{"role":"assistant","content":"Let me look.","tool_calls":[{"index":0,"id":"call_1","type":"function","function":{"name":"read","arguments":""}}]}}]}|}
  ; {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"{\"path\":"}}]}}]}|}
  ; {|{"choices":[{"index":0,"delta":{"tool_calls":[{"index":0,"function":{"arguments":"\"notes.txt\"}"}}]}}]}|}
  ; {|{"choices":[{"index":0,"delta":{},"finish_reason":"tool_calls"}]}|}
  ; {|{"choices":[],"usage":{"prompt_tokens":50,"completion_tokens":10}}|}
  ]
;;

let text_reply =
  [ {|{"choices":[{"index":0,"delta":{"content":"The note says hello."}}]}|}
  ; {|{"choices":[{"index":0,"delta":{},"finish_reason":"stop"}],"usage":{"prompt_tokens":80,"completion_tokens":6}}|}
  ]
;;

let%expect_test
    "an agent run with a tool call against an OpenAI-compatible server"
  =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  write t "notes.txt" "hello\n";
  let server =
    start_server
      ~t
      ~sw
      ~key:"sk-e2e"
      ~replies:[ tool_call_reply; text_reply ]
      ()
  in
  write_config
    t
    (sprintf
       {|{"providers": {"aiproxy": {"base_url": "%s", "headers": {"X-Trace": "1"}}}}|}
       (Server.url server "/v1"));
  ok_exn (Auth_store.set (store t) (Custom "aiproxy") (Api_key "sk-e2e"));
  let models = registry t ~sw in
  Model_registry.refresh models ();
  let provider =
    Provider_router.create ~env:t.env ~getenv:no_env ~models ~store:(store t) ()
  in
  let agent =
    Agent.create
      ~env:t.env
      ~sw
      ~provider
      ~tools:Tools.all
      ~sessions_dir:(Filename.concat t.dir "sessions")
      ~home:t.dir
      ~models
      ~model:(Option.value_exn (Model_registry.find models "aiproxy/gpt-4o"))
      ~cwd:t.dir
      ()
  in
  Agent.subscribe agent ~f:(function
    | Loop (Message_end (Assistant a)) ->
      print_s [%sexp (a : Message.Assistant.t)]
    | Loop (Tool_end { result; _ }) ->
      printf "tool result: %s\n" (String.strip result.text)
    | Notice n -> print_endline ("notice: " ^ n)
    | _ -> ());
  ok_exn (Agent.prompt agent "what is in notes.txt?");
  Agent.wait_idle agent;
  let state = Agent.state agent in
  print_s [%sexp (Model.key state.model : string), (state.usage : Usage.t)];
  [%expect
    {|
    ((content
      ((Text "Let me look.")
       (Tool_call
        ((id call_1) (name read) (arguments "{\"path\":\"notes.txt\"}")))))
     (stop_reason Tool_use) (usage ((input 50) (output 10) (cache_read 0)))
     (model aiproxy/gpt-4o))
    tool result: hello
    ((content ((Text "The note says hello."))) (stop_reason End_turn)
     (usage ((input 80) (output 6) (cache_read 0))) (model aiproxy/gpt-4o))
    (aiproxy/gpt-4o ((input 130) (output 16) (cache_read 0)))
    |}];
  List.iter (Server.requests server) ~f:(fun r ->
    printf
      "%s auth=%s trace=%s\n"
      r.request_line
      (Option.value (Server.Request.header r "authorization") ~default:"-")
      (Option.value (Server.Request.header r "x-trace") ~default:"-"));
  (match List.last (Server.requests server) with
   | None -> ()
   | Some r ->
     let body = Json.of_string r.body in
     (match Json.member "messages" body with
      | Some (`Array messages) ->
        List.iter (List.drop messages 2) ~f:(fun m ->
          print_endline (Json.to_string m))
      | _ -> ()));
  [%expect
    {|
    GET /v1/models HTTP/1.1 auth=Bearer sk-e2e trace=1
    POST /v1/chat/completions HTTP/1.1 auth=Bearer sk-e2e trace=1
    POST /v1/chat/completions HTTP/1.1 auth=Bearer sk-e2e trace=1
    {"role":"assistant","content":"Let me look.","tool_calls":[{"id":"call_1","type":"function","function":{"name":"read","arguments":"{\"path\":\"notes.txt\"}"}}]}
    {"role":"tool","tool_call_id":"call_1","content":"hello\n"}
    |}]
;;

let%expect_test "router: keyless servers, other API styles, unknown providers" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let server =
    Server.start ~sw ~env:t.env ~handler:(fun (r : Server.Request.t) ->
      if String.is_substring r.request_line ~substring:"/responses"
      then Server.Reply.simple 401 {|{"error":{"message":"bad key"}}|}
      else if String.is_substring r.request_line ~substring:"/v2/"
      then Server.Reply.simple 403 {|{"error":{"message":"key required"}}|}
      else Server.Reply.simple 418 {|{"error":{"message":"just checking"}}|})
  in
  let url = Server.url server "/v1" in
  write_config
    t
    (sprintf
       {|{"providers": {
  "keyless": {"base_url": "%s"},
  "local": {"base_url": "%s"},
  "gw": {"base_url": "%s", "api": "anthropic"},
  "resp": {"base_url": "%s", "api": "responses"}
}}|}
       (Server.url server "/v2")
       url
       url
       url);
  ok_exn (Auth_store.set (store t) (Custom "gw") (Api_key "sk-gw"));
  ok_exn (Auth_store.set (store t) (Custom "resp") (Api_key "sk-resp"));
  let models = registry t ~sw in
  let provider =
    Provider_router.create ~env:t.env ~getenv:no_env ~models ~store:(store t) ()
  in
  List.iter
    [ "local/llama3"; "gw/claude-x"; "resp/gpt-x"; "keyless/llama3" ]
    ~f:(fun key ->
      let model = Option.value_exn (Model_registry.find models key) in
      let message =
        provider.stream
          { model
          ; system = None
          ; messages = [ Message.user "hi" ]
          ; tools = []
          ; thinking = Off
          ; max_tokens = None
          }
          ~cancel:Cancellation.never
          ~on_event:ignore
      in
      print_s [%sexp (key : string), (message.stop_reason : Stop_reason.t)]);
  List.iter (Server.requests server) ~f:(fun r ->
    printf
      "%s authorization=%s x-api-key=%s\n"
      r.request_line
      (Option.value (Server.Request.header r "authorization") ~default:"-")
      (Option.value (Server.Request.header r "x-api-key") ~default:"-"));
  let ghost =
    { (Option.value_exn (Model_registry.find models "local/llama3")) with
      provider = Custom "ghost"
    }
  in
  print_s
    [%sexp
      ((provider.stream
          { model = ghost
          ; system = None
          ; messages = [ Message.user "hi" ]
          ; tools = []
          ; thinking = Off
          ; max_tokens = None
          }
          ~cancel:Cancellation.never
          ~on_event:ignore)
         .stop_reason
       : Stop_reason.t)];
  [%expect
    {|
    (local/llama3 (Error "HTTP 418: just checking"))
    (gw/claude-x (Error "HTTP 418: just checking"))
    (resp/gpt-x
     (Error
      "HTTP 401: bad key (the server refused the API key: /login resp changes it)"))
    (keyless/llama3
     (Error
      "HTTP 403: key required (the server wants an API key: /login keyless adds one, or set KEYLESS_API_KEY)"))
    POST /v1/chat/completions HTTP/1.1 authorization=- x-api-key=-
    POST /v1/messages HTTP/1.1 authorization=Bearer sk-gw x-api-key=sk-gw
    POST /v1/responses HTTP/1.1 authorization=Bearer sk-resp x-api-key=-
    POST /v2/chat/completions HTTP/1.1 authorization=- x-api-key=-
    (Error
     "custom provider ghost is not configured: add it with /login custom (or check providers.ghost in config.json)")
    |}]
;;

(* ---- RPC ----------------------------------------------------------------- *)

let%expect_test
    "RPC: list_models, set_model, auth_status, login custom and logout"
  =
  with_login ~scripted:false
  @@ fun t ~sw models login ->
  let server =
    start_server ~t ~sw ~models:[ "gpt-4o"; "openai/gpt-4o-mini" ] ()
  in
  write_config t {|{"providers": {"broken": {"api": "chat"}}}|};
  Model_registry.reload models;
  let sessions_dir = Filename.concat t.dir "sessions" in
  let rpc =
    Rpc_server.create
      ~env:t.env
      ~sw
      ~login
      ~sessions_dir
      ~cwd:t.dir
      ~new_agent:(fun ?session ~cwd () ->
        Agent.create
          ~env:t.env
          ~sw
          ~provider:(Faux_provider.create [])
          ~tools:[]
          ~sessions_dir
          ~home:t.dir
          ~models
          ?session
          ~cwd
          ())
      ()
  in
  let sent = Queue.create () in
  let client = Rpc_server.connect rpc ~send:(Queue.enqueue sent) in
  let flush () =
    Queue.iter sent ~f:(fun j -> print_endline (mask t (Json.to_string j)));
    Queue.clear sent
  in
  let call ?(params = "{}") meth =
    let request =
      Json.of_string
        (sprintf {|{"id": "r1", "method": "%s", "params": %s}|} meth params)
    in
    let response = Rpc_server.handle rpc client request in
    (* Let a login flow reach its next prompt. *)
    for _ = 1 to 5 do
      Eio.Fiber.yield ()
    done;
    flush ();
    print_endline (mask t (Json.to_string response))
  in
  let custom_keys () =
    let models =
      List.filter (Model_registry.models models) ~f:(fun m ->
        Provider_id.is_custom m.provider)
    in
    print_s [%sexp (List.map models ~f:Model.key : string list)]
  in
  call ~params:{|{"provider": "custom"}|} "login";
  call ~params:{|{"id": "p1", "value": "aiproxy"}|} "auth_respond";
  call
    ~params:(sprintf {|{"id": "p2", "value": "%s"}|} (Server.url server "/v1"))
    "auth_respond";
  call ~params:{|{"id": "p3", "value": "chat"}|} "auth_respond";
  call ~params:{|{"id": "p4", "value": ""}|} "auth_respond";
  Login_manager.wait login;
  flush ();
  [%expect
    {|
    {"type":"event","event":"auth","kind":"prompt","id":"p1","prompt":"text","message":"Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)","placeholder":"aiproxy","default":""}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"event","event":"auth","kind":"prompt","id":"p2","prompt":"text","message":"Base URL of aiproxy's API (the part before /chat/completions, usually ending in /v1)","placeholder":"http://localhost:3000/v1","default":""}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"event","event":"auth","kind":"prompt","id":"p3","prompt":"select","message":"API style of aiproxy","options":[{"id":"chat","label":"OpenAI chat completions (/chat/completions) - most servers"},{"id":"responses","label":"OpenAI Responses (/responses)"},{"id":"anthropic","label":"Anthropic messages (/messages)"}]}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"event","event":"auth","kind":"prompt","id":"p4","prompt":"secret","message":"API key for aiproxy (leave empty if the server needs none)","allow_empty":true}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"event","event":"auth","kind":"progress","message":"Checking http://127.0.0.1:PORT/v1/models ..."}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"event","event":"auth","kind":"progress","message":"Found 2 models: gpt-4o, openai/gpt-4o-mini"}
    {"type":"event","event":"auth","kind":"done","provider":"aiproxy","method":"api_key"}
    |}];
  custom_keys ();
  let call_quiet ?(params = "{}") meth =
    let request =
      Json.of_string
        (sprintf {|{"id": "r1", "method": "%s", "params": %s}|} meth params)
    in
    let response = Rpc_server.handle rpc client request in
    flush ();
    response
  in
  let custom_entries json ~f =
    match Json.member "result" json with
    | Some (`Array items) ->
      List.iter items ~f:(fun item ->
        match Json.member "provider" item with
        | Some (`String ("aiproxy" | "broken")) ->
          print_endline (mask t (Json.to_string (f item)))
        | _ -> ())
    | _ -> print_endline (Json.to_string json)
  in
  custom_entries (call_quiet "list_models") ~f:Fn.id;
  custom_entries (call_quiet "list_models") ~f:Fn.id;
  [%expect
    {|
    (aiproxy/gpt-4o aiproxy/openai/gpt-4o-mini)
    {"type":"event","event":"notice","text":"providers.broken needs a \"base_url\" such as \"http://localhost:3000/v1\" (in $DIR/.prigh/config.json); that provider is skipped"}
    {"id":"gpt-4o","provider":"aiproxy","key":"aiproxy/gpt-4o","name":"gpt-4o","context_window":128000,"max_output":16384,"supports_thinking":false,"cost":{"input":0,"output":0,"cache_read":0}}
    {"id":"openai/gpt-4o-mini","provider":"aiproxy","key":"aiproxy/openai/gpt-4o-mini","name":"openai/gpt-4o-mini","context_window":128000,"max_output":16384,"supports_thinking":false,"cost":{"input":0,"output":0,"cache_read":0}}
    {"id":"gpt-4o","provider":"aiproxy","key":"aiproxy/gpt-4o","name":"gpt-4o","context_window":128000,"max_output":16384,"supports_thinking":false,"cost":{"input":0,"output":0,"cache_read":0}}
    {"id":"openai/gpt-4o-mini","provider":"aiproxy","key":"aiproxy/openai/gpt-4o-mini","name":"openai/gpt-4o-mini","context_window":128000,"max_output":16384,"supports_thinking":false,"cost":{"input":0,"output":0,"cache_read":0}}
    |}];
  call ~params:{|{"model": "aiproxy/openai/gpt-4o-mini"}|} "set_model";
  call ~params:{|{"model": "aiproxy/gpt-5"}|} "set_model";
  let state = call_quiet "get_state" in
  print_s
    [%sexp
      (Option.bind (Json.member "result" state) ~f:(Json.member "model")
       |> Option.bind ~f:(Json.member "key")
       |> Option.map ~f:Json.to_string
       : string option)];
  custom_entries (call_quiet "auth_status") ~f:Fn.id;
  [%expect
    {|
    {"type":"event","event":"state","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"$DIR","git_branch":null,"model":{"id":"openai/gpt-4o-mini","provider":"aiproxy","key":"aiproxy/openai/gpt-4o-mini","name":"openai/gpt-4o-mini","context_window":128000,"max_output":16384,"supports_thinking":false,"cost":{"input":0,"output":0,"cache_read":0}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}],"subagents":[],"jobs":[]}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":false,"error":"unknown model \"aiproxy/gpt-5\"; did you mean: aiproxy/gpt-4o (gpt-4o), aiproxy/openai/gpt-4o-mini (openai/gpt-4o-mini)"}
    ("\"aiproxy/openai/gpt-4o-mini\"")
    {"provider":"aiproxy","name":"aiproxy","methods":[{"method":"api_key","label":"aiproxy API key"}],"configured":{"method":"api_key","source":"no key"},"expires_ms":null,"custom":{"base_url":"http://127.0.0.1:PORT/v1","api":"chat","api_label":"OpenAI chat completions (/chat/completions)"}}
    |}];
  (* Logging out asks the caller which to remove. *)
  call ~params:{|{"provider": "aiproxy"}|} "logout";
  call ~params:{|{"id": "p5", "value": "all"}|} "auth_respond";
  Login_manager.wait login;
  flush ();
  custom_keys ();
  call ~params:{|{"provider": "aiproxy"}|} "logout";
  [%expect
    {|
    {"type":"event","event":"auth","kind":"prompt","id":"p5","prompt":"select","message":"Log out of aiproxy (http://127.0.0.1:PORT/v1)","options":[{"id":"all","label":"Remove the provider from config.json"},{"id":"keep","label":"Keep it"}]}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"event","event":"auth","kind":"logged_out","provider":"aiproxy"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    ()
    {"type":"response","id":"r1","ok":false,"error":"unknown provider \"aiproxy\" (one of: anthropic, openai, openai-codex, deepseek; or custom to add an OpenAI-compatible endpoint)"}
    |}]
;;
