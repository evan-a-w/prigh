open! Core
open! Expect_test_helpers_core
open Prigh_ui
module P = Prigh_protocol

(* Drives the pure state machine: every command the app emits is printed, and
   [show] prints what the user would see (cursor drawn as ▏). *)
module H = struct
  type t = { mutable model : App.Model.t }

  let create ?(width = 60) ?(height = 12) () =
    let model, _ = App.update App.init (Resize { width; height }) in
    { model }
  ;;

  let step ?(quiet = false) t (action : App.Action.t) =
    let model, commands = App.update t.model action in
    t.model <- model;
    if not quiet
    then List.iter commands ~f:(fun c -> print_s [%sexp (c : App.Command.t)])
  ;;

  let show t =
    print_endline (Screen.to_plain ~show_cursor:true (Render.screen t.model))
  ;;

  let key t k = step t (Key k)
  let keys t s = String.iter s ~f:(fun c -> key t (Key.char c))
  let enter t = key t (Key.plain Enter)
  let esc t = key t (Key.plain Escape)
  let event t e = step t (Event e)

  let reply ?quiet t tag json =
    step ?quiet t (Reply (tag, Ok (Or_error.ok_exn (P.Json.parse json))))
  ;;

  let reply_error t tag message = step t (Reply (tag, Error message))
  let mode t = print_endline (Mode.name t.model.mode)
end

let model_json ?(provider = "anthropic") id name =
  sprintf
    {|{"id":"%s","provider":"%s","key":"%s/%s","name":"%s","context_window":1000000,"max_output":128000,"supports_thinking":true,"cost":{"input":10,"output":50,"cache_read":1}}|}
    id
    provider
    provider
    id
    name
;;

let models_json =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ model_json "claude-fable-5" "Claude Fable 5"
       ; model_json "claude-fable-5-1" "Claude Fable 5.1"
       ; model_json ~provider:"openai" "gpt-5.5" "GPT-5.5"
       ; model_json ~provider:"deepseek" "deepseek-flash" "DeepSeek V4.1 Flash"
       ])
;;

let state_json
  ?(model =
    model_json ~provider:"deepseek" "deepseek-flash" "DeepSeek V4.1 Flash")
  ?(running = false)
  ()
  =
  sprintf
    {|{"session_id":"abc123","session_path":"/home/u/.prigh/sessions/1.jsonl","cwd":"/work","model":%s,"thinking":"off","running":%b,"message_count":2,"usage":{"input":1200,"output":300,"cache_read":0},"cost_usd":0.0123,"context_tokens":1500}|}
    model
    running
;;

let state ?model ?running () =
  Or_error.ok_exn
    (P.State.of_json
       (Or_error.ok_exn (P.Json.parse (state_json ?model ?running ()))))
;;

let auth_json =
  {|[{"provider":"anthropic","name":"Anthropic","methods":[{"method":"oauth","label":"Claude Pro/Max"},{"method":"api_key","label":"API key"}],"configured":null,"expires_ms":null},{"provider":"openai","name":"OpenAI","methods":[{"method":"api_key","label":"API key"}],"configured":null,"expires_ms":null},{"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"API key"}],"configured":{"method":"api_key","source":"auth.json"},"expires_ms":null}]|}
;;

let assistant ?(stop = "end_turn") text =
  Or_error.ok_exn
    (P.Message.of_json
       (Or_error.ok_exn
          (P.Json.parse
             (sprintf
                {|{"role":"assistant","content":[{"type":"text","text":%s}],"stop_reason":{"type":"%s"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}|}
                (P.Json.to_string (P.Json.str text))
                stop))))
;;

let partial =
  match assistant "" with
  | Assistant a -> a
  | _ -> assert false
;;

let connected ?width ?height () =
  let h = H.create ?width ?height () in
  H.step ~quiet:true h Start;
  H.reply ~quiet:true h Initial_state (state_json ());
  H.reply
    ~quiet:true
    h
    Initial_messages
    {|[{"role":"user","text":"earlier question"},{"role":"assistant","content":[{"type":"text","text":"earlier **answer**"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}]|};
  H.reply ~quiet:true h Auth_refresh auth_json;
  h
;;

let%expect_test "startup: requests state, messages and auth; renders history" =
  let h = H.create () in
  H.show h;
  [%expect
    {|
    ────────────────────────────────────────────────────────────
    > ▏
    connecting…
    |}];
  H.step h Start;
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    |}];
  H.reply h Initial_state (state_json ());
  H.reply
    h
    Initial_messages
    {|[{"role":"user","text":"earlier question"},{"role":"assistant","content":[{"type":"text","text":"earlier **answer**"}],"stop_reason":{"type":"end_turn"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"}]|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "prompt, streaming with embedded newlines, tool call, steer \
                 while running"
  =
  let h = connected () in
  H.keys h "list the files";
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > list the files▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.enter h;
  [%expect
    {| (Rpc (method_ prompt) (params ((text "list the files"))) (tag Show_error)) |}];
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (User "list the files"));
  H.event h (Message_update { partial; delta = Thinking_delta "let me look" });
  H.event h (Message_update { partial; delta = Text_delta "Sure, here" });
  H.event
    h
    (Message_update { partial; delta = Text_delta " they are:\nfirst li" });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    > list the files
      let me look
    Sure, here they are:
    first li
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.event h (Message_update { partial; delta = Text_delta "ne done" });
  H.event
    h
    (Tool_start
       { id = "c1"; name = "bash"; arguments = {|{"command":"ls -la\n/work"}|} });
  H.event h (Tool_output { call_id = "c1"; chunk = "a.ml\nb.ml\npart" });
  H.show h;
  [%expect
    {|
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    > list the files
      let me look
    Sure, here they are:
    first line done
    ⚙ bash command=ls -la⏎/work
      part
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  (* Steering while running is queued, not sent as a prompt. *)
  H.keys h "also count them";
  H.enter h;
  [%expect
    {| (Rpc (method_ steer) (params ((text "also count them"))) (tag Show_error)) |}];
  H.event h (Tool_output { call_id = "c1"; chunk = "ial\n" });
  H.event
    h
    (Tool_end
       { call = { id = "c1"; name = "bash"; arguments = "{}" }
       ; result =
           { tool_call_id = "c1"
           ; tool_name = "bash"
           ; text = "a.ml\nb.ml\npartial\n"
           ; is_error = false
           }
       });
  H.event h (Message_end (assistant "Sure, here they are:\nfirst line done"));
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
    > list the files
      let me look
    Sure, here they are:
    first line done
    ⚙ bash command=ls -la⏎/work
    queued (delivered after the current turn)
      a.ml
      b.ml
      partial
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "error and aborted stop reasons are surfaced once" =
  let h = connected () in
  H.event h (Message_end (assistant ~stop:"aborted" ""));
  H.event
    h
    (Message_end
       (Or_error.ok_exn
          (P.Message.of_json
             (Or_error.ok_exn
                (P.Json.parse
                   {|{"role":"assistant","content":[],"stop_reason":{"type":"error","message":"HTTP 401: not logged in; use /login anthropic"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|})))));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    [aborted]
    error: HTTP 401: not logged in; use /login anthropic
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "/model opens the picker; typing filters; Enter sets the model" =
  let h = connected () in
  H.keys h "/model";
  H.enter h;
  [%expect
    {| (Rpc (method_ list_models) (params ()) (tag (Models_for_picker ""))) |}];
  H.reply h (Models_for_picker "") models_json;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Model  (4)
    / ▏
      Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M  …
      Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0M…
      GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 pe…
    * DeepSeek V4.1 Flash  deepseek/deepseek-flash  ctx 1.0M  $…
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.keys h "fable";
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Model  (2)
    / fable▏
      Claude Fable 5    anthropic/claude-fable-5  ctx 1.0M  $10…
      Claude Fable 5.1  anthropic/claude-fable-5-1  ctx 1.0M  $…
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.plain Down);
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5-1)))
      (tag Set_model_done))
    |}];
  H.mode h;
  H.event
    h
    (State (state ~model:(model_json "claude-fable-5-1" "Claude Fable 5.1") ()));
  H.show h;
  [%expect
    {|
    editing





    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    anthropic/claude-fable-5-1  thinking:off  ctx:1.5k (0%)  in…
    |}]
;;

let%expect_test "/model <display name> switches directly; unknown suggests and \
                 opens the picker"
  =
  let h = connected () in
  H.keys h "/model claude fable 5.1";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ list_models)
      (params ())
      (tag (Models_for_switch "claude fable 5.1")))
    |}];
  H.reply h (Models_for_switch "claude fable 5.1") models_json;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5-1)))
      (tag Set_model_done))
    |}];
  (* Models are cached now. *)
  H.keys h "/model GPT-5.5";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model openai/gpt-5.5)))
      (tag Set_model_done))
    |}];
  H.keys h "/model claude-fable";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    several models match "claude-fable"
    Model  (2)
    / claude-fable▏
      Claude Fable 5    anthropic/claude-fable-5  ctx 1.0M  $10…
      Claude Fable 5.1  anthropic/claude-fable-5-1  ctx 1.0M  $…
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.esc h;
  H.keys h "/model zzz";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    several models match "claude-fable"
    unknown model "zzz"; did you mean: GPT-5.5, Claude Fable 5,
    DeepSeek V4.1 Flash
    Model  (0)
    / zzz▏
      no matches
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.esc h;
  (* A backend rejection (e.g. from another client's catalog) also recovers via
     the picker. *)
  H.reply_error h Set_model_done {|unknown model "q"; did you mean: a, b|};
  H.show h;
  [%expect
    {|
    several models match "claude-fable"
    unknown model "zzz"; did you mean: GPT-5.5, Claude Fable 5,
    DeepSeek V4.1 Flash
    unknown model "q"; did you mean: a, b
    Model  (4)
    / ▏
      Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M  …
      Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0M…
      GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 pe…
    * DeepSeek V4.1 Flash  deepseek/deepseek-flash  ctx 1.0M  $…
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "Esc closes a dialog without aborting; Esc while running aborts"
  =
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.keys h "/thinking";
  H.enter h;
  H.show h;
  [%expect
    {|
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Thinking level  (5)
    / ▏
    * off
      on
      low
      high
      max
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.esc h;
  H.mode h;
  [%expect {| editing |}];
  H.esc h;
  [%expect {| (Rpc (method_ abort) (params ()) (tag Show_error)) |}];
  (* Opening a dialog while one is open is refused with a notice. *)
  H.keys h "/thinking";
  H.enter h;
  H.event
    h
    (Auth (Prompt { id = "p1"; prompt = Secret { message = "API key" } }));
  [%expect {| (Rpc (method_ auth_cancel) (params ()) (tag Show_error)) |}];
  H.key h (Key.plain Down);
  H.enter h;
  [%expect
    {| (Rpc (method_ set_thinking) (params ((thinking on))) (tag Show_error)) |}];
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    login prompt arrived while a dialog was open; press Esc to
    reach it
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "login: url, masked secret prompt, answer, done switches \
                 provider"
  =
  let h = connected () in
  H.keys h "/login anthropic api_key";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ login)
      (params (
        (provider anthropic)
        (method   api_key)))
      (tag Show_error))
    |}];
  H.event
    h
    (Auth
       (Auth_url
          { url = "https://claude.ai/oauth?x=1"
          ; instructions = "Approve in the browser, then paste the code."
          }));
  [%expect {| (Open_browser https://claude.ai/oauth?x=1) |}];
  H.event
    h
    (Auth
       (Prompt
          { id = "p1"
          ; prompt = Secret { message = "Paste your Anthropic API key" }
          }));
  H.keys h "sk-ant-secret";
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Open this URL to log in:
      https://claude.ai/oauth?x=1
    Approve in the browser, then paste the code.
    Paste your Anthropic API key
    ────────────────────────────────────────────────────────────
    ? *************▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ auth_respond)
      (params (
        (id    p1)
        (value sk-ant-secret)))
      (tag Show_error))
    |}];
  H.mode h;
  [%expect {| editing |}];
  H.event h (Auth (Progress "exchanging code"));
  H.event h (Auth (Done { provider = "anthropic"; method_ = "api_key" }));
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ list_models) (params ()) (tag (Models_after_login anthropic)))
    |}];
  H.reply h (Models_after_login "anthropic") models_json;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5)))
      (tag Set_model_done))
    |}];
  H.show h;
  [%expect
    {|
    > earlier question
    earlier answer
    Open this URL to log in:
      https://claude.ai/oauth?x=1
    Approve in the browser, then paste the code.
    Paste your Anthropic API key
    exchanging code
    logged in to anthropic (api_key)
    model set to anthropic/claude-fable-5; /model to change
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "login: select prompt is a picker; Esc cancels; backend \
                 cancellation restores"
  =
  let h = connected () in
  H.event
    h
    (Auth
       (Prompt
          { id = "p2"
          ; prompt =
              Select
                { message = "Choose a login method"
                ; options = [ "oauth", "Claude Pro/Max"; "api_key", "API key" ]
                }
          }));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Choose a login method  (2)
    / ▏
      Claude Pro/Max  oauth
      API key         api_key
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.plain Down);
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ auth_respond)
      (params (
        (id    p2)
        (value api_key)))
      (tag Show_error))
    |}];
  H.event
    h
    (Auth
       (Prompt
          { id = "p3"
          ; prompt =
              Manual_code
                { message = "Paste the redirect URL"
                ; placeholder = "http://localhost:1455/callback"
                }
          }));
  H.keys h "http://loc";
  H.esc h;
  [%expect {| (Rpc (method_ auth_cancel) (params ()) (tag Show_error)) |}];
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Paste the redirect URL
    e.g. http://localhost:1455/callback?code=...
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.event h (Auth (Prompt { id = "p4"; prompt = Secret { message = "key" } }));
  H.mode h;
  H.event h (Auth (Prompt_cancelled { id = "p4" }));
  H.mode h;
  H.event h (Auth (Failed { provider = "anthropic"; error = "denied" }));
  H.show h;
  [%expect
    {|
    login
    editing

    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Paste the redirect URL
    e.g. http://localhost:1455/callback?code=...
    key
    login to anthropic failed: denied
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "Ctrl+C clears, then warns, then quits; never quits with a \
                 dialog open"
  =
  let h = connected () in
  H.keys h "draft";
  H.key h (Key.ctrl 'c');
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.ctrl 'c');
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    press Ctrl+C again to quit
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.keys h "x";
  H.key h (Key.ctrl 'c');
  H.key h (Key.ctrl 'c');
  [%expect {| |}];
  H.key h (Key.ctrl 'c');
  [%expect {| Quit |}];
  (* With a dialog open, Ctrl+C only closes it. *)
  let h = connected () in
  H.keys h "/thinking";
  H.enter h;
  H.key h (Key.ctrl 'c');
  H.mode h;
  [%expect {| editing |}];
  H.key h (Key.ctrl 'c');
  [%expect {| |}];
  H.key h (Key.ctrl 'c');
  [%expect {| Quit |}];
  let h = connected () in
  H.event h (Auth (Prompt { id = "p1"; prompt = Secret { message = "key" } }));
  H.key h (Key.ctrl 'c');
  [%expect {| (Rpc (method_ auth_cancel) (params ()) (tag Show_error)) |}];
  H.mode h;
  [%expect {| editing |}];
  let h = connected () in
  H.key h (Key.ctrl 'd');
  [%expect {| Quit |}]
;;

let%expect_test "Tab completes commands or opens the command picker; / alone \
                 opens it"
  =
  let h = connected () in
  H.keys h "/mo";
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /model ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.ctrl 'u');
  H.keys h "/s";
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Commands  (3)
    / s▏
      /state          show session state
      /switch [path]  switch to a saved session
      /sessions       pick a saved session to switch to
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.keys h "w";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /switch ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.ctrl 'u');
  H.keys h "/";
  H.enter h;
  H.mode h;
  [%expect {| picker |}];
  H.esc h;
  H.keys h "/modle";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    unknown command /modle; did you mean /model? (Tab or / lists
    commands)
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "/sessions picker switches and reloads; /logout confirms" =
  let h = connected () in
  H.keys h "/sessions";
  H.enter h;
  [%expect
    {| (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker)) |}];
  H.reply
    h
    Sessions_picker
    {|[{"id":"1","path":"/home/u/.prigh/sessions/1.jsonl","cwd":"/work","created_at":"2025-06-01T10:00:00Z","first_prompt":"fix the build\nplease","message_count":12},{"id":"2","path":"/home/u/.prigh/sessions/2.jsonl","cwd":"/other","created_at":"2025-06-02T11:30:00Z","first_prompt":null,"message_count":0}]|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Sessions  (2)
    / ▏
    * 2025-06-01T10:00:00  fix the build please  12 msgs  /work
      2025-06-02T11:30:00  (empty)    0 msgs  /other
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.plain Down);
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ switch_session)
      (params ((path /home/u/.prigh/sessions/2.jsonl)))
      (tag Reload_messages))
    |}];
  H.reply h Reload_messages {|[{"role":"user","text":"in the other session"}]|};
  [%expect {| (Rpc (method_ get_state) (params ()) (tag Initial_state)) |}];
  H.show h;
  [%expect
    {|
    > in the other session
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.keys h "/logout deepseek";
  H.enter h;
  H.show h;
  [%expect
    {|
    > in the other session
    Log out of deepseek and delete its credential? (y/n)
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.keys h "n";
  H.mode h;
  [%expect {| editing |}];
  H.keys h "/logout";
  H.enter h;
  H.reply h Auth_logout_picker auth_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_logout_picker))





    > in the other session
    cancelled
    Log out of  (1)
    / ▏
      DeepSeek  api_key via auth.json
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.enter h;
  H.keys h "y";
  [%expect
    {| (Rpc (method_ logout) (params ((provider deepseek))) (tag Show_error)) |}];
  H.event h (Auth (Logged_out "deepseek"));
  [%expect {| (Rpc (method_ auth_status) (params ()) (tag Auth_refresh)) |}]
;;

let%expect_test "/auth, /help, /state, /clear, unknown method errors" =
  let h = connected ~height:16 () in
  H.keys h "/auth";
  H.enter h;
  H.reply h Auth_show auth_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_show))





    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    anthropic  not configured  [oauth (Claude Pro/Max), api_key
    (API key)]
    openai     not configured  [api_key (API key)]
    deepseek   logged in via auth.json  [api_key (API key)]
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.keys h "/clear";
  H.enter h;
  H.keys h "/help";
  H.enter h;
  H.show h;
  [%expect
    {|
    Home / Ctrl+A       start of line
    End / Ctrl+E        end of line
    PageUp              scroll the transcript / list up a page
    PageDown            scroll the transcript / list down a page
    Backspace / Ctrl+H  delete the character before the cursor
    Delete              delete the character under the cursor
    Ctrl+K              delete to end of line
    Ctrl+U              delete the whole line
    Ctrl+W              delete the word before the cursor
    Ctrl+L              clear the transcript
    Ctrl+O              expand / collapse tool output
    Ctrl+C              clear the editor, then (again) quit
    Ctrl+D              quit
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.ctrl 'l');
  H.reply_error h Show_error "unknown method \"bogus\"";
  H.step h (Protocol_error "bad line");
  H.step h (Stderr "warning from backend");
  H.show h;
  [%expect
    {|
    unknown method "bogus"
    protocol error: bad line
    backend: warning from backend
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "tool output collapses; Ctrl+O expands; PageUp scrolls and \
                 follows again"
  =
  let h = connected ~height:10 () in
  let text =
    String.concat ~sep:"\n" (List.init 12 ~f:(fun i -> sprintf "line %d" i))
  in
  H.event
    h
    (Tool_start { id = "c1"; name = "read"; arguments = {|{"path":"a.ml"}|} });
  H.event
    h
    (Tool_end
       { call = { id = "c1"; name = "read"; arguments = "{}" }
       ; result =
           { tool_call_id = "c1"; tool_name = "read"; text; is_error = true }
       });
  H.show h;
  [%expect
    {|
      line 2
      line 3
      line 4
      line 5
      line 6
      line 7
      … (4 more lines; Ctrl+O expands)
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
      line 5
      line 6
      line 7
      line 8
      line 9
      line 10
      line 11
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.plain Page_up);
  H.show h;
  [%expect
    {|
      line 0
      line 1
      line 2
      line 3
      line 4
      line 5
      line 6
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.plain Page_down);
  H.key h (Key.plain Page_down);
  H.show h;
  [%expect
    {|
      line 5
      line 6
      line 7
      line 8
      line 9
      line 10
      line 11
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "resize mid-stream re-wraps without losing lines" =
  let h = connected ~width:60 ~height:8 () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Message_update
       { partial
       ; delta =
           Text_delta
             "The quick brown fox jumps over the lazy dog and keeps running.\n\
              second"
       });
  H.show h;
  [%expect
    {|
    > earlier question
    earlier answer
    The quick brown fox jumps over the lazy dog and keeps
    running.
    second
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.step h (Resize { width = 30; height = 8 });
  H.show h;
  [%expect
    {|
    earlier answer
    The quick brown fox jumps over
    the lazy dog and keeps
    running.
    second
    ──────────────────────────────
    > ▏
    deepseek/deepseek-flash  thin…
    |}];
  H.step h (Resize { width = 60; height = 8 });
  H.event h (Message_update { partial; delta = Text_delta " part" });
  H.show h;
  [%expect
    {|
    > earlier question
    earlier answer
    The quick brown fox jumps over the lazy dog and keeps
    running.
    second part
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "multi-line editing: Alt+Enter, cursor movement, history" =
  let h = connected () in
  H.keys h "first line";
  H.key h (Key.alt Enter);
  H.keys h "second";
  H.key h (Key.plain Up);
  H.key h (Key.plain Home);
  H.key h (Key.plain Right);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > f▏rst line
      second
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.enter h;
  [%expect
    {| (Rpc (method_ prompt) (params ((text "first line\nsecond"))) (tag Show_error)) |}];
  H.key h (Key.plain Up);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > first line
      second▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}];
  (* Pasted text with newlines goes in as one insert. *)
  H.step h (Intent (Insert "a\nb\nc"));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > a
      b
      c▏
    deepseek/deepseek-flash  thinking:off  ctx:1.5k (0%)  in:1.…
    |}]
;;

let%expect_test "backend exit quits" =
  let h = connected () in
  H.step h Backend_closed;
  [%expect {| Quit |}];
  H.keys h "x";
  H.enter h;
  [%expect {| |}]
;;
