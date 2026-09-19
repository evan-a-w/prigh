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
  let next_agent t = key t { (Key.plain Tab) with shift = true }
  let focus_agent t n = key t (Key.alt (Key.Code.Char (Int.to_string n)))
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

let tool_call ?(name = "bash") ?(arguments = "{}") id : P.Tool_call.t =
  { id; name; arguments }
;;

let tool_result ?(name = "bash") ?(is_error = false) ~id text
  : P.Message.Tool_result.t
  =
  { tool_call_id = id; tool_name = name; text; is_error }
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
      a.ml
      b.ml
      partial
    queued (delivered after the current turn)
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}]
;;

let%expect_test "/model opens the picker; typing filters; Enter sets the model" =
  let h = connected () in
  H.keys h "/model";
  H.enter h;
  [%expect {| |}];
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    anthropic/claude-fable-5-1  thinking:off  view:normal  ctx:…
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
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5)))
      (tag Set_model_done))





    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    unknown model "zzz"; did you mean: GPT-5.5, Claude Fable 5,
    DeepSeek V4.1 Flash
    Model  (0)
    / zzz▏
      no matches
    ────────────────────────────────────────────────────────────
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.esc h;
  (* A backend rejection (e.g. from another client's catalog) also recovers via
     the picker. *)
  H.reply_error h Set_model_done {|unknown model "q"; did you mean: a, b|};
  H.show h;
  [%expect
    {|
    earlier answer
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}]
;;

let%expect_test "Esc closes autocomplete or dialog without aborting; Esc while \
                 running aborts"
  =
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.keys h "/thinking";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /thinking ▏
    ▸ off
      on
      low
      high
      max
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.esc h;
  H.mode h;
  [%expect {| editing |}];
  H.esc h;
  [%expect {| (Rpc (method_ abort) (params ()) (tag Abort_done)) |}];
  (* A real dialog is also closed by Esc without aborting; an incoming auth
     prompt is refused while one is open. *)
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.keys h "/logout deepseek";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Log out of deepseek and delete its credential? (y/n)
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.event
    h
    (Auth (Prompt { id = "p1"; prompt = Secret { message = "API key" } }));
  [%expect {| (Rpc (method_ auth_cancel) (params ()) (tag Show_error)) |}];
  H.keys h "n";
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    login prompt arrived while a dialog was open; press Esc to
    reach it
    cancelled
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.mode h;
  [%expect {| editing |}];
  H.esc h;
  [%expect {| (Rpc (method_ abort) (params ()) (tag Abort_done)) |}]
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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

let%expect_test "typing / lists commands, Down twice + Tab fills /login " =
  let h = connected () in
  H.keys h "/";
  H.show h;
  [%expect
    {|
    earlier answer
    ────────────────────────────────────────────────────────────
    > /▏
    ▸ /help                           show commands and keys
      /model [name|id|provider/id]    pick or switch the model
      /login [provider] [api_key|oauth]  log in to a provider
      /logout [provider]              remove a provider's store…
      /thinking [off|on|low|high|max]  pick or set the thinking…
      /verbosity [quiet|normal|verbose]  set the transcript ver…
      /auth                           show which providers are …
      /compact                        summarise older messages …
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
    earlier answer
    ────────────────────────────────────────────────────────────
    > /▏
      /help                           show commands and keys
    ▸ /model [name|id|provider/id]    pick or switch the model
      /login [provider] [api_key|oauth]  log in to a provider
      /logout [provider]              remove a provider's store…
      /thinking [off|on|low|high|max]  pick or set the thinking…
      /verbosity [quiet|normal|verbose]  set the transcript ver…
      /auth                           show which providers are …
      /compact                        summarise older messages …
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
    earlier answer
    ────────────────────────────────────────────────────────────
    > /▏
      /help                           show commands and keys
      /model [name|id|provider/id]    pick or switch the model
    ▸ /login [provider] [api_key|oauth]  log in to a provider
      /logout [provider]              remove a provider's store…
      /thinking [off|on|low|high|max]  pick or set the thinking…
      /verbosity [quiet|normal|verbose]  set the transcript ver…
      /auth                           show which providers are …
      /compact                        summarise older messages …
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /login ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}]
;;

let%expect_test "/mo Enter opens argument completion over models; typing fab \
                 Enter calls set_model"
  =
  let h = connected () in
  H.reply ~quiet:true h (Models_for_picker "") models_json;
  H.esc h;
  H.keys h "/mo";
  H.key h (Key.plain Enter);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /model ▏
    ▸ Claude Fable 5       anthropic/claude-fable-5
      Claude Fable 5.1     anthropic/claude-fable-5-1
      GPT-5.5              openai/gpt-5.5
      DeepSeek V4.1 Flash  deepseek/deepseek-flash
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.keys h "fab";
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /model fab▏
    ▸ Claude Fable 5    anthropic/claude-fable-5
      Claude Fable 5.1  anthropic/claude-fable-5-1
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.key h (Key.plain Enter);
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5)))
      (tag Set_model_done))
    |}]
;;

let%expect_test "Esc closes autocomplete without abort while running" =
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.keys h "/mo";
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /mo▏
    ▸ /model [name|id|provider/id]  pick or switch the model
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /mo▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.esc h;
  [%expect {| (Rpc (method_ abort) (params ()) (tag Abort_done)) |}]
;;

let%expect_test "@ completion is asynchronous and drops stale replies" =
  let h = connected () in
  H.keys h "@sr";
  [%expect
    {|
    (List_paths (prefix "") (tag (Paths_for_autocomplete "")))
    (List_paths (prefix s) (tag (Paths_for_autocomplete s)))
    (List_paths (prefix sr) (tag (Paths_for_autocomplete sr)))
    |}];
  H.keys h "c";
  [%expect {| (List_paths (prefix src) (tag (Paths_for_autocomplete src))) |}];
  H.reply h (Paths_for_autocomplete "sr") {|["stale/only"]|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > @src▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.reply h (Paths_for_autocomplete "src") {|["src/","src/app.ml"]|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > @src▏
    ▸ src/
      src/app.ml
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}]
;;

let%expect_test "submit with @file sends attachments param" =
  let h = connected () in
  H.step h (Intent (Insert "look at @src/app.ml"));
  [%expect
    {| (List_paths (prefix src/app.ml) (tag (Paths_for_autocomplete src/app.ml))) |}];
  H.reply h (Paths_for_autocomplete "src/app.ml") {|["src/app.ml"]|};
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params ((text "look at @src/app.ml") (attachments (src/app.ml))))
      (tag Show_error))
    |}];
  (* Unknown @tokens stay plain text and produce no attachments param. *)
  H.step ~quiet:true h (Intent (Insert "check @nope"));
  H.enter h;
  [%expect
    {| (Rpc (method_ prompt) (params ((text "check @nope"))) (tag Show_error)) |}]
;;

let%expect_test "/switch fetches sessions then reopens completion" =
  let h = connected () in
  H.step h (Intent (Insert "/switch "));
  [%expect {| (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache)) |}];
  H.reply
    h
    Sessions_cache
    {|[{"id":"1","path":"/home/u/.prigh/sessions/1.jsonl","cwd":"/work","created_at":"2025-06-01T10:00:00Z","first_prompt":"fix the build\nplease","message_count":12},{"id":"2","path":"/home/u/.prigh/sessions/2.jsonl","cwd":"/other","created_at":"2025-06-02T11:30:00Z","first_prompt":null,"message_count":0}]|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /switch ▏
    ▸ fix the build please  /work
      (empty)               /other
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}]
;;

let%expect_test "/cd completes paths and submits the selected one" =
  let h = connected () in
  H.step h (Intent (Insert "/cd sr"));
  [%expect {| (List_paths (prefix sr) (tag (Paths_for_autocomplete sr))) |}];
  H.reply h (Paths_for_autocomplete "sr") {|["src/","src/app.ml"]|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /cd sr▏
    ▸ src/
      src/app.ml
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.key h (Key.plain Down);
  H.key h (Key.plain Enter);
  [%expect
    {| (Rpc (method_ set_cwd) (params ((path src/app.ml))) (tag Show_error)) |}]
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.keys h "n";
  H.mode h;
  [%expect {| editing |}];
  H.keys h "/logout";
  H.esc h;
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.keys h "/clear";
  H.enter h;
  H.keys h "/help";
  H.enter h;
  H.show h;
  [%expect
    {|
    PageDown            scroll the transcript / list down a page
    Backspace / Ctrl+H  delete the character before the cursor
    Delete              delete the character under the cursor
    Ctrl+K              delete to end of line
    Ctrl+U              delete the whole line
    Ctrl+W              delete the word before the cursor
    Ctrl+L              clear the transcript
    Ctrl+O              cycle transcript verbosity (quiet /
    normal / verbose)
    Shift+Tab           cycle focus: main → agent 1 → … → main
    Alt+1               focus agent N (Alt+1…9)
    Ctrl+C              clear the editor, then (again) quit
    Ctrl+D              quit
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}]
;;

let%expect_test "verbosity cycles Normal / Verbose / Quiet; /verbosity sets it" =
  let h = connected ~height:14 () in
  H.keys h "run it";
  H.enter h;
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (User "run it"));
  let call = tool_call ~arguments:{|{"command":"ls -la"}|} "c1" in
  H.event h (Tool_start call);
  H.event h (Tool_output { call_id = "c1"; chunk = "a\nb\n" });
  H.event h (Tool_output { call_id = "c1"; chunk = "c\nd\n" });
  H.event h (Tool_output { call_id = "c1"; chunk = "e\n" });
  let text =
    String.concat ~sep:"\n" (List.init 20 ~f:(fun i -> sprintf "out %d" i))
  in
  H.event h (Tool_end { call; result = tool_result ~id:"c1" text });
  H.event h (Message_update { partial; delta = Text_delta "all done" });
  H.event h (Message_end (assistant "all done"));
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text "run it"))) (tag Show_error))
    > earlier question
    earlier answer
    > run it
    ⚙ bash command=ls -la
      out 0
      out 1
      out 2
      out 3
      out 4
      … (15 more)
    all done
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
      out 11
      out 12
      out 13
      out 14
      out 15
      out 16
      out 17
      out 18
      out 19
    all done
    view: verbose — everything is shown
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:verbose  ctx:1.…
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    > earlier question
    earlier answer
    > run it
    ⚙ bash ls -la ✓ 20 lines
    all done
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:quiet  ctx:1.5k…
    |}];
  H.keys h "/verbosity normal";
  H.enter h;
  H.show h;
  [%expect
    {|
    ⚙ bash command=ls -la
      out 0
      out 1
      out 2
      out 3
      out 4
      … (15 more)
    all done
    view: verbose — everything is shown
    view: quiet — intermediate output and thinking are hidden
    view: normal — tool output is summarised
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}]
;;

let%expect_test "/verbosity with no argument opens argument completion" =
  let h = connected () in
  H.keys h "/verbosity";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > /verbosity ▏
    ▸ Quiet
      Normal
      Verbose
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
    |}];
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    view: verbose — everything is shown
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:verbose  ctx:1.…
    |}]
;;

let%expect_test "quiet hides intermediate text and thinking" =
  let h = connected ~height:14 () in
  H.keys h "/verbosity quiet";
  H.enter h;
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (User "hi"));
  H.event
    h
    (Message_update
       { partial; delta = Thinking_delta "secret chain of thought" });
  H.event
    h
    (Message_update { partial; delta = Text_delta "let me look at the files" });
  let call = tool_call ~arguments:{|{"command":"ls"}|} "c1" in
  H.event h (Tool_start call);
  H.event h (Tool_end { call; result = tool_result ~id:"c1" "a.ml\nb.ml" });
  H.event h (Message_update { partial; delta = Text_delta "all done" });
  H.event h (Message_end (assistant "all done"));
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
    > earlier question
    earlier answer
    > hi
    let me look at the files
    ⚙ bash ls ✓ 2 lines
    all done
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:quiet  ctx:1.5k…
    |}]
;;

let%expect_test "tool error is always visible in Quiet" =
  let h = connected ~height:12 () in
  H.keys h "/verbosity quiet";
  H.enter h;
  let call = tool_call ~arguments:{|{"command":"cat missing"}|} "c1" in
  H.event h (Tool_start call);
  H.event
    h
    (Tool_end
       { call
       ; result =
           tool_result
             ~id:"c1"
             ~is_error:true
             "no such file\nsecond line\nthird line\nfourth line"
       });
  H.show h;
  [%expect
    {|
    > earlier question
    earlier answer
    ⚙ bash cat missing ✗ 4 lines
      no such file
      second line
      third line
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:quiet  ctx:1.5k…
    |}]
;;

let%expect_test "reload pairs tool calls with their results" =
  let h = connected ~height:12 () in
  H.keys h "/verbosity quiet";
  H.enter h;
  H.reply
    h
    Reload_messages
    {|[{"role":"user","text":"go"},{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"ls\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"},{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"a.ml\nb.ml","is_error":false}]|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))







    > go
    ⚙ bash ls ✓ 2 lines
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:quiet  ctx:1.5k…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5…
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

let%expect_test "abort restores queued messages" =
  let h = connected ~width:100 () in
  H.event h (State (state ~running:true ()));
  H.keys h "first";
  H.enter h;
  [%expect {| (Rpc (method_ steer) (params ((text first))) (tag Show_error)) |}];
  H.keys h "second";
  H.enter h;
  [%expect
    {| (Rpc (method_ steer) (params ((text second))) (tag Show_error)) |}];
  H.event h (Queue_update { steer = 2; follow_up = 0 });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    queued (delivered after the current turn)
    queued (delivered after the current turn)
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123  queued…
    |}];
  H.esc h;
  [%expect {| (Rpc (method_ abort) (params ()) (tag Abort_done)) |}];
  H.reply h Abort_done {|{"restored":["first","second"]}|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    queued (delivered after the current turn)
    queued (delivered after the current turn)
    restored 2 queued messages to the editor
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > first

      second▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123  queued…
    |}];
  H.event h (Queue_update { steer = 0; follow_up = 0 });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    queued (delivered after the current turn)
    queued (delivered after the current turn)
    restored 2 queued messages to the editor
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > first

      second▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123  ⠋ work…
    |}]
;;

let%expect_test "scroll stays anchored while streaming" =
  let h = connected ~width:100 ~height:10 () in
  H.keys h "prompt";
  H.enter h;
  [%expect
    {| (Rpc (method_ prompt) (params ((text prompt))) (tag Show_error)) |}];
  H.event h (Message_start (Assistant partial));
  let stream from count =
    List.iter
      (List.init count ~f:(fun i -> sprintf "line %d\n" (from + i)))
      ~f:(fun text ->
        H.event h (Message_update { partial; delta = Text_delta text }))
  in
  stream 0 12;
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
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123
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
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123
    |}];
  stream 12 10;
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
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123  ↓ 10 n…
    |}];
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
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123  ↓ 10 n…
    |}];
  H.key h (Key.plain End);
  H.show h;
  [%expect
    {|
    line 15
    line 16
    line 17
    line 18
    line 19
    line 20
    line 21
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123
    |}]
;;

let%expect_test "page_down past the bottom returns to follow" =
  let h = connected ~width:100 ~height:10 () in
  H.event h (Message_start (Assistant partial));
  let stream =
    List.iter
      (List.init 20 ~f:(fun i -> sprintf "line %d\n" i))
      ~f:(fun text ->
        H.event h (Message_update { partial; delta = Text_delta text }))
  in
  stream;
  H.key h (Key.plain Page_up);
  H.show h;
  [%expect
    {|
    line 8
    line 9
    line 10
    line 11
    line 12
    line 13
    line 14
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123
    |}];
  H.key h (Key.plain Page_up);
  H.show h;
  [%expect
    {|
    line 3
    line 4
    line 5
    line 6
    line 7
    line 8
    line 9
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123
    |}];
  H.key h (Key.plain Page_down);
  H.show h;
  [%expect
    {|
    line 8
    line 9
    line 10
    line 11
    line 12
    line 13
    line 14
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123
    |}];
  H.key h (Key.plain Page_down);
  H.show h;
  [%expect
    {|
    line 13
    line 14
    line 15
    line 16
    line 17
    line 18
    line 19
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123
    |}]
;;

let%expect_test "home/end with text in the editor move the cursor, not the \
                 viewport"
  =
  let h = connected ~width:100 ~height:10 () in
  H.event h (Message_start (Assistant partial));
  let stream =
    List.iter
      (List.init 20 ~f:(fun i -> sprintf "line %d\n" i))
      ~f:(fun text ->
        H.event h (Message_update { partial; delta = Text_delta text }))
  in
  stream;
  H.key h (Key.plain Page_up);
  H.keys h "draft";
  H.key h (Key.plain End);
  H.show h;
  [%expect
    {|
    line 8
    line 9
    line 10
    line 11
    line 12
    line 13
    line 14
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > draft▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123
    |}];
  H.key h (Key.plain Home);
  H.show h;
  [%expect
    {|
    line 8
    line 9
    line 10
    line 11
    line 12
    line 13
    line 14
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏raft
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123
    |}]
;;

let%expect_test "abort restore is singular, prepends, and tolerates no field" =
  let h = connected ~width:100 () in
  H.event h (State (state ~running:true ()));
  H.keys h "queued";
  H.enter h;
  [%expect
    {| (Rpc (method_ steer) (params ((text queued))) (tag Show_error)) |}];
  H.event h (Queue_update { steer = 1; follow_up = 0 });
  H.keys h "draft";
  H.reply h Abort_done {|{"restored":["queued"]}|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    queued (delivered after the current turn)
    restored 1 queued message to the editor
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > queued

      draft▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123  queued…
    |}];
  H.reply h Abort_done {|{}|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    queued (delivered after the current turn)
    restored 1 queued message to the editor
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > queued

      draft▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:300  $0.0123  queued…
    |}]
;;

let%expect_test "two parallel subagents: strip, live tails, focus cycling, Esc" =
  let h = connected ~width:80 ~height:18 () in
  H.keys h "go";
  H.enter h;
  [%expect {| (Rpc (method_ prompt) (params ((text go))) (tag Show_error)) |}];
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (User "go"));
  let call1 =
    tool_call
      ~name:"subagent"
      ~arguments:{|{"task":"find all auth code in the repository"}|}
      "c1"
  in
  let call2 =
    tool_call ~name:"subagent" ~arguments:{|{"task":"run the test suite"}|} "c2"
  in
  H.event h (Tool_start call1);
  H.event
    h
    (Subagent_start
       { call_id = "c1"
       ; agent_id = "c1"
       ; task = "find all auth code in the repository"
       ; model = "claude-haiku"
       ; tools = [ "read"; "grep" ]
       });
  H.event h (Tool_start call2);
  H.event
    h
    (Subagent_start
       { call_id = "c2"
       ; agent_id = "c2"
       ; task = "run the test suite"
       ; model = "claude-sonnet"
       ; tools = [ "bash" ]
       });
  H.event h (Subagent { call_id = "c1"; agent_id = "c1"; event = Turn_start });
  H.event
    h
    (Subagent
       { call_id = "c1"
       ; agent_id = "c1"
       ; event = Message_start (User "find all auth code in the repository")
       });
  H.event
    h
    (Subagent
       { call_id = "c1"
       ; agent_id = "c1"
       ; event =
           Tool_start
             (tool_call
                ~name:"bash"
                ~arguments:{|{"command":"grep -r auth src"}|}
                "t1")
       });
  H.event h (Subagent { call_id = "c2"; agent_id = "c2"; event = Turn_start });
  H.event
    h
    (Subagent
       { call_id = "c2"
       ; agent_id = "c2"
       ; event =
           Tool_start
             (tool_call
                ~name:"bash"
                ~arguments:{|{"command":"dune runtest"}|}
                "t2")
       });
  H.event
    h
    (Subagent
       { call_id = "c2"
       ; agent_id = "c2"
       ; event = Message_update { partial; delta = Text_delta "all tests pass" }
       });
  H.event
    h
    (Subagent
       { call_id = "c2"
       ; agent_id = "c2"
       ; event = Message_end (assistant "all tests pass")
       });
  H.event
    h
    (Subagent_end
       { call_id = "c2"
       ; agent_id = "c2"
       ; usage = { input = 10; output = 5; cache_read = 0 }
       ; turns = 1
       ; cost_usd = 0.02
       ; result = { text = "all tests pass"; is_error = false }
       });
  (* The matching [Tool_end] arrives too and must not duplicate the item. *)
  H.event
    h
    (Tool_end
       { call = call2
       ; result = tool_result ~name:"subagent" ~id:"c2" "all tests pass"
       });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    > go
    ⚙ subagent "find all auth code in the repository" … 1 turns
      ⚙ bash grep -r auth src
    ⚙ subagent "run the test suite" ✓ 1 turns $0.02
      all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    agents: [main] 1⠋ 2✓  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:…
    |}];
  H.next_agent h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/2  claude-haiku  ⠋ running 1 turns  "find all auth code in the rep…











    > find all auth code in the repository
    ⚙ bash command=grep -r auth src
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    agents: main [1⠋] 2✓  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:…
    |}];
  H.next_agent h;
  H.show h;
  [%expect
    {|
    ◆ subagent 2/2  claude-sonnet  ✓ done 1 turns $0.02  "run the test suite"











    ⚙ bash command=dune runtest
    all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    agents: main 1⠋ [2✓]  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:…
    |}];
  H.next_agent h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    > go
    ⚙ subagent "find all auth code in the repository" … 1 turns
      ⚙ bash grep -r auth src
    ⚙ subagent "run the test suite" ✓ 1 turns $0.02
      all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    agents: [main] 1⠋ 2✓  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:…
    |}];
  H.focus_agent h 2;
  H.show h;
  [%expect
    {|
    ◆ subagent 2/2  claude-sonnet  ✓ done 1 turns $0.02  "run the test suite"











    ⚙ bash command=dune runtest
    all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    agents: main 1⠋ [2✓]  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:…
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    > go
    ⚙ subagent "find all auth code in the repository" … 1 turns
      ⚙ bash grep -r auth src
    ⚙ subagent "run the test suite" ✓ 1 turns $0.02
      all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    agents: [main] 1⠋ 2✓  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in:1.2k out:…
    |}]
;;

let%expect_test "verbosity applies inside an agent view" =
  let h = connected ~width:70 ~height:16 () in
  H.keys h "/verbosity normal";
  H.enter h;
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_start
       (tool_call ~name:"subagent" ~arguments:{|{"task":"audit"}|} "c1"));
  H.event
    h
    (Subagent_start
       { call_id = "c1"
       ; agent_id = "c1"
       ; task = "audit"
       ; model = "claude-haiku"
       ; tools = [ "bash" ]
       });
  H.event h (Subagent { call_id = "c1"; agent_id = "c1"; event = Turn_start });
  let call = tool_call ~name:"bash" ~arguments:{|{"command":"ls"}|} "t1" in
  H.event
    h
    (Subagent { call_id = "c1"; agent_id = "c1"; event = Tool_start call });
  let lines =
    String.concat ~sep:"\n" (List.init 8 ~f:(fun i -> sprintf "line %d" i))
  in
  H.event
    h
    (Subagent
       { call_id = "c1"
       ; agent_id = "c1"
       ; event = Tool_end { call; result = tool_result ~id:"t1" lines }
       });
  H.event
    h
    (Subagent
       { call_id = "c1"
       ; agent_id = "c1"
       ; event = Message_end (assistant "the report")
       });
  H.next_agent h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ⠋ running 1 turns  "audit"




    ⚙ bash command=ls
      line 0
      line 1
      line 2
      line 3
      line 4
      … (3 more)
    ──────────────────────────────────────────────────────────────────────
    > ▏
    agents: main [1⠋]  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in…
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ⠋ running 1 turns  "audit"
      {
        command: "ls"
      }
      line 0
      line 1
      line 2
      line 3
      line 4
      line 5
      line 6
      line 7
    ──────────────────────────────────────────────────────────────────────
    > ▏
    agents: main [1⠋]  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:verbose  ctx:1.5k (0%)  i…
    |}]
;;

let%expect_test "/agents picker lists task, status and model; Enter focuses" =
  let h = connected ~width:70 ~height:16 () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Subagent_start
       { call_id = "c1"
       ; agent_id = "c1"
       ; task = "find auth"
       ; model = "claude-haiku"
       ; tools = [ "read" ]
       });
  H.event
    h
    (Subagent_start
       { call_id = "c2"
       ; agent_id = "c2"
       ; task = "run tests"
       ; model = "claude-sonnet"
       ; tools = [ "bash" ]
       });
  H.event
    h
    (Subagent_end
       { call_id = "c2"
       ; agent_id = "c2"
       ; usage = { input = 1; output = 2; cache_read = 0 }
       ; turns = 3
       ; cost_usd = 0.02
       ; result = { text = "green"; is_error = false }
       });
  H.keys h "/agents";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    Subagents  (2)
    / ▏
      find auth  running  claude-haiku
      run tests  done 3 turns $0.02  claude-sonnet
    ──────────────────────────────────────────────────────────────────────
    agents: [main] 1⠋ 2✓  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in…
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/2  claude-haiku  ⠋ running 0 turns  "find auth"











    ──────────────────────────────────────────────────────────────────────
    > ▏
    agents: main [1⠋] 2✓  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in…
    |}]
;;

let%expect_test "new user prompt clears finished agents" =
  let h = connected ~width:70 ~height:14 () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Subagent_start
       { call_id = "c1"
       ; agent_id = "c1"
       ; task = "look around"
       ; model = "claude-haiku"
       ; tools = [ "read" ]
       });
  H.event
    h
    (Subagent_end
       { call_id = "c1"
       ; agent_id = "c1"
       ; usage = { input = 1; output = 2; cache_read = 0 }
       ; turns = 1
       ; cost_usd = 0.01
       ; result = { text = "done"; is_error = false }
       });
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    agents: [main] 1✓  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in…
    |}];
  H.keys h "next task";
  H.enter h;
  [%expect
    {| (Rpc (method_ prompt) (params ((text "next task"))) (tag Show_error)) |}];
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in…
    |}]
;;

let%expect_test "session reload drops subagent transcripts and focus" =
  let h = connected ~width:70 ~height:14 () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Subagent_start
       { call_id = "c1"
       ; agent_id = "c1"
       ; task = "look"
       ; model = "m"
       ; tools = []
       });
  H.event
    h
    (Subagent_end
       { call_id = "c1"
       ; agent_id = "c1"
       ; usage = { input = 1; output = 1; cache_read = 0 }
       ; turns = 1
       ; cost_usd = 0.0
       ; result = { text = "report"; is_error = false }
       });
  H.next_agent h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  m  ✓ done 1 turns $0.00  "look"









    ──────────────────────────────────────────────────────────────────────
    > ▏
    agents: main [1✓]  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in…
    |}];
  H.reply
    h
    Reload_messages
    {|[{"role":"user","text":"go"},{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"subagent","arguments":"{\"task\":\"look\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"},{"role":"tool_result","tool_call_id":"c1","tool_name":"subagent","text":"report","is_error":false}]|};
  [%expect {| (Rpc (method_ get_state) (params ()) (tag Initial_state)) |}];
  H.show h;
  [%expect
    {|
    > go
    ⚙ subagent task=look
      report
    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in…
    |}]
;;

let%expect_test "nested subagent events recurse into the parent's children" =
  let h = connected ~width:70 ~height:16 () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Subagent_start
       { call_id = "p"
       ; agent_id = "p"
       ; task = "outer task"
       ; model = "m1"
       ; tools = [ "subagent" ]
       });
  H.event
    h
    (Subagent
       { call_id = "p"
       ; agent_id = "p"
       ; event =
           Tool_start
             (tool_call
                ~name:"subagent"
                ~arguments:{|{"task":"inner task"}|}
                "c1")
       });
  H.event
    h
    (Subagent
       { call_id = "p"
       ; agent_id = "p"
       ; event =
           Subagent_start
             { call_id = "c1"
             ; agent_id = "p/c1"
             ; task = "inner task"
             ; model = "m2"
             ; tools = [ "read" ]
             }
       });
  H.event
    h
    (Subagent
       { call_id = "p"
       ; agent_id = "p"
       ; event =
           Subagent { call_id = "c1"; agent_id = "p/c1"; event = Turn_start }
       });
  H.event
    h
    (Subagent
       { call_id = "p"
       ; agent_id = "p"
       ; event =
           Subagent
             { call_id = "c1"
             ; agent_id = "p/c1"
             ; event =
                 Tool_start
                   (tool_call ~name:"read" ~arguments:{|{"path":"a.ml"}|} "t1")
             }
       });
  H.event
    h
    (Subagent
       { call_id = "p"
       ; agent_id = "p"
       ; event =
           Subagent_end
             { call_id = "c1"
             ; agent_id = "p/c1"
             ; usage = { input = 1; output = 1; cache_read = 0 }
             ; turns = 1
             ; cost_usd = 0.0
             ; result = { text = "inner done"; is_error = false }
             }
       });
  H.next_agent h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  m1  ⠋ running 0 turns  "outer task"









    ⚙ subagent "inner task" ✓ 1 turns $0.00
      inner done
    ──────────────────────────────────────────────────────────────────────
    > ▏
    agents: main [1⠋]  (Shift+Tab)
    deepseek/deepseek-flash  thinking:off  view:normal  ctx:1.5k (0%)  in…
    |}]
;;
