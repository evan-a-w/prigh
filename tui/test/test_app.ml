open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
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
    (match action with
     | Key key -> Coverage.record_key key
     | Intent intent -> Coverage.record intent
     | _ -> ());
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

  let set_pending t confirms =
    t.model <- { t.model with pending_confirms = confirms }
  ;;
end

let connected ?width ?height ?model () =
  let h = H.create ?width ?height () in
  H.step ~quiet:true h Start;
  H.reply ~quiet:true h Initial_state (state_json ?model ());
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
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    Load_history
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text "list the files"))) (tag Show_error))
    (Append_history "list the files")
    |}];
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
    …deepseek-flash  $0.01  ⠋ working (Esc aborts; Enter steers)
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
    …deepseek-flash  $0.01  ⠋ working (Esc aborts; Enter steers)
    |}];
  (* Steering while running is queued, not sent as a prompt. *)
  H.keys h "also count them";
  H.enter h;
  [%expect
    {|
    (Rpc (method_ steer) (params ((text "also count them"))) (tag Show_error))
    (Append_history "also count them")
    |}];
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
    earlier answer
    > list the files
      let me look
    Sure, here they are:
    first line done
    ⚙ bash command=ls -la⏎/work
      a.ml
      b.ml
      partial
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    * DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M …
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …claude-fable-5-1  think:off  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    * DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M …
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
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
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Log out of deepseek and delete its credential? (y/n)     │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  confirm: y / n
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
    …deepseek-flash  $0.01  ⠋ working (Esc aborts; Enter steers)
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
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Log in ─────────────────────────────────────────────────┐
    │ Open this URL to log in:                                 │
    │   https://claude.ai/oauth?x=1                            │
    │ Approve in the browser, then paste the code.             │
    │ Paste your Anthropic API key                             │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? *************▏
    …deepseek-flash  $0.01  login: Enter answers, Esc cancels
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
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    logged in to anthropic (api_key)
    model set to anthropic/claude-fable-5; /model to change
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    login to anthropic failed: denied
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  ctx:0% 1.5k  $0.01  Ctrl+C again quits
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
      /hotkeys                        show keyboard shortcuts
      /model [name|id|provider/id]    pick or switch the model
      /scoped-models                  pick the models Ctrl+P cy…
      /login [provider] [api_key|oauth]  log in to a provider
      /logout [provider]              remove a provider's store…
      /thinking [off|on|low|high|max]  pick or set the thinking…
      /verbosity [quiet|normal|verbose]  set the transcript ver…
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
    |}];
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
    earlier answer
    ────────────────────────────────────────────────────────────
    > /▏
      /help                           show commands and keys
    ▸ /hotkeys                        show keyboard shortcuts
      /model [name|id|provider/id]    pick or switch the model
      /scoped-models                  pick the models Ctrl+P cy…
      /login [provider] [api_key|oauth]  log in to a provider
      /logout [provider]              remove a provider's store…
      /thinking [off|on|low|high|max]  pick or set the thinking…
      /verbosity [quiet|normal|verbose]  set the transcript ver…
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
    |}];
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
    earlier answer
    ────────────────────────────────────────────────────────────
    > /▏
      /help                           show commands and keys
      /hotkeys                        show keyboard shortcuts
    ▸ /model [name|id|provider/id]    pick or switch the model
      /scoped-models                  pick the models Ctrl+P cy…
      /login [provider] [api_key|oauth]  log in to a provider
      /logout [provider]              remove a provider's store…
      /thinking [off|on|low|high|max]  pick or set the thinking…
      /verbosity [quiet|normal|verbose]  set the transcript ver…
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
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
    > /model ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
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
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
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
      /scoped-models                pick the models Ctrl+P cycl…
      /import [path]                import a session from a JSO…
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
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
    …deepseek-flash  $0.01  ⠋ working (Esc aborts; Enter steers)
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
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
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
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
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
    (Append_history "look at @src/app.ml")
    |}];
  (* Unknown @tokens stay plain text and produce no attachments param. *)
  H.step ~quiet:true h (Intent (Insert "check @nope"));
  H.enter h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text "check @nope"))) (tag Show_error))
    (Append_history "check @nope")
    |}]
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
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
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
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
    |}];
  H.key h (Key.plain Down);
  H.key h (Key.plain Enter);
  [%expect
    {|
    (Rpc
      (method_ set_cwd)
      (params ((path src/app.ml)))
      (tag (Notice_on_success "cwd changed")))
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
    {|[{"id":"1","path":"/home/u/.prigh/sessions/1.jsonl","name":"build fix","cwd":"/work","created_at":"2025-06-01T10:00:00Z","updated_at":"2025-06-01T10:00:11Z","first_prompt":"fix the build\nplease","message_count":12,"parent":null},{"id":"2","path":"/home/u/.prigh/sessions/2.jsonl","name":null,"cwd":"/other","created_at":"2025-06-02T11:30:00Z","updated_at":"2025-06-02T11:30:22Z","first_prompt":null,"message_count":0,"parent":null}]|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Sessions  (2)
    / ▏
    * build fix ∣ 2025-06-01T10:00 ∣ 12 msgs ∣ fix the build pl…
      (unnamed) ∣ 2025-06-02T11:30 ∣ 0 msgs ∣ (empty)  /other
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
  H.reply h Reload_messages {|{}|};
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    |}];
  H.reply
    ~quiet:true
    h
    Initial_messages
    {|[{"role":"user","text":"in the other session"}]|};
  H.show h;
  [%expect
    {|
    > in the other session
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.keys h "/logout deepseek";
  H.enter h;
  H.show h;
  [%expect
    {|
    > in the other session
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Log out of deepseek and delete its credential? (y/n)     │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  confirm: y / n
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.enter h;
  H.keys h "y";
  [%expect
    {| (Rpc (method_ logout) (params ((provider deepseek))) (tag Show_error)) |}];
  H.event h (Auth (Logged_out "deepseek"));
  [%expect {| (Rpc (method_ auth_status) (params ()) (tag Auth_refresh)) |}]
;;

let%expect_test "/name sets the name directly or prompts in a text dialog" =
  let h = connected () in
  H.keys h "/name my session";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_session_name)
      (params ((name "my session")))
      (tag (Notice_on_success "session named")))
    |}];
  H.reply h (Notice_on_success "session named") {|{}|};
  H.event h (State (state ~session_name:"my session" ()));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    session named
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.keys h "/name";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    session named
    ┌─ Session name ───────────────────────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    …deepseek-flash  ctx:0% 1.5k  Enter submits, Esc cancels
    |}];
  H.keys h "renamed";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_session_name)
      (params ((name renamed)))
      (tag (Notice_on_success "session named")))
    |}];
  H.esc h;
  H.mode h;
  [%expect {| editing |}]
;;

let%expect_test "/session prints the stats table" =
  let h = connected () in
  H.keys h "/session";
  H.enter h;
  [%expect {| (Rpc (method_ session_stats) (params ()) (tag Session_stats)) |}];
  H.reply h Session_stats stats_json;
  H.show h;
  [%expect
    {|
    messages       4
    turns          2
    tools          bash 1, read 2
    usage          in 30 out 13 cache 5
    cost           $0.0001
    context        1.5%
    model changes  1
    compactions    0
    duration       12.5s
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "/sessions Ctrl+N filters named only; Ctrl+D confirms delete" =
  let h = connected () in
  H.keys h "/sessions";
  H.enter h;
  H.reply h Sessions_picker sessions_json;
  H.key h (Key.ctrl 'n');
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))



    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Sessions (named)  (1)
    / ▏
    * build fix ∣ 2025-06-01T10:00 ∣ 12 msgs ∣ fix the build pl…
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.ctrl 'n');
  H.key h (Key.ctrl 'd');
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Delete session build fix? (y/n)                          │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  confirm: y / n
    |}];
  H.keys h "y";
  [%expect
    {|
    (Rpc
      (method_ delete_session)
      (params ((path /home/u/.prigh/sessions/1.jsonl)))
      (tag Deleted_session))
    |}];
  H.reply h Deleted_session {|{}|};
  [%expect
    {| (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker)) |}];
  H.reply h Sessions_picker sessions_json;
  H.mode h;
  [%expect {| picker |}];
  H.esc h;
  H.mode h;
  [%expect {| editing |}]
;;

let%expect_test "/fork picks a user message, forks at it and prefills the \
                 editor"
  =
  let h = connected () in
  H.keys h "/fork";
  H.enter h;
  [%expect {| (Rpc (method_ get_entries) (params ()) (tag Entries_for_fork)) |}];
  H.reply h Entries_for_fork entries_json;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Fork at  (2)
    / ▏
      first question   #1
    * second question  #2
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.enter h;
  [%expect {| (Rpc (method_ fork) (params ((at u2))) (tag Reload_messages)) |}];
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > second question
      more▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "/rewind picks a user message, then confirms the rewind" =
  let h = connected () in
  H.keys h "/rewind";
  H.enter h;
  H.reply h Entries_for_rewind entries_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_rewind))


    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Rewind to  (2)
    / ▏
      first question   #1
    * second question  #2
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Rewind to "second question"? Later messages are abandon… │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  confirm: y / n
    |}];
  H.keys h "y";
  [%expect
    {| (Rpc (method_ rewind) (params ((to u2))) (tag Reload_messages)) |}]
;;

let%expect_test "/tree renders branches with the active path marked" =
  let h = connected () in
  H.keys h "/tree";
  H.enter h;
  [%expect
    {| (Rpc (method_ get_entries) (params ((all true))) (tag Entries_for_tree)) |}];
  H.reply h Entries_for_tree tree_json;
  H.show h;
  [%expect
    {|
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Session tree  (5)
    / ▏
    * > root question
    *   · first answer
    *     · second answer
        · branch answer
          ⚙ branch tool
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.enter h;
  [%expect
    {| (Rpc (method_ rewind) (params ((to ab))) (tag Reload_messages)) |}]
;;

let%expect_test "/clone reloads and notices" =
  let h = connected () in
  H.keys h "/clone";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ clone)
      (params ())
      (tag (Reload_messages_notice "cloned session")))
    |}];
  H.reply h (Reload_messages_notice "cloned session") {|{}|};
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    |}];
  H.reply ~quiet:true h Initial_messages {|[]|};
  H.show h;
  [%expect
    {|
    cloned session
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "/export chooses jsonl by extension, or prompts for a path" =
  let h = connected () in
  H.step h (Intent (Insert "/export data.jsonl"));
  H.enter h;
  [%expect
    {|
    (List_paths (prefix data.jsonl) (tag (Paths_for_autocomplete data.jsonl)))
    (Rpc
      (method_ export)
      (params (
        (format jsonl)
        (path   data.jsonl)))
      (tag Export_done))
    |}];
  H.step h (Intent (Insert "/export notes.md"));
  H.enter h;
  [%expect
    {|
    (List_paths (prefix notes.md) (tag (Paths_for_autocomplete notes.md)))
    (Rpc
      (method_ export)
      (params (
        (format markdown)
        (path   notes.md)))
      (tag Export_done))
    |}];
  H.reply h Export_done {|{"path":"/tmp/notes.md"}|};
  H.step h (Intent (Insert "/export "));
  H.enter h;
  H.show h;
  [%expect
    {|
    (List_paths (prefix "") (tag (Paths_for_autocomplete "")))


    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    exported to /tmp/notes.md
    ┌─ Export to ──────────────────────────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    …deepseek-flash  ctx:0% 1.5k  Enter submits, Esc cancels
    |}];
  H.step h (Intent (Insert "out.md"));
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ export)
      (params (
        (format markdown)
        (path   out.md)))
      (tag Export_done))
    |}]
;;

let%expect_test "/import imports a path or prompts for one" =
  let h = connected () in
  H.step h (Intent (Insert "/import saved.jsonl"));
  H.enter h;
  [%expect
    {|
    (List_paths (prefix saved.jsonl) (tag (Paths_for_autocomplete saved.jsonl)))
    (Rpc (method_ import) (params ((path saved.jsonl))) (tag Reload_messages))
    |}];
  H.step h (Intent (Insert "/import "));
  H.enter h;
  H.show h;
  [%expect
    {|
    (List_paths (prefix "") (tag (Paths_for_autocomplete "")))



    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Import from ────────────────────────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    …deepseek-flash  ctx:0% 1.5k  Enter submits, Esc cancels
    |}];
  H.step h (Intent (Insert "other.jsonl"));
  H.enter h;
  [%expect
    {| (Rpc (method_ import) (params ((path other.jsonl))) (tag Reload_messages)) |}]
;;

let%expect_test "/cd changes the directory or prompts for a path" =
  let h = connected () in
  H.step h (Intent (Insert "/cd /var"));
  H.enter h;
  [%expect
    {|
    (List_paths (prefix /var) (tag (Paths_for_autocomplete /var)))
    (Rpc
      (method_ set_cwd)
      (params ((path /var)))
      (tag (Notice_on_success "cwd changed")))
    |}];
  H.step h (Intent (Insert "/cd "));
  H.enter h;
  H.show h;
  [%expect
    {|
    (List_paths (prefix "") (tag (Paths_for_autocomplete "")))



    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Change directory to ────────────────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    …deepseek-flash  ctx:0% 1.5k  Enter submits, Esc cancels
    |}];
  H.step h (Intent (Insert "/tmp"));
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_cwd)
      (params ((path /tmp)))
      (tag (Notice_on_success "cwd changed")))
    |}]
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.keys h "/clear";
  H.enter h;
  H.keys h "/help";
  H.enter h;
  H.show h;
  [%expect
    {|
    Ctrl+P                  cycle to the next scoped model
    (Shift+Ctrl+P is unavailable; Alt+P goes back)
    Alt+P                   cycle to the previous scoped model
    Ctrl+T                  cycle the thinking level
    Ctrl+N                  picker: toggle the named-only /
    logged-in-only filter
    Ctrl+X                  copy the last assistant message
    Ctrl+Z                  suspend to the shell
    Shift+Tab               cycle focus: main → agent 1 → … →
    main
    Alt+1                   focus agent N (Alt+1…9)
    Ctrl+C                  clear the editor, then (again) quit
    Ctrl+D                  quit
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.ctrl 'l');
  H.reply_error h Show_error "unknown method \"bogus\"";
  H.step h (Protocol_error "bad line");
  H.step h (Stderr "warning from backend");
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_models) (params ()) (tag (Models_for_picker "")))
    Alt+P                   cycle to the previous scoped model
    Ctrl+T                  cycle the thinking level
    Ctrl+N                  picker: toggle the named-only /
    logged-in-only filter
    Ctrl+X                  copy the last assistant message
    Ctrl+Z                  suspend to the shell
    Shift+Tab               cycle focus: main → agent 1 → … →
    main
    Alt+1                   focus agent N (Alt+1…9)
    Ctrl+C                  clear the editor, then (again) quit
    Ctrl+D                  quit
    unknown method "bogus"
    protocol error: bad line
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    (Append_history "run it")
    ⚙ bash command=ls -la
      out 0
      out 1
      out 2
      out 3
      out 4
      … (12 lines hidden)
      out 17
      out 18
      out 19
    all done
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:verbose  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:quiet  ctx:0% 1.5k  $0.01
    |}];
  H.keys h "/verbosity normal";
  H.enter h;
  H.show h;
  [%expect
    {|
      out 2
      out 3
      out 4
      … (12 lines hidden)
      out 17
      out 18
      out 19
    all done
    view: verbose — everything is shown
    view: quiet — intermediate output and thinking are hidden
    view: normal — tool output is summarised
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
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
    …deepseek-flash  think:off  view:verbose  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:quiet  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:quiet  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "reload pairs tool calls with their results" =
  let h = connected ~height:12 () in
  H.keys h "/verbosity quiet";
  H.enter h;
  H.reply h Reload_messages {|{}|};
  H.reply
    ~quiet:true
    h
    Initial_messages
    {|[{"role":"user","text":"go"},{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"ls\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"},{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"a.ml\nb.ml","is_error":false}]|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))







    > go
    ⚙ bash ls ✓ 2 lines
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:quiet  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  $0.01  ⠋ working (Esc aborts; Enter steers)
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
    …deepseek-flash  ctx:0% 1.5k
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
    …deepseek-flash  $0.01  ⠋ working (Esc aborts; Enter steers)
    |}]
;;

let%expect_test "multi-line editing: Alt+J, cursor movement, history" =
  let h = connected () in
  H.keys h "first line";
  H.key h (Key.alt (Char "j"));
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.enter h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text "first line\nsecond"))) (tag Show_error))
    (Append_history "first line\nsecond")
    |}];
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "backend crash shows the stderr tail; Ctrl+C quits" =
  let h = connected () in
  H.step h (Stderr "warn one");
  H.step h (Stderr "warn two");
  H.step h (Stderr "warn three");
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.step h Backend_closed;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    backend exited
    warn one
    warn two
    warn three
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  backend exited — Ctrl+C or /quit to exit
    |}];
  H.step h (Intent Model_picker);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    backend exited
    warn one
    warn two
    warn three
    backend is gone
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  backend exited — Ctrl+C or /quit to exit
    |}];
  H.key h (Key.ctrl 'c');
  [%expect {| Quit |}];
  H.keys h "x";
  H.enter h;
  [%expect {| |}];
  let h = connected () in
  H.step ~quiet:true h Backend_closed;
  H.keys h "/quit";
  H.enter h;
  [%expect {| Quit |}]
;;

let%expect_test "abort restores queued messages" =
  let h = connected ~width:100 () in
  H.event h (State (state ~running:true ()));
  H.keys h "first";
  H.enter h;
  [%expect
    {|
    (Rpc (method_ steer) (params ((text first))) (tag Show_error))
    (Append_history first)
    |}];
  H.keys h "second";
  H.enter h;
  [%expect
    {|
    (Rpc (method_ steer) (params ((text second))) (tag Show_error))
    (Append_history second)
    |}];
  H.event h (Queue_update { steer = 2; follow_up = 0 });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    queued (2): first ∣ second
    > ▏
    /work  deepseek-flash  think:off  ctx:0% 1.5k  $0.01  queued:2  ⠋ working (Esc aborts; Enter steers)
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
    restored 2 queued messages to the editor
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    queued (2): first ∣ second
    > first

      second▏
    /work  deepseek-flash  think:off  ctx:0% 1.5k  $0.01  queued:2  ⠋ working (Esc aborts; Enter steers)
    |}];
  H.event h (Queue_update { steer = 0; follow_up = 0 });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    restored 2 queued messages to the editor
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > first

      second▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
    |}]
;;

let%expect_test "scroll stays anchored while streaming" =
  let h = connected ~width:100 ~height:10 () in
  H.keys h "prompt";
  H.enter h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text prompt))) (tag Show_error))
    (Append_history prompt)
    |}];
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01  ↓ 10 new
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01  ↓ 10 new
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "abort restore is singular, prepends, and tolerates no field" =
  let h = connected ~width:100 () in
  H.event h (State (state ~running:true ()));
  H.keys h "queued";
  H.enter h;
  [%expect
    {|
    (Rpc (method_ steer) (params ((text queued))) (tag Show_error))
    (Append_history queued)
    |}];
  H.event h (Queue_update { steer = 1; follow_up = 0 });
  H.keys h "draft";
  H.reply h Abort_done {|{"restored":["queued"]}|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    restored 1 queued message to the editor
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    queued (1): queued
    > queued

      draft▏
    /work  deepseek-flash  think:off  ctx:0% 1.5k  $0.01  queued:1  ⠋ working (Esc aborts; Enter steers)
    |}];
  H.reply h Abort_done {|{}|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    restored 1 queued message to the editor
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    queued (1): queued
    > queued

      draft▏
    /work  deepseek-flash  think:off  ctx:0% 1.5k  $0.01  queued:1  ⠋ working (Esc aborts; Enter steers)
    |}]
;;

let%expect_test "two parallel subagents: strip, live tails, focus cycling, Esc" =
  let h = connected ~width:80 ~height:18 () in
  H.keys h "go";
  H.enter h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text go))) (tag Show_error))
    (Append_history go)
    |}];
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
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
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
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
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
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
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
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
    |}];
  H.focus_agent h 1;
  H.focus_agent h 2;
  H.show h;
  [%expect
    {|
    ◆ subagent 2/2  claude-sonnet  ✓ done 1 turns $0.02  "run the test suite"












    ⚙ bash command=dune runtest
    all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
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
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
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
      line 5
      line 6
      line 7
    ──────────────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0% 1.5k  ⠋ working (Esc aborts; Enter steers)
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ⠋ running 1 turns  "audit"
    ⚙ bash
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
    …deepseek-flash  ctx:0% 1.5k  ⠋ working (Esc aborts; Enter steers)
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
    …deepseek-flash  picker: type to filter, Enter selects, Esc closes
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/2  claude-haiku  ⠋ running 0 turns  "find auth"












    ──────────────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0% 1.5k  ⠋ working (Esc aborts; Enter steers)
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
    /work  deepseek-flash  think:off  ctx:0% 1.5k  $0.01  agents:[main] 1✓
    |}];
  H.keys h "next task";
  H.enter h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text "next task"))) (tag Show_error))
    (Append_history "next task")
    |}];
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
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
    …deepseek-flash  ctx:0% 1.5k  ⠋ working (Esc aborts; Enter steers)
    |}];
  H.reply h Reload_messages {|{}|};
  H.reply
    ~quiet:true
    h
    Initial_messages
    {|[{"role":"user","text":"go"},{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"subagent","arguments":"{\"task\":\"look\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"},{"role":"tool_result","tool_call_id":"c1","tool_name":"subagent","text":"report","is_error":false}]|};
  [%expect
    {|
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    |}];
  H.show h;
  [%expect
    {|
    > go
    ⚙ subagent task=look
      report
    ──────────────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0% 1.5k  ⠋ working (Esc aborts; Enter steers)
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
    …deepseek-flash  ctx:0% 1.5k  ⠋ working (Esc aborts; Enter steers)
    |}]
;;

let%expect_test "Alt+Enter queues a follow-up; queued block and status" =
  let h = connected ~width:160 () in
  H.event h (State (state ~running:true ()));
  H.keys h "after this";
  H.key h (Key.alt Enter);
  [%expect
    {|
    (Rpc (method_ follow_up) (params ((text "after this"))) (tag Show_error))
    (Append_history "after this")
    |}];
  H.event h (Queue_update { steer = 0; follow_up = 1 });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    queued (1): after this
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01  queued:1  ⠋ working (Esc aborts; Enter steers)
    |}];
  H.keys h "and more";
  H.key h (Key.alt Enter);
  [%expect
    {|
    (Rpc (method_ follow_up) (params ((text "and more"))) (tag Show_error))
    (Append_history "and more")
    |}];
  H.event h (Queue_update { steer = 0; follow_up = 2 });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    queued (2): after this ∣ and more
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01  queued:2  ⠋ working (Esc aborts; Enter steers)
    |}]
;;

let%expect_test "Alt+Up dequeues the last queued message into the editor" =
  let h = connected ~width:100 () in
  H.event h (State (state ~running:true ()));
  H.keys h "queued text";
  H.key h (Key.alt Enter);
  [%expect
    {|
    (Rpc (method_ follow_up) (params ((text "queued text"))) (tag Show_error))
    (Append_history "queued text")
    |}];
  H.event h (Queue_update { steer = 0; follow_up = 1 });
  H.keys h "draft";
  H.key h (Key.alt Key.Code.Up);
  [%expect {| (Rpc (method_ dequeue) (params ()) (tag Dequeued)) |}];
  H.reply h Dequeued {|{"text":"queued text","attachments":[]}|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    queued (1)
    > queued text

      draft▏
    /work  deepseek-flash  think:off  ctx:0% 1.5k  $0.01  queued:1  ⠋ working (Esc aborts; Enter steers)
    |}];
  H.event h (Queue_update { steer = 0; follow_up = 0 });
  H.key h (Key.alt Key.Code.Up);
  [%expect {| (Rpc (method_ dequeue) (params ()) (tag Dequeued)) |}];
  H.reply h Dequeued {|null|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    nothing queued
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > queued text

      draft▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
    |}]
;;

let%expect_test "inline bash: !cmd adds to context, !!cmd does not" =
  let h = connected ~width:100 () in
  H.keys h "!ls -la";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ shell)
      (params (
        (command        "ls -la")
        (add_to_context true)))
      (tag Show_error))
    (Append_history "!ls -la")
    |}];
  let call = tool_call ~name:"shell" ~arguments:{|{"command":"ls -la"}|} "s1" in
  H.event h (Tool_start call);
  H.event
    h
    (Tool_end { call; result = tool_result ~name:"shell" ~id:"s1" "a.ml\nb.ml" });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ⚙ shell command=ls -la
      a.ml
      b.ml
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.keys h "!!pwd";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ shell)
      (params (
        (command        pwd)
        (add_to_context false)))
      (tag Show_error))
    (Append_history !!pwd)
    |}];
  H.event h (State (state ~running:true ()));
  H.keys h "!echo hi";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Append_history "!echo hi")


    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ⚙ shell command=ls -la
      a.ml
      b.ml
    wait for the current turn
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
    |}]
;;

let%expect_test "bracketed paste renders a chip until the cursor enters it" =
  let h = connected ~width:80 ~height:14 () in
  H.step h (Intent (Paste "one\ntwo\nthree\nfour"));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────────────────────────
    > [4 lines pasted]▏
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.plain Up);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────────────────────────
    > one
      two
      thre▏
      four
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "history loads at start and is appended on submit" =
  let h = connected ~width:100 () in
  H.reply h History {|["old one","old two"]|};
  H.key h (Key.plain Up);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > old two▏
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.ctrl 'c');
  H.keys h "fresh prompt";
  H.enter h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text "fresh prompt"))) (tag Show_error))
    (Append_history "fresh prompt")
    |}]
;;

let%expect_test "Ctrl+G edits externally and the reply replaces the prompt" =
  let h = connected () in
  H.keys h "draft text";
  H.key h (Key.ctrl 'g');
  [%expect {| (Edit_externally "draft text") |}];
  H.reply h Editor_text {|"from the editor"|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > from the editor▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "Ctrl+X copies the last assistant message, or the focused \
                 report"
  =
  let h = connected () in
  H.key h (Key.ctrl 'x');
  [%expect {| (Copy_to_clipboard "earlier **answer**") |}];
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_start (tool_call ~name:"subagent" ~arguments:{|{"task":"t"}|} "c1"));
  H.event
    h
    (Subagent_start
       { call_id = "c1"; agent_id = "c1"; task = "t"; model = "m"; tools = [] });
  H.event h (Subagent { call_id = "c1"; agent_id = "c1"; event = Turn_start });
  H.event
    h
    (Subagent
       { call_id = "c1"
       ; agent_id = "c1"
       ; event = Message_update { partial; delta = Text_delta "the report" }
       });
  H.event
    h
    (Subagent
       { call_id = "c1"
       ; agent_id = "c1"
       ; event = Message_end (assistant "the report")
       });
  H.next_agent h;
  H.key h (Key.ctrl 'x');
  [%expect {| (Copy_to_clipboard "the report") |}]
;;

let%expect_test "Ctrl+Z suspends; Ctrl+R completes a path; Ctrl+L picks a model"
  =
  let h = connected () in
  H.key h (Key.ctrl 'z');
  [%expect {| Suspend |}];
  H.key h (Key.ctrl 'r');
  [%expect {| (List_paths (prefix "") (tag (Paths_for_autocomplete ""))) |}];
  H.keys h "sr";
  [%expect
    {|
    (List_paths (prefix s) (tag (Paths_for_autocomplete s)))
    (List_paths (prefix sr) (tag (Paths_for_autocomplete sr)))
    |}];
  H.reply h (Paths_for_autocomplete "sr") {|["src/","src/app.ml"]|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > @sr▏
    ▸ src/
      src/app.ml
    …deepseek-flash  ctx:0% 1.5k  Tab/Enter accept · Esc close
    |}];
  H.key h (Key.ctrl 'l');
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
    * DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M …
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "config: get_config at Start installs the reply; \
                 config_changed updates"
  =
  let h = H.create () in
  H.step h Start;
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    Load_history
    |}];
  H.reply
    h
    Config
    {|{"scoped_models":["anthropic/claude-fable-5-1"],"confirm_tools":true}|};
  print_s [%sexp (h.model.config : P.Config.t option)];
  H.event
    h
    (P.Event.Config_changed
       { scoped_models = [ "deepseek/deepseek-flash" ]; confirm_tools = false });
  print_s [%sexp (h.model.config : P.Config.t option)];
  [%expect
    {|
    (((scoped_models (anthropic/claude-fable-5-1)) (confirm_tools true)))
    (((scoped_models (deepseek/deepseek-flash)) (confirm_tools false)))
    |}]
;;

let%expect_test "/scoped-models: multi-select toggle, Ctrl+A, Ctrl+X, save" =
  let h = connected () in
  H.reply ~quiet:true h Models_catalog models_json;
  H.reply
    ~quiet:true
    h
    Config
    {|{"scoped_models":["deepseek/deepseek-flash"],"confirm_tools":false}|};
  H.keys h "/scoped-models";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Scoped models  (4)
    / ▏
    [ ] Claude Fable 5       Claude Fable 5
    [ ] Claude Fable 5.1     Claude Fable 5.1
    [ ] GPT-5.5              GPT-5.5
    [x] DeepSeek V4.1 Flash  DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.keys h " ";
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Scoped models  (4)
    / ▏
    [x] Claude Fable 5       Claude Fable 5
    [ ] Claude Fable 5.1     Claude Fable 5.1
    [ ] GPT-5.5              GPT-5.5
    [x] DeepSeek V4.1 Flash  DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.ctrl 'a');
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Scoped models  (4)
    / ▏
    [x] Claude Fable 5       Claude Fable 5
    [x] Claude Fable 5.1     Claude Fable 5.1
    [x] GPT-5.5              GPT-5.5
    [x] DeepSeek V4.1 Flash  DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.ctrl 'x');
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Scoped models  (4)
    / ▏
    [ ] Claude Fable 5       Claude Fable 5
    [ ] Claude Fable 5.1     Claude Fable 5.1
    [ ] GPT-5.5              GPT-5.5
    [ ] DeepSeek V4.1 Flash  DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.ctrl 'a');
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((
        config (
          (scoped_models (
            anthropic/claude-fable-5
            anthropic/claude-fable-5-1
            openai/gpt-5.5
            deepseek/deepseek-flash))
          (confirm_tools false)))))
      (tag Config_saved))
    |}];
  H.reply
    h
    Config_saved
    {|{"scoped_models":["anthropic/claude-fable-5","anthropic/claude-fable-5-1","openai/gpt-5.5","deepseek/deepseek-flash"],"confirm_tools":false}|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    scoped models saved (4)
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "Ctrl+P cycles the scoped models and wraps; Alt+P goes back" =
  let h = connected ~model:(model_json "claude-fable-5" "Claude Fable 5") () in
  H.reply ~quiet:true h Models_catalog models_json;
  H.reply
    ~quiet:true
    h
    Config
    {|{"scoped_models":["anthropic/claude-fable-5","anthropic/claude-fable-5-1","openai/gpt-5.5"],"confirm_tools":false}|};
  H.key h (Key.ctrl 'p');
  H.key h (Key.ctrl 'p');
  H.key h (Key.ctrl 'p');
  H.key h (Key.alt (Key.Code.Char "p"));
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5-1)))
      (tag Set_model_done))
    (Rpc
      (method_ set_model)
      (params ((model openai/gpt-5.5)))
      (tag Set_model_done))
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5)))
      (tag Set_model_done))
    (Rpc
      (method_ set_model)
      (params ((model openai/gpt-5.5)))
      (tag Set_model_done))

    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    model: anthropic/claude-fable-5-1
    model: openai/gpt-5.5
    model: anthropic/claude-fable-5
    model: openai/gpt-5.5
    ────────────────────────────────────────────────────────────
    > ▏
    /work  gpt-5.5  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "Ctrl+P with one scoped model notices instead of switching" =
  let h = connected () in
  H.reply ~quiet:true h Models_catalog models_json;
  H.reply
    ~quiet:true
    h
    Config
    {|{"scoped_models":["deepseek/deepseek-flash"],"confirm_tools":false}|};
  H.key h (Key.ctrl 'p');
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    only one model in scope
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "Ctrl+T cycles off/low/on/high/max and is n/a when unsupported" =
  let h =
    connected ~model:(model_json "claude-fable-5-1" "Claude Fable 5.1") ()
  in
  for _ = 1 to 5 do
    H.key h (Key.ctrl 't')
  done;
  H.show h;
  [%expect
    {|
    (Rpc (method_ set_thinking) (params ((thinking low))) (tag Show_error))
    (Rpc (method_ set_thinking) (params ((thinking on))) (tag Show_error))
    (Rpc (method_ set_thinking) (params ((thinking high))) (tag Show_error))
    (Rpc (method_ set_thinking) (params ((thinking max))) (tag Show_error))
    (Rpc (method_ set_thinking) (params ((thinking off))) (tag Show_error))
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    thinking: low
    thinking: on
    thinking: high
    thinking: max
    thinking: off
    ────────────────────────────────────────────────────────────
    > ▏
    …claude-fable-5-1  think:off  ctx:0% 1.5k  $0.01
    |}];
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json
       ~model:(model_json ~supports_thinking:false "gpt-5.5" "GPT-5.5")
       ());
  H.key h (Key.ctrl 't');
  H.show h;
  [%expect
    {|
    earlier answer
    thinking: low
    thinking: on
    thinking: high
    thinking: max
    thinking: off
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    thinking: n/a for this model
    ────────────────────────────────────────────────────────────
    > ▏
    /work  gpt-5.5  think:n/a  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "status line: width 120 full, width 40 keeps the model and \
                 drops the cwd"
  =
  let model =
    model_json ~context_window:145000 "claude-fable-5-1" "Claude Fable 5.1"
  in
  let h = connected ~width:120 ~height:20 ~model () in
  H.step ~quiet:true h (Set_home "/home/u");
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json
       ~model
       ~cwd:"/home/u/proj"
       ~git_branch:"main"
       ~session_name:"my session"
       ~thinking:"high"
       ~context_tokens:61000
       ~cost_usd:0.1234
       ());
  H.event h (Queue_update { steer = 1; follow_up = 0 });
  H.event
    h
    (Subagent_start
       { call_id = "c1"
       ; agent_id = "c1"
       ; task = "find auth"
       ; model = "claude-haiku"
       ; tools = []
       });
  H.event
    h
    (Subagent_start
       { call_id = "c2"
       ; agent_id = "c2"
       ; task = "run tests"
       ; model = "claude-sonnet"
       ; tools = []
       });
  H.event
    h
    (Subagent_end
       { call_id = "c2"
       ; agent_id = "c2"
       ; usage = { input = 1; output = 0; cache_read = 0 }
       ; turns = 1
       ; cost_usd = 0.02
       ; result = { text = "ok"; is_error = false }
       });
  print_endline (Content.Line.to_plain (Render.status h.model));
  [%expect
    {| ~/proj (main) "my session"  claude-fable-5-1  think:high  view:normal  ctx:42% 61k  $0.12  queued:1  agents:[main] 1⠋ 2✓ |}];
  H.step ~quiet:true h (Resize { width = 40; height = 20 });
  print_endline (Content.Line.to_plain (Render.status h.model));
  [%expect {| …claude-fable-5-1  ctx:42% 61k  queued:1 |}]
;;

let%expect_test "status context percentage is green/yellow/red" =
  let show tokens =
    let model = model_json ~context_window:100 "m" "M" in
    let h = connected ~model () in
    H.reply
      ~quiet:true
      h
      Initial_state
      (state_json ~model ~context_tokens:tokens ());
    let style =
      Render.status h.model
      |> List.find_map ~f:(fun (s : Content.Span.t) ->
        if String.is_prefix s.text ~prefix:"ctx:" then Some s.style else None)
    in
    print_s [%sexp (style : Style.t option)]
  in
  show 40;
  show 60;
  show 85;
  [%expect
    {|
    ((
      (fg        Green)
      (bold      false)
      (dim       false)
      (italic    false)
      (underline false)
      (invert    false)
      (strike    false)
      (link ())))
    ((
      (fg        Yellow)
      (bold      false)
      (dim       false)
      (italic    false)
      (underline false)
      (invert    false)
      (strike    false)
      (link ())))
    ((
      (fg        Red)
      (bold      false)
      (dim       false)
      (italic    false)
      (underline false)
      (invert    false)
      (strike    false)
      (link ())))
    |}]
;;

let%expect_test "/model Ctrl+N filters to logged-in models; scoped mark" =
  let h = connected () in
  H.reply ~quiet:true h Models_catalog models_json;
  H.reply
    ~quiet:true
    h
    Config
    {|{"scoped_models":["anthropic/claude-fable-5-1"],"confirm_tools":false}|};
  H.key h (Key.ctrl 'l');
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
      Claude Fable 5.1     anthropic/claude-fable-5-1 ◆  ctx 1.…
      GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 pe…
    * DeepSeek V4.1 Flash  deepseek/deepseek-flash  ctx 1.0M  $…
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.ctrl 'n');
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    Model (logged in)  (1)
    / ▏
    * DeepSeek V4.1 Flash  deepseek/deepseek-flash  ctx 1.0M  $…
    ────────────────────────────────────────────────────────────
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "login dialog is a bordered block with url, progress and prompt"
  =
  let h = connected () in
  H.event
    h
    (Auth
       (Auth_url
          { url = "https://claude.ai/oauth?x=1"
          ; instructions = "Approve in the browser."
          }));
  H.event h (Auth (Progress "waiting for browser"));
  H.event
    h
    (Auth
       (Prompt { id = "p1"; prompt = Secret { message = "Paste your API key" } }));
  H.keys h "sk-secret";
  H.show h;
  [%expect
    {|
    (Open_browser https://claude.ai/oauth?x=1)
    > earlier question
    earlier answer
    ┌─ Log in ─────────────────────────────────────────────────┐
    │ Open this URL to log in:                                 │
    │   https://claude.ai/oauth?x=1                            │
    │ Approve in the browser.                                  │
    │ waiting for browser                                      │
    │ Paste your API key                                       │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? *********▏
    …deepseek-flash  $0.01  login: Enter answers, Esc cancels
    |}]
;;

let%expect_test "confirm dialog is a bordered block" =
  let h = connected () in
  H.keys h "/logout deepseek";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Log out of deepseek and delete its credential? (y/n)     │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  confirm: y / n
    |}]
;;

let%expect_test "search: Ctrl+F, type, n, N, Esc" =
  let h = connected () in
  H.key h (Key.ctrl 'f');
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    / ▏
    …deepseek-flash  $0.01  search: type to find · Esc closes
    |}];
  H.keys h "earlier";
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    / earlier▏
    …deepseek-flash  search 1/2 · ↓↑ next/prev · Esc closes
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
    / earlier▏
    …deepseek-flash  search 2/2 · ↓↑ next/prev · Esc closes
    |}];
  H.key h (Key.plain Up);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    / earlier▏
    …deepseek-flash  search 1/2 · ↓↑ next/prev · Esc closes
    |}];
  H.esc h;
  H.mode h;
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
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "Ctrl+Up / Ctrl+Down jump between user messages at height 8" =
  let h = connected ~height:8 () in
  let add_exchange question answer =
    H.event h (Message_start (User question));
    H.event h (Message_update { partial; delta = Text_delta answer });
    H.event h (Message_end (assistant answer))
  in
  add_exchange "second question" "second answer";
  add_exchange "third question" "third answer";
  H.show h;
  [%expect
    {|
    earlier answer
    > second question
    second answer
    > third question
    third answer
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  let ctrl_up = { (Key.plain Up) with ctrl = true } in
  let ctrl_down = { (Key.plain Down) with ctrl = true } in
  H.key h ctrl_up;
  H.show h;
  [%expect
    {|
    > earlier question
    earlier answer
    > second question
    second answer
    > third question
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h ctrl_up;
  H.show h;
  [%expect
    {|
    > earlier question
    earlier answer
    > second question
    second answer
    > third question
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0% 1.5k  $0.01  ↓ 1 new
    |}];
  H.key h ctrl_down;
  H.show h;
  [%expect
    {|
    > second question
    second answer
    > third question
    third answer
    no earlier message
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h ctrl_down;
  H.show h;
  [%expect
    {|
    > second question
    second answer
    > third question
    third answer
    no earlier message
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h ctrl_down;
  H.show h;
  [%expect
    {|
    > second question
    second answer
    > third question
    third answer
    no earlier message
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  (* Past the last user message the viewport returns to Follow: a new notice is
     visible at the bottom instead of counted as hidden new lines. *)
  H.event h (Notice "tail notice");
  H.show h;
  [%expect
    {|
    second answer
    > third question
    third answer
    no earlier message
    tail notice
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "bash result at Normal shows the head and tail" =
  let h = connected () in
  let call : P.Tool_call.t =
    { id = "c1"; name = "bash"; arguments = {|{"command":"seq 20"}|} }
  in
  H.event h (Tool_start call);
  H.event
    h
    (Tool_end
       { call
       ; result =
           { tool_call_id = "c1"
           ; tool_name = "bash"
           ; text =
               String.concat
                 ~sep:"\n"
                 (List.init 20 ~f:(fun i -> sprintf "line %d" (i + 1)))
               ^ "\n"
           ; is_error = false
           }
       });
  H.show h;
  [%expect
    {|
      line 1
      line 2
      line 3
      line 4
      line 5
      … (12 lines hidden)
      line 18
      line 19
      line 20
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "/hotkeys prints the keys half of /help" =
  let h = connected () in
  H.keys h "/hotkeys";
  H.enter h;
  H.show h;
  [%expect
    {|
    Ctrl+N                  picker: toggle the named-only /
    logged-in-only filter
    Ctrl+X                  copy the last assistant message
    Ctrl+Z                  suspend to the shell
    Shift+Tab               cycle focus: main → agent 1 → … →
    main
    Alt+1                   focus agent N (Alt+1…9)
    Ctrl+C                  clear the editor, then (again) quit
    Ctrl+D                  quit
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "/help model prints one command's usage" =
  let h = connected () in
  H.keys h "/help model";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    /model [name|id|provider/id]  pick or switch the model
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "/help with an unknown command suggests the closest name" =
  let h = connected () in
  H.keys h "/help modle";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    unknown command /modle; did you mean /model?
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "tool confirm: allow, deny, queue and Esc-abort" =
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_confirm { call_id = "c1"; name = "bash"; summary = "rm -rf build" });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Run bash: rm -rf build? (y/n)                            │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  confirm: y / n
    |}];
  H.keys h "y";
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   true)))
      (tag Ignore))
    |}];
  H.mode h;
  [%expect {| editing |}];
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_confirm { call_id = "c1"; name = "bash"; summary = "rm -rf build" });
  H.keys h "n";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   false)))
      (tag Ignore))




    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    denied bash
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  $0.01  ⠋ working (Esc aborts; Enter steers)
    |}];
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_confirm { call_id = "c1"; name = "bash"; summary = "rm -rf build" });
  H.event
    h
    (Tool_confirm { call_id = "c2"; name = "write"; summary = "/work/a.txt" });
  H.event
    h
    (Tool_confirm { call_id = "c3"; name = "edit"; summary = "/work/b.txt" });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Run bash: rm -rf build? (y/n)                            │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  confirm: y / n
    |}];
  H.keys h "y";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   true)))
      (tag Ignore))


    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Write /work/a.txt? (y/n)                                 │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  confirm: y / n
    |}];
  H.keys h "y";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c2)
        (allow   true)))
      (tag Ignore))


    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Edit /work/b.txt? (y/n)                                  │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  ctx:0% 1.5k  $0.01  confirm: y / n
    |}];
  H.keys h "n";
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c3)
        (allow   false)))
      (tag Ignore))
    |}];
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.set_pending h [ "c1", "bash", "rm -rf build"; "c2", "write", "/work/a.txt" ];
  H.esc h;
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   false)))
      (tag Ignore))
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c2)
        (allow   false)))
      (tag Ignore))
    (Rpc (method_ abort) (params ()) (tag Abort_done))
    |}]
;;

let%expect_test "/confirm on saves confirm_tools through set_config" =
  let h = connected () in
  H.reply
    ~quiet:true
    h
    Config
    {|{"scoped_models":["deepseek/deepseek-flash"],"confirm_tools":false}|};
  H.keys h "/confirm on";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((
        config ((scoped_models (deepseek/deepseek-flash)) (confirm_tools true)))))
      (tag (Notice_on_success "tool confirmation on")))
    |}];
  H.reply h (Notice_on_success "tool confirmation on") {|{}|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    tool confirmation on
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.keys h "/confirm";
  H.esc h;
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    tool confirmation on
    tool confirmation on
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  (* When config is not loaded yet, /confirm off fetches it first. *)
  let h = connected () in
  H.keys h "/confirm off";
  H.enter h;
  [%expect
    {| (Rpc (method_ get_config) (params ()) (tag (Config_for_confirm false))) |}];
  H.reply
    h
    (Config_for_confirm false)
    {|{"scoped_models":[],"confirm_tools":true}|};
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((scoped_models ()) (confirm_tools false)))))
      (tag (Notice_on_success "tool confirmation off")))
    |}];
  H.reply h (Notice_on_success "tool confirmation off") {|{}|};
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    tool confirmation off
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "bash timeout is merged into the tool line" =
  let h = connected ~height:12 () in
  let call : P.Tool_call.t =
    { id = "c1"; name = "bash"; arguments = {|{"command":"sleep 999"}|} }
  in
  H.event h (Tool_start call);
  H.event
    h
    (Tool_end
       { call
       ; result =
           { tool_call_id = "c1"
           ; tool_name = "bash"
           ; text = "partial output\n[timed out after 120s]"
           ; is_error = true
           }
       });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ⚙ bash sleep 999 ✗ timed out after 120s
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "resize keeps the confirm dialog and picker within the width" =
  let h = connected ~width:120 ~height:16 () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_confirm { call_id = "c1"; name = "bash"; summary = "rm -rf build" });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │ Run bash: rm -rf build? (y/n)                                                                                        │
    └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01  confirm: y / n
    |}];
  H.step h (Resize { width = 40; height = 16 });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for
    commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    ┌─ Confirm ────────────────────────────┐
    │ Run bash: rm -rf build? (y/n)        │
    └──────────────────────────────────────┘
    ────────────────────────────────────────
    ? ▏
    …deepseek-flash  $0.01  confirm: y / n
    |}];
  let h = connected ~width:120 ~height:16 () in
  H.keys h "/sessions";
  H.enter h;
  H.reply ~quiet:true h Sessions_picker sessions_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))







    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    Sessions  (2)
    / ▏
    * build fix ∣ 2025-06-01T10:00 ∣ 12 msgs ∣ fix the build please  /work
      (unnamed) ∣ 2025-06-02T11:30 ∣ 0 msgs ∣ (empty)               /other
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0% 1.5k  picker: type to filter, Enter selects, Esc closes · Ctrl+N named only · Ctrl+D delete
    |}];
  H.step h (Resize { width = 40; height = 16 });
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for
    commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    Sessions  (2)
    / ▏
    * build fix ∣ 2025-06-01T10:00 ∣ 12 msg…
      (unnamed) ∣ 2025-06-02T11:30 ∣ 0 msgs…
    ────────────────────────────────────────
    …deepseek-flash  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "resize 120->40 mid-stream keeps every line" =
  let h = connected ~width:120 ~height:12 () in
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (Assistant partial));
  for i = 1 to 6 do
    H.event
      h
      (Message_update
         { partial
         ; delta =
             Text_delta
               (sprintf
                  "delta %d with several words that wrap at forty columns\n"
                  i)
         })
  done;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts, Ctrl+C twice quits.
    > earlier question
    earlier answer
    delta 1 with several words that wrap at forty columns
    delta 2 with several words that wrap at forty columns
    delta 3 with several words that wrap at forty columns
    delta 4 with several words that wrap at forty columns
    delta 5 with several words that wrap at forty columns
    delta 6 with several words that wrap at forty columns
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01  ⠋ working (Esc aborts; Enter steers)
    |}];
  H.step h (Resize { width = 40; height = 12 });
  H.show h;
  [%expect
    {|
    forty columns
    delta 3 with several words that wrap at
    forty columns
    delta 4 with several words that wrap at
    forty columns
    delta 5 with several words that wrap at
    forty columns
    delta 6 with several words that wrap at
    forty columns
    ────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "login end to end includes a prompt_cancelled" =
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
          ; instructions = "Approve in the browser."
          }));
  [%expect {| (Open_browser https://claude.ai/oauth?x=1) |}];
  H.event
    h
    (Auth
       (Prompt { id = "p1"; prompt = Secret { message = "Paste your API key" } }));
  H.step h (Intent (Insert "sk-ant-secret"));
  H.show h;
  [%expect
    {|
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ┌─ Log in ─────────────────────────────────────────────────┐
    │ Open this URL to log in:                                 │
    │   https://claude.ai/oauth?x=1                            │
    │ Approve in the browser.                                  │
    │ Paste your API key                                       │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? *************▏
    …deepseek-flash  $0.01  login: Enter answers, Esc cancels
    |}];
  H.event h (Auth (Prompt_cancelled { id = "p1" }));
  H.mode h;
  [%expect {| editing |}];
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.event h (Auth (Progress "exchanging code"));
  H.event h (Auth (Done { provider = "anthropic"; method_ = "api_key" }));
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ list_models) (params ()) (tag (Models_after_login anthropic)))
    |}];
  H.reply h (Models_after_login "anthropic") models_json;
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
    logged in to anthropic (api_key)
    model set to anthropic/claude-fable-5; /model to change
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "Ctrl+C closes every dialog and search" =
  let h = connected () in
  H.keys h "/sessions";
  H.enter h;
  H.reply ~quiet:true h Sessions_picker sessions_json;
  H.key h (Key.ctrl 'c');
  H.mode h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))
    editing
    |}];
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_confirm { call_id = "c1"; name = "bash"; summary = "rm -rf build" });
  H.key h (Key.ctrl 'c');
  H.mode h;
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   false)))
      (tag Ignore))
    editing
    |}];
  let h = connected () in
  H.event h (Auth (Prompt { id = "p1"; prompt = Secret { message = "key" } }));
  H.key h (Key.ctrl 'c');
  H.mode h;
  [%expect
    {|
    (Rpc (method_ auth_cancel) (params ()) (tag Show_error))
    editing
    |}];
  let h = connected () in
  H.keys h "/name";
  H.enter h;
  H.key h (Key.ctrl 'c');
  H.mode h;
  [%expect {| editing |}];
  let h = connected () in
  H.key h (Key.ctrl 'f');
  H.key h (Key.ctrl 'c');
  H.mode h;
  [%expect {| editing |}]
;;

let%expect_test "stderr lines are dim notices at Verbose only" =
  let h = connected () in
  H.step ~quiet:true h (Stderr "warning from backend");
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.keys h "/verbosity verbose";
  H.enter h;
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    backend: warning from backend
    view: verbose — everything is shown
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:verbose  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "wide characters keep the editor cursor column" =
  let h = connected ~width:40 () in
  H.step h (Intent (Insert "日本語"));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for
    commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────
    > 日本語▏
    …deepseek-flash  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.plain Home);
  H.key h (Key.plain Right);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for
    commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────
    > 日▏本語
    …deepseek-flash  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.plain End);
  H.step h (Intent (Insert "🐹!"));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for
    commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────
    > 日本語🐹!▏
    …deepseek-flash  ctx:0% 1.5k  $0.01
    |}];
  H.enter h;
  H.event h (Message_start (User "日本語🐹 wide"));
  H.event h (Message_update { partial; delta = Text_delta "emoji 🐹 and 日本語" });
  H.event h (Message_end (assistant "emoji 🐹 and 日本語"));
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params ((text "\230\151\165\230\156\172\232\170\158\240\159\144\185!")))
      (tag Show_error))
    (Append_history "\230\151\165\230\156\172\232\170\158\240\159\144\185!")


    session abc123 in /work. /help for
    commands, Esc aborts, Ctrl+C twice
    quits.
    > earlier question
    earlier answer
    > 日本語🐹 wide
    emoji 🐹 and 日本語
    ────────────────────────────────────────
    > ▏
    …deepseek-flash  ctx:0% 1.5k  $0.01
    |}]
;;

let%expect_test "editing intents: cursor, word, kill, yank, undo, delete" =
  let h = connected () in
  H.keys h "one two three";
  H.key h (Key.plain Left);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > one two thre▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.plain Right);
  H.key h (Key.alt (Char "b"));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > one two ▏hree
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.alt (Char "f"));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > one two three▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.alt (Char "d"));
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > one two three▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.ctrl 'k');
  H.key h (Key.ctrl 'u');
  H.key h (Key.ctrl 'w');
  H.key h (Key.ctrl 'y');
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > one two three▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}];
  H.key h (Key.alt (Char "y"));
  H.key h (Key.ctrl '_');
  H.key h (Key.plain Delete);
  H.key h (Key.plain Backspace);
  H.show h;
  [%expect
    {|
    session abc123 in /work. /help for commands, Esc aborts,
    Ctrl+C twice quits.
    > earlier question
    earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:normal  ctx:0% 1.5k  $0.01
    |}]
;;

(* Runs last in this file, after every scenario below has populated
   [Coverage.hit]. Every keymap binding must have been reached by a scenario. *)
let%expect_test "keymap: every binding is covered by a scenario" =
  List.iter Keymap.bindings ~f:(fun b ->
    let keys = List.map b.keys ~f:Key.to_string |> String.concat ~sep:"/" in
    printf
      "%-16s %-24s %s\n"
      keys
      (Sexp.to_string (Intent.sexp_of_t b.intent))
      (if Coverage.covered b.intent then "covered" else "MISSING"));
  [%expect
    {|
    Enter            Submit                   covered
    Alt+Enter        Queue_follow_up          covered
    Ctrl+J/Alt+J     Newline                  covered
    Esc              Cancel                   covered
    Tab              Complete                 covered
    Up               Up                       covered
    Down             Down                     covered
    Alt+Up           Dequeue                  covered
    Left             Left                     covered
    Right            Right                    covered
    Alt+B/Ctrl+Left  Word_left                covered
    Alt+F/Ctrl+Right Word_right               covered
    Alt+D            Delete_word_forward      covered
    Home/Ctrl+A      Home                     covered
    End/Ctrl+E       End                      covered
    PageUp           Page_up                  covered
    PageDown         Page_down                covered
    Ctrl+Up          Prev_user_message        covered
    Ctrl+Down        Next_user_message        covered
    Backspace/Ctrl+H Backspace                covered
    Delete           Delete                   covered
    Ctrl+K           Kill_to_end              covered
    Ctrl+U           Kill_to_start            covered
    Ctrl+W/Alt+Backspace Kill_word                covered
    Ctrl+Y           Yank                     covered
    Alt+Y            Yank_pop                 covered
    Ctrl+_           Undo                     covered
    Ctrl+O           Cycle_verbosity          covered
    Ctrl+R           Path_complete            covered
    Ctrl+F           Search                   covered
    Ctrl+G           Edit_externally          covered
    Ctrl+L           Model_picker             covered
    Ctrl+P           Next_model               covered
    Alt+P            Prev_model               covered
    Ctrl+T           Next_thinking            covered
    Ctrl+N           Picker_toggle_filter     covered
    Ctrl+X           Copy_last                covered
    Ctrl+Z           Suspend                  covered
    Shift+Tab        Next_agent               covered
    Alt+1            (Focus_agent 1)          covered
    Ctrl+C           Interrupt                covered
    Ctrl+D           Force_quit               covered
    |}]
;;
