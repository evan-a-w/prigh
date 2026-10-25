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
    t.model
    <- { t.model with
         pending_confirms =
           List.map confirms ~f:(fun (call_id, name, summary) ->
             { P.Pending.Confirm.call_id
             ; agent_id = None
             ; name
             ; summary
             ; rules = []
             })
       }
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
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > list the files▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text "list the files"))) (tag Show_error))
    (Append_history "list the files")
    |}];
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (user "list the files"));
  H.event h (Message_update { partial; delta = Thinking_delta "let me look" });
  H.event h (Message_update { partial; delta = Text_delta "Sure, here" });
  H.event
    h
    (Message_update { partial; delta = Text_delta " they are:\nfirst li" });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ list the files
    ┆ let me look
      Sure, here they are:
      first li
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  ⠋ writing · Esc aborts · Enter steers
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
    ▌ earlier question
      earlier answer

    ▌ list the files
    ┆ let me look
      Sure, here they are:
      first line done
    … bash $ ls -la …
        part
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ⠋ running tools · Esc aborts · Enter steers
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
           ; images = []
           ; at = None
           ; elided = None
           }
       });
  H.event h (Queue_update { steer = []; follow_up = [] });
  H.event h (Message_end (assistant "Sure, here they are:\nfirst line done"));
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
    ▌ list the files
    ┆ let me look
      Sure, here they are:
      first line done
    ✓ bash $ ls -la …
        a.ml
        b.ml
        partial
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      [aborted]
      error: HTTP 401: not logged in; use /login anthropic
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
       Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M …
       Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0…
       GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 p…
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M…
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "fable";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Model  (2)
    / fable▏
    ▸  Claude Fable 5    anthropic/claude-fable-5  ctx 1.0M  $1…
       Claude Fable 5.1  anthropic/claude-fable-5-1  ctx 1.0M  …
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.key h (Key.plain Down);
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5-1)))
      (tag (Set_model_done anthropic/claude-fable-5-1)))
    |}];
  H.mode h;
  H.event
    h
    (State (state ~model:(model_json "claude-fable-5-1" "Claude Fable 5.1") ()));
  H.show h;
  [%expect
    {|
    editing





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    claude-fable-5-1  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/model <display name> switches directly; unknown suggests and opens the \
     picker"
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
      (tag (Set_model_done anthropic/claude-fable-5-1)))
    |}];
  (* Models are cached now. *)
  H.keys h "/model GPT-5.5";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model openai/gpt-5.5)))
      (tag (Set_model_done openai/gpt-5.5)))
    |}];
  H.keys h "/model claude-fable";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5)))
      (tag (Set_model_done anthropic/claude-fable-5)))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.esc h;
  H.keys h "/model zzz";
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
       Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M …
       Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0…
       GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 p…
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M…
    ──────────────────── unknown model "zzz"; pick one below ───
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.esc h;
  (* A backend rejection (e.g. from another client's catalog) also recovers via
     the picker. *)
  H.reply_error h (Set_model_done "q") {|unknown model "q"; did you mean: a, b|};
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
       Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M …
       Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0…
       GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 p…
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M…
    ────────────────── unknown model "q"; did you mean: a, b ───
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;

let%expect_test
    "Esc closes autocomplete or dialog without aborting; Esc while running \
     aborts"
  =
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.keys h "/thinking";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ off
      low
      on
      high
      max
    ────────────────────────────────────────────────────────────
    > /thinking ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Log out of deepseek and delete its credential? (y/n)     │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  confirm: y / n
    |}];
  H.event
    h
    (Auth
       (Prompt
          { id = "p1"
          ; prompt = Secret { message = "API key"; allow_empty = false }
          }));
  [%expect {| (Rpc (method_ auth_cancel) (params ()) (tag Show_error)) |}];
  H.keys h "n";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────── cancelled ───
    > ▏
    deepseek-flash  $0.01  ⠋ working · Esc aborts · Enter steers
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
          ; prompt =
              Secret
                { message = "Paste your Anthropic API key"
                ; allow_empty = false
                }
          }));
  H.keys h "sk-ant-secret";
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    ┌─ Log in ─────────────────────────────────────────────────┐
    │ Open this URL to log in:                                 │
    │   https://claude.ai/oauth?x=1                            │
    │ Approve in the browser, then paste the code.             │
    │ Paste your Anthropic API key                             │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? *************▏
    deepseek-flash  $0.01  login: Enter answers · Esc cancels
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
      (tag (Set_model_done anthropic/claude-fable-5)))
    |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ model set to anthropic/claude-fable-5; /model to change  │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Choose a login method  (2)
    / ▏
    ▸  Claude Pro/Max  oauth
       API key         api_key
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.event
    h
    (Auth
       (Prompt
          { id = "p4"
          ; prompt = Secret { message = "key"; allow_empty = false }
          }));
  H.mode h;
  H.event h (Auth (Prompt_cancelled { id = "p4" }));
  H.mode h;
  H.event h (Auth (Failed { provider = "anthropic"; error = "denied" }));
  H.show h;
  [%expect
    {|
    login
    editing





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────── login to anthropic failed: denied ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'c');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Ctrl+C again quits
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
  H.event
    h
    (Auth
       (Prompt
          { id = "p1"
          ; prompt = Secret { message = "key"; allow_empty = false }
          }));
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
    ▸ /help                           show commands and keys
      /hotkeys                        show keyboard shortcuts
      /model [name|id|provider/id]    pick or switch the model
      /scoped-models                  pick the models Ctrl+P cy…
      /change_default                 save the current model an…
      /fallback [off|model...]        show or set the models th…
      /default-dir [off|path]         show or set the directory…
      /login [provider] [api_key|oauth]  log in to a provider, …
      ↕ 1–8 of 62
    ────────────────────────────────────────────────────────────
    > /▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
      /help                           show commands and keys
    ▸ /hotkeys                        show keyboard shortcuts
      /model [name|id|provider/id]    pick or switch the model
      /scoped-models                  pick the models Ctrl+P cy…
      /change_default                 save the current model an…
      /fallback [off|model...]        show or set the models th…
      /default-dir [off|path]         show or set the directory…
      /login [provider] [api_key|oauth]  log in to a provider, …
      ↕ 1–8 of 62
    ────────────────────────────────────────────────────────────
    > /▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
      /help                           show commands and keys
      /hotkeys                        show keyboard shortcuts
    ▸ /model [name|id|provider/id]    pick or switch the model
      /scoped-models                  pick the models Ctrl+P cy…
      /change_default                 save the current model an…
      /fallback [off|model...]        show or set the models th…
      /default-dir [off|path]         show or set the directory…
      /login [provider] [api_key|oauth]  log in to a provider, …
      ↕ 1–8 of 62
    ────────────────────────────────────────────────────────────
    > /▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /model ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/mo Enter opens argument completion over models; typing fab Enter calls \
     set_model"
  =
  let h = connected () in
  H.reply ~quiet:true h (Models_for_picker "") models_json;
  H.esc h;
  H.keys h "/mo";
  H.key h (Key.plain Enter);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ ask
      auto-edit
      auto
      plan
    ────────────────────────────────────────────────────────────
    > /mode ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides
    |}];
  H.keys h "fab";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /mode fab▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.plain Enter);
  [%expect {| |}]
;;

let%expect_test "Esc closes autocomplete without abort while running" =
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.keys h "/mo";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ /mode [ask|auto-edit|auto|plan]  pick or set the mode: wh…
      /model [name|id|provider/id]    pick or switch the model
      /scoped-models                  pick the models Ctrl+P cy…
      /import [path]                  import a session from a J…
      /permissions                    show the mode and the all…
    ────────────────────────────────────────────────────────────
    > /mo▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /mo▏
    deepseek-flash  $0.01  ⠋ working · Esc aborts · Enter steers
    |}];
  H.esc h;
  [%expect {| (Rpc (method_ abort) (params ()) (tag Abort_done)) |}]
;;

let%expect_test "@ completion is asynchronous and drops stale replies" =
  let h = connected () in
  H.keys h "@sr";
  [%expect
    {|
    (Rpc
      (method_ list_paths)
      (params ((prefix "")))
      (tag (Paths_for_autocomplete "")))
    (Rpc
      (method_ list_paths)
      (params ((prefix s)))
      (tag (Paths_for_autocomplete s)))
    (Rpc
      (method_ list_paths)
      (params ((prefix sr)))
      (tag (Paths_for_autocomplete sr)))
    |}];
  H.keys h "c";
  [%expect
    {|
    (Rpc
      (method_ list_paths)
      (params ((prefix src)))
      (tag (Paths_for_autocomplete src)))
    |}];
  H.reply h (Paths_for_autocomplete "sr") {|["stale/only"]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > @src▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.reply h (Paths_for_autocomplete "src") {|["src/","src/app.ml"]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ src/
      src/app.ml
    ────────────────────────────────────────────────────────────
    > @src▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}]
;;

let%expect_test "submit with @file sends attachments param" =
  let h = connected () in
  H.step h (Intent (Insert "look at @src/app.ml"));
  [%expect
    {|
    (Rpc
      (method_ list_paths)
      (params ((prefix src/app.ml)))
      (tag (Paths_for_autocomplete src/app.ml)))
    |}];
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ fix the build please  /work
      (empty)               /other
    ────────────────────────────────────────────────────────────
    > /switch ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides
    |}]
;;

let%expect_test "/cd completes paths and submits the selected one" =
  let h = connected () in
  H.step h (Intent (Insert "/cd sr"));
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params ((prefix sr)))
      (tag (Dirs_for_autocomplete sr)))
    |}];
  H.reply h (Dirs_for_autocomplete "sr") {|["src/"]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ src/
    ────────────────────────────────────────────────────────────
    > /cd sr▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.key h (Key.plain Down);
  H.key h (Key.plain Enter);
  [%expect
    {|
    (Rpc
      (method_ set_cwd)
      (params (
        (path       src/)
        (if_missing ask)))
      (tag (
        Retry_in_editor
        (command "/cd src/")
        (hint    "Tab after /cd completes directories")
        (then_ (Dir_changed Cwd)))))
    |}]
;;

let%expect_test
    "Alt+S and Esc then Enter send what is typed, not the highlight"
  =
  let h = connected () in
  let popup text =
    H.step ~quiet:true h (Intent (Insert text));
    H.reply ~quiet:true h (Dirs_for_autocomplete "new") {|["newer/"]|}
  in
  popup "/cd new";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ newer/
    ────────────────────────────────────────────────────────────
    > /cd new▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.key h (Key.alt (Char "s"));
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_cwd)
      (params (
        (path       new)
        (if_missing ask)))
      (tag (
        Retry_in_editor
        (command "/cd new")
        (hint    "Tab after /cd completes directories")
        (then_ (Dir_changed Cwd)))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  popup "/cd new";
  H.esc h;
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_cwd)
      (params (
        (path       new)
        (if_missing ask)))
      (tag (
        Retry_in_editor
        (command "/cd new")
        (hint    "Tab after /cd completes directories")
        (then_ (Dir_changed Cwd)))))
    |}];
  (* The /cd prompt's popup too. *)
  H.step ~quiet:true h (Intent (Insert "/cd "));
  H.enter h;
  popup "new";
  H.show h;
  H.key h (Key.alt (Char "s"));
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Change directory to ────────────────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ▸ newer/
    ────────────────────────────────────────────────────────────
    ? new▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    (Rpc
      (method_ set_cwd)
      (params (
        (path       new)
        (if_missing ask)))
      (tag (Dir_changed Cwd)))
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (2)  Alt+A (un)archives · Alt+D deletes archived
    / ▏
    ▸* build fix ∣ 2025-06-01T10:00 ∣ 12 msgs ∣ fix the build p…
       (unnamed) ∣ 2025-06-02T11:30 ∣ 0 msgs ∣ (empty)  /other
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter switches · Tab marks
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
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    |}];
  H.reply
    ~quiet:true
    h
    Initial_messages
    {|[{"role":"user","text":"in the other session"}]|};
  H.show h;
  [%expect
    {|
    ▌ in the other session
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/logout deepseek";
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ in the other session
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Log out of deepseek and delete its credential? (y/n)     │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  confirm: y / n
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
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))





    ▌ in the other session
    Log out of  (1)
    / ▏
    ▸  DeepSeek  api_key via auth.json
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────── session named ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/name";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Session name ───────────────────────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  ctx:0.1%/1.0M  Enter submits · Esc cancels
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
    ┌─ Session ────────────────────────────────────────────────┐
    │ messages       4                                         │
    │ turns          2                                         │
    │ tools          bash 1, read 2                            │
    │ usage          in 30 out 13 cache 5                      │
    │ cost           $0.0001                                   │
    │ context        1.5%                                      │
    │ model changes  1                                         │
    └──────────── 1–7 of 9 · ↑↓ PgUp PgDn scroll · Esc closes ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ↑↓ scroll · Esc closes
    |}]
;;

let%expect_test "/compact passes its instructions on" =
  let h = connected () in
  H.keys h "/compact keep the file names";
  H.enter h;
  H.keys h "/compact";
  H.enter h;
  [%expect {|
    (Rpc
      (method_ compact)
      (params ((instructions "keep the file names")))
      (tag Compact_done))
    (Rpc (method_ compact) (params ()) (tag Compact_done))
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


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions (named)  (1)  Alt+A (un)archives
    / ▏
    ▸* build fix ∣ 2025-06-01T10:00 ∣ 12 msgs ∣ fix the build p…
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter switches · Tab marks
    |}];
  H.key h (Key.ctrl 'n');
  (* Not the session we are in. *)
  H.key h (Key.ctrl 'd');
  H.key h (Key.plain Down);
  H.key h (Key.ctrl 'd');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Delete session 2 (0 B)? Enter deletes, Esc keeps it      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  confirm: y / n
    |}];
  H.keys h "y";
  [%expect
    {|
    (Rpc
      (method_ delete_sessions)
      (params ((paths (/home/u/.prigh/sessions/2.jsonl))))
      (tag Sessions_deleted))
    |}];
  H.mode h;
  [%expect {| picker |}];
  H.reply
    h
    Sessions_deleted
    {|{"deleted":[{"path":"/home/u/.prigh/sessions/2.jsonl","id":"2","bytes":0}],"refused":[]}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (1)  Alt+A (un)archives · Alt+D deletes archived
    / ▏
    ▸* build fix ∣ 2025-06-01T10:00 ∣ 12 msgs ∣ fix the build p…
    ──────────────────────────────── Deleted 1 session (0 B) ───
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter switches · Tab marks
    |}];
  H.esc h;
  H.mode h;
  [%expect {| editing |}]
;;

(* Sessions as [list_sessions] sends them (most recent first): two archived
   ones among the active, one running only through background work. *)
let archive_sessions_json =
  {|[
  {"id":"1","path":"/home/u/.prigh/sessions/1.jsonl","name":"build fix","cwd":"/work","created_at":"2025-06-05T10:00:00Z","updated_at":"2025-06-05T10:00:00Z","first_prompt":"fix the build","message_count":12,"live":true,"bytes":2048},
  {"id":"3a","path":"/s/3a.jsonl","name":null,"cwd":"/old","created_at":"2025-05-01T10:00:00Z","updated_at":"2025-06-04T10:00:00Z","first_prompt":"try a spike","message_count":4,"archived":true,"bytes":1500000},
  {"id":"2","path":"/s/2.jsonl","name":"deploy","cwd":"/work","created_at":"2025-06-03T10:00:00Z","updated_at":"2025-06-03T10:00:00Z","first_prompt":"deploy it","message_count":6,"live":true,"running":true,"background":2,"bytes":4096},
  {"id":"3b","path":"/s/3b.jsonl","name":"old spike","cwd":"/old","created_at":"2025-04-01T10:00:00Z","updated_at":"2025-04-02T10:00:00Z","first_prompt":"another spike","message_count":2,"archived":true,"bytes":1048576},
  {"id":"5","path":"/s/5.jsonl","name":null,"cwd":"/tmp","created_at":"2025-06-01T10:00:00Z","updated_at":"2025-06-01T10:00:00Z","first_prompt":null,"message_count":0,"bytes":100}
  ]|}
;;

let sessions_open ?(width = 100) ?(height = 16) () =
  let h = connected ~width ~height () in
  H.keys h "/sessions";
  H.enter h;
  H.reply h Sessions_picker archive_sessions_json;
  h
;;

let picker_rows h =
  List.iter (Render.screen h.H.model).lines ~f:(fun line ->
    let plain = Content.Line.to_plain line in
    if String.is_prefix plain ~prefix:" " || String.is_prefix plain ~prefix:"▸"
    then print_endline (Content.to_styled [ line ]))
;;

let%expect_test
    "sessions picker: active first, archived after a divider and dimmed; \
     background work shows as running"
  =
  let h = sessions_open () in
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
    ▸* ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
       ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
       (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
      ── archived ──
       (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
       old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  ctx:0.1%/1.0M  Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  picker_rows h;
  [%expect {|
      [gray]prigh in /work · /help · Esc aborts · Ctrl+C twice quits[/]
      earlier [bold]answer[/]
    [bold][invert]▸* ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build[/][dim][invert]  /work[/]
       ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it[dim]  /work[/]
       (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)   [dim]  /tmp[/]
    [dim]  ── archived ──[/]
    [dim]   (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike[/][dim]  /old[/]
    [dim]   old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike[/][dim]  /old[/]
    |}];
  (* Filtering keeps the sections apart. *)
  H.keys h "spike";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (2)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / spike▏
      ── archived ──
    ▸  (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
       old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  ctx:0.1%/1.0M  Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}]
;;

let%expect_test
    "sessions picker: Tab marks, Ctrl+D confirms with the count and size, Esc \
     goes back, Enter deletes and reports refusals"
  =
  let h = sessions_open () in
  H.key h (Key.plain Tab);
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
     ✓ ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
     ✓ ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
    ▸  (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
      ── archived ──
       (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
       old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  $0.01  2 marked (6.0 KB) · Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  (* Tab again unmarks. *)
  H.key h (Key.plain Up);
  H.key h (Key.plain Tab);
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
     ✓ ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
       ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
       (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
      ── archived ──
       (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
    ▸✓ old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  $0.01  2 marked (1.0 MB) · Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  (* Not the session we are in: it says so, the picker stays, Tab unmarks
     it. *)
  H.key h (Key.ctrl 'd');
  H.show h;
  H.key h (Key.plain Home);
  H.key h (Key.plain Tab);
  H.key h (Key.plain Tab);
  H.key h (Key.ctrl 'd');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
     ✓ ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
       ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
       (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
      ── archived ──
       (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
    ▸✓ old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ──── session "build fix" is the session you are in: switch to another one first, or leave it out ───
    >
    deepseek-flash  $0.01  2 marked (1.0 MB) · Enter switches · Tab marks · Ctrl+D deletes · Esc closes






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
    │ Delete 2 sessions (1.0 MB)? Enter deletes, Esc keeps them                                        │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
     * ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
     ✓ ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
    ▸  (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
      ── archived ──
       (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
     ✓ old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  $0.01  2 marked (1.0 MB) · Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  H.key h (Key.ctrl 'd');
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ delete_sessions)
      (params ((paths (/s/2.jsonl /s/3b.jsonl))))
      (tag Sessions_deleted))
    |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
     * ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
     ✓ ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
    ▸  (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
      ── archived ──
       (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
     ✓ old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  $0.01  2 marked (1.0 MB) · Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  H.reply
    h
    Sessions_deleted
    {|{"deleted":[{"path":"/s/3b.jsonl","id":"3b","bytes":1048576}],"refused":[{"path":"/s/2.jsonl","error":"session \"deploy\" is running: wait for it, or switch to it and /abort"}]}|};
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    ┌─ Warning ────────────────────────────────────────────────────────────────────────────────────────┐
    │ Deleted 1 session (1.0 MB)                                                                       │
    │ Not deleted: session "deploy" is running: wait for it, or switch to it and /abort                │
    └────────────────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
    Sessions  (4)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
     * ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
     ✓ ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
    ▸  (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
      ── archived ──
       (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  $0.01  1 marked (4.0 KB) · Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  (* Without marks Ctrl+D deletes the highlighted session. *)
  H.key h (Key.plain Up);
  H.key h (Key.plain Tab);
  H.key h (Key.ctrl 'd');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
    │ Delete session 5 (100 B)? Enter deletes, Esc keeps it                                            │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.keys h "n";
  H.mode h;
  [%expect {| picker |}]
;;

let%expect_test
    "sessions picker: Alt+A archives and unarchives at once; a refusal puts it \
     back"
  =
  let h = sessions_open () in
  (* A running session (here through background work) is never archived. *)
  H.key h (Key.plain Down);
  H.key h (Key.alt (Char "a"));
  H.key h (Key.plain Down);
  H.key h (Key.alt (Char "a"));
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))
    (Rpc
      (method_ set_archived)
      (params (
        (path     /s/5.jsonl)
        (archived true)))
      (tag (
        Archive_done
        (name     "session 5")
        (archived true))))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
     * ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
       ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
      ── archived ──
    ▸  (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
       old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
       (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  ctx:0.1%/1.0M  Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  H.reply_error
    h
    (Archive_done { name = "session 5"; archived = true })
    "no such session \"/s/5.jsonl\"";
  H.reply h Sessions_refresh archive_sessions_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_refresh))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
     * ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
       ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
       (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
      ── archived ──
    ▸  (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
       old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ─────────────────────────────────────────────────────────────────── no such session "/s/5.jsonl" ───
    >
    deepseek-flash  think:off  ctx:0.1%/1.0M  Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  (* An archived one comes back among the active. *)
  H.key h (Key.plain End);
  H.key h (Key.alt (Char "a"));
  H.reply
    h
    (Archive_done { name = "session \"old spike\""; archived = false })
    "{}";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_archived)
      (params (
        (path     /s/3b.jsonl)
        (archived false)))
      (tag (
        Archive_done
        (name     "session \"old spike\"")
        (archived false))))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
     * ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
       ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
       old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
       (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
      ── archived ──
    ▸  (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
    ───────────────────────────────────────────────────────────────── unarchived session "old spike" ───
    >
    deepseek-flash  think:off  ctx:0.1%/1.0M  Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}]
;;

let%expect_test
    "Alt+D and /delete-archived confirm deleting every archived session"
  =
  let h = sessions_open () in
  H.key h (Key.alt (Char "d"));
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
    │ Delete all 2 archived sessions (2.4 MB)? Enter deletes, Esc keeps them                           │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.enter h;
  H.mode h;
  [%expect
    {|
    (Rpc
      (method_ delete_sessions)
      (params ((paths (/s/3a.jsonl /s/3b.jsonl))))
      (tag Sessions_deleted))
    picker
    |}];
  H.esc h;
  H.keys h "/delete-archived";
  H.enter h;
  H.reply h Archived_to_delete archive_sessions_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Archived_to_delete))






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
    │ Delete all 2 archived sessions (2.4 MB)? Enter deletes, Esc keeps them                           │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.esc h;
  H.mode h;
  [%expect {| editing |}];
  H.keys h "/delete-archived";
  H.enter h;
  H.reply h Archived_to_delete sessions_json;
  H.mode h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Archived_to_delete))
    editing
    |}]
;;

let run h command =
  H.step ~quiet:true h (Intent (Insert command));
  H.step ~quiet:true h (Intent Cancel);
  H.enter h
;;

let%expect_test "/archive and /unarchive: the current session, or one named" =
  let h = connected () in
  run h "/archive";
  [%expect
    {|
    (Rpc
      (method_ set_archived)
      (params (
        (path     /home/u/.prigh/sessions/1.jsonl)
        (archived true)))
      (tag (
        Notice_on_success
        "archived this session (your next message unarchives it)")))
    |}];
  H.reply
    h
    (Notice_on_success "archived this session (your next message unarchives it)")
    "{}";
  run h "/unarchive";
  [%expect
    {|
    (Rpc
      (method_ set_archived)
      (params (
        (path     /home/u/.prigh/sessions/1.jsonl)
        (archived false)))
      (tag (Notice_on_success "unarchived this session")))
    |}];
  (* Completion lists the sessions; a name resolves through that list. *)
  H.keys h "/unarchive ";
  [%expect {| (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache)) |}];
  H.reply h Sessions_cache archive_sessions_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ try a spike  archived · /old
      old spike    archived · another spike · /old
    ────────────────────────────────────────────────────────────
    > /unarchive ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides
    |}];
  H.key h (Key.ctrl 'u');
  run h "/unarchive old spike";
  [%expect
    {|
    (Rpc
      (method_ list_sessions)
      (params ())
      (tag (
        Session_to_archive
        (arg      "old spike")
        (archived false))))
    |}];
  H.reply
    h
    (Session_to_archive { arg = "old spike"; archived = false })
    archive_sessions_json;
  [%expect
    {|
    (Rpc
      (method_ set_archived)
      (params (
        (path     /s/3b.jsonl)
        (archived false)))
      (tag (Notice_on_success "unarchived session \"old spike\"")))
    |}];
  H.reply h (Notice_on_success "unarchived session \"old spike\"") "{}";
  run h "/archive 3";
  H.reply
    h
    (Session_to_archive { arg = "3"; archived = true })
    archive_sessions_json;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_sessions)
      (params ())
      (tag (
        Session_to_archive
        (arg      3)
        (archived true))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─── "3" matches 2 sessions (3a, 3b): give more of the id ───
    > /archive 3▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* The backend's refusal says what to do. *)
  H.key h (Key.ctrl 'u');
  run h "/archive deploy";
  H.reply
    ~quiet:true
    h
    (Session_to_archive { arg = "deploy"; archived = true })
    archive_sessions_json;
  H.reply_error
    h
    (Notice_on_success "archived session \"deploy\"")
    "session \"deploy\" is running: wait for it or /abort it before archiving";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_sessions)
      (params ())
      (tag (
        Session_to_archive
        (arg      deploy)
        (archived true))))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ session "deploy" is running: wait for it or /abort it    │
    │ before archiving                                         │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/delete: by id, prefix or name, after a confirmation" =
  let h = connected ~width:100 () in
  run h "/delete 3 deploy";
  [%expect
    {|
    (Rpc
      (method_ list_sessions)
      (params ())
      (tag (Sessions_to_delete (3 deploy))))
    |}];
  H.reply h (Sessions_to_delete [ "3"; "deploy" ]) archive_sessions_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────────────────────── "3" matches 2 sessions (3a, 3b): give more of the id ───
    > /delete 3 deploy▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  run h "/delete 3b deploy";
  H.reply h (Sessions_to_delete [ "3b"; "deploy" ]) archive_sessions_json;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_sessions)
      (params ())
      (tag (Sessions_to_delete (3b deploy))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────┐
    │ Delete 2 sessions (1.0 MB)? Enter deletes, Esc keeps them                                        │
    └──────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  confirm: y / n
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ delete_sessions)
      (params ((paths (/s/3b.jsonl /s/2.jsonl))))
      (tag Sessions_deleted))
    |}];
  H.mode h;
  [%expect {| editing |}];
  H.reply
    h
    Sessions_deleted
    {|{"deleted":[{"path":"/s/3b.jsonl","id":"3b","bytes":1048576},{"path":"/s/2.jsonl","id":"2","bytes":4096}],"refused":[]}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────── Deleted 2 sessions (1.0 MB) ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/delete nope";
  H.reply ~quiet:true h (Sessions_to_delete [ "nope" ]) archive_sessions_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag (Sessions_to_delete (nope))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────── no session matches "nope": /sessions lists them, or Tab after the command completes them ───
    > /delete nope▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  (* Without arguments it opens the picker, where Tab marks. *)
  run h "/delete";
  [%expect
    {| (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker)) |}]
;;

let%expect_test "/delete completes session after session" =
  let h = connected ~width:100 () in
  H.keys h "/delete ";
  [%expect {| (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache)) |}];
  H.reply h Sessions_cache archive_sessions_json;
  H.key h (Key.plain Tab);
  H.keys h " ";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ (empty)      /tmp
      try a spike  archived · /old
      old spike    archived · another spike · /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > /delete 2 ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  Tab accepts · Enter as typed · Esc hides
    |}]
;;

let%expect_test
    "sessions_changed refetches the open picker and the cached list, keeping \
     the query, marks and highlight"
  =
  (* Nothing shows sessions: nothing to fetch. *)
  let h = connected () in
  H.event h Sessions_changed;
  [%expect {| |}];
  (* The completion's list is kept current. *)
  H.keys h "/switch ";
  H.reply ~quiet:true h Sessions_cache sessions_json;
  H.key h (Key.ctrl 'u');
  H.event h Sessions_changed;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_refresh))
    |}];
  let h = sessions_open () in
  H.keys h "d";
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / d▏
     ✓ ▶ deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ 2 in background ∣ deploy it  /work
    ▸  (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
     * ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
      ── archived ──
       (unnamed) ∣ 2025-06-04T10:00 ∣ 4 msgs ∣ try a spike  /old
       old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  $0.01  1 marked (4.0 KB) · Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  H.event h Sessions_changed;
  [%expect
    {| (Rpc (method_ list_sessions) (params ()) (tag Sessions_refresh)) |}];
  (* 3a is gone, deploy finished its background work and a new one came
     along. *)
  H.reply
    h
    Sessions_refresh
    {|[
  {"id":"6","path":"/s/6.jsonl","name":"dev","cwd":"/work","created_at":"2025-06-06T10:00:00Z","updated_at":"2025-06-06T10:00:00Z","first_prompt":"something new","message_count":1,"bytes":10},
  {"id":"1","path":"/home/u/.prigh/sessions/1.jsonl","name":"build fix","cwd":"/work","created_at":"2025-06-05T10:00:00Z","updated_at":"2025-06-05T10:00:00Z","first_prompt":"fix the build","message_count":12,"live":true,"bytes":2048},
  {"id":"2","path":"/s/2.jsonl","name":"deploy","cwd":"/work","created_at":"2025-06-03T10:00:00Z","updated_at":"2025-06-03T10:00:00Z","first_prompt":"deploy it","message_count":6,"live":true,"bytes":4096},
  {"id":"3b","path":"/s/3b.jsonl","name":"old spike","cwd":"/old","created_at":"2025-04-01T10:00:00Z","updated_at":"2025-04-02T10:00:00Z","first_prompt":"another spike","message_count":2,"archived":true,"bytes":1048576},
  {"id":"5","path":"/s/5.jsonl","name":null,"cwd":"/tmp","created_at":"2025-06-01T10:00:00Z","updated_at":"2025-06-01T10:00:00Z","first_prompt":null,"message_count":0,"bytes":100}
  ]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (5)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / d▏
     ✓ ● deploy ∣ 2025-06-03T10:00 ∣ 6 msgs ∣ deploy it    /work
       dev ∣ 2025-06-06T10:00 ∣ 1 msgs ∣ something new     /work
    ▸  (unnamed) ∣ 2025-06-01T10:00 ∣ 0 msgs ∣ (empty)     /tmp
     * ● build fix ∣ 2025-06-05T10:00 ∣ 12 msgs ∣ fix the build  /work
      ── archived ──
       old spike ∣ 2025-04-02T10:00 ∣ 2 msgs ∣ another spike  /old
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  $0.01  1 marked (4.0 KB) · Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}]
;;

let%expect_test "/tree skips archived and other settings entries" =
  let h = connected () in
  H.keys h "/tree";
  H.enter h;
  H.reply
    ~quiet:true
    h
    Entries_for_tree
    {|{"head":"ar2","entries":[{"id":"u1","parent":null,"kind":"message","message":{"role":"user","text":"root question"}},{"id":"ar1","parent":"u1","kind":"archived","archived":true},{"id":"c1","parent":"ar1","kind":"cwd","cwd":"/x"},{"id":"u2","parent":"c1","kind":"message","message":{"role":"user","text":"back again"}},{"id":"ar2","parent":"u2","kind":"archived","archived":true}]}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_entries) (params ((all true))) (tag Entries_for_tree))

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Session tree  (2)
    / ▏
    ▸* > root question
     *   > back again
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Fork at  (2)
    / ▏
       first question   #1
    ▸* second question  #2
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.enter h;
  [%expect {| (Rpc (method_ fork) (params ((at u2))) (tag Reload_messages)) |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > second question
      more▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Rewind to  (2)
    / ▏
       first question   #1
    ▸* second question  #2
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Rewind to "second question"? Later messages are          │
    │ abandoned (y/n)                                          │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  confirm: y / n
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
    ▌ earlier question
      earlier answer
    Session tree  (5)
    / ▏
    ▸* > root question
     *   · first answer
     *     · second answer
         · branch answer
           ⚙ branch tool
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
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
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    |}];
  H.reply ~quiet:true h Initial_messages {|[]|};
  H.show h;
  [%expect
    {|
    ───────────────────────────────────────── cloned session ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/export chooses jsonl by extension, or prompts for a path" =
  let h = connected () in
  H.step h (Intent (Insert "/export data.jsonl"));
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ list_paths)
      (params ((prefix data.jsonl)))
      (tag (Paths_for_autocomplete data.jsonl)))
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
    (Rpc
      (method_ list_paths)
      (params ((prefix notes.md)))
      (tag (Paths_for_autocomplete notes.md)))
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
    (Rpc
      (method_ list_paths)
      (params ((prefix "")))
      (tag (Paths_for_autocomplete "")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Export to ──────────────────────────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  ctx:0.1%/1.0M  Enter submits · Esc cancels
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
    (Rpc
      (method_ list_paths)
      (params ((prefix saved.jsonl)))
      (tag (Paths_for_autocomplete saved.jsonl)))
    (Rpc (method_ import) (params ((path saved.jsonl))) (tag Reload_messages))
    |}];
  H.step h (Intent (Insert "/import "));
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_paths)
      (params ((prefix "")))
      (tag (Paths_for_autocomplete "")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Import from ────────────────────────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  ctx:0.1%/1.0M  Enter submits · Esc cancels
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
    (Rpc
      (method_ list_dirs)
      (params ((prefix /var)))
      (tag (Dirs_for_autocomplete /var)))
    (Rpc
      (method_ set_cwd)
      (params (
        (path       /var)
        (if_missing ask)))
      (tag (
        Retry_in_editor
        (command "/cd /var")
        (hint    "Tab after /cd completes directories")
        (then_ (Dir_changed Cwd)))))
    |}];
  H.step h (Intent (Insert "/cd "));
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params ((prefix "")))
      (tag (Dirs_for_autocomplete "")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Change directory to ────────────────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  ctx:0.1%/1.0M  Enter submits · Esc cancels
    |}];
  H.step h (Intent (Insert "/tmp"));
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params (
        (prefix /tmp)
        (host   backend)))
      (tag (Dirs_for_autocomplete /tmp)))
    (Rpc
      (method_ set_cwd)
      (params (
        (path       /tmp)
        (if_missing ask)))
      (tag (Dir_changed Cwd)))
    |}]
;;

let%expect_test "/cd to a missing directory asks to create it; /mkdir creates" =
  let h = connected () in
  let cd path : Prigh_ui.App.Reply_tag.t =
    Retry_in_editor
      { command = "/cd " ^ path
      ; hint = "Tab after /cd completes directories"
      ; then_ = Dir_changed Cwd
      }
  in
  H.step h (Intent (Insert "/cd /work/new"));
  H.enter h;
  H.reply h (cd "/work/new") {|{"missing":"/work/new"}|};
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params ((prefix /work/new)))
      (tag (Dirs_for_autocomplete /work/new)))
    (Rpc
      (method_ set_cwd)
      (params (
        (path       /work/new)
        (if_missing ask)))
      (tag (
        Retry_in_editor
        (command "/cd /work/new")
        (hint    "Tab after /cd completes directories")
        (then_ (Dir_changed Cwd)))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Create /work/new? (y/n)                                  │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  confirm: y / n
    |}];
  (* Esc, like n, creates nothing. *)
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────────── not created: /work/new ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.step h (Intent (Insert "/cd /work/new"));
  H.enter h;
  H.reply h (cd "/work/new") {|{"missing":"/work/new"}|};
  H.key h (Key.char 'n');
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params ((prefix /work/new)))
      (tag (Dirs_for_autocomplete /work/new)))
    (Rpc
      (method_ set_cwd)
      (params (
        (path       /work/new)
        (if_missing ask)))
      (tag (
        Retry_in_editor
        (command "/cd /work/new")
        (hint    "Tab after /cd completes directories")
        (then_ (Dir_changed Cwd)))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────────── not created: /work/new ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.step h (Intent (Insert "/cd /work/new"));
  H.enter h;
  H.reply h (cd "/work/new") {|{"missing":"/work/new"}|};
  H.key h (Key.char 'y');
  H.reply h (Dir_changed Cwd) {|{"cwd":"/work/new","created":true}|};
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params ((prefix /work/new)))
      (tag (Dirs_for_autocomplete /work/new)))
    (Rpc
      (method_ set_cwd)
      (params (
        (path       /work/new)
        (if_missing ask)))
      (tag (
        Retry_in_editor
        (command "/cd /work/new")
        (hint    "Tab after /cd completes directories")
        (then_ (Dir_changed Cwd)))))
    (Rpc
      (method_ set_cwd)
      (params (
        (path       /work/new)
        (if_missing create)))
      (tag (Dir_changed Cwd)))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────── created /work/new; cwd changed ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* /mkdir creates without asking; without a path it prompts for one. *)
  H.step h (Intent (Insert "/mkdir /work/a/b"));
  H.enter h;
  let mkdir : Prigh_ui.App.Reply_tag.t =
    Retry_in_editor
      { command = "/mkdir /work/a/b"; hint = ""; then_ = Dir_changed Cwd }
  in
  H.reply_error h mkdir "/work/a: permission denied";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params ((prefix /work/a/b)))
      (tag (Dirs_for_autocomplete /work/a/b)))
    (Rpc
      (method_ set_cwd)
      (params (
        (path       /work/a/b)
        (if_missing create)))
      (tag (
        Retry_in_editor
        (command "/mkdir /work/a/b")
        (hint    "")
        (then_ (Dir_changed Cwd)))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────── /work/a: permission denied ───
    > /mkdir /work/a/b▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.step h (Intent Kill_to_start);
  H.step h (Intent (Insert "/mkdir "));
  H.enter h;
  H.show h;
  H.step h (Intent (Insert "/work/c"));
  H.enter h;
  H.reply h (Dir_changed Cwd) {|{"cwd":"/work/c","created":true}|};
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params ((prefix "")))
      (tag (Dirs_for_autocomplete "")))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Create and change to directory ─────────────────────────┐
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  ctx:0.1%/1.0M  Enter submits · Esc cancels
    (Rpc
      (method_ list_dirs)
      (params (
        (prefix /work/c)
        (host   backend)))
      (tag (Dirs_for_autocomplete /work/c)))
    (Rpc
      (method_ set_cwd)
      (params (
        (path       /work/c)
        (if_missing create)))
      (tag (Dir_changed Cwd)))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────── created /work/c; cwd changed ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Logins ─────────────────────────────────────────────────┐
    │ anthropic  not configured  [oauth (Claude Pro/Max),      │
    │ api_key (API key)]                                       │
    │ openai     not configured  [api_key (API key)]           │
    │ deepseek   logged in via auth.json  [api_key (API key)]  │
    │ /login custom adds an OpenAI-compatible endpoint         │
    └───────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ↑↓ scroll · Esc closes
    |}];
  H.keys h "/clear";
  H.enter h;
  H.keys h "/help";
  H.enter h;
  H.show h;
  [%expect
    {|
    ┌─ Help ───────────────────────────────────────────────────┐
    │ Commands                                                 │
    │ /help                              show commands and     │
    │ keys                                                     │
    │ /hotkeys                           show keyboard         │
    │ shortcuts                                                │
    │ /model [name|id|provider/id]       pick or switch the    │
    │ model                                                    │
    │ /scoped-models                     pick the models       │
    │ Ctrl+P cycles through                                    │
    │ /change_default                    save the current      │
    │ model and thinking level as the default for new sessions │
    └───────── 1–11 of 244 · ↑↓ PgUp PgDn scroll · Esc closes ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ↑↓ scroll · Esc closes
    |}];
  H.key h (Key.ctrl 'l');
  H.reply_error h Show_error "unknown method \"bogus\"";
  H.step h (Protocol_error "bad line");
  H.step h (Stderr "warning from backend");
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_models) (params ()) (tag (Models_for_picker "")))












      protocol error: bad line
    ───────────────────────────────── unknown method "bogus" ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "verbosity cycles Normal / Verbose / Quiet; /verbosity sets it" =
  let h = connected ~height:14 () in
  H.keys h "run it";
  H.enter h;
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (user "run it"));
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
    ✓ bash $ ls -la
        out 0
        out 1
        out 2
        out 3
        out 4
        … 12 more lines
        out 17
        out 18
        out 19
      all done
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
        out 10
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
    ──────────────────── view: verbose — everything is shown ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ run it
    ✓ bash $ ls -la  20 lines
      all done
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ view: quiet — intermediate output and thinking are       │
    │ hidden                                                   │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/verbosity normal";
  H.enter h;
  H.show h;
  [%expect
    {|
    ✓ bash $ ls -la
        out 0
        out 1
        out 2
        out 3
        out 4
        … 12 more lines
        out 17
        out 18
        out 19
      all done
    ─────────────── view: normal — tool output is summarised ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let png : P.Image.t = { mime_type = "image/png"; bytes = 35021; data = "" }

let%expect_test
    "images show as a line each: a user attachment and a read result, at every \
     verbosity"
  =
  let h = connected ~height:16 () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Message_start
       (user
          ~images:[ png; { mime_type = "image/jpeg"; bytes = 512; data = "" } ]
          "what is in these?"));
  let call = tool_call ~name:"read" ~arguments:{|{"path":"shot.png"}|} "c1" in
  H.event h (Tool_start call);
  H.event
    h
    (Tool_end
       { call
       ; result =
           tool_result
             ~name:"read"
             ~id:"c1"
             ~images:[ png ]
             "Read image file [image/png, 800x600]\n\
              [Image downscaled from 4000x3000 to fit]"
       });
  H.event h (Message_update { partial; delta = Text_delta "A cat." });
  H.event h (Message_end (assistant "A cat."));
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ what is in these?
    ▌ [image: image/png, 34.2 KB]
    ▌ [image: image/jpeg, 512 B]
    ✓ read shot.png  1 image
        [image: image/png, 34.2 KB]
      A cat.
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ what is in these?
    ▌ [image: image/png, 34.2 KB]
    ▌ [image: image/jpeg, 512 B]
    ✓ read shot.png  1 image
        {
          path: "shot.png"
        }
        [image: image/png, 34.2 KB]
      A cat.
    ──────────────────── view: verbose — everything is shown ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ what is in these?
    ▌ [image: image/png, 34.2 KB]
    ▌ [image: image/jpeg, 512 B]
    ✓ read shot.png  1 image
      A cat.
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ view: quiet — intermediate output and thinking are       │
    │ hidden                                                   │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "images in reloaded history; an image-only message is labelled by its \
     image in pickers"
  =
  let h = connected () in
  H.reply ~quiet:true h Reload_messages "null";
  H.reply
    h
    Initial_messages
    {|[{"role":"user","text":"","images":[{"mime_type":"image/gif","data":"R0lGODlhAQABAAAAACw="}]},{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"read","arguments":"{\"path\":\"a.webp\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"},{"role":"tool_result","tool_call_id":"c1","tool_name":"read","text":"Read image file [image/webp, 2x2]","is_error":false,"images":[{"mime_type":"image/webp","data":"UklGRiQAAABXRUJQ"},{"mime_type":"image/webp","data":"UklGRg=="}]}]|};
  H.show h;
  [%expect
    {|
    ▌ [image: image/gif, 14 B]
    ✓ read a.webp  2 images
        [image: image/webp, 12 B]
        [image: image/webp, 4 B]
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/verbosity quiet";
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ [image: image/gif, 14 B]
    ✓ read a.webp  2 images
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ view: quiet — intermediate output and thinking are       │
    │ hidden                                                   │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/rewind";
  H.enter h;
  H.reply
    h
    Entries_for_rewind
    {|{"head":"u1","entries":[{"id":"u1","parent":null,"kind":"message","message":{"role":"user","text":"","images":[{"mime_type":"image/gif","data":"R0lGODlhAQABAAAAACw="}]}}]}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_rewind))




    ▌ [image: image/gif, 14 B]
    ✓ read a.webp  2 images
    Rewind to  (1)
    / ▏
    ▸* [image: image/gif, 14 B]  #1
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;

let%expect_test "/verbosity with no argument opens argument completion" =
  let h = connected () in
  H.keys h "/verbosity";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ Quiet
      Normal
      Verbose
    ────────────────────────────────────────────────────────────
    > /verbosity ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides
    |}];
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────── view: verbose — everything is shown ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "quiet hides intermediate text and thinking" =
  let h = connected ~height:14 () in
  H.keys h "/verbosity quiet";
  H.enter h;
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (user "hi"));
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
    ▌ earlier question
      earlier answer

    ▌ hi
      let me look at the files
    ✓ bash $ ls  2 lines
      all done
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ view: quiet — intermediate output and thinking are       │
    │ hidden                                                   │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
    ✕ bash $ cat missing
        no such file
        second line
        third line
        … 1 more line
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ view: quiet — intermediate output and thinking are       │
    │ hidden                                                   │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))







    ▌ go
    ✓ bash $ ls  2 lines
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
    ▌ earlier question
      earlier answer
      The quick brown fox jumps over the lazy dog and keeps
      running.
      second
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  ⠋ writing · Esc aborts · Enter steers
    |}];
  H.step h (Resize { width = 30; height = 8 });
  H.show h;
  [%expect
    {|
      earlier answer
      The quick brown fox jumps
      over the lazy dog and keeps
      running.
      second
    ──────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M
    |}];
  H.step h (Resize { width = 60; height = 8 });
  H.event h (Message_update { partial; delta = Text_delta " part" });
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
      The quick brown fox jumps over the lazy dog and keeps
      running.
      second part
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  ⠋ writing · Esc aborts · Enter steers
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > f▏rst line
      second
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > first line
      second▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* Pasted text with newlines goes in as one insert. *)
  H.step h (Intent (Insert "a\nb\nc"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > a
      b
      c▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.step h Backend_closed;
  H.show h;
  [%expect
    {|
    (Reconnect
      (generation 1)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      backend connection lost; reconnecting
      (/retry-backend-connection to retry now, Ctrl+C quits)
      warn one
      warn two
      warn three
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  backend gone · retry 1 in 0s
    |}];
  H.step h (Intent Model_picker);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      backend connection lost; reconnecting
      (/retry-backend-connection to retry now, Ctrl+C quits)
      warn one
      warn two
      warn three
    ──────────────────────────────────────── backend is gone ───
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  backend gone · retry 1 in 0s
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
  H.event h (Queue_update { steer = [ "first"; "second" ]; follow_up = [] });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    queued (2) · Alt+Up: edit the last · /queue: edit or remove
      steer      first
      steer      second
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  queued:2  ⠋ working · Esc aborts · Enter steers
    |}];
  H.esc h;
  [%expect {| (Rpc (method_ abort) (params ()) (tag Abort_done)) |}];
  H.reply h Abort_done {|{"restored":["first","second"]}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────────────────────────────────── restored 2 queued messages to the editor ───
    > first

      second▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ working · Esc aborts · Enter steers
    |}];
  H.event h (Queue_update { steer = []; follow_up = [] });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────────────────────────────────── restored 2 queued messages to the editor ───
    > first

      second▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ working · Esc aborts · Enter steers
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ↑ scrolled
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ↓ 10 new
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ↓ 10 new
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ↑ scrolled
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ↑ scrolled
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ↑ scrolled
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ↑ scrolled
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
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ↑ scrolled
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
  H.event h (Queue_update { steer = [ "queued" ]; follow_up = [] });
  H.keys h "draft";
  H.reply h Abort_done {|{"restored":["queued"]}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────── restored 1 queued message to the editor ───
    > queued

      draft▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ working · Esc aborts · Enter steers
    |}];
  H.reply h Abort_done {|{}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────── restored 1 queued message to the editor ───
    > queued

      draft▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ working · Esc aborts · Enter steers
    |}]
;;

let say h text =
  H.event h (Message_update { partial; delta = Text_delta text });
  H.event h (Message_end (assistant text))
;;

let%expect_test "background subagent: runs while main is idle, survives a \
                 prompt, cancelled from the picker, delivery rendered \
                 compactly"
  =
  let h = connected ~width:100 ~height:16 () in
  H.keys h "go";
  H.enter h;
  H.event h (State (state ~running:true ()));
  H.event h (Message_start (user "go"));
  let call =
    tool_call ~name:"subagent" ~arguments:{|{"task":"index the repo"}|} "c1"
  in
  H.event h (Tool_start call);
  H.event
    h
    (Subagent_start
       { call_id = "c1"
       ; agent_id = "a1"
       ; task = "index the repo"
       ; model = "claude-haiku"
       ; tools = [ "bash" ]
       });
  H.event
    h
    (Tool_end
       { call
       ; result =
           tool_result
             ~name:"subagent"
             ~id:"c1"
             "started agent a1 (index the repo); its result will be delivered \
              to you when it finishes: keep working or end your turn meanwhile"
       });
  say h "started it";
  H.event h (State (state ~subagents:[ "a1", "index the repo", true ] ()));
  (* Events keep arriving after the spawning call ended and the turn is over. *)
  H.event h (Subagent { call_id = "c1"; agent_id = "a1"; event = Turn_start });
  H.event
    h
    (Subagent
       { call_id = "c1"
       ; agent_id = "a1"
       ; event =
           Tool_start
             (tool_call ~name:"bash" ~arguments:{|{"command":"ls -R"}|} "t1")
       });
  H.show h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text go))) (tag Show_error))
    (Append_history go)




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ go
    … subagent index the repo  1 turn
        ↳ bash $ ls -R
      started it
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  agents:[main] 1⠋ bg
    |}];
  print_s [%sexp (App.Model.agents_running h.model : bool)];
  [%expect {| true |}];
  (* A new prompt keeps the running agent cyclable. *)
  H.keys h "meanwhile";
  H.enter h;
  H.event
    h
    (State (state ~running:true ~subagents:[ "a1", "index the repo", true ] ()));
  H.event h (Message_start (user "meanwhile"));
  say h "sure";
  H.event h (State (state ~subagents:[ "a1", "index the repo", true ] ()));
  H.next_agent h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text meanwhile))) (tag Show_error))
    (Append_history meanwhile)
    ◆ subagent 1/1  claude-haiku  ⠋ running 1 turns  "index the repo"











    … bash $ ls -R
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  agents:main [1⠋] bg
    |}];
  H.next_agent h;
  (* Ctrl+D in the picker cancels the highlighted running agent. *)
  H.keys h "/agents";
  H.enter h;
  H.show h;
  H.key h (Key.ctrl 'd');
  H.mode h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ go
    … subagent index the repo  1 turn
        ↳ bash $ ls -R
      started it

    ▌ meanwhile
      sure
    Subagents  (1)
    / ▏
    ▸  index the repo  a1  running  claude-haiku
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  agents:[main] 1⠋ bg  Enter focuses · Esc closes · Ctrl+D cancels
    (Rpc
      (method_ cancel_subagent)
      (params ((agent_id a1)))
      (tag (Notice_on_success "cancelling subagent a1")))
    editing
    |}];
  H.reply h (Notice_on_success "cancelling subagent a1") "{}";
  let report =
    "[cancelled]\n[subagent: 1 turns, 10 in / 5 out tokens, $0.0000]"
  in
  H.event
    h
    (Subagent_end
       { call_id = "c1"
       ; agent_id = "a1"
       ; usage = { input = 10; output = 5; cache_read = 0 }
       ; turns = 1
       ; cost_usd = 0.
       ; result = { text = report; is_error = true }
       });
  (* The backend hands the report to the idle main agent as a new turn. *)
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Message_start (delivery ("[subagent a1 failed] index the repo\n" ^ report)));
  say h "it was cancelled";
  H.event h (State (state ()));
  H.keys h "/agents cancel 1";
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ go
    ■ subagent index the repo  cancelled
        [cancelled]
        [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
      started it

    ▌ meanwhile
      sure
    ↩ subagent a1 "index the repo"  failed
        [cancelled]
        [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
      it was cancelled
    ───────────────────────────────────────────────────────────────────── subagent a1 is not running ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  agents:[main] 1✗
    |}];
  (* Delivered and finished: the next prompt drops it. *)
  H.keys h "next";
  H.enter h;
  H.keys h "/agents";
  H.enter h;
  H.keys h "/agents cancel 3";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text next))) (tag Show_error))
    (Append_history next)

    ▌ go
    ■ subagent index the repo  cancelled
        [cancelled]
        [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
      started it

    ▌ meanwhile
      sure
    ↩ subagent a1 "index the repo"  failed
        [cancelled]
        [subagent: 1 turns, 10 in / 5 out tokens, $0.0000]
      it was cancelled
    ───────────────────────────────────────────────────────────────────────── no subagents to cancel ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "delivered reports reload compactly; unseen agents still show" =
  let h = connected ~width:90 ~height:12 () in
  H.reply
    ~quiet:true
    h
    Initial_messages
    {|[{"role":"user","text":"[subagent a1 finished] find auth\nauth lives in src/auth.ml\n[subagent: 2 turns, 1 in / 2 out tokens, $0.0100]\n\n[subagent a2 failed] run tests\nsubagent failed: boom\n[subagent: 1 turns, 1 in / 1 out tokens, $0.0000]","origin":{"source":{"kind":"delivery","title":"Reports"}}}]|};
  H.event h (State (state ~subagents:[ "a3", "elsewhere", true ] ()));
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    ↩ subagent a1 "find auth"  finished
        auth lives in src/auth.ml
        [subagent: 2 turns, 1 in / 2 out tokens, $0.0100]
    ↩ subagent a2 "run tests"  failed
        subagent failed: boom
        [subagent: 1 turns, 1 in / 1 out tokens, $0.0000]
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  agents:1 running
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
  H.event h (Message_start (user "go"));
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
       ; event = Message_start (user "find all auth code in the repository")
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ go
    … subagent find all auth code in the repository  1 turn
        ↳ bash $ grep -r auth src
    ✓ subagent run the test suite  1 turn $0.02
        all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.next_agent h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/2  claude-haiku  ⠋ running 1 turns  "find all auth code in the rep…












    ▌ find all auth code in the repository
    … bash $ grep -r auth src
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.next_agent h;
  H.show h;
  [%expect
    {|
    ◆ subagent 2/2  claude-sonnet  ✓ done 1 turns $0.02  "run the test suite"












    … bash $ dune runtest
      all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.next_agent h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ go
    … subagent find all auth code in the repository  1 turn
        ↳ bash $ grep -r auth src
    ✓ subagent run the test suite  1 turn $0.02
        all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.focus_agent h 1;
  H.focus_agent h 2;
  H.show h;
  [%expect
    {|
    ◆ subagent 2/2  claude-sonnet  ✓ done 1 turns $0.02  "run the test suite"












    … bash $ dune runtest
      all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ go
    … subagent find all auth code in the repository  1 turn
        ↳ bash $ grep -r auth src
    ✓ subagent run the test suite  1 turn $0.02
        all tests pass
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ running tools · Esc aborts · Enter steers
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



    ✓ bash $ ls
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
    deepseek-flash  think:off  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    ◆ subagent 1/1  claude-haiku  ⠋ running 1 turns  "audit"
    ✓ bash $ ls
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
    ────────────────────────────── view: verbose — everything is shown ───
    > ▏
    deepseek-flash  think:off  ⠋ running tools · Esc aborts · Enter steers
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Subagents  (2)
    / ▏
    ▸  find auth  c1  running  claude-haiku
       run tests  c2  done 3 turns $0.02  claude-sonnet
    ──────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  Enter focuses · Esc closes · Ctrl+D cancels
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    ◆ subagent 1/2  claude-haiku  ⠋ running 0 turns  "find auth"












    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ working · Esc aborts · Enter steers
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  agents:[main] 1✓
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
    deepseek-flash  ctx:0.1%/1.0M  ⠋ working · Esc aborts · Enter steers
    |}];
  H.reply h Reload_messages {|{}|};
  H.reply
    ~quiet:true
    h
    Initial_messages
    {|[{"role":"user","text":"go"},{"role":"assistant","content":[{"type":"tool_call","id":"c1","name":"subagent","arguments":"{\"task\":\"look\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":1,"output":2,"cache_read":0},"model":"m"},{"role":"tool_result","tool_call_id":"c1","tool_name":"subagent","text":"report","is_error":false}]|};
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    |}];
  H.show h;
  [%expect
    {|
    ▌ go
    ✓ subagent look
        report
    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ working · Esc aborts · Enter steers
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










    ✓ subagent inner task  1 turn $0.00
        inner done
    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ⠋ working · Esc aborts · Enter steers
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
  H.event h (Queue_update { steer = []; follow_up = [ "after this" ] });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    queued (1) · Alt+Up: edit the last · /queue: edit or remove
      follow-up  after this
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  queued:1  ⠋ working · Esc aborts · Enter steers
    |}];
  H.keys h "and more";
  H.key h (Key.alt Enter);
  [%expect
    {|
    (Rpc (method_ follow_up) (params ((text "and more"))) (tag Show_error))
    (Append_history "and more")
    |}];
  H.event
    h
    (Queue_update { steer = []; follow_up = [ "after this"; "and more" ] });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    queued (2) · Alt+Up: edit the last · /queue: edit or remove
      follow-up  after this
      follow-up  and more
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  queued:2  ⠋ working · Esc aborts · Enter steers
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
  H.event h (Queue_update { steer = []; follow_up = [ "queued text" ] });
  H.keys h "draft";
  H.key h (Key.alt Key.Code.Up);
  [%expect {| (Rpc (method_ dequeue) (params ()) (tag Dequeued)) |}];
  H.reply h Dequeued {|{"text":"queued text","attachments":[]}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    queued (1) · Alt+Up: edit the last · /queue: edit or remove
      follow-up  queued text
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > queued text

      draft▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  queued:1  ⠋ working · Esc aborts · Enter steers
    |}];
  H.event h (Queue_update { steer = []; follow_up = [] });
  H.key h (Key.alt Key.Code.Up);
  [%expect {| (Rpc (method_ dequeue) (params ()) (tag Dequeued)) |}];
  H.reply h Dequeued {|null|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────────────────────────────────────────────────────────── nothing queued ───
    > queued text

      draft▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ working · Esc aborts · Enter steers
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ✓ shell ls -la
        a.ml
        b.ml
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
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

    ▌ earlier question
      earlier answer
    ✓ shell ls -la
        a.ml
        b.ml
    ┌─ Warning ────────────────────────────────────────────────────────────────────────────────────────┐
    │ !echo hi waits for the turn to end (kept in the editor); !&echo hi runs it in the background now │
    └────────────────────────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > !echo hi▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  ⠋ running tools · Esc aborts · Enter steers
    |}]
;;

let%expect_test "bracketed paste renders a chip until the cursor enters it" =
  let h = connected ~width:80 ~height:14 () in
  H.step h (Intent (Paste "one\ntwo\nthree\nfour"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────
    > [4 lines pasted]▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.plain Up);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────
    > one
      two
      thre▏
      four
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "history loads at start and is appended on submit" =
  let h = connected ~width:100 () in
  H.reply h History {|["old one","old two"]|};
  H.key h (Key.plain Up);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > old two▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > from the editor▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
  [%expect
    {|
    (Rpc
      (method_ list_paths)
      (params ((prefix "")))
      (tag (Paths_for_autocomplete "")))
    |}];
  H.keys h "sr";
  [%expect
    {|
    (Rpc
      (method_ list_paths)
      (params ((prefix s)))
      (tag (Paths_for_autocomplete s)))
    (Rpc
      (method_ list_paths)
      (params ((prefix sr)))
      (tag (Paths_for_autocomplete sr)))
    |}];
  H.reply h (Paths_for_autocomplete "sr") {|["src/","src/app.ml"]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ src/
      src/app.ml
    ────────────────────────────────────────────────────────────
    > @sr▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.key h (Key.ctrl 'l');
  [%expect
    {| (Rpc (method_ list_models) (params ()) (tag (Models_for_picker ""))) |}];
  H.reply h (Models_for_picker "") models_json;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
       Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M …
       Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0…
       GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 p…
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M…
    ────────────────────────────────────────────────────────────
    > @sr
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;

let%expect_test
    "config: get_config at Start installs the reply; config_changed updates"
  =
  let h = H.create () in
  H.step h Start;
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
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
       { P.Config.default with scoped_models = [ "deepseek/deepseek-flash" ] });
  print_s [%sexp (h.model.config : P.Config.t option)];
  [%expect
    {|
    ((
      (scoped_models (anthropic/claude-fable-5-1))
      (approval_mode Ask)
      (permissions (
        (allow ())
        (ask   ())
        (deny  ())))
      (default_model    ())
      (default_thinking ())
      (fallback_models  ())
      (default_cwd      ())
      (archive_after_days 7)
      (notify             bell)
      (snippets ())))
    ((
      (scoped_models (deepseek/deepseek-flash))
      (approval_mode Auto)
      (permissions (
        (allow ())
        (ask   ())
        (deny  ())))
      (default_model    ())
      (default_thinking ())
      (fallback_models  ())
      (default_cwd      ())
      (archive_after_days 7)
      (notify             bell)
      (snippets ())))
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
    ▌ earlier question
      earlier answer
    Scoped models  (4)
    / ▏
    ▸[ ] Claude Fable 5       Claude Fable 5
     [ ] Claude Fable 5.1     Claude Fable 5.1
     [ ] GPT-5.5              GPT-5.5
     [x] DeepSeek V4.1 Flash  DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Space toggle · Ctrl+A all
    |}];
  H.keys h " ";
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Scoped models  (4)
    / ▏
    ▸[x] Claude Fable 5       Claude Fable 5
     [ ] Claude Fable 5.1     Claude Fable 5.1
     [ ] GPT-5.5              GPT-5.5
     [x] DeepSeek V4.1 Flash  DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Space toggle · Ctrl+A all
    |}];
  H.key h (Key.ctrl 'a');
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Scoped models  (4)
    / ▏
    ▸[x] Claude Fable 5       Claude Fable 5
     [x] Claude Fable 5.1     Claude Fable 5.1
     [x] GPT-5.5              GPT-5.5
     [x] DeepSeek V4.1 Flash  DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Space toggle · Ctrl+A all
    |}];
  H.key h (Key.ctrl 'x');
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Scoped models  (4)
    / ▏
    ▸[ ] Claude Fable 5       Claude Fable 5
     [ ] Claude Fable 5.1     Claude Fable 5.1
     [ ] GPT-5.5              GPT-5.5
     [ ] DeepSeek V4.1 Flash  DeepSeek V4.1 Flash
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Space toggle · Ctrl+A all
    |}];
  H.key h (Key.ctrl 'a');
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((
        config ((
          scoped_models (
            anthropic/claude-fable-5
            anthropic/claude-fable-5-1
            openai/gpt-5.5
            deepseek/deepseek-flash))))))
      (tag Config_saved))
    |}];
  H.reply
    h
    Config_saved
    {|{"scoped_models":["anthropic/claude-fable-5","anthropic/claude-fable-5-1","openai/gpt-5.5","deepseek/deepseek-flash"],"confirm_tools":false}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────── scoped models saved (4) ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
  (* The status line switches at once; the transcript notice waits for the
     backend to confirm. *)
  H.key h (Key.ctrl 'p');
  H.reply h (Set_model_done "anthropic/claude-fable-5-1") "{}";
  H.key h (Key.ctrl 'p');
  H.reply h (Set_model_done "openai/gpt-5.5") "{}";
  H.key h (Key.ctrl 'p');
  H.reply h (Set_model_done "anthropic/claude-fable-5") "{}";
  H.key h (Key.alt (Key.Code.Char "p"));
  H.reply h (Set_model_done "openai/gpt-5.5") "{}";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5-1)))
      (tag (Set_model_done anthropic/claude-fable-5-1)))
    (Rpc
      (method_ set_model)
      (params ((model openai/gpt-5.5)))
      (tag (Set_model_done openai/gpt-5.5)))
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5)))
      (tag (Set_model_done anthropic/claude-fable-5)))
    (Rpc
      (method_ set_model)
      (params ((model openai/gpt-5.5)))
      (tag (Set_model_done openai/gpt-5.5)))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─── model: openai/gpt-5.5 (not logged in; /login openai) ───
    > ▏
    /work  gpt-5.5  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────── only one model in scope ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────── thinking: off ───
    > ▏
    claude-fable-5-1  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ─────────────────────────── thinking: n/a for this model ───
    > ▏
    /work  gpt-5.5  think:n/a  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "status line: width 120 full, width 40 keeps the model and drops the cwd"
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
  H.event h (Queue_update { steer = [ "check the tests" ]; follow_up = [] });
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
    {| claude-fable-5-1  think:high  mode:auto  view:normal  ctx:42%/145k  $0.12  queued:1  agents:[main] 1⠋ 2✓ bg |}];
  H.step ~quiet:true h (Resize { width = 40; height = 20 });
  print_endline (Content.Line.to_plain (Render.status h.model));
  [%expect {| claude-fable-5-1  ctx:42%/145k  queued:1 |}]
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
      (fg        Gray)
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
    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
       Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M …
       Claude Fable 5.1     anthropic/claude-fable-5-1 ◆  ctx 1…
       GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 p…
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash  ctx 1.0M  …
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.key h (Key.ctrl 'n');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Model (logged in)  (1)
    / ▏
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash  ctx 1.0M  …
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
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
       (Prompt
          { id = "p1"
          ; prompt =
              Secret { message = "Paste your API key"; allow_empty = false }
          }));
  H.keys h "sk-secret";
  H.show h;
  [%expect
    {|
    (Open_browser https://claude.ai/oauth?x=1)
    ▌ earlier question
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
    deepseek-flash  $0.01  login: Enter answers · Esc cancels
    |}]
;;

let%expect_test "confirm dialog is a bordered block" =
  let h = connected () in
  H.keys h "/logout deepseek";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Log out of deepseek and delete its credential? (y/n)     │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  confirm: y / n
    |}]
;;

let%expect_test "search: Ctrl+F, type, n, N, Esc" =
  let h = connected () in
  H.key h (Key.ctrl 'f');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    / ▏
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  search: type to find · Esc closes
    |}];
  H.keys h "earlier";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
    / earlier▏
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  search 1/2 · ↓↑ next/prev · Esc closes
    |}];
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
    / earlier▏
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  search 2/2 · ↓↑ next/prev · Esc closes
    |}];
  H.key h (Key.plain Up);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
    / earlier▏
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  search 1/2 · ↓↑ next/prev · Esc closes
    |}];
  H.esc h;
  H.mode h;
  H.show h;
  [%expect
    {|
    editing





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ↑ scrolled
    |}]
;;

let%expect_test "Ctrl+Up / Ctrl+Down jump between user messages at height 8" =
  let h = connected ~height:8 () in
  let add_exchange question answer =
    H.event h (Message_start (user question));
    H.event h (Message_update { partial; delta = Text_delta answer });
    H.event h (Message_end (assistant answer))
  in
  add_exchange "second question" "second answer";
  add_exchange "third question" "third answer";
  H.show h;
  [%expect
    {|
    ▌ second question
      second answer

    ▌ third question
      third answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  let ctrl_up = { (Key.plain Up) with ctrl = true } in
  let ctrl_down = { (Key.plain Down) with ctrl = true } in
  H.key h ctrl_up;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ second question
      second answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ↑ scrolled
    |}];
  H.key h ctrl_up;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ second question
      second answer
    ───────────────────────────────────── no earlier message ───
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ↑ scrolled
    |}];
  H.key h ctrl_down;
  H.show h;
  [%expect
    {|
    ▌ second question
      second answer

    ▌ third question
      third answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ↑ scrolled
    |}];
  H.key h ctrl_down;
  H.show h;
  [%expect
    {|
    ▌ second question
      second answer

    ▌ third question
      third answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ↑ scrolled
    |}];
  H.key h ctrl_down;
  H.show h;
  [%expect
    {|
    ▌ second question
      second answer

    ▌ third question
      third answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* Past the last user message the viewport returns to Follow: a new notice is
     visible at the bottom instead of counted as hidden new lines. *)
  H.event h (Notice "tail notice");
  H.show h;
  [%expect
    {|
      second answer

    ▌ third question
      third answer
      tail notice
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
           ; images = []
           ; at = None
           ; elided = None
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
        … 12 more lines
        line 18
        line 19
        line 20
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/hotkeys prints the keys half of /help" =
  let h = connected () in
  H.keys h "/hotkeys";
  H.enter h;
  H.show h;
  [%expect
    {|
    ┌─ Keys ───────────────────────────────────────────────────┐
    │ Keys                                                     │
    │ Enter                   send the prompt / accept the     │
    │ highlighted item                                         │
    │ Alt+Enter               queue a follow-up to run after   │
    │ the current turn                                         │
    │ Alt+S                   send exactly what is typed,      │
    │ ignoring the completion popup                            │
    └─────────── 1–7 of 92 · ↑↓ PgUp PgDn scroll · Esc closes ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ↑↓ scroll · Esc closes
    |}]
;;

let%expect_test "/help model prints one command's usage" =
  let h = connected () in
  H.keys h "/help model";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ /model ─────────────────────────────────────────────────┐
    │ /model [name|id|provider/id]  pick or switch the model   │
    └───────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  ↑↓ scroll · Esc closes
    |}]
;;

let%expect_test "/help with an unknown command suggests the closest name" =
  let h = connected () in
  H.keys h "/help modle";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────┐
    │ unknown command /modle (back in the editor); did you     │
    │ mean /mode?                                              │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > /help modle▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "tool confirm: allow, deny, queue and Esc-abort" =
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_confirm
       { call_id = "c1"
       ; agent_id = None
       ; rules = []
       ; name = "bash"
       ; summary = "rm -rf build"
       });
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Run bash: rm -rf build?                                  │
    │ y  allow once                                            │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  confirm: y/n/r
    |}];
  H.keys h "y";
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   true)))
      (tag Show_error))
    |}];
  H.mode h;
  [%expect {| editing |}];
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_confirm
       { call_id = "c1"
       ; agent_id = None
       ; rules = []
       ; name = "bash"
       ; summary = "rm -rf build"
       });
  H.keys h "n";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   false)))
      (tag Show_error))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      denied bash
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  ⠋ working · Esc aborts · Enter steers
    |}];
  let h = connected () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_confirm
       { call_id = "c1"
       ; agent_id = None
       ; rules = []
       ; name = "bash"
       ; summary = "rm -rf build"
       });
  H.event
    h
    (Tool_confirm
       { call_id = "c2"
       ; agent_id = None
       ; rules = []
       ; name = "write"
       ; summary = "/work/a.txt"
       });
  H.event
    h
    (Tool_confirm
       { call_id = "c3"
       ; agent_id = None
       ; rules = []
       ; name = "edit"
       ; summary = "/work/b.txt"
       });
  H.event
    h
    (Tool_confirm
       { call_id = "c4"
       ; agent_id = None
       ; rules = []
       ; name = "apply_patch"
       ; summary = "src/a.ml, src/b.ml"
       });
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Run bash: rm -rf build?                                  │
    │ y  allow once                                            │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  ctx:0.1%/1.0M  to answer:3  confirm: y/n/r
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
      (tag Show_error))
    ▌ earlier question
      earlier answer
      allowed bash
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Write /work/a.txt?                                       │
    │ y  allow once                                            │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  ctx:0.1%/1.0M  to answer:2  confirm: y/n/r
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
      (tag Show_error))
      earlier answer
      allowed bash
      allowed write
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Edit /work/b.txt?                                        │
    │ y  allow once                                            │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  ctx:0.1%/1.0M  to answer:1  confirm: y/n/r
    |}];
  H.keys h "n";
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c3)
        (allow   false)))
      (tag Show_error))
    |}];
  H.show h;
  [%expect
    {|
      allowed bash
      allowed write
      denied edit
    ┌─ Confirm ────────────────────────────────────────────────┐
    │ Apply patch to src/a.ml, src/b.ml?                       │
    │ y  allow once                                            │
    │ n  deny                                                  │
    │ r  deny, saying why                                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  confirm: y/n/r
    |}];
  H.keys h "y";
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c4)
        (allow   true)))
      (tag Show_error))
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

let%expect_test "/change_default saves the model and thinking level" =
  let h = connected () in
  H.reply
    ~quiet:true
    h
    Config
    {|{"scoped_models":["deepseek/deepseek-flash"],"confirm_tools":true}|};
  H.keys h "/change_default";
  H.enter h;
  H.reply
    h
    Default_saved
    {|{"scoped_models":["deepseek/deepseek-flash"],"confirm_tools":true,"default_model":"deepseek/deepseek-flash","default_thinking":"high"}|};
  print_s [%sexp (h.model.config : P.Config.t option)];
  H.show h;
  (* A later set_config keeps the saved defaults. *)
  H.keys h "/confirm off";
  H.enter h;
  [%expect
    {|
    (Rpc (method_ change_default) (params ()) (tag Default_saved))
    ((
      (scoped_models (deepseek/deepseek-flash))
      (approval_mode Ask)
      (permissions (
        (allow ())
        (ask   ())
        (deny  ())))
      (default_model    (deepseek/deepseek-flash))
      (default_thinking (high))
      (fallback_models ())
      (default_cwd     ())
      (archive_after_days 7)
      (notify             bell)
      (snippets ())))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────── default: deepseek/deepseek-flash, thinking high ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    (Rpc
      (method_ set_config)
      (params ((config ((approval_mode auto)))))
      (tag (
        Notice_on_success
        "tool confirmation off (approval mode auto, also for new sessions)")))
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
      (params ((config ((approval_mode ask)))))
      (tag (
        Notice_on_success
        "tool confirmation on (approval mode ask, also for new sessions)")))
    |}];
  H.reply h (Notice_on_success "tool confirmation on") {|{}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────────────── tool confirmation on ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/confirm";
  H.esc h;
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ mode: auto — tools run without asking (/confirm on|off   │
    │ sets ask or auto; /mode has all)                         │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* Without the config loaded, only confirm_tools is sent: the backend keeps
     the rest. *)
  let h = connected () in
  H.keys h "/confirm off";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_config)
      (params ((config ((approval_mode auto)))))
      (tag (
        Notice_on_success
        "tool confirmation off (approval mode auto, also for new sessions)")))
    |}];
  H.reply h (Notice_on_success "tool confirmation off") {|{}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────── tool confirmation off ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
           ; images = []
           ; at = None
           ; elided = None
           }
       });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ✕ bash $ sleep 999  timed out after 120s
        partial output
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "resize keeps the confirm dialog and picker within the width" =
  let h = connected ~width:120 ~height:16 () in
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Tool_confirm
       { call_id = "c1"
       ; agent_id = None
       ; rules = []
       ; name = "bash"
       ; summary = "rm -rf build"
       });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │ Run bash: rm -rf build?                                                                                              │
    │ y  allow once                                                                                                        │
    │ n  deny                                                                                                              │
    │ r  deny, saying why                                                                                                  │
    └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    ? ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  confirm: y/n/r
    |}];
  H.step h (Resize { width = 40; height = 16 });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts ·
      Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ────────────────────────────┐
    │ Run bash: rm -rf build?              │
    │ y  allow once                        │
    │ n  deny                              │
    │ r  deny, saying why                  │
    └──────────────────────────────────────┘
    ────────────────────────────────────────
    ? ▏
    deepseek-flash  $0.01  confirm: y/n/r
    |}];
  let h = connected ~width:120 ~height:16 () in
  H.keys h "/sessions";
  H.enter h;
  H.reply ~quiet:true h Sessions_picker sessions_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_picker))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (2)  Alt+A (un)archives · Alt+D deletes archived · Ctrl+N named
    / ▏
    ▸* build fix ∣ 2025-06-01T10:00 ∣ 12 msgs ∣ fix the build please  /work
       (unnamed) ∣ 2025-06-02T11:30 ∣ 0 msgs ∣ (empty)               /other
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  Enter switches · Tab marks · Ctrl+D deletes · Esc closes
    |}];
  H.step h (Resize { width = 40; height = 16 });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts ·
      Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Sessions  (2)  Alt+A (un)archives
    / ▏
    ▸* build fix ∣ 2025-06-01T10:00 ∣ 12 ms…
       (unnamed) ∣ 2025-06-02T11:30 ∣ 0 msg…
    ────────────────────────────────────────
    >
    deepseek-flash  think:off  ctx:0.1%/1.0M
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
    ▌ earlier question
      earlier answer
      delta 1 with several words that wrap at forty columns
      delta 2 with several words that wrap at forty columns
      delta 3 with several words that wrap at forty columns
      delta 4 with several words that wrap at forty columns
      delta 5 with several words that wrap at forty columns
      delta 6 with several words that wrap at forty columns
    ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  ⠋ writing · Esc aborts · Enter steers
    |}];
  H.step h (Resize { width = 40; height = 12 });
  H.show h;
  [%expect
    {|
      at forty columns
      delta 3 with several words that wrap
      at forty columns
      delta 4 with several words that wrap
      at forty columns
      delta 5 with several words that wrap
      at forty columns
      delta 6 with several words that wrap
      at forty columns
    ────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M
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
       (Prompt
          { id = "p1"
          ; prompt =
              Secret { message = "Paste your API key"; allow_empty = false }
          }));
  H.step h (Intent (Insert "sk-ant-secret"));
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    ┌─ Log in ─────────────────────────────────────────────────┐
    │ Open this URL to log in:                                 │
    │   https://claude.ai/oauth?x=1                            │
    │ Approve in the browser.                                  │
    │ Paste your API key                                       │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? *************▏
    deepseek-flash  $0.01  login: Enter answers · Esc cancels
    |}];
  H.event h (Auth (Prompt_cancelled { id = "p1" }));
  H.mode h;
  [%expect {| editing |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
      (tag (Set_model_done anthropic/claude-fable-5)))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────┐
    │ model set to anthropic/claude-fable-5; /model to change  │
    └────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
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
    (Tool_confirm
       { call_id = "c1"
       ; agent_id = None
       ; rules = []
       ; name = "bash"
       ; summary = "rm -rf build"
       });
  H.key h (Key.ctrl 'c');
  H.mode h;
  [%expect
    {|
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c1)
        (allow   false)))
      (tag Show_error))
    editing
    |}];
  let h = connected () in
  H.event
    h
    (Auth
       (Prompt
          { id = "p1"
          ; prompt = Secret { message = "key"; allow_empty = false }
          }));
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
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/verbosity verbose";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      backend: warning from backend
    ──────────────────── view: verbose — everything is shown ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "wide characters keep the editor cursor column" =
  let h = connected ~width:40 () in
  H.step h (Intent (Insert "日本語"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts ·
      Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────
    > 日本語▏
    deepseek-flash  think:off  ctx:0.1%/1.0M
    |}];
  H.key h (Key.plain Home);
  H.key h (Key.plain Right);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts ·
      Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────
    > 日▏本語
    deepseek-flash  think:off  ctx:0.1%/1.0M
    |}];
  H.key h (Key.plain End);
  H.step h (Intent (Insert "🐹!"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts ·
      Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────
    > 日本語🐹!▏
    deepseek-flash  think:off  ctx:0.1%/1.0M
    |}];
  H.enter h;
  H.event h (Message_start (user "日本語🐹 wide"));
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

      prigh in /work · /help · Esc aborts ·
      Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ 日本語🐹 wide
      emoji 🐹 and 日本語
    ────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M
    |}]
;;

let%expect_test "editing intents: cursor, word, kill, yank, undo, delete" =
  let h = connected () in
  H.keys h "one two three";
  H.key h (Key.plain Left);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > one two thre▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.plain Right);
  H.key h (Key.alt (Char "b"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > one two ▏hree
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.alt (Char "f"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > one two three▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.alt (Char "d"));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > one two three▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'k');
  H.key h (Key.ctrl 'u');
  H.key h (Key.ctrl 'w');
  H.key h (Key.ctrl 'y');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > one two three▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.alt (Char "y"));
  H.key h (Key.ctrl '_');
  H.key h (Key.plain Delete);
  H.key h (Key.plain Backspace);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "Alt+M cycles the mode; the status line shows it at once" =
  let h = connected () in
  H.event h (State { (state ()) with approval_mode = Ask });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:ask  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.alt (Char "m"));
  [%expect
    {|
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto-edit)
        (plan_mode     false)))
      (tag (
        Mode_set
        "mode: auto-edit \226\128\148 file edits run; bash, job_input and MCP tools ask first")))
    |}];
  H.reply
    h
    (Mode_set "mode: auto-edit — file edits run; bash and MCP tools ask first")
    (state_json ());
  H.key h (Key.alt (Char "m"));
  H.key h (Key.alt (Char "m"));
  H.key h (Key.alt (Char "m"));
  [%expect
    {|
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto)
        (plan_mode     true)))
      (tag (
        Mode_set
        "mode: plan \226\128\148 read-only planning: no file edits; bash, job_input and MCP tools ask first")))
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode ask)
        (plan_mode     false)))
      (tag (
        Mode_set
        "mode: ask \226\128\148 bash, job_input, edits and MCP tools ask first")))
    (Rpc
      (method_ set_mode)
      (params (
        (approval_mode auto-edit)
        (plan_mode     false)))
      (tag (
        Mode_set
        "mode: auto-edit \226\128\148 file edits run; bash, job_input and MCP tools ask first")))
    |}];
  (* A refusal puts the backend's mode back. *)
  H.reply_error h (Mode_set "x") "unknown approval mode";
  [%expect {| (Rpc (method_ get_state) (params ()) (tag Refresh_state)) |}];
  H.reply h Refresh_state (state_json ());
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────── unknown approval mode ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

(* Every keymap binding must have been reached by a scenario of this file:
   dune runs each file's inline tests in a separate process, so the other
   test_app_*.ml files' keys do not count here. *)
let%expect_test "Alt+N: the snippet picker inserts at the cursor" =
  let h = connected () in
  let alt_n () = H.key h (Key.alt (Char "n")) in
  alt_n ();
  H.reply ~quiet:true h Config {|{"snippets":[]}|};
  alt_n ();
  H.keys h "Please ";
  H.reply
    ~quiet:true
    h
    Config
    {|{"snippets":[{"name":"review","text":"review the diff"},{"name":"tests","text":"run the tests"}]}|};
  alt_n ();
  H.show h;
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Snippets  (2)
    / ▏
    ▸  review  review the diff
       tests   run the tests
    ────────────────────────────────────────────────────────────
    > Please
    deepseek-flash  ctx:0.1%/1.0M  Enter inserts the text at th…





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > Please review the diff▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

(* Two subagents (one done) and a background job: what the sidebar lists. *)
let with_agents ~width ~height =
  let h = connected ~width ~height () in
  H.event
    h
    (State
       (state ~running:true ~jobs:[ "j1", "sleep 100 && make deploy", None ] ()));
  H.event h (Message_start (user "go"));
  List.iter
    [ "c1", "find all auth code in the repository", "claude-haiku"
    ; "c2", "run the test suite", "claude-sonnet"
    ]
    ~f:(fun (id, task, model) ->
      H.event
        h
        (Tool_start
           (tool_call
              ~name:"subagent"
              ~arguments:(sprintf {|{"task":%S}|} task)
              id));
      H.event
        h
        (Subagent_start
           { call_id = id; agent_id = id; task; model; tools = [ "bash" ] }));
  H.event h (Subagent { call_id = "c1"; agent_id = "c1"; event = Turn_start });
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
  h
;;

let%expect_test
    "sidebar: off by default; Alt+L shows agents and jobs on wide terminals"
  =
  let h = with_agents ~width:130 ~height:16 in
  H.show h;
  [%expect {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ go
    … subagent find all auth code in the repository  1 turn
    ✓ subagent run the test suite  1 turn $0.02
        all tests pass
    ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  agents:[main] 1⠋ 2✓  jobs:1  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.key h (Key.alt (Char "l"));
  H.show h;
  [%expect {|
     Agents                     │
    ▌● main              running│
       running tools            │
     ● 1 find all auth…  running│
       1 turn · claude-haiku    │   prigh in /work · /help · Esc aborts · Ctrl+C twice quits
     ● 2 run the test sui…  done│
       1 turn · $0.02           │ ▌ earlier question
                                │   earlier answer
     Jobs                       │
     ● j1                running│ ▌ go
       sleep 100 && make deploy │ … subagent find all auth code in the repository  1 turn
                                │ ✓ subagent run the test suite  1 turn $0.02
     Alt+L hides · ⇧Tab focuses │     all tests pass
    ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  agents:[main] 1⠋ 2✓  jobs:1  ⠋ running tools · Esc aborts · Enter steers
    |}];
  (* Focusing an agent highlights it in the sidebar too. *)
  H.focus_agent h 1;
  H.show h;
  [%expect {|
     Agents                     │ ◆ subagent 1/2  claude-haiku  ⠋ running 1 turns  "find all auth code in the repository"
     ● main              running│
       running tools            │
    ▌● 1 find all auth…  running│
       1 turn · claude-haiku    │
     ● 2 run the test sui…  done│
       1 turn · $0.02           │
                                │
     Jobs                       │
     ● j1                running│
       sleep 100 && make deploy │
                                │
     Alt+L hides · ⇧Tab focuses │
    ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  agents:main [1⠋] 2✓  jobs:1  ⠋ running tools · Esc aborts · Enter steers
    |}];
  (* The choice holds for the run. *)
  H.step ~quiet:true h (Resize { width = 150; height = 16 });
  H.show h;
  [%expect {|
     Agents                     │ ◆ subagent 1/2  claude-haiku  ⠋ running 1 turns  "find all auth code in the repository"
     ● main              running│
       running tools            │
    ▌● 1 find all auth…  running│
       1 turn · claude-haiku    │
     ● 2 run the test sui…  done│
       1 turn · $0.02           │
                                │
     Jobs                       │
     ● j1                running│
       sleep 100 && make deploy │
                                │
     Alt+L hides · ⇧Tab focuses │
    ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  agents:main [1⠋] 2✓  jobs:1  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.key h (Key.alt (Char "l"));
  H.show h;
  [%expect {|
    ◆ subagent 1/2  claude-haiku  ⠋ running 1 turns  "find all auth code in the repository"












    ──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  agents:main [1⠋] 2✓  jobs:1  ⠋ running tools · Esc aborts · Enter steers
    |}]
;;

let%expect_test "sidebar: hidden below 120 columns, where nothing else changes" =
  let h = with_agents ~width:119 ~height:16 in
  (* Toggling says why nothing shows. *)
  H.key h (Key.alt (Char "l"));
  H.show h;
  [%expect {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ go
    … subagent find all auth code in the repository  1 turn
    ✓ subagent run the test suite  1 turn $0.02
        all tests pass
    ──────────────────────────────────────────────────────────── sidebar on: it shows at 120 columns and more (now 119) ───
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  agents:[main] 1⠋ 2✓  jobs:1  ⠋ running tools · Esc aborts · Enter steers
    |}];
  H.key h (Key.alt (Char "l"));
  H.show h;
  [%expect {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ go
    … subagent find all auth code in the repository  1 turn
    ✓ subagent run the test suite  1 turn $0.02
        all tests pass
    ─────────────────────────────────────────────────────────────────────────────────────────────────────── sidebar off ───
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  agents:[main] 1⠋ 2✓  jobs:1  ⠋ running tools · Esc aborts · Enter steers
    |}]
;;

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
    Alt+S            Submit_as_typed          covered
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
    Alt+N            Snippet_picker           covered
    Ctrl+P           Next_model               covered
    Alt+P            Prev_model               covered
    Ctrl+T           Next_thinking            covered
    Alt+M            Cycle_mode               covered
    Ctrl+N           Picker_toggle_filter     covered
    Alt+A            Toggle_archived          covered
    Ctrl+X           Copy_last                covered
    Ctrl+Z           Suspend                  covered
    Alt+L            Toggle_sidebar           covered
    Shift+Tab        Next_agent               covered
    Alt+1            (Focus_agent 1)          covered
    Ctrl+C           Interrupt                covered
    Ctrl+D           Force_quit               covered
    |}]
;;

let%expect_test "reconnect: backoff caps before integer overflow" =
  List.iter [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 1_000_000 ] ~f:(fun attempt ->
    printf "%dms\n" (App.Connection.delay_ms ~attempt));
  [%expect
    {|
    250ms
    500ms
    1000ms
    2000ms
    4000ms
    8000ms
    16000ms
    32000ms
    60000ms
    60000ms
    60000ms
    |}]
;;

let%expect_test "reconnect: backoff doubles to the 60s cap, stale replies are \
                 ignored, /retry-backend-connection retries now, success \
                 resyncs"
  =
  let h = connected ~width:70 () in
  H.step h Backend_closed;
  [%expect
    {|
    (Reconnect
      (generation 1)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    |}];
  (* Losing the transport again mid-reconnect counts as a failed attempt. *)
  H.step h Backend_closed;
  [%expect
    {|
    (Reconnect
      (generation 2)
      (delay_ms   500)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    |}];
  let fail generation =
    H.reply_error h (Reconnect generation) "connection refused"
  in
  fail 1;
  [%expect {| |}];
  List.iter [ 2; 3; 4; 5; 6; 7; 8 ] ~f:fail;
  [%expect
    {|
    (Reconnect
      (generation 3)
      (delay_ms   1000)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    (Reconnect
      (generation 4)
      (delay_ms   2000)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    (Reconnect
      (generation 5)
      (delay_ms   4000)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    (Reconnect
      (generation 6)
      (delay_ms   8000)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    (Reconnect
      (generation 7)
      (delay_ms   16000)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    (Reconnect
      (generation 8)
      (delay_ms   32000)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    (Reconnect
      (generation 9)
      (delay_ms   60000)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    |}];
  print_s [%sexp (h.model.connection : App.Connection.t)];
  [%expect
    {|
    (Reconnecting
      (attempt    9)
      (generation 9)
      (delay_ms   60000)
      (session (/home/u/.prigh/sessions/1.jsonl)))
    |}];
  (* A late reply from an earlier attempt is ignored. *)
  fail 3;
  H.reply h (Reconnect 3) {|{"client_id":"client-9"}|};
  print_s [%sexp (h.model.connection : App.Connection.t)];
  [%expect
    {|
    (Reconnecting
      (attempt    9)
      (generation 9)
      (delay_ms   60000)
      (session (/home/u/.prigh/sessions/1.jsonl)))
    |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      backend connection lost; reconnecting (/retry-backend-connection to
      retry now, Ctrl+C quits)
    ┌─ Warning ──────────────────────────────────────────────────────────┐
    │ reconnect failed: connection refused; retrying in 60s (attempt 9)  │
    └──────────────────────────────────────────────────── Esc dismisses ─┘
    ──────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  backend gone · retry 9 in 60s
    |}];
  (* The user cuts the wait short; the pending attempt 9 becomes stale. *)
  H.keys h "/retry-backend-connection";
  H.enter h;
  [%expect
    {|
    (Reconnect
      (generation 10)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    |}];
  fail 9;
  [%expect {| |}];
  H.reply h (Reconnect 10) {|{"client_id":"client-7"}|};
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    |}];
  print_s
    [%sexp
      (h.model.connection : App.Connection.t)
    , (h.model.client_id : string option)];
  [%expect {| (Connected (client-7)) |}];
  H.reply h Initial_state (state_json ());
  H.reply h Initial_messages {|[{"role":"user","text":"earlier question"}]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
    ─────────────────────────────────────── reconnected to the backend ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/retry-backend-connection";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
    ───────────────────────────────────────────── backend is connected ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* A later disconnection starts a fresh backoff. *)
  H.step h Backend_closed;
  [%expect
    {|
    (Reconnect
      (generation 11)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    |}]
;;

let%expect_test "reconnect: a session the backend no longer knows (never \
                 saved, or deleted) is dropped instead of retried forever"
  =
  let h = connected ~width:70 () in
  H.step h Backend_closed;
  [%expect
    {|
    (Reconnect
      (generation 1)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user ()))
    |}];
  (* A fresh backend has no record of an empty session: it was never written to
     disk. The retry is immediate, without the session, and keeps the attempt
     count. *)
  H.reply_error h (Reconnect 1) "no session \"/home/u/.prigh/sessions/1.jsonl\"";
  [%expect
    {|
    (Reconnect
      (generation 2)
      (delay_ms   0)
      (session ())
      (as_user ()))
    |}];
  print_s [%sexp (h.model.connection : App.Connection.t)];
  [%expect
    {|
    (Reconnecting
      (attempt    1)
      (generation 2)
      (delay_ms   0)
      (session ()))
    |}];
  (* Other failures keep backing off, still without the session; and losing the
     transport again does not bring it back. *)
  H.reply_error h (Reconnect 2) "connection refused";
  H.step h Backend_closed;
  [%expect
    {|
    (Reconnect
      (generation 3)
      (delay_ms   500)
      (session ())
      (as_user ()))
    (Reconnect
      (generation 4)
      (delay_ms   1000)
      (session ())
      (as_user ()))
    |}];
  H.reply h (Reconnect 4) {|{"client_id":"client-2"}|};
  [%expect
    {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    |}];
  H.reply
    h
    Initial_state
    (state_json ~session:("fresh1", "/home/u/.prigh/sessions/2.jsonl") ());
  H.reply h Initial_messages {|[]|};
  H.show h;
  [%expect
    {|
      session /home/u/.prigh/sessions/1.jsonl no longer exists (it was
      never saved, or was deleted); starting a new one
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ─────────────────────────────────────── reconnected to the backend ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* The next disconnection resumes the new session as usual. *)
  H.step h Backend_closed;
  [%expect
    {|
    (Reconnect
      (generation 5)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/2.jsonl))
      (as_user ()))
    |}]
;;

let%expect_test "wheel scrolling moves the transcript a few lines; arrows \
                 still walk the prompt history"
  =
  let h = connected ~height:10 () in
  H.reply ~quiet:true h History {|["older prompt","newer prompt"]|};
  List.iter (List.range 0 8) ~f:(fun i ->
    H.event h (Message_start (user (sprintf "message %d" i))));
  H.show h;
  [%expect
    {|
    ▌ message 4

    ▌ message 5

    ▌ message 6

    ▌ message 7
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.step h (Intent Scroll_up);
  H.show h;
  [%expect
    {|
    ▌ message 3

    ▌ message 4

    ▌ message 5

    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ↑ scrolled
    |}];
  H.step h (Intent Scroll_up);
  print_s [%sexp (h.model.viewport : Viewport.t)];
  H.step h (Intent Scroll_down);
  H.step h (Intent Scroll_down);
  print_s [%sexp (h.model.viewport : Viewport.t)];
  [%expect
    {|
    (Anchored
      (top       7)
      (new_lines 0))
    Follow
    |}];
  H.key h (Key.plain Up);
  H.show h;
  [%expect
    {|
    ▌ message 4

    ▌ message 5

    ▌ message 6

    ▌ message 7
    ────────────────────────────────────────────────────────────
    > newer prompt▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "switching tool host asks for the directory there, prefilled \
                 with the current cwd"
  =
  let h = connected ~width:70 () in
  let state =
    Or_error.ok_exn
      (P.State.of_json
         (Or_error.ok_exn
            (P.Json.parse
               {|{"session_id":"abc123","session_path":"/s","session_name":null,"cwd":"/work","git_branch":null,"model":{"id":"m","provider":"deepseek","key":"deepseek/m","name":"M","context_window":1000,"max_output":10,"supports_thinking":false,"cost":{"input":1,"output":1,"cache_read":1}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"srv","cwd":"/work"},{"id":"client-1","name":"laptop","cwd":"/home/me"}]}|})))
  in
  H.event h (State state);
  H.step h (Hello { client_id = "client-1"; namespace = None; user = None });
  H.keys h "/host laptop";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Working directory on laptop (here) ───────────────────────────────┐
    └────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────────────────────
    ? /work▏
    m  think:n/a  mode:auto  ctx:0.0%/1.0k  Enter submits · Esc cancels
    |}];
  (* Directories complete on the host being switched to: Tab asks, Tab again
     accepts and drills down, Enter submits the text. *)
  H.key h (Key.plain Tab);
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params (
        (prefix /work)
        (host   client-1)))
      (tag (Dirs_for_autocomplete /work)))
    |}];
  H.reply h (Dirs_for_autocomplete "/work") {|["/work/"]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Working directory on laptop (here) ───────────────────────────────┐
    └────────────────────────────────────────────────────────────────────┘
    ▸ /work/
    ──────────────────────────────────────────────────────────────────────
    ? /work▏
    m  think:n/a  ctx:0.0%/1.0k  $0.00  Tab/Enter accept · Alt+S as typed
    |}];
  H.key h (Key.plain Tab);
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params (
        (prefix /work/)
        (host   client-1)))
      (tag (Dirs_for_autocomplete /work/)))
    |}];
  H.reply h (Dirs_for_autocomplete "/work/") {|["/work/src/","/work/test/"]|};
  H.step ~quiet:true h (Key (Key.char 's'));
  H.reply ~quiet:true h (Dirs_for_autocomplete "/work/s") {|["/work/src/"]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Working directory on laptop (here) ───────────────────────────────┐
    └────────────────────────────────────────────────────────────────────┘
    ▸ /work/src/
    ──────────────────────────────────────────────────────────────────────
    ? /work/s▏
    m  think:n/a  ctx:0.0%/1.0k  $0.00  Tab/Enter accept · Alt+S as typed
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_active_host)
      (params (
        (host       client-1)
        (cwd        /work/src/)
        (if_missing ask)))
      (tag (
        Dir_changed (
          Host
          (id   client-1)
          (name laptop)))))
    |}];
  H.mode h;
  [%expect {| editing |}];
  (* A missing directory there is created once confirmed. *)
  let host : P.Dir_change.Target.t = Host { id = "client-1"; name = "laptop" } in
  H.reply h (Dir_changed host) {|{"missing":"/work/src"}|};
  H.show h;
  H.key h (Key.char 'y');
  H.reply h (Dir_changed host) {|{"cwd":"/work/src","created":true}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Confirm ──────────────────────────────────────────────────────────┐
    │ Create /work/src on laptop? (y/n)                                  │
    └────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────────────────────
    ? ▏
    /work  m  think:n/a  mode:auto  ctx:0.0%/1.0k  $0.00  confirm: y / n
    (Rpc
      (method_ set_active_host)
      (params (
        (host       client-1)
        (cwd        /work/src)
        (if_missing create)))
      (tag (
        Dir_changed (
          Host
          (id   client-1)
          (name laptop)))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────── created /work/src; tool host switched ───
    > ▏
    /work  m  think:n/a  mode:auto  view:normal  ctx:0.0%/1.0k  $0.00
    |}];
  (* The picker route ends in the same prompt; Esc abandons it. *)
  H.keys h "/host";
  H.enter h;
  H.mode h;
  H.key h (Key.plain Down);
  H.enter h;
  H.show h;
  [%expect
    {|
    picker



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Working directory on laptop (here) ───────────────────────────────┐
    └────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────────────────────
    ? /work▏
    m  think:n/a  mode:auto  ctx:0.0%/1.0k  Enter submits · Esc cancels
    |}];
  H.esc h;
  H.mode h;
  H.keys h "/host nope";
  H.enter h;
  H.show h;
  [%expect
    {|
    editing





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────── unknown host "nope"; one of: srv, laptop (here) ───
    > ▏
    /work  m  think:n/a  mode:auto  view:normal  ctx:0.0%/1.0k  $0.00
    |}]
;;

(* A tool-capable TUI sends a [host_id] in every hello; the backend lists its
   host under that id, the same across reconnects, so it stays "(here)". *)
let%expect_test "the hello's host id marks this frontend, also after a \
                 reconnect"
  =
  let h = connected ~width:70 () in
  let state =
    {|{"session_id":"abc123","session_path":"/s","session_name":null,"cwd":"/home/me","git_branch":null,"model":{"id":"m","provider":"deepseek","key":"deepseek/m","name":"M","context_window":1000,"max_output":10,"supports_thinking":false,"cost":{"input":1,"output":1,"cache_read":1}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"host-abc","hosts":[{"id":"backend","name":"srv","cwd":"/work"},{"id":"client-1","name":"desk","cwd":"/d"},{"id":"host-abc","name":"laptop","cwd":"/home/me"}]}|}
  in
  let picker () =
    H.keys h "/host";
    H.enter h;
    H.show h;
    H.esc h
  in
  H.event
    h
    (State
       (Or_error.ok_exn
          (P.State.of_json (Or_error.ok_exn (P.Json.parse state)))));
  H.step
    h
    (Hello
       (Or_error.ok_exn
          (P.Hello_reply.of_json
             (Or_error.ok_exn
                (P.Json.parse {|{"client_id":"client-1","host_id":"host-abc"}|})))));
  picker ();
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Tool host  (3)
    / ▏
       srv            /work
       desk           /d
    ▸* laptop (here)  /home/me
    ──────────────────────────────────────────────────────────────────────
    >
    m  think:n/a  mode:auto  ctx:0.0%/1.0k  Enter selects · Esc closes
    |}];
  H.step h Backend_closed;
  H.reply h (Reconnect 1) {|{"client_id":"client-2","host_id":"host-abc"}|};
  print_s [%sexp (h.model.client_id : string option)];
  H.reply h Initial_state state;
  H.reply h Initial_messages {|[]|};
  picker ();
  [%expect
    {|
    (Reconnect
      (generation 1)
      (delay_ms   0)
      (session (/s))
      (as_user ()))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ list_models) (params ()) (tag Models_catalog))
    (host-abc)



      prigh in /home/me · /help · Esc aborts · Ctrl+C twice quits
    Tool host  (3)
    / ▏
       srv            /work
       desk           /d
    ▸* laptop (here)  /home/me
    ──────────────────────────────────────────────────────────────────────
    >
    m  think:n/a  mode:auto  ctx:0.0%/1.0k  Enter selects · Esc closes
    |}]
;;

let%expect_test "the host picker shows which session other frontends are in" =
  let h = connected ~width:70 () in
  let state =
    Or_error.ok_exn
      (P.State.of_json
         (Or_error.ok_exn
            (P.Json.parse
               {|{"session_id":"abc123","session_path":"/s","session_name":null,"cwd":"/work","git_branch":null,"model":{"id":"m","provider":"deepseek","key":"deepseek/m","name":"M","context_window":1000,"max_output":10,"supports_thinking":false,"cost":{"input":1,"output":1,"cache_read":1}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"client-3","hosts":[{"id":"backend","name":"srv","cwd":"/work","session_id":null,"session_name":null},{"id":"client-1","name":"laptop","cwd":"/home/me","session_id":"abc123","session_name":null},{"id":"client-2","name":"desktop","cwd":"/home/me/other","session_id":"def456","session_name":"refactor"},{"id":"client-3","name":"pi","cwd":"/pi","session_id":"9876","session_name":null}]}|})))
  in
  H.event h (State state);
  H.step h (Hello { client_id = "client-1"; namespace = None; user = None });
  H.keys h "/host";
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Tool host  (4)
    / ▏
       srv                    /work
       laptop (here)          /home/me
       desktop (in refactor)  /home/me/other
    ▸* pi (in 9876)           /pi
    ──────────────────────────────────────────────────────────────────────
    >
    m  think:n/a  mode:auto  ctx:0.0%/1.0k  Enter selects · Esc closes
    |}];
  H.esc h;
  H.keys h "/host desktop";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Working directory on desktop (in refactor) ───────────────────────┐
    └────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────────────────────
    ? /work▏
    m  think:n/a  mode:auto  ctx:0.0%/1.0k  Enter submits · Esc cancels
    |}]
;;

(* ---- UX guarantees ---------------------------------------------------- *)

let%expect_test
    "/model Enter Enter opens the picker instead of silently picking the first \
     model; a filter or Down makes Enter accept"
  =
  let h = connected () in
  H.reply ~quiet:true h Models_catalog models_json;
  H.keys h "/model";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ Claude Fable 5       anthropic/claude-fable-5
      Claude Fable 5.1     anthropic/claude-fable-5-1
      GPT-5.5              openai/gpt-5.5
      DeepSeek V4.1 Flash  deepseek/deepseek-flash
    ────────────────────────────────────────────────────────────
    > /model ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides
    |}];
  (* Nothing typed, nothing chosen: Enter runs /model, which is the picker. *)
  H.enter h;
  H.mode h;
  H.show h;
  [%expect
    {|
    picker

    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
       Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M …
       Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0…
       GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 p…
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M…
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.esc h;
  (* Down means the highlight was chosen on purpose. *)
  H.keys h "/model ";
  H.key h (Key.plain Down);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      Claude Fable 5       anthropic/claude-fable-5
    ▸ Claude Fable 5.1     anthropic/claude-fable-5-1
      GPT-5.5              openai/gpt-5.5
      DeepSeek V4.1 Flash  deepseek/deepseek-flash
    ────────────────────────────────────────────────────────────
    > /model ▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model anthropic/claude-fable-5-1)))
      (tag (Set_model_done anthropic/claude-fable-5-1)))
    |}];
  (* So does typing a filter. *)
  H.keys h "/model gpt";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model openai/gpt-5.5)))
      (tag (Set_model_done openai/gpt-5.5)))
    |}];
  (* Tab always accepts the highlight. *)
  H.keys h "/model ";
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /model anthropic/claude-fable-5▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  (* The same rule sends /cd to its prompt rather than into the first directory. *)
  H.key h (Key.ctrl 'u');
  H.keys h "/cd ";
  H.reply ~quiet:true h (Dirs_for_autocomplete "") {|["src/"]|};
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params ((prefix "")))
      (tag (Dirs_for_autocomplete "")))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ src/
    ────────────────────────────────────────────────────────────
    > /cd ▏
    deepseek-flash  Tab accepts · Enter as typed · Esc hides
    |}];
  H.enter h;
  H.mode h;
  [%expect {| text_prompt |}]
;;

let%expect_test
    "picking a model reports it, and flags a provider that is not logged in"
  =
  let h = connected () in
  H.reply ~quiet:true h Models_catalog models_json;
  H.keys h "/model gpt";
  H.enter h;
  H.reply h (Set_model_done "openai/gpt-5.5") "{}";
  H.keys h "/model flash";
  H.enter h;
  H.reply h (Set_model_done "deepseek/deepseek-flash") "{}";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ set_model)
      (params ((model openai/gpt-5.5)))
      (tag (Set_model_done openai/gpt-5.5)))
    (Rpc
      (method_ set_model)
      (params ((model deepseek/deepseek-flash)))
      (tag (Set_model_done deepseek/deepseek-flash)))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────── model: deepseek/deepseek-flash ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "the scoped mark only appears when the scope is a real subset" =
  let h = connected () in
  H.reply ~quiet:true h Models_catalog models_json;
  (* No config scope and only deepseek logged in: the scope is the logged-in
     models, a subset, so it is marked. *)
  H.key h (Key.ctrl 'l');
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
       Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M …
       Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0…
       GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 p…
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash ◆  ctx 1.0M…
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.esc h;
  (* Nobody logged in: every model is in scope, so no row is marked. *)
  H.reply ~quiet:true h Auth_refresh "[]";
  H.key h (Key.ctrl 'l');
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Model  (4)
    / ▏
       Claude Fable 5       anthropic/claude-fable-5  ctx 1.0M …
       Claude Fable 5.1     anthropic/claude-fable-5-1  ctx 1.0…
       GPT-5.5              openai/gpt-5.5  ctx 1.0M  $10/$50 p…
    ▸* DeepSeek V4.1 Flash  deepseek/deepseek-flash  ctx 1.0M  …
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;

let%expect_test "picker: Enter with no match keeps it open; the title shows \
                 the window for long lists"
  =
  let h = connected ~width:70 () in
  let many =
    sprintf
      "[%s]"
      (String.concat
         ~sep:","
         (List.init 25 ~f:(fun i ->
            model_json (sprintf "m-%02d" i) (sprintf "Model %02d" i))))
  in
  H.reply ~quiet:true h Models_catalog many;
  H.key h (Key.ctrl 'l');
  H.keys h "zzz";
  H.enter h;
  H.mode h;
  H.show h;
  [%expect
    {|
    picker


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Model  (0)
    / zzz▏
      no matches
    ──────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  Enter selects · Esc closes · Ctrl+N logged-in only
    |}];
  H.key h (Key.ctrl 'u');
  H.show h;
  [%expect
    {|
    Model  (1–7 of 25)
    / ▏
    ▸  Model 00  anthropic/m-00  ctx 1.0M  $10/$50 per M  not logged in
       Model 01  anthropic/m-01  ctx 1.0M  $10/$50 per M  not logged in
       Model 02  anthropic/m-02  ctx 1.0M  $10/$50 per M  not logged in
       Model 03  anthropic/m-03  ctx 1.0M  $10/$50 per M  not logged in
       Model 04  anthropic/m-04  ctx 1.0M  $10/$50 per M  not logged in
       Model 05  anthropic/m-05  ctx 1.0M  $10/$50 per M  not logged in
       Model 06  anthropic/m-06  ctx 1.0M  $10/$50 per M  not logged in
    ──────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  Enter selects · Esc closes · Ctrl+N logged-in only
    |}];
  H.key h (Key.plain Page_down);
  H.key h (Key.plain Page_down);
  H.show h;
  [%expect
    {|
    Model  (12–18 of 25)
    / ▏
       Model 11  anthropic/m-11  ctx 1.0M  $10/$50 per M  not logged in
       Model 12  anthropic/m-12  ctx 1.0M  $10/$50 per M  not logged in
       Model 13  anthropic/m-13  ctx 1.0M  $10/$50 per M  not logged in
    ▸  Model 14  anthropic/m-14  ctx 1.0M  $10/$50 per M  not logged in
       Model 15  anthropic/m-15  ctx 1.0M  $10/$50 per M  not logged in
       Model 16  anthropic/m-16  ctx 1.0M  $10/$50 per M  not logged in
       Model 17  anthropic/m-17  ctx 1.0M  $10/$50 per M  not logged in
    ──────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  Enter selects · Esc closes · Ctrl+N logged-in only
    |}]
;;

let%expect_test "Tab on an empty editor opens the command list; the list shows \
                 its window"
  =
  let h = connected () in
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
    ▸ /help                           show commands and keys
      /hotkeys                        show keyboard shortcuts
      /model [name|id|provider/id]    pick or switch the model
      /scoped-models                  pick the models Ctrl+P cy…
      /change_default                 save the current model an…
      /fallback [off|model...]        show or set the models th…
      /default-dir [off|path]         show or set the directory…
      /login [provider] [api_key|oauth]  log in to a provider, …
      ↕ 1–8 of 62
    ────────────────────────────────────────────────────────────
    > /▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.key h (Key.plain End);
  H.show h;
  [%expect
    {|
    ▸ /help                           show commands and keys
      /hotkeys                        show keyboard shortcuts
      /model [name|id|provider/id]    pick or switch the model
      /scoped-models                  pick the models Ctrl+P cy…
      /change_default                 save the current model an…
      /fallback [off|model...]        show or set the models th…
      /default-dir [off|path]         show or set the directory…
      /login [provider] [api_key|oauth]  log in to a provider, …
      ↕ 1–8 of 62
    ────────────────────────────────────────────────────────────
    > /▏
    deepseek-flash  think:off  Tab/Enter accept · Alt+S as typed
    |}];
  H.keys h "qu";
  H.enter h;
  [%expect {| Quit |}]
;;

let%expect_test "Ctrl+C while a turn runs aborts it first; the next press quits"
  =
  let h = connected () in
  H.keys h "do it";
  H.enter h;
  H.event h (State (state ~running:true ()));
  H.set_pending h [ "c9", "bash", "rm -rf /" ];
  (* A pending confirmation is denied as part of the abort. *)
  H.key h (Key.ctrl 'c');
  H.show h;
  [%expect
    {|
    (Rpc (method_ prompt) (params ((text "do it"))) (tag Show_error))
    (Append_history "do it")
    (Rpc
      (method_ tool_confirm_respond)
      (params (
        (call_id c9)
        (allow   false)))
      (tag Ignore))
    (Rpc (method_ abort) (params ()) (tag Abort_done))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────── aborting; Ctrl+C again quits ───
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Ctrl+C again quits
    |}];
  (* Any other key forgets the pending quit. *)
  H.keys h "x";
  H.key h (Key.ctrl 'c');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  ⠋ working · Esc aborts · Enter steers
    |}];
  H.key h (Key.ctrl 'c');
  [%expect {| (Rpc (method_ abort) (params ()) (tag Abort_done)) |}];
  H.key h (Key.ctrl 'c');
  [%expect {| Quit |}]
;;

let%expect_test "scrolled up: the status says so, and Esc returns to the \
                 bottom when idle (aborts when running)"
  =
  let h = connected ~height:8 () in
  List.iter (List.range 0 6) ~f:(fun i ->
    H.event h (Message_start (user (sprintf "line %d" i))));
  H.key h (Key.plain Page_up);
  H.show h;
  [%expect
    {|
    ▌ line 1

    ▌ line 2

    ▌ line 3
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ↑ scrolled
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
    ▌ line 3

    ▌ line 4

    ▌ line 5
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.event h (State (state ~running:true ()));
  H.key h (Key.plain Page_up);
  H.esc h;
  [%expect {| (Rpc (method_ abort) (params ()) (tag Abort_done)) |}];
  print_s [%sexp (h.model.viewport : Viewport.t)];
  [%expect {|
    (Anchored
      (top       7)
      (new_lines 0))
    |}]
;;

let%expect_test "/thinking lists levels in cycling order with the default \
                 marked"
  =
  let h = connected () in
  H.keys h "/thinking";
  H.enter h;
  H.enter h;
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
    Thinking level  (5)
    / ▏
    ▸* off
       low
       on    provider default
       high
       max
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;

let%expect_test "/new and /switch leave the previous session's subagents behind"
  =
  let h = connected () in
  H.event h Agent_start;
  H.event
    h
    (Subagent_start
       { call_id = "c1"
       ; agent_id = "c1"
       ; task = "old work"
       ; model = "m"
       ; tools = []
       });
  H.event h (Agent_end []);
  H.next_agent h;
  H.mode h;
  print_s
    [%sexp
      (List.length h.model.agents : int)
    , (h.model.focus : [ `Main | `Agent of string ])];
  [%expect
    {|
    editing
    (1 (Agent c1))
    |}];
  H.keys h "/new";
  H.enter h;
  H.reply h (Reload_messages_notice "new session") "{}";
  print_s
    [%sexp
      (List.length h.model.agents : int)
    , (h.model.focus : [ `Main | `Agent of string ])];
  H.next_agent h;
  print_s [%sexp (h.model.focus : [ `Main | `Agent of string ])];
  [%expect
    {|
    (Rpc
      (method_ new_session)
      (params ())
      (tag (Reload_messages_notice "new session")))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (0 Main)
    Main
    |}]
;;

let%expect_test "logged-in user: shown in the status line, kept across \
                 reconnects; /signout asks the platform"
  =
  let h = connected ~width:80 () in
  H.step h (Hello
       { client_id = "client-1"; namespace = Some "lloyd"; user = Some "lloyd" });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  user:lloyd
    |}];
  (* Narrow screens drop it before the more useful parts. *)
  H.step h (Resize { width = 50; height = 12 });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C
      twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  H.step h (Resize { width = 80; height = 12 });
  H.step ~quiet:true h Backend_closed;
  H.reply
    ~quiet:true
    h
    (Reconnect 1)
    {|{"client_id":"client-2","namespace":"alice"}|};
  print_s
    [%sexp
      (h.model.client_id : string option), (h.model.namespace : string option)];
  [%expect {|
    ((client-2)
     (alice))
    |}];
  (* Outside namespace mode the reply's namespace is null: nothing is shown. *)
  H.step ~quiet:true h Backend_closed;
  H.reply
    ~quiet:true
    h
    (Reconnect 2)
    {|{"client_id":"client-3","namespace":null}|};
  print_s
    [%sexp
      (h.model.client_id : string option), (h.model.namespace : string option)];
  H.reply ~quiet:true h Initial_state (state_json ());
  H.show h;
  [%expect
    {|
    ((client-3) ())








      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ───────────────────────────────────────────────── reconnected to the backend ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/signout";
  H.enter h;
  [%expect {| Sign_out |}]
;;

let jobs_json =
  {|[{"id":"j1","command":"make test","running":true,"exit":null,"delivered":false,"elapsed":75.4,"bytes":2048,"last_line":"PASS parser"},{"id":"j2","command":"docker build .","running":false,"exit":"exited 0","delivered":true,"elapsed":3.2,"bytes":10,"last_line":null}]|}
;;

let%expect_test "background jobs: !&, status line while idle, /jobs picker, \
                 output, kill, delivery"
  =
  let h = connected ~width:100 ~height:16 () in
  H.keys h "!&make test";
  H.enter h;
  H.reply h Job_started {|{"job_id":"j1"}|};
  H.event h (State (state ~jobs:[ "j1", "make test", None ] ()));
  H.show h;
  print_s [%sexp (App.Model.jobs_running h.model : bool)];
  [%expect
    {|
    (Rpc
      (method_ shell)
      (params (
        (command    "make test")
        (background true)))
      (tag Job_started))
    (Append_history "!&make test")









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────── started job j1; /jobs shows it, and its exit is reported to the agent ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  jobs:1 ⠋
    true
    |}];
  (* The picker lists id, state, elapsed time and the last line; Ctrl+D kills
     the highlighted job. *)
  H.keys h "/jobs";
  H.enter h;
  H.reply h Jobs_picker jobs_json;
  H.show h;
  H.key h (Key.ctrl 'd');
  H.mode h;
  [%expect
    {|
    (Rpc (method_ list_jobs) (params ()) (tag Jobs_picker))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Jobs  (2)
    / ▏
    ▸  make test       j1  running  1m15s  PASS parser
       docker build .  j2  exited 0  3s
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  ctx:0.1%/1.0M  jobs:1 ⠋  Enter shows output · Esc closes · Ctrl+D kills
    (Rpc
      (method_ kill_job)
      (params ((job_id j1)))
      (tag (Notice_on_success "killing job j1")))
    editing
    |}];
  (* Enter shows the recent output in the transcript. *)
  H.keys h "/jobs";
  H.enter h;
  H.reply ~quiet:true h Jobs_picker jobs_json;
  H.key h (Key.plain Down);
  H.enter h;
  H.reply
    h
    Job_output
    {|{"text":"[job j2 exited 0 after 3s, 10 bytes; lines 1-2 of 2] docker build .\nStep 1/2\nStep 2/2"}|};
  H.keys h "/jobs kill j1";
  H.enter h;
  H.keys h "/jobs j1";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_jobs) (params ()) (tag Jobs_picker))
    (Rpc
      (method_ job_output)
      (params (
        (job_id j2)
        (lines  200)))
      (tag Job_output))
    (Rpc
      (method_ kill_job)
      (params ((job_id j1)))
      (tag (Notice_on_success "killing job j1")))
    (Rpc
      (method_ job_output)
      (params (
        (job_id j1)
        (lines  200)))
      (tag Job_output))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01  jobs:1 ⠋
    |}];
  (* The exit report reaches the idle agent and renders compactly. *)
  H.event h (State (state ~running:true ()));
  H.event
    h
    (Message_start
       (delivery
          "[job j1 killed] make test\n\
           ok parser\n\
           PASS parser\n\n\
           [job j3 exited 2] make lint\n\
           lint.ml:3: error"));
  say h "the tests were killed";
  H.event h (State (state ()));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ↩ job j1 "make test"  killed
        ok parser
        PASS parser
    ↩ job j3 "make lint"  exited 2
        lint.ml:3: error
      the tests were killed
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "job reports reload compactly; look-alike text stays a user \
                 message"
  =
  let h = connected ~width:80 ~height:12 () in
  H.reply
    ~quiet:true
    h
    Initial_messages
    {|[{"role":"user","text":"[job j4 failed: tool host disconnected] npm run dev\nlistening on :3000","origin":{"source":{"kind":"delivery","title":"Reports"}}},{"role":"user","text":"[job board] what is on it?"}]|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ↩ job j4 "npm run dev"  failed: tool host disconnected
        listening on :3000

    ▌ [job board] what is on it?
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/setusr: lists users, acts as one, reconnects as them" =
  let h = connected ~width:80 () in
  H.step
    h
    (Hello { client_id = "client-1"; namespace = Some "s"; user = Some "s" });
  H.keys h "/setusr";
  H.enter h;
  [%expect {| (Rpc (method_ list_users) (params ()) (tag Users_list)) |}];
  H.reply ~quiet:true h Users_list {|["a","s"]|};
  H.reply ~quiet:true h Users_list {|{"oops":1}|};
  H.show h;
  H.keys h "/setusr a";
  H.enter h;
  [%expect
    {|
    ▌ earlier question
      earlier answer
      protocol error: expected array, got {"oops":1}
    ┌─ Users ──────────────────────────────────────────────────────────────────────┐
    │ users (/setusr NAME to act as one):                                          │
    │   a                                                                          │
    │   s                                                                          │
    └───────────────────────────────────────────────────────────────── Esc closes ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ↑↓ scroll · Esc closes · typing goes to the editor
    (Rpc (method_ set_user) (params ((user a))) (tag User_switched))
    |}];
  H.reply
    h
    User_switched
    {|{"client_id":"client-2","namespace":"a","user":"s"}|};
  H.reply ~quiet:true h Initial_state (state_json ());
  H.show h;
  print_s [%sexp (App.Model.acting_as h.model : string option)];
  [%expect {|
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
    (Rpc (method_ get_config) (params ()) (tag Config))








      prigh in /work · /help · Esc aborts · Ctrl+C twice quits
    ──────────────────────────────────────────────────────────────── acting as a ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  user:s as a
    (a)
    |}];
  (* A reconnect asks to act as a again. *)
  H.step h Backend_closed;
  [%expect {|
    (Reconnect
      (generation 1)
      (delay_ms   0)
      (session (/home/u/.prigh/sessions/1.jsonl))
      (as_user (a)))
    |}];
  H.reply
    ~quiet:true
    h
    (Reconnect 1)
    {|{"client_id":"client-3","namespace":"a","user":"s"}|};
  H.keys h "/setusr s";
  H.enter h;
  H.reply
    ~quiet:true
    h
    User_switched
    {|{"client_id":"client-4","namespace":"s","user":"s"}|};
  print_s [%sexp (App.Model.acting_as h.model : string option)];
  H.keys h "/setusr a b";
  H.enter h;
  H.show h;
  [%expect {|
    (Rpc (method_ set_user) (params ((user s))) (tag User_switched))
    ()









    ────────────────────────────────────────────────────── usage: /setusr [user] ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  user:s
    |}]
;;

let custom_auth_json =
  {|[{"provider":"deepseek","name":"DeepSeek","methods":[{"method":"api_key","label":"API key"}],"configured":{"method":"api_key","source":"auth.json"},"expires_ms":null},{"provider":"ollama","name":"ollama","methods":[{"method":"api_key","label":"ollama API key"}],"configured":{"method":"api_key","source":"no key"},"expires_ms":null,"custom":{"base_url":"http://localhost:11434/v1","api":"chat","api_label":"OpenAI chat completions (/chat/completions)"}}]|}
;;

let%expect_test
    "custom provider: /login picker, prefilled text prompts, empty key, the \
     new models"
  =
  let h = connected ~height:14 () in
  H.reply ~quiet:true h Auth_refresh custom_auth_json;
  H.keys h "/login";
  H.esc h;
  H.enter h;
  H.reply h Auth_login_picker custom_auth_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_login_picker))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Log in to  (4)
    / ▏
    ▸* DeepSeek (api_key)              API key  logged in via a…
       ollama (edit)                   custom: http://localhost…
       custom (add an OpenAI-compatible endpoint)  aiproxy, Lit…
      ── Git hosting ──
       Forgejo: another host…          Forgejo or Gitea on any …
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.keys h "custom";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ login)
      (params (
        (provider custom)
        (method   api_key)))
      (tag Show_error))
    |}];
  H.event
    h
    (Auth
       (Prompt
          { id = "p1"
          ; prompt =
              Text
                { message =
                    "\"AI Proxy\" is not a valid name: use lowercase letters\n\
                     Name of the provider"
                ; placeholder = "aiproxy"
                ; default = "AI Proxy"
                }
          }));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Log in ─────────────────────────────────────────────────┐
    │ "AI Proxy" is not a valid name: use lowercase letters    │
    │ Name of the provider                                     │
    │ e.g. aiproxy                                             │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? AI Proxy▏
    deepseek-flash  $0.01  login: Enter answers · Esc cancels
    |}];
  (* The prefilled answer is edited, not retyped. *)
  H.key h (Key.ctrl 'a');
  H.key h (Key.ctrl 'k');
  H.keys h "aiproxy";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ auth_respond)
      (params (
        (id    p1)
        (value aiproxy)))
      (tag Show_error))
    |}];
  H.event
    h
    (Auth
       (Prompt
          { id = "p2"
          ; prompt =
              Text
                { message = "Base URL of aiproxy's API"
                ; placeholder = "http://localhost:3000/v1"
                ; default = "http://gpu:3000/v1"
                }
          }));
  H.show h;
  H.enter h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Log in ─────────────────────────────────────────────────┐
    │ Base URL of aiproxy's API                                │
    │ e.g. http://localhost:3000/v1                            │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? http://gpu:3000/v1▏
    deepseek-flash  $0.01  login: Enter answers · Esc cancels
    (Rpc
      (method_ auth_respond)
      (params (
        (id    p2)
        (value http://gpu:3000/v1)))
      (tag Show_error))
    |}];
  H.event
    h
    (Auth
       (Prompt
          { id = "p3"
          ; prompt =
              Secret
                { message =
                    "API key for aiproxy (leave empty if the server needs none)"
                ; allow_empty = true
                }
          }));
  H.show h;
  H.enter h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Log in ─────────────────────────────────────────────────┐
    │ API key for aiproxy (leave empty if the server needs     │
    │ none)                                                    │
    │ (Enter with nothing typed for none)                      │
    └──────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────
    ? ▏
    deepseek-flash  $0.01  login: Enter answers · Esc cancels
    (Rpc
      (method_ auth_respond)
      (params (
        (id    p3)
        (value "")))
      (tag Show_error))
    |}];
  H.event h (Auth (Progress "Found 1 models: gpt-4o"));
  H.event h (Auth (Done { provider = "aiproxy"; method_ = "api_key" }));
  H.reply
    ~quiet:true
    h
    Auth_refresh
    (String.substr_replace_all
       custom_auth_json
       ~pattern:"ollama"
       ~with_:"aiproxy");
  H.reply
    h
    (Models_after_login "aiproxy")
    {|[{"id":"gpt-4o","provider":"aiproxy","key":"aiproxy/gpt-4o","name":"gpt-4o","context_window":128000,"max_output":16384,"supports_thinking":false,"cost":{"input":0,"output":0,"cache_read":0}}]|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    (Rpc (method_ list_models) (params ()) (tag (Models_after_login aiproxy)))
    (Rpc
      (method_ set_model)
      (params ((model aiproxy/gpt-4o)))
      (tag (Set_model_done aiproxy/gpt-4o)))







      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────── model set to aiproxy/gpt-4o; /model to change ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/model";
  H.esc h;
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Model  (1)
    / ▏
    ▸  gpt-4o  aiproxy/gpt-4o  ctx 128k  price unknown
    ────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}]
;;

let%expect_test
    "custom provider: /logout asks the backend, not y/n; /auth shows the \
     endpoint"
  =
  let h = connected ~width:90 () in
  H.reply ~quiet:true h Auth_refresh custom_auth_json;
  H.keys h "/logout ollama";
  H.enter h;
  H.mode h;
  [%expect
    {|
    (Rpc (method_ logout) (params ((provider ollama))) (tag Show_error))
    editing
    |}];
  H.event
    h
    (Auth
       (Prompt
          { id = "p1"
          ; prompt =
              Select
                { message = "Log out of ollama (http://localhost:11434/v1)"
                ; options =
                    [ "all", "Remove the provider from config.json"
                    ; "keep", "Keep it"
                    ]
                }
          }));
  H.enter h;
  H.event h (Auth (Logged_out "ollama"));
  [%expect
    {|
    (Rpc
      (method_ auth_respond)
      (params (
        (id    p1)
        (value all)))
      (tag Show_error))
    (Rpc (method_ auth_status) (params ()) (tag Auth_refresh))
    |}];
  H.keys h "/auth";
  H.enter h;
  H.reply h Auth_show custom_auth_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ auth_status) (params ()) (tag Auth_show))
    (Rpc (method_ forge_status) (params ()) (tag Forge_refresh))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Logins ───────────────────────────────────────────────────────────────────────────────┐
    │ deepseek  logged in via auth.json  [api_key (API key)]                                 │
    │ ollama    custom http://localhost:11434/v1 (chat)  no key                              │
    │ /login custom adds an OpenAI-compatible endpoint                                       │
    └─────────────────────────────────────────────────────────────────────────── Esc closes ─┘
    ──────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  ↑↓ scroll · Esc closes · typing goes to the editor
    |}]
;;

(* ---- status line, banner, errors and queue: the UX audit ------------- *)

let%expect_test
    "banner: where prigh runs (~ for home, the session name) and the keys that \
     matter, quietly"
  =
  let h = connected ~width:80 () in
  H.step ~quiet:true h (Set_home "/home/u");
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json ~cwd:"/home/u/proj" ~session_name:"refactor" ());
  (* Only the banners and the status line: the transcript has its own tests. *)
  List.iter (Render.screen h.model).lines ~f:(fun line ->
    let plain = Content.Line.to_plain line in
    if
      String.is_prefix plain ~prefix:"prigh in"
      || String.is_prefix plain ~prefix:"~/proj"
    then print_endline (Content.to_styled [ line ]));
  [%expect {| [gray]~/proj "refactor"[/]  [gray]deepseek-flash[/]  [gray]think:off[/]  [gray]mode:auto[/]  [gray]ctx:0.1%/1.0M[/]  [gray]$0.01[/] |}]
;;

let%expect_test
    "status line: gray; yellow marks modes, running work and warm context; red \
     marks full context and a lost backend"
  =
  let model = model_json ~context_window:200_000 "m" "M" in
  let h = connected ~width:100 ~model () in
  let status () = print_endline (Content.to_styled [ Render.status h.model ]) in
  status ();
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json ~model ~context_tokens:120_000 ());
  status ();
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json ~model ~context_tokens:170_000 ());
  status ();
  H.event h (State (state ~model ~running:true ~context_tokens:40 ()));
  status ();
  H.key h (Key.ctrl 't');
  H.key h (Key.ctrl 'l');
  status ();
  H.esc h;
  H.step ~quiet:true h Backend_closed;
  status ();
  [%expect
    {|
    [gray]/work[/]  [gray]m[/]  [gray]think:off[/]  [gray]mode:auto[/]  [gray]view:normal[/]  [gray]ctx:0.8%/200k[/]  [gray]$0.01[/]
    [gray]/work[/]  [gray]m[/]  [gray]think:off[/]  [gray]mode:auto[/]  [gray]view:normal[/]  [yellow]ctx:60%/200k[/]  [gray]$0.01[/]
    [gray]/work[/]  [gray]m[/]  [gray]think:off[/]  [gray]mode:auto[/]  [gray]view:normal[/]  [red]ctx:85%/200k[/]  [gray]$0.01[/]
    [gray]/work[/]  [gray]m[/]  [gray]think:off[/]  [gray]mode:auto[/]  [gray]ctx:0.0%/200k[/]  [gray]$0.01[/]  [yellow]⠋ working · Esc aborts · Enter steers[/]
    (Rpc (method_ set_thinking) (params ((thinking low))) (tag Show_error))
    (Rpc (method_ list_models) (params ()) (tag (Models_for_picker "")))
    [gray]/work[/]  [gray]m[/]  [gray]think:low[/]  [gray]mode:auto[/]  [gray]ctx:0.0%/200k[/]  [gray]$0.01[/]  [yellow]⠋ working · Esc aborts · Enter steers[/]
    (Rpc (method_ abort) (params ()) (tag Abort_done))
    [gray]/work[/]  [gray]m[/]  [gray]think:low[/]  [gray]mode:auto[/]  [gray]view:normal[/]  [gray]ctx:0.0%/200k[/]  [gray]$0.01[/]  [red]backend gone · retry 1 in 0s[/]
    |}];
  (* Without a known window, the tokens are shown instead. *)
  let model = model_json ~context_window:0 "m" "M" in
  H.reply
    ~quiet:true
    h
    Initial_state
    (state_json ~model ~context_tokens:1500 ());
  print_endline (Content.Line.to_plain (Render.status h.model));
  [%expect
    {| /work  m  think:off  mode:auto  view:normal  ctx:1.5k  $0.01  backend gone · retry 1 in 0s |}]
;;

let%expect_test "a mode hint that does not fit is cut short, not hidden" =
  let h = connected ~width:50 () in
  H.event h (State (state ~running:true ()));
  print_endline (Content.Line.to_plain (Render.status h.model));
  [%expect {| deepseek-flash  ctx:0.1%/1.0M  ⠋ working |}]
;;

let%expect_test
    "unknown commands go back into the editor as typed, naming the closest \
     command"
  =
  let h = connected ~width:80 () in
  H.keys h "/modle gpt";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────── unknown command /modle (back in the editor); did you mean /mode? ───
    > /modle gpt▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  (* Nothing close: the text is kept, so a long prompt starting with / is not
     lost. *)
  H.keys h "/usr/bin/env is missing on the box";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─ unknown command /usr/bin/env (back in the editor); Tab or / lists commands ───
    > /usr/bin/env is missing on the box▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  H.keys h "/help thnking";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───── unknown command /thnking (back in the editor); did you mean /thinking? ───
    > /help thnking▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/switch and /cd failures put the command back, with what to do next"
  =
  let h = connected ~width:80 () in
  H.keys h "/switch nope";
  H.esc h;
  H.enter h;
  H.reply_error
    h
    (Retry_in_editor
       { command = "/switch nope"
       ; hint = "/sessions lists them, or Tab after /switch completes"
       ; then_ = Reload_messages
       })
    {|no session "nope"|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions_cache))
    (Rpc
      (method_ switch_session)
      (params ((path nope)))
      (tag (
        Retry_in_editor
        (command "/switch nope")
        (hint "/sessions lists them, or Tab after /switch completes")
        (then_ Reload_messages))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──── no session "nope"; /sessions lists them, or Tab after /switch completes ───
    > /switch nope▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'u');
  H.keys h "/cd /nope";
  H.esc h;
  H.enter h;
  H.reply_error
    h
    (Retry_in_editor
       { command = "/cd /nope"
       ; hint = "Tab after /cd completes directories"
       ; then_ = Notice_on_success "cwd changed"
       })
    "not a directory: /nope";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_dirs)
      (params ((prefix "")))
      (tag (Dirs_for_autocomplete "")))
    (Rpc
      (method_ list_dirs)
      (params ((prefix /)))
      (tag (Dirs_for_autocomplete /)))
    (Rpc
      (method_ list_dirs)
      (params ((prefix /n)))
      (tag (Dirs_for_autocomplete /n)))
    (Rpc
      (method_ list_dirs)
      (params ((prefix /no)))
      (tag (Dirs_for_autocomplete /no)))
    (Rpc
      (method_ list_dirs)
      (params ((prefix /nop)))
      (tag (Dirs_for_autocomplete /nop)))
    (Rpc
      (method_ list_dirs)
      (params ((prefix /nope)))
      (tag (Dirs_for_autocomplete /nope)))
    (Rpc
      (method_ set_cwd)
      (params (
        (path       /nope)
        (if_missing ask)))
      (tag (
        Retry_in_editor
        (command "/cd /nope")
        (hint    "Tab after /cd completes directories")
        (then_ (Dir_changed Cwd)))))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────── not a directory: /nope; Tab after /cd completes directories ───
    > /cd /nope▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  (* Success behaves as the wrapped tag. *)
  H.key h (Key.ctrl 'u');
  H.reply
    h
    (Retry_in_editor
       { command = "/cd /tmp"
       ; hint = ""
       ; then_ = Notice_on_success "cwd changed"
       })
    "{}";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────── cwd changed ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "a shell command during a turn stays in the editor" =
  let h = connected ~width:80 () in
  H.event h (State (state ~running:true ()));
  H.keys h "!make test";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Append_history "!make test")

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Warning ────────────────────────────────────────────────────────────────────┐
    │ !make test waits for the turn to end (kept in the editor); !&make test runs  │
    │ it in the background now                                                     │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > !make test▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  ⠋ working · Esc aborts · Enter steers
    |}]
;;

let%expect_test "/agents cancel: errors say what exists" =
  let h = connected ~width:80 () in
  H.keys h "/agents cancel 3";
  H.enter h;
  H.event
    h
    (Subagent_start
       { call_id = "c1"; agent_id = "c1"; task = "t"; model = "m"; tools = [] });
  H.keys h "/agents cancel 3";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────── no subagent 3; /agents lists them (1–1), Ctrl+D there cancels ───
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  agents:[main] 1⠋ bg
    |}]
;;

let%expect_test
    "queued messages: kind per line, oldest first, gone once delivered; more \
     than three are counted"
  =
  let h = connected ~width:80 ~height:16 () in
  H.event h (State (state ~running:true ()));
  H.step ~quiet:true h (Key (Key.char 'a'));
  H.enter h;
  List.iter [ "b"; "c"; "d" ] ~f:(fun text ->
    H.keys h text;
    H.key h (Key.alt Enter));
  H.event h (Queue_update { steer = [ "a" ]; follow_up = [ "b"; "c"; "d" ] });
  H.show h;
  [%expect
    {|
    (Rpc (method_ steer) (params ((text a))) (tag Show_error))
    (Append_history a)
    (Rpc (method_ follow_up) (params ((text b))) (tag Show_error))
    (Append_history b)
    (Rpc (method_ follow_up) (params ((text c))) (tag Show_error))
    (Append_history c)
    (Rpc (method_ follow_up) (params ((text d))) (tag Show_error))
    (Append_history d)




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    queued (4) · Alt+Up: edit the last · /queue: edit or remove
      steer      a
      follow-up  b
      follow-up  c
      +1 more
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  queued:4  ⠋ working · Esc aborts · Enter steers
    |}];
  (* The steer is delivered: it shows in the transcript and leaves the block. *)
  H.event h (Message_start (user "a"));
  H.event h (Queue_update { steer = []; follow_up = [ "b"; "c"; "d" ] });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ a
    queued (3) · Alt+Up: edit the last · /queue: edit or remove
      follow-up  b
      follow-up  c
      follow-up  d
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  queued:3  ⠋ working · Esc aborts · Enter steers
    |}];
  (* Alt+Up pops the newest. *)
  H.key h (Key.alt Key.Code.Up);
  H.event h (Queue_update { steer = []; follow_up = [ "b"; "c" ] });
  H.reply h Dequeued {|{"text":"d","attachments":[]}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ dequeue) (params ()) (tag Dequeued))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ a
    queued (2) · Alt+Up: edit the last · /queue: edit or remove
      follow-up  b
      follow-up  c
    ────────────────────────────────────────────────────────────────────────────────
    > d▏
    deepseek-flash  ctx:0.1%/1.0M  queued:2  ⠋ working · Esc aborts · Enter steers
    |}];
  (* Switching sessions forgets the old session's queue. *)
  H.reply ~quiet:true h Reload_messages "{}";
  print_s [%sexp (h.model.queued_messages : Queued_message.t list)];
  [%expect {| () |}]
;;

let%expect_test
    "/queue: any queued message, Enter edits it, Ctrl+D removes it; queued \
     elsewhere too"
  =
  let h = connected ~width:80 ~height:16 () in
  run h "/queue";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────────────────────────────────────── nothing queued ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.event h (State (state ~running:true ()));
  (* Queued by another frontend: listed all the same. *)
  H.event
    h
    (Queue_update
       { steer = [ "steer me" ]; follow_up = [ "first"; "second\nline" ] });
  run h "/queue";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Queued  (3)
    / ▏
    ▸  steer me     steers the current turn
       first        follows up after the turn
       second line  follows up after the turn
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  ctx:0.1%/1.0M  $0.01  Enter edits · Esc closes · Ctrl+D removes
    |}];
  (* Ctrl+D removes the highlighted one. *)
  H.key h (Key.plain Down);
  H.key h (Key.ctrl 'd');
  [%expect
    {|
    (Rpc
      (method_ remove_queued)
      (params (
        (queue follow_up)
        (index 0)
        (text  first)))
      (tag (Notice_on_success "removed from the queue: first")))
    |}];
  H.event
    h
    (Queue_update { steer = [ "steer me" ]; follow_up = [ "second\nline" ] });
  H.reply h (Notice_on_success "removed from the queue: first") "{}";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    queued (2) · Alt+Up: edit the last · /queue: edit or remove
      steer      steer me
      follow-up  second line
    ────────────────────────────────────────────── removed from the queue: first ───
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  queued:2  ⠋ working · Esc aborts · Enter steers
    |}];
  (* Enter pulls one back into the editor. *)
  run h "/queue";
  H.key h (Key.plain Down);
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ remove_queued)
      (params (
        (queue follow_up)
        (index 0)
        (text  "second\nline")))
      (tag Dequeued))
    |}];
  H.event h (Queue_update { steer = [ "steer me" ]; follow_up = [] });
  H.reply h Dequeued {|{"text":"second\nline","attachments":[]}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    queued (1) · Alt+Up: edit the last · /queue: edit or remove
      steer      steer me
    ────────────────────────────────────────────────────────────────────────────────
    > second
      line▏
    deepseek-flash  ctx:0.1%/1.0M  queued:1  ⠋ working · Esc aborts · Enter steers
    |}];
  (* Delivered or removed meanwhile: the backend says so. *)
  let h = connected ~width:80 ~height:16 () in
  H.event h (State (state ~running:true ()));
  H.event h (Queue_update { steer = [ "steer me" ]; follow_up = [] });
  run h "/queue";
  H.key h (Key.ctrl 'd');
  H.event h (Queue_update { steer = []; follow_up = [] });
  H.reply_error
    h
    (Notice_on_success "removed from the queue: steer me")
    "that message is no longer queued: it was delivered or removed already";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ remove_queued)
      (params (
        (queue steer)
        (index 0)
        (text  "steer me")))
      (tag (Notice_on_success "removed from the queue: steer me")))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────── that message is no longer queued: it was delivered or removed already ───
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  ⠋ working · Esc aborts · Enter steers
    |}]
;;

let%expect_test
    "what the agent is doing: waiting, thinking without text, a retry and its \
     attempt, writing, tools"
  =
  let h = connected ~width:100 ~height:8 () in
  let retry : P.Retry.t =
    { attempt = 2
    ; max_attempts = 6
    ; error = "overloaded_error: Overloaded"
    ; delay_ms = 2000
    }
  in
  H.event h (State (state ~running:true ()));
  H.event h Agent_start;
  H.event h (Message_start (user "go"));
  H.event h Turn_start;
  H.event h (Message_start (Assistant partial));
  H.show h;
  (* Newer models' thinking has no text to show: a block starts, nothing
     more comes until it ends. *)
  H.event h (Message_update { partial; delta = Thinking_delta "" });
  H.show h;
  (* What the failed attempt streamed is dropped for the retry's notice. *)
  H.event h (Message_update { partial; delta = Text_delta "half a rep" });
  H.event h (Retry retry);
  H.show h;
  H.event h (Message_start (Assistant partial));
  H.show h;
  H.event h (Message_update { partial; delta = Text_delta "A reply." });
  H.show h;
  H.event h (Tool_start (tool_call "c1"));
  H.show h;
  (* Seen running after a reconnect: nothing known but that. *)
  H.event h (State (state ()));
  H.event h (State (state ~running:true ()));
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ go
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ⠋ waiting for the model · Esc aborts · Enter steers

    ▌ earlier question
      earlier answer

    ▌ go
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ thinking · Esc aborts · Enter steers
    ▌ earlier question
      earlier answer

    ▌ go
      retrying in 2s (attempt 2/6): overloaded_error: Overloaded
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  retry:2/6  ⠋ retrying in 2s · Esc aborts · Enter steers
    ▌ earlier question
      earlier answer

    ▌ go
      retrying in 2s (attempt 2/6): overloaded_error: Overloaded
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  $0.01  retry:2/6  ⠋ waiting for the model (attempt 2/6) · Esc aborts · Enter steers
      earlier answer

    ▌ go
      retrying in 2s (attempt 2/6): overloaded_error: Overloaded
      A reply.
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ writing · Esc aborts · Enter steers

    ▌ go
      retrying in 2s (attempt 2/6): overloaded_error: Overloaded
      A reply.
    … bash
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  ⠋ running tools · Esc aborts · Enter steers

    ▌ go
      retrying in 2s (attempt 2/6): overloaded_error: Overloaded
      A reply.
    … bash
    ────────────────────────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01  ⠋ working · Esc aborts · Enter steers
    |}]
;;
