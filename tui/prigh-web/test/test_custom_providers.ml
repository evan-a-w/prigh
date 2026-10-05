open! Core
module H = Harness

let auth_event fields = sprintf {|{"event":"auth",%s}|} fields

let text_prompt ~id ~message ~placeholder ~default =
  auth_event
    (sprintf
       {|"kind":"prompt","id":"%s","prompt":"text","message":%S,"placeholder":"%s","default":"%s"|}
       id
       message
       placeholder
       default)
;;

let name_message =
  "Name of the provider (models are then <name>/<model id>; an existing custom \
   provider's name edits it)"
;;

let base_url_message =
  "Base URL of aiproxy's API (the part before /chat/completions, usually \
   ending in /v1)"
;;

let api_prompt ~id =
  auth_event
    (sprintf
       {|"kind":"prompt","id":"%s","prompt":"select","message":"API style of aiproxy","options":[{"id":"chat","label":"OpenAI chat completions (/chat/completions) - most servers"},{"id":"responses","label":"OpenAI Responses (/responses)"},{"id":"anthropic","label":"Anthropic messages (/messages)"}]|}
       id)
;;

let key_prompt ~id =
  auth_event
    (sprintf
       {|"kind":"prompt","id":"%s","prompt":"secret","message":"API key for aiproxy (leave empty if the server needs none)","allow_empty":true|}
       id)
;;

let progress message =
  auth_event (sprintf {|"kind":"progress","message":%S|} message)
;;

let custom_status ?(key = {|{"method":"api_key","source":"no key"}|}) () =
  sprintf
    {|{"provider":"aiproxy","name":"aiproxy","methods":[{"method":"api_key","label":"aiproxy API key"}],"configured":%s,"expires_ms":null,"custom":{"base_url":"http://localhost:3000/v1","api":"chat","api_label":"OpenAI chat completions (/chat/completions)"}}|}
    key
;;

(* [H.auth_json] with a custom provider after the built-in ones. *)
let auth_with_custom ?key () =
  String.chop_suffix_exn (String.strip H.auth_json) ~suffix:"]"
  ^ ",\n"
  ^ custom_status ?key ()
  ^ "]"
;;

let custom_model id =
  sprintf
    {|{"id":"%s","provider":"aiproxy","key":"aiproxy/%s","name":"%s","context_window":128000,"max_output":16384,"supports_thinking":false,"cost":{"input":0,"output":0,"cache_read":0}}|}
    id
    id
    id
;;

let models_with_custom =
  String.chop_suffix_exn (String.strip H.models_json) ~suffix:"]"
  ^ ","
  ^ custom_model "gpt-4o"
  ^ ","
  ^ custom_model "openai/gpt-4o-mini"
  ^ "]"
;;

let%expect_test "/login custom: name, base URL, API style, key; then /model" =
  let h = H.create () in
  H.type_ h "/login";
  H.act h Send;
  H.reply h "auth_status" H.auth_json;
  H.act h (Picker_query "custom");
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Save_history (/login))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Login_picker)))
    (Focus picker-input)
    Custom provider add an OpenAI-compatible endpoint (aiproxy, LiteLLM, OpenRouter, vLLM, Ollama…)
    |}];
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".modal";
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ login) (params ((provider custom) (method api_key)))
     (tag Login_started))
    Add a custom provider
    (Close (Esc))
    Waiting for the provider…
    (Cancel)
    |}];
  (* The first prompt may come before the reply. *)
  H.event
    h
    (text_prompt
       ~id:"p1"
       ~message:name_message
       ~placeholder:"aiproxy"
       ~default:"");
  H.reply h "login" "{}";
  H.text h ~selector:".modal";
  H.show h ~selector:"#dialog-input";
  [%expect
    {|
    (Focus dialog-input)
    Add a custom provider
    (Close (Esc))
    Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)
    []
    (Cancel) (Continue)
    <input id="dialog-input"
           type="text"
           placeholder="aiproxy"
           autocomplete="off"
           spellcheck="false"
           class="text-input"
           #value=""
           @on_input/>
    |}];
  H.act h (Dialog_input "AI Proxy");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ auth_respond) (params ((id p1) (value "AI Proxy")))
     (tag Show_error))
    |}];
  (* A bad answer is asked again with the error, prefilled. *)
  H.event
    h
    (text_prompt
       ~id:"p2"
       ~message:
         ("name \"AI Proxy\" must be lowercase letters, digits, - and _\n"
          ^ name_message)
       ~placeholder:"aiproxy"
       ~default:"AI Proxy");
  H.text h ~selector:".login-prompt";
  [%expect
    {|
    (Focus dialog-input)
    name "AI Proxy" must be lowercase letters, digits, - and _
    Name of the provider (models are then <name>/<model id>; an existing custom provider's name edits it)
    [AI Proxy]
    |}];
  H.act h (Dialog_input "aiproxy");
  H.key h "Enter" ~target:Field;
  H.event
    h
    (text_prompt
       ~id:"p3"
       ~message:base_url_message
       ~placeholder:"http://localhost:3000/v1"
       ~default:"");
  H.act h (Dialog_input "http://localhost:3000/v1/");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ auth_respond) (params ((id p2) (value aiproxy)))
     (tag Show_error))
    (Focus dialog-input)
    Dialog_accept
    (Rpc (method_ auth_respond)
     (params ((id p3) (value http://localhost:3000/v1/))) (tag Show_error))
    |}];
  H.event
    h
    (progress
       "Using http://localhost:3000/v1 (the endpoint paths are added per \
        request)");
  H.event h (api_prompt ~id:"p4");
  H.text h ~selector:".modal-body";
  [%expect
    {|
    Using http://localhost:3000/v1 (the endpoint paths are added per request)
    API style of aiproxy
    OpenAI chat completions (/chat/completions) - most servers
    OpenAI Responses (/responses)
    Anthropic messages (/messages)
    |}];
  H.key h "Enter" ~target:Page;
  H.event h (key_prompt ~id:"p5");
  H.text h ~selector:".login-prompt";
  H.show h ~selector:"#dialog-input";
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ auth_respond) (params ((id p4) (value chat))) (tag Show_error))
    (Focus dialog-input)
    API key for aiproxy (leave empty if the server needs none)
    []
    <input id="dialog-input"
           type="password"
           placeholder=""
           autocomplete="off"
           spellcheck="false"
           class="text-input"
           #value=""
           @on_input/>
    |}];
  (* This secret may be empty. *)
  H.key h "Enter" ~target:Field;
  H.event h (progress "Checking http://localhost:3000/v1/models ...");
  H.event h (progress "Found 2 models: gpt-4o, openai/gpt-4o-mini");
  H.text h ~selector:".modal";
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ auth_respond) (params ((id p5) (value ""))) (tag Show_error))
    Add a custom provider
    (Close (Esc))
    Using http://localhost:3000/v1 (the endpoint paths are added per request)
    Checking http://localhost:3000/v1/models ...
    Found 2 models: gpt-4o, openai/gpt-4o-mini
    Waiting for the provider…
    (Cancel)
    |}];
  H.event
    h
    (auth_event {|"kind":"done","provider":"aiproxy","method":"api_key"|});
  H.text h ~selector:".modal";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ list_models) (params ()) (tag Models))
    Saved aiproxy: /model lists its models as aiproxy/<id>
    |}];
  H.reply h "auth_status" (auth_with_custom ());
  H.reply h "list_models" models_with_custom;
  H.act h (Dismiss_toast 0);
  H.type_ h "/model";
  H.act h Send;
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Save_history (/model /login))
    (Focus picker-input)
    Claude Opus 5.5 anthropic · 200k ctx ✓
    Claude Sonnet 5 anthropic · 200k ctx
    gpt-4o aiproxy · 128k ctx
    openai/gpt-4o-mini aiproxy · 128k ctx
    GPT-6 openai · 200k ctx · not logged in
    DeepSeek Chat deepseek · 200k ctx · not logged in
    |}];
  H.act h (Picker_query "mini");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_model) (params ((model aiproxy/openai/gpt-4o-mini)))
     (tag Show_error))
    |}]
;;

let%expect_test
    "/login custom when the models can't be listed: save, retry or cancel"
  =
  let h = H.create () in
  H.type_ h "/login custom";
  H.act h Send;
  H.reply h "login" "{}";
  H.event
    h
    (auth_event
       {|"kind":"prompt","id":"p6","prompt":"select","message":"Could not list aiproxy's models: connection refused\nCheck the base URL (it usually ends in /v1), the API key, and that the server is running.","options":[{"id":"save","label":"Save anyway"},{"id":"edit","label":"Change the settings"},{"id":"cancel","label":"Cancel (nothing is saved)"}]|});
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history ("/login custom"))
    (Focus dialog)
    (Rpc (method_ login) (params ((provider custom))) (tag Login_started))
    Add a custom provider
    (Close (Esc))
    Could not list aiproxy's models: connection refused
    Check the base URL (it usually ends in /v1), the API key, and that the server is running.
    Save anyway
    Change the settings
    Cancel (nothing is saved)
    (Cancel) (Continue)
    |}];
  (* Esc cancels the flow; its failure is expected, so no toast. *)
  H.key h "Escape" ~target:Page;
  H.event
    h
    (auth_event
       {|"kind":"failed","provider":"custom","error":"login cancelled"|});
  H.text h ~selector:".toasts";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Rpc (method_ auth_cancel) (params ()) (tag Show_error))
    |}]
;;

let%expect_test "custom providers in /auth, /login and /logout: edit and remove"
  =
  let h = H.create () in
  H.type_ h "/auth";
  H.act h Send;
  H.reply h "auth_status" (auth_with_custom ());
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/auth))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Show)))
    (Focus dialog)
    Providers
    (Close (Esc))
    Anthropic logged in with oauth (auth.json)
    (Log out)
    OpenAI not logged in
    (Log in)
    DeepSeek not logged in
    (Log in)
    aiproxy custom · http://localhost:3000/v1 · OpenAI chat completions (/chat/completions) · no key
    (Edit) (Remove)
    /login and /logout pick a provider (Add a custom provider) (Done)
    |}];
  (* Editing asks the same questions, prefilled. *)
  H.act h (Start_login "aiproxy");
  H.event
    h
    (text_prompt
       ~id:"p1"
       ~message:base_url_message
       ~placeholder:"http://localhost:3000/v1"
       ~default:"http://localhost:3000/v1");
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ login) (params ((provider aiproxy))) (tag Login_started))
    (Focus dialog-input)
    Edit aiproxy
    (Close (Esc))
    Base URL of aiproxy's API (the part before /chat/completions, usually ending in /v1)
    [http://localhost:3000/v1]
    (Cancel) (Continue)
    |}];
  H.key h "Enter" ~target:Field;
  H.event
    h
    (auth_event
       {|"kind":"failed","provider":"aiproxy","error":"something broke"|});
  H.text h ~selector:".modal-body";
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ auth_respond)
     (params ((id p1) (value http://localhost:3000/v1))) (tag Show_error))
    Login failed: something broke. /login tries again.
    |}];
  H.act h Close_dialog;
  (* The login picker offers to edit it, or to add another. *)
  H.type_ h "/login";
  H.act h Send;
  H.reply
    h
    "auth_status"
    (auth_with_custom ~key:{|{"method":"api_key","source":"auth.json"}|} ());
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Focus editor)
    (Save_history (/login /auth))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Login_picker)))
    (Focus picker-input)
    Anthropic Claude subscription · logged in via auth.json ✓
    Anthropic API key
    OpenAI API key
    DeepSeek API key
    aiproxy custom · http://localhost:3000/v1 · OpenAI chat completions (/chat/completions) · edit
    Custom provider add an OpenAI-compatible endpoint (aiproxy, LiteLLM, OpenRouter, vLLM, Ollama…)
    |}];
  H.act h Close_dialog;
  H.type_ h "/login ";
  H.text h ~selector:".popup";
  [%expect
    {|
    (Focus editor)
    Providers ↑↓ Tab Enter Esc
    Anthropic logged in
    OpenAI
    DeepSeek
    aiproxy custom · http://localhost:3000/v1
    custom add an OpenAI-compatible endpoint
    |}];
  (* Removing asks what to remove; answered, the dialog closes. *)
  H.type_ h "/logout";
  H.act h Send;
  H.reply
    h
    "auth_status"
    (auth_with_custom ~key:{|{"method":"api_key","source":"auth.json"}|} ());
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Save_history (/logout /login /auth))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Logout_picker)))
    (Focus picker-input)
    Anthropic oauth via auth.json
    aiproxy custom · http://localhost:3000/v1 · OpenAI chat completions (/chat/completions) · key: auth.json
    |}];
  H.act h (Picker_query "aiproxy");
  H.key h "Enter" ~target:Field;
  H.event
    h
    (auth_event
       {|"kind":"prompt","id":"p2","prompt":"select","message":"Log out of aiproxy (http://localhost:3000/v1)","options":[{"id":"key","label":"Remove the API key only (keep the provider)"},{"id":"all","label":"Remove the API key and the provider"}]|});
  H.reply h "logout" "{}";
  H.text h ~selector:".modal";
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    (Rpc (method_ logout) (params ((provider aiproxy))) (tag Login_started))
    Log out of aiproxy
    (Close (Esc))
    Log out of aiproxy (http://localhost:3000/v1)
    Remove the API key only (keep the provider)
    Remove the API key and the provider
    (Cancel) (Continue)
    |}];
  H.act h (Login_choose 1);
  H.event h (auth_event {|"kind":"logged_out","provider":"aiproxy"|});
  H.text h ~selector:".modal";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ auth_respond) (params ((id p2) (value all))) (tag Show_error))
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ list_models) (params ()) (tag Models))
    Logged out of aiproxy
    |}];
  (* Keeping everything ends the flow without an event. *)
  H.reply h "auth_status" (auth_with_custom ());
  H.act h (Logout "aiproxy");
  H.event
    h
    (auth_event
       {|"kind":"prompt","id":"p3","prompt":"select","message":"Log out of aiproxy (http://localhost:3000/v1)","options":[{"id":"all","label":"Remove the provider from config.json"},{"id":"keep","label":"Keep it"}]|});
  H.key h "ArrowDown" ~target:Page;
  H.key h "Enter" ~target:Page;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ logout) (params ((provider aiproxy))) (tag Login_started))
    (Dialog_move 1)
    Dialog_accept
    (Focus editor)
    (Rpc (method_ auth_respond) (params ((id p3) (value keep))) (tag Show_error))
    |}];
  (* A failed logout says so. *)
  H.act h (Logout "aiproxy");
  H.fail h "logout" "unknown provider \"aiproxy\"";
  H.text h ~selector:".modal-body";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ logout) (params ((provider aiproxy))) (tag Login_started))
    Logout failed: unknown provider "aiproxy". /logout tries again.
    |}]
;;
