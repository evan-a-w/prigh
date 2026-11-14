open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H
module P = Prigh_protocol

(* /mcp and the MCP servers in the user's settings (Settings → MCP servers in
   prigh-web): add, remove, enable, disable; /mcp lists them with the host's. *)

(* As typed into an empty editor (a failed command is put back in it). *)
let run h text =
  H.step ~quiet:true h (Key (Key.ctrl 'u'));
  String.iter text ~f:(fun c -> H.step ~quiet:true h (Key (Key.char c)));
  if Option.is_some h.H.model.autocomplete then H.esc h;
  H.enter h
;;

let retry command then_ : App.Reply_tag.t =
  Retry_in_editor { command; hint = ""; then_ }
;;

let settings_json =
  {|{"servers":[{"name":"web","enabled":true,"config":{"type":"http","url":"https://example.com/mcp","headers":{"Authorization":"Bearer secret-token"}}},{"name":"off","enabled":false,"config":{"command":"npx","args":["-y","off-server"]}},{"name":"fs","enabled":true,"config":{"command":"fs-mcp"}}]}|}
;;

let list_json =
  {|{"servers":[{"name":"web","source":"settings","project":false,"status":"ready","tools":[{"name":"mcp__web__fetch","description":"Fetch a page"}]},{"name":"fs","source":"/home/u/.prigh/mcp.json","project":false,"status":"ready","tools":[]}],"problems":[]}|}
;;

let%expect_test "/mcp lists settings servers, disabled and overridden ones too" =
  let h = Test_app.connected ~width:80 ~height:16 () in
  run h "/mcp";
  H.reply h Mcp_settings_for_picker settings_json;
  [%expect
    {|
    (Rpc (method_ get_mcp_settings) (params ()) (tag Mcp_settings_for_picker))
    (Rpc
      (method_ list_mcp)
      (params ())
      (tag (
        Mcp_picker (
          ((name    web)
           (enabled true)
           (transport (
             Http
             (url https://example.com/mcp)
             (headers ((Authorization <hidden>))))))
          ((name    off)
           (enabled false)
           (transport (Stdio (command npx) (args (-y off-server)) (env ()))))
          ((name    fs)
           (enabled true)
           (transport (
             Stdio
             (command fs-mcp)
             (args ())
             (env  ()))))))))
    |}];
  let settings =
    Or_error.ok_exn
      (P.Mcp_settings.of_json (Or_error.ok_exn (P.Json.parse settings_json)))
  in
  H.reply ~quiet:true h (Mcp_picker settings) list_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    MCP servers  (4)
    / ▏
    ▸  web  ready, 1 tool  Settings
       fs   ready, 0 tools  /home/u/.prigh/mcp.json
       off  disabled  Settings
       fs   overridden by /home/u/.prigh/mcp.json  Settings
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  $0.01  Enter approves a server or lists its tools · Esc closes
    |}];
  H.keys h "off";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ───────────────────────────────── off is disabled; /mcp enable off starts it ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/mcp";
  H.reply ~quiet:true h Mcp_settings_for_picker settings_json;
  H.reply ~quiet:true h (Mcp_picker settings) list_json;
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.key h (Key.plain Down);
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_mcp_settings) (params ()) (tag Mcp_settings_for_picker))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ the fs in your settings is not used: /home/u/.prigh/mcp.json defines fs too, │
    │ and the closest definition wins; rename one of them to use both              │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "an older backend: /mcp falls back to the host's servers" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/mcp";
  H.reply_error h Mcp_settings_for_picker "unknown method get_mcp_settings";
  [%expect
    {|
    (Rpc (method_ get_mcp_settings) (params ()) (tag Mcp_settings_for_picker))
    (Rpc (method_ list_mcp) (params ()) (tag (Mcp_picker ())))
    |}]
;;

let%expect_test "/mcp add, remove, enable, disable change the settings" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/mcp add docs https://example.com/mcp";
  [%expect
    {|
    (Rpc
      (method_ add_mcp_servers)
      (params ((
        servers ((
          docs (
            (type http)
            (url  https://example.com/mcp)))))))
      (tag (
        Retry_in_editor
        (command "/mcp add docs https://example.com/mcp")
        (hint    "")
        (then_ (
          Notice_on_success
          "added MCP server docs (https://example.com/mcp) to your settings")))))
    |}];
  run h {|/mcp add fs npx -y "@scope/server fs" '/my dir'|};
  [%expect
    {|
    (Rpc
      (method_ add_mcp_servers)
      (params ((
        servers ((fs ((command npx) (args (-y "@scope/server fs" "/my dir"))))))))
      (tag (
        Retry_in_editor
        (command "/mcp add fs npx -y \"@scope/server fs\" '/my dir'")
        (hint "")
        (then_ (
          Notice_on_success
          "added MCP server fs (npx -y '@scope/server fs' '/my dir') to your settings")))))
    |}];
  run
    h
    {|/mcp add {"gh": {"type": "http", "url": "https://api.github.com/mcp"}}|};
  [%expect
    {|
    (Rpc
      (method_ add_mcp_servers)
      (params ((
        text
        "{\"gh\": {\"type\": \"http\", \"url\": \"https://api.github.com/mcp\"}}")))
      (tag (
        Retry_in_editor
        (command
         "/mcp add {\"gh\": {\"type\": \"http\", \"url\": \"https://api.github.com/mcp\"}}")
        (hint "")
        (then_ (Notice_on_success "added the MCP servers to your settings")))))
    |}];
  run h "/mcp remove docs";
  [%expect
    {|
    (Rpc
      (method_ remove_mcp_server)
      (params ((name docs)))
      (tag (
        Retry_in_editor
        (command "/mcp remove docs")
        (hint    "")
        (then_ (Notice_on_success "removed MCP server docs from your settings")))))
    |}];
  run h "/mcp disable fs";
  [%expect
    {|
    (Rpc
      (method_ set_mcp_server_enabled)
      (params (
        (name    fs)
        (enabled false)))
      (tag (
        Retry_in_editor
        (command "/mcp disable fs")
        (hint    "")
        (then_ (Notice_on_success "disabled MCP server fs")))))
    |}];
  run h "/mcp enable fs";
  [%expect
    {|
    (Rpc
      (method_ set_mcp_server_enabled)
      (params (
        (name    fs)
        (enabled true)))
      (tag (
        Retry_in_editor
        (command "/mcp enable fs")
        (hint    "")
        (then_ (Notice_on_success "enabled MCP server fs")))))
    |}];
  H.reply
    h
    (retry "/mcp enable fs" (Notice_on_success "enabled MCP server fs"))
    {|{"servers":[]}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────── enabled MCP server fs ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/mcp: mistakes say what to type; a refusal puts it back" =
  let h = Test_app.connected ~width:80 ~height:14 () in
  run h "/mcp add docs";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ── give a name, then a URL or a command: /mcp add NAME URL|COMMAND [ARGS...] ───
    > /mcp add docs▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h {|/mcp add fs npx "unclosed|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ a double quote (") is not closed; usage: /mcp [reconnect], /mcp add NAME     │
    │ URL|COMMAND [ARGS...], /mcp add {"NAME": {...}} (mcpServers JSON), /mcp      │
    │ remove|enable|disable NAME                                                   │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /mcp add fs npx "unclosed▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/mcp remove";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ remove  NAME: remove a server from your settings
    ──────────────────────────────────────────────────── usage: /mcp remove NAME ───
    > /mcp remove▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    |}];
  run h "/mcp frobnicate";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ usage: /mcp [reconnect], /mcp add NAME URL|COMMAND [ARGS...], /mcp add       │
    │ {"NAME": {...}} (mcpServers JSON), /mcp remove|enable|disable NAME           │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /mcp frobnicate▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.esc h;
  run h "/mcp add web https://example.com/mcp";
  H.reply_error
    h
    (retry
       "/mcp add web https://example.com/mcp"
       (Notice_on_success "added MCP server web"))
    "there is already an MCP server \"web\" in the settings: choose another \
     name, or edit that one";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ add_mcp_servers)
      (params ((
        servers ((
          web (
            (type http)
            (url  https://example.com/mcp)))))))
      (tag (
        Retry_in_editor
        (command "/mcp add web https://example.com/mcp")
        (hint    "")
        (then_ (
          Notice_on_success
          "added MCP server web (https://example.com/mcp) to your settings")))))



      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ there is already an MCP server "web" in the settings: choose another name,   │
    │ or edit that one                                                             │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /mcp add web https://example.com/mcp▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;
