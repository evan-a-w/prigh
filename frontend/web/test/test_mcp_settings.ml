open! Core
module H = Harness

(* Settings' MCP servers: the user's (kept on the backend, on every host) and
   the session's host's own, from its files. *)

let settings_json =
  {|{"servers":[
     {"name":"github","enabled":true,"config":{"url":"https://api.githubcopilot.com/mcp/","headers":{"Authorization":"Bearer ghp_secret"}}},
     {"name":"fs","enabled":true,"config":{"command":"npx","args":["-y","@modelcontextprotocol/server-filesystem","/home/u"],"env":{"TOKEN":"s3cret"}}},
     {"name":"notes","enabled":false,"config":{"command":"notes-mcp"}}]}|}
;;

let host_json =
  {|{"servers":[
     {"name":"github","source":"settings","project":false,"status":"ready","tools":[
        {"name":"mcp__github__search","description":"Search code"},
        {"name":"mcp__github__issue","description":""}]},
     {"name":"fs","source":"/home/u/.prigh/mcp.json","project":false,"status":"ready","tools":[
        {"name":"mcp__fs__read_file","description":"Read a file"}]},
     {"name":"web","source":"/home/u/.prigh/mcp.json","project":false,"status":"failed","error":"spawn uvx: not found","tools":[]},
     {"name":"db","source":"/work/.mcp.json","project":true,"status":"needs_approval","tools":[]}],
   "problems":["/work/.mcp.json: server \"x\": give a \"command\" (stdio) or a \"url\" (http)"]}|}
;;

let section h = H.text h ~selector:".settings-panel"

let opened ?(settings = settings_json) ?(host = host_json) () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Settings_section Mcp);
    H.reply h "get_mcp_settings" settings;
    H.reply h "list_mcp" host);
  h
;;

let%expect_test "opening asks for both lists; empty, it says what to do" =
  let h = H.create () in
  H.act h (Run "/settings mcp");
  section h;
  H.reply h "get_mcp_settings" {|{"servers":[]}|};
  H.reply h "list_mcp" {|{"servers":[],"problems":[]}|};
  section h;
  [%expect {|
    (Focus dialog)
    (Rpc (method_ get_mcp_settings) (params ()) (tag Mcp_settings_loaded))
    (Rpc (method_ list_mcp) (params ()) (tag Mcp_host_listed))
    MCP servers
    Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    Your servers
    Reading your servers…
    Waiting…
    On the backend From its files: change them there
    Asking the host…
    Waiting…
    MCP servers
    Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    Your servers
    No servers yet
    Add one by the command that starts it or by its URL, or paste the mcpServers JSON from another tool's settings. It runs on every host your sessions use.
    (Add server) (Paste JSON)
    On the backend From its files: change them there
    None from files
    A host's ~/.prigh/mcp.json and a project's .mcp.json add servers on that host only; a server there with the name of one of yours replaces it.
    |}]
;;

let%expect_test "yours with health on this host, overridden, off; the host's below" =
  let h = opened () in
  section h;
  [%expect {|
    MCP servers
    Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    Your servers 3 servers
    github https://api.githubcopilot.com/mcp/
    Ready on the backend · (2 tools)
    (Stop it on every host (/mcp disable github)) (Edit) (Remove (/mcp remove github))
    fs npx -y @modelcontextprotocol/server-filesystem /home/u
    Overridden by /home/u/.prigh/mcp.json on the backend: that one runs instead. Rename one of them to use both.
    (Stop it on every host (/mcp disable fs)) (Edit) (Remove (/mcp remove fs))
    notes notes-mcp
    Off: not started on any host
    (Start it on every host (/mcp enable notes)) (Edit) (Remove (/mcp remove notes))
    (Add server) (Paste JSON)
    On the backend From its files: change them there
    /work/.mcp.json: server "x": give a "command" (stdio) or a "url" (http)
    fs /home/u/.prigh/mcp.json
    Ready · (1 tool)
    web /home/u/.prigh/mcp.json Failed: Spawn uvx: not found
    (Retry)
    db /work/.mcp.json The project asks to run it: approve it to start it
    (Approve)
    |}]
;;

let%expect_test "secrets are password fields until shown" =
  let h = opened () in
  H.act h (Mcp_edit (Some "fs"));
  H.show h ~selector:".mcp-pair-value";
  H.click h ~selector:".mcp-pair-show";
  H.show h ~selector:".mcp-pair-value";
  [%expect {|
    (Focus mcp-name)
    <input type="password"
           aria-label="Environment variable 1: value"
           placeholder="value"
           autocomplete="off"
           autocapitalize="off"
           spellcheck="false"
           class="mcp-pair-value text-input"
           #value="s3cret"
           @on_input/>
    (Mcp_field (Pair_shown 0))
    <input type="text"
           aria-label="Environment variable 1: value"
           placeholder="value"
           autocomplete="off"
           autocapitalize="off"
           spellcheck="false"
           class="mcp-pair-value text-input"
           #value="s3cret"
           @on_input/>
    |}]
;;

let%expect_test "tools show and hide" =
  let h = opened () in
  H.click h ~selector:"[data-server=github] .mcp-tools-toggle";
  H.text h ~selector:".mcp-server[data-server=github]";
  H.click h ~selector:"[data-server=github] .mcp-tools-toggle";
  H.text h ~selector:".mcp-server[data-server=github]";
  [%expect {|
    (Mcp_show_tools github)
    github https://api.githubcopilot.com/mcp/
    Ready on the backend · (2 tools)
    (Stop it on every host (/mcp disable github)) (Edit) (Remove (/mcp remove github))
    mcp__github__search Search code
    mcp__github__issue
    (Mcp_show_tools github)
    github https://api.githubcopilot.com/mcp/
    Ready on the backend · (2 tools)
    (Stop it on every host (/mcp disable github)) (Edit) (Remove (/mcp remove github))
    |}]
;;

let%expect_test "add a command server: the form, Ctrl+Enter saves, the reply closes it" =
  let h = opened () in
  H.click h ~selector:".mcp-add";
  H.text h ~selector:".mcp-draft";
  H.input h ~selector:"#mcp-name" "search";
  H.input h ~selector:"#mcp-command" "uvx";
  H.input h ~selector:"#mcp-args" "mcp-server-search\n--port\n\n8080";
  H.click h ~selector:".mcp-pair-add";
  H.input h ~selector:".mcp-pair input[aria-label='Environment variable 1: name']" "API_KEY";
  H.input h ~selector:".mcp-pair input[aria-label='Environment variable 1: value']" "k-123";
  H.key h ~ctrl:true ~target:Field "Enter";
  H.text h ~selector:".mcp-draft";
  H.reply
    h
    "add_mcp_servers"
    {|{"servers":[{"name":"search","enabled":true,"config":{"command":"uvx","args":["mcp-server-search","--port","8080"],"env":{"API_KEY":"k-123"}}}]}|};
  H.quiet h (fun () ->
    H.reply
      h
      "get_mcp_settings"
      {|{"servers":[{"name":"search","enabled":true,"config":{"command":"uvx","args":["mcp-server-search","--port","8080"],"env":{"API_KEY":"k-123"}}}]}|};
    H.reply
      h
      "list_mcp"
      {|{"servers":[{"name":"search","source":"settings","project":false,"status":"ready","tools":[]}],"problems":[]}|});
  H.text h ~selector:".settings-panel";
  [%expect {|
    (Mcp_edit ())
    (Focus mcp-name)
    New server
    Name
    [] Its tools are called mcp__NAME__TOOL.
    (Command) (URL)
    Command
    []
    Arguments
    [] One per line, as they are: no quoting needed.
    Environment (Add variable) Values are kept on the backend and hidden here until shown; ${VAR} takes VAR from the tool host's environment.
    Ctrl+Enter saves, Esc cancels (Cancel) (Add server)
    (Mcp_field (Name search))
    (Mcp_field (Command uvx))
    (Mcp_field (Args  "mcp-server-search\
                     \n--port\
                     \n\
                     \n8080"))
    (Mcp_field Pair_add)
    (Mcp_field (Pair_key 0 API_KEY))
    (Mcp_field (Pair_value 0 k-123))
    Dialog_accept
    (Rpc (method_ add_mcp_servers)
     (params
      ((servers
        ((search
          ((command uvx) (args (mcp-server-search --port 8080))
           (env ((API_KEY k-123)))))))))
     (tag Mcp_draft_saved))
    New server
    Name
    [search] Its tools are called mcp__NAME__TOOL.
    (Command) (URL)
    Command
    [uvx]
    Arguments
    [mcp-server-search
    --port

    8080] One per line, as they are: no quoting needed.
    Environment
    [API_KEY] [k-123] (Hide) (Remove)
    (Add variable) Values are kept on the backend and hidden here until shown; ${VAR} takes VAR from the tool host's environment.
    Ctrl+Enter saves, Esc cancels (Cancel) (Saving…)
    (Rpc (method_ get_mcp_settings) (params ()) (tag Mcp_settings_loaded))
    (Rpc (method_ list_mcp) (params ()) (tag Mcp_host_listed))
    MCP servers
    Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    Your servers 1 server
    search uvx mcp-server-search --port 8080
    Ready on the backend · (0 tools)
    (Stop it on every host (/mcp disable search)) (Edit) (Remove (/mcp remove search))
    (Add server) (Paste JSON)
    On the backend From its files: change them there
    None from files
    A host's ~/.prigh/mcp.json and a project's .mcp.json add servers on that host only; a server there with the name of one of yours replaces it.
    |}]
;;

let%expect_test "add a URL server; a refusal stays in the form with the error" =
  let h = opened () in
  H.click h ~selector:".mcp-add";
  H.act h (Mcp_field (Kind Http));
  H.input h ~selector:"#mcp-name" "github";
  H.input h ~selector:"#mcp-url" "https://example.com/mcp";
  H.click h ~selector:".mcp-save";
  H.fail
    h
    "add_mcp_servers"
    {|there is already an MCP server "github" in the settings: choose another name, or edit that one|};
  H.text h ~selector:".mcp-draft";
  [%expect {|
    (Mcp_edit ())
    (Focus mcp-name)
    (Mcp_field (Name github))
    (Mcp_field (Url https://example.com/mcp))
    Mcp_save
    (Rpc (method_ add_mcp_servers)
     (params ((servers ((github ((type http) (url https://example.com/mcp)))))))
     (tag Mcp_draft_saved))
    New server
    Name
    [github] Its tools are called mcp__NAME__TOOL.
    (Command) (URL)
    URL
    [https://example.com/mcp]
    Headers (Add header) Values are kept on the backend and hidden here until shown; ${VAR} takes VAR from the tool host's environment.
    There is already an MCP server "github" in the settings: choose another name, or edit that one
    Ctrl+Enter saves, Esc cancels (Cancel) (Add server)
    |}]
;;

let%expect_test "an empty name or command is caught before sending" =
  let h = opened () in
  H.click h ~selector:".mcp-add";
  H.click h ~selector:".mcp-save";
  H.text h ~selector:".mcp-draft";
  [%expect {|
    (Mcp_edit ())
    (Focus mcp-name)
    Mcp_save
    New server
    Name
    [] Its tools are called mcp__NAME__TOOL.
    (Command) (URL)
    Command
    []
    Arguments
    [] One per line, as they are: no quoting needed.
    Environment (Add variable) Values are kept on the backend and hidden here until shown; ${VAR} takes VAR from the tool host's environment.
    Give the server a name: its tools are called mcp__NAME__TOOL
    Ctrl+Enter saves, Esc cancels (Cancel) (Add server)
    |}]
;;

let%expect_test "edit: rename and change, Esc cancels without sending" =
  let h = opened () in
  H.click h ~selector:"[data-server=notes] .mcp-edit";
  H.input h ~selector:"#mcp-name" "notebook";
  H.key h ~target:Field "Escape";
  H.text h ~selector:".settings-panel";
  H.click h ~selector:"[data-server=notes] .mcp-edit";
  H.input h ~selector:"#mcp-name" "notebook";
  H.input h ~selector:"#mcp-command" "notes-mcp --stdio";
  H.click h ~selector:".mcp-save";
  [%expect {|
    (Mcp_edit (notes))
    (Focus mcp-name)
    (Mcp_field (Name notebook))
    Close_dialog
    MCP servers
    Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    Your servers 3 servers
    github https://api.githubcopilot.com/mcp/
    Ready on the backend · (2 tools)
    (Stop it on every host (/mcp disable github)) (Edit) (Remove (/mcp remove github))
    fs npx -y @modelcontextprotocol/server-filesystem /home/u
    Overridden by /home/u/.prigh/mcp.json on the backend: that one runs instead. Rename one of them to use both.
    (Stop it on every host (/mcp disable fs)) (Edit) (Remove (/mcp remove fs))
    notes notes-mcp
    Off: not started on any host
    (Start it on every host (/mcp enable notes)) (Edit) (Remove (/mcp remove notes))
    (Add server) (Paste JSON)
    On the backend From its files: change them there
    /work/.mcp.json: server "x": give a "command" (stdio) or a "url" (http)
    fs /home/u/.prigh/mcp.json
    Ready · (1 tool)
    web /home/u/.prigh/mcp.json Failed: Spawn uvx: not found
    (Retry)
    db /work/.mcp.json The project asks to run it: approve it to start it
    (Approve)
    (Mcp_edit (notes))
    (Focus mcp-name)
    (Mcp_field (Name notebook))
    (Mcp_field (Command "notes-mcp --stdio"))
    Mcp_save
    (Rpc (method_ update_mcp_server)
     (params
      ((name notes) (rename notebook) (config ((command "notes-mcp --stdio")))))
     (tag Mcp_draft_saved))
    |}]
;;

let%expect_test "switch off and on; remove asks first, Keep and Esc keep it" =
  let h = opened () in
  H.click h ~selector:"[data-server=github] .switch";
  H.reply
    h
    "set_mcp_server_enabled"
    {|{"servers":[{"name":"github","enabled":false,"config":{"url":"https://api.githubcopilot.com/mcp/"}}]}|};
  H.text h ~selector:".settings-panel";
  H.click h ~selector:"[data-server=github] .mcp-remove";
  H.text h ~selector:"[data-server=github]";
  H.key h "Escape";
  H.text h ~selector:"[data-server=github]";
  H.click h ~selector:"[data-server=github] .mcp-remove";
  H.click h ~selector:"#mcp-remove-yes";
  H.reply h "remove_mcp_server" {|{"servers":[]}|};
  H.text h ~selector:".settings-panel";
  [%expect {|
    (Mcp_toggle github)
    (Rpc (method_ set_mcp_server_enabled)
     (params ((name github) (enabled false))) (tag (Mcp_toggled github)))
    (Rpc (method_ get_mcp_settings) (params ()) (tag Mcp_settings_loaded))
    (Rpc (method_ list_mcp) (params ()) (tag Mcp_host_listed))
    MCP servers
    Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    Your servers 3 servers
    github https://api.githubcopilot.com/mcp/
    Ready on the backend · (2 tools)
    (Stop it on every host (/mcp disable github)) (Edit) (Remove (/mcp remove github))
    fs npx -y @modelcontextprotocol/server-filesystem /home/u
    Overridden by /home/u/.prigh/mcp.json on the backend: that one runs instead. Rename one of them to use both.
    (Stop it on every host (/mcp disable fs)) (Edit) (Remove (/mcp remove fs))
    notes notes-mcp
    Off: not started on any host
    (Start it on every host (/mcp enable notes)) (Edit) (Remove (/mcp remove notes))
    (Add server) (Paste JSON)
    On the backend From its files: change them there
    /work/.mcp.json: server "x": give a "command" (stdio) or a "url" (http)
    fs /home/u/.prigh/mcp.json
    Ready · (1 tool)
    web /home/u/.prigh/mcp.json Failed: Spawn uvx: not found
    (Retry)
    db /work/.mcp.json The project asks to run it: approve it to start it
    (Approve)
    (Mcp_remove github)
    (Focus mcp-remove-yes)
    github https://api.githubcopilot.com/mcp/ Remove it from your settings? Its tools go from every session.
    (Keep) (Remove)
    Close_dialog
    github https://api.githubcopilot.com/mcp/
    Ready on the backend · (2 tools)
    (Stop it on every host (/mcp disable github)) (Edit) (Remove (/mcp remove github))
    (Mcp_remove github)
    (Focus mcp-remove-yes)
    Mcp_remove_confirm
    (Rpc (method_ remove_mcp_server) (params ((name github)))
     (tag (Mcp_removed github)))
    (Rpc (method_ get_mcp_settings) (params ()) (tag Mcp_settings_loaded))
    (Rpc (method_ list_mcp) (params ()) (tag Mcp_host_listed))
    (Expire_toast (id 0) (after_ms 2820))
    MCP servers
    Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    Your servers 3 servers
    github https://api.githubcopilot.com/mcp/
    Ready on the backend · (2 tools)
    (Stop it on every host (/mcp disable github)) (Edit) (Remove (/mcp remove github))
    fs npx -y @modelcontextprotocol/server-filesystem /home/u
    Overridden by /home/u/.prigh/mcp.json on the backend: that one runs instead. Rename one of them to use both.
    (Stop it on every host (/mcp disable fs)) (Edit) (Remove (/mcp remove fs))
    notes notes-mcp
    Off: not started on any host
    (Start it on every host (/mcp enable notes)) (Edit) (Remove (/mcp remove notes))
    (Add server) (Paste JSON)
    On the backend From its files: change them there
    /work/.mcp.json: server "x": give a "command" (stdio) or a "url" (http)
    fs /home/u/.prigh/mcp.json
    Ready · (1 tool)
    web /home/u/.prigh/mcp.json Failed: Spawn uvx: not found
    (Retry)
    db /work/.mcp.json The project asks to run it: approve it to start it
    (Approve)
    |}]
;;

let%expect_test "paste mcpServers JSON; a mistake is shown in place" =
  let h = opened () in
  H.click h ~selector:".mcp-paste-open";
  H.input h ~selector:"#mcp-paste-text" {|{"mcpServers": {"x": {}}}|};
  H.key h ~ctrl:true ~target:Field "Enter";
  H.fail h "add_mcp_servers" {|server "x": give a "command" (stdio) or a "url" (http)|};
  H.text h ~selector:".mcp-paste";
  H.input
    h
    ~selector:"#mcp-paste-text"
    {|{"mcpServers": {"x": {"command": "x-mcp"}}}|};
  H.click h ~selector:".mcp-paste-save";
  H.reply
    h
    "add_mcp_servers"
    {|{"servers":[{"name":"x","enabled":true,"config":{"command":"x-mcp"}}]}|};
  H.text h ~selector:".settings-panel";
  [%expect {|
    Mcp_paste
    (Focus mcp-paste-text)
    (Mcp_paste_text "{\"mcpServers\": {\"x\": {}}}")
    Dialog_accept
    (Rpc (method_ add_mcp_servers)
     (params ((text "{\"mcpServers\": {\"x\": {}}}"))) (tag Mcp_pasted))
    Paste JSON An mcpServers object, as in Claude Code's or Cursor's settings, with or without the mcpServers around it. Every server in it is added, or none is.
    [{"mcpServers": {"x": {}}}] Server "x": give a "command" (stdio) or a "url" (http)
    Ctrl+Enter adds, Esc cancels (Cancel) (Add servers)
    (Mcp_paste_text "{\"mcpServers\": {\"x\": {\"command\": \"x-mcp\"}}}")
    Mcp_paste_save
    (Rpc (method_ add_mcp_servers)
     (params ((text "{\"mcpServers\": {\"x\": {\"command\": \"x-mcp\"}}}")))
     (tag Mcp_pasted))
    (Rpc (method_ get_mcp_settings) (params ()) (tag Mcp_settings_loaded))
    (Rpc (method_ list_mcp) (params ()) (tag Mcp_host_listed))
    MCP servers
    Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    Your servers 3 servers
    github https://api.githubcopilot.com/mcp/
    Ready on the backend · (2 tools)
    (Stop it on every host (/mcp disable github)) (Edit) (Remove (/mcp remove github))
    fs npx -y @modelcontextprotocol/server-filesystem /home/u
    Overridden by /home/u/.prigh/mcp.json on the backend: that one runs instead. Rename one of them to use both.
    (Stop it on every host (/mcp disable fs)) (Edit) (Remove (/mcp remove fs))
    notes notes-mcp
    Off: not started on any host
    (Start it on every host (/mcp enable notes)) (Edit) (Remove (/mcp remove notes))
    (Add server) (Paste JSON)
    On the backend From its files: change them there
    /work/.mcp.json: server "x": give a "command" (stdio) or a "url" (http)
    fs /home/u/.prigh/mcp.json
    Ready · (1 tool)
    web /home/u/.prigh/mcp.json Failed: Spawn uvx: not found
    (Retry)
    db /work/.mcp.json The project asks to run it: approve it to start it
    (Approve)
    |}]
;;

let%expect_test "the host's: approve a project's server, retry the failed" =
  let h = opened () in
  H.click h ~selector:"[data-server=db] button";
  H.click h ~selector:"[data-server=web] button";
  [%expect {|
    (Mcp_approve (source /work/.mcp.json) (server db))
    (Rpc (method_ mcp_approve) (params ((source /work/.mcp.json) (server db)))
     (tag Mcp_host_listed))
    Mcp_reconnect
    (Rpc (method_ list_mcp) (params ((reconnect true))) (tag Mcp_host_listed))
    |}]
;;

let%expect_test "another client's change arrives as an event" =
  let h = opened () in
  H.event
    h
    {|{"type":"event","event":"mcp_settings_changed","servers":[{"name":"only","enabled":true,"config":{"command":"only-mcp"}}]}|};
  H.text h ~selector:".settings-panel";
  [%expect {|
    (Rpc (method_ list_mcp) (params ()) (tag Mcp_host_listed))
    MCP servers
    Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    Your servers 1 server
    only only-mcp
    Checking the backend…
    (Stop it on every host (/mcp disable only)) (Edit) (Remove (/mcp remove only))
    (Add server) (Paste JSON)
    On the backend From its files: change them there
    Asking the host…
    Waiting…
    |}]
;;

let%expect_test "/mcp add in the composer; a refusal comes back to the composer" =
  let h = H.create () in
  H.type_ h "/mcp add docs https://docs.example.com/mcp";
  H.key h "Enter";
  H.fail h "add_mcp_servers" {|there is already an MCP server "docs" in the settings|};
  H.text h ~selector:".toast";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect {|
    Send
    (Rpc (method_ push_pref)
     (params
      ((key history) (value "/mcp add docs https://docs.example.com/mcp")
       (max 500)))
     (tag Ignore))
    (Rpc (method_ add_mcp_servers)
     (params
      ((servers ((docs ((type http) (url https://docs.example.com/mcp)))))))
     (tag
      (Mcp_command (command "/mcp add docs https://docs.example.com/mcp")
       (notice
        "added MCP server docs (https://docs.example.com/mcp) to your settings"))))
    There is already an MCP server "docs" in the settings. /mcp add docs https://docs.example.com/mcp is back in the editor. (Dismiss)
    "/mcp add docs https://docs.example.com/mcp"
    |}]
;;
