open! Core
open Prigh_web
module H = Harness

(* Skills ([/skills], [/skill:name] and its completion, invoked skills in the
   transcript) and MCP servers ([/mcp]). *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let toasts h = H.text h ~selector:".toast"

let skills_json =
  {|{"skills":[
     {"name":"frontend-design","description":"Distinctive, production-grade web pages","path":"/work/.claude/skills/frontend-design/SKILL.md","model_invocable":true},
     {"name":"release","description":"Cut a release: changelog, tag, publish","path":"/home/u/.prigh/skills/release/SKILL.md","model_invocable":false}]}|}
;;

let draft h =
  let m = H.model h in
  print_s [%message (m.draft : string) (m.cursor : int)]
;;

let state ?(session = "s1") ?(cwd = "/work") ?(running = false) () =
  Jsonaf.of_string
    (H.state_json
       ~fields:
         [ "session_id", `String session
         ; "session_path", `String (sprintf "/sessions/%s.jsonl" session)
         ; "cwd", `String cwd
         ; ("running", if running then `True else `False)
         ]
       ())
;;

let set_state h ?session ?cwd ?running () =
  H.act h (Reply (State, Ok (state ?session ?cwd ?running ())))
;;

let%expect_test "/skills: a fuzzy picker; Enter puts /skill:name in the editor" =
  let h = H.create () in
  run h "/skills";
  H.reply h "list_skills" skills_json;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/skills))
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s1\
                         \nbackend\
                         \n/work") (purpose (Picker "")))))
    (Focus picker-input)
    Skills
    (Close (Esc))
    []
    frontend-design Distinctive, production-grade web pages · /work/.claude/skills/frontend-design
    release Cut a release: changelog, tag, publish · /home/u/.prigh/skills/release · only you invoke it
    ↑↓ move · Enter choose · Esc close
    |}];
  (* The description is searched too. *)
  H.act h (Picker_query "changelog");
  H.text h ~selector:".picker-items";
  [%expect
    {| release Cut a release: changelog, tag, publish · /home/u/.prigh/skills/release · only you invoke it |}];
  H.key h "Enter" ~target:Field;
  draft h;
  H.text h ~selector:"#editor";
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    ((m.draft "/skill:release ") (m.cursor 15))
    [/skill:release ]
    |}];
  (* The listing fills the completion's cache: no second request. *)
  H.type_ h "/skill:";
  H.text h ~selector:".popup";
  [%expect
    {|
    Skills ↑↓ Tab Enter Esc
    frontend-design Distinctive, production-grade web pages
    release Cut a release: changelog, tag, publish
    |}];
  (* Esc closes without side effects. *)
  H.type_ h "";
  run h "/skills front";
  H.reply h "list_skills" skills_json;
  H.text h ~selector:".picker-items";
  H.key h "Escape" ~target:Field;
  draft h;
  [%expect
    {|
    (Save_history ("/skills front" /skills))
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s1\
                         \nbackend\
                         \n/work") (purpose (Picker front)))))
    (Focus picker-input)
    frontend-design Distinctive, production-grade web pages · /work/.claude/skills/frontend-design
    Close_dialog
    (Focus editor)
    ((m.draft "") (m.cursor 0))
    |}]
;;

let%expect_test "/skills with none, and when listing fails" =
  let h = H.create () in
  run h "/skills";
  H.reply h "list_skills" {|{"skills":[]}|};
  run h "/skill:";
  H.fail h "list_skills" "unknown method list_skills";
  toasts h;
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect
    {|
    (Save_history (/skills))
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s1\
                         \nbackend\
                         \n/work") (purpose (Picker "")))))
    (Expire_toast (id 0) (after_ms 4000))
    (Save_history (/skill: /skills))
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s1\
                         \nbackend\
                         \n/work") (purpose (Picker "")))))
    No skills here: put one in .prigh/skills/<name>/SKILL.md (or .claude/skills/, .agents/skills/) in the project, or in ~/.prigh/skills/.
    Couldn't list the skills: unknown method list_skills
    false
    |}]
;;

let%expect_test "/skill: completes skill names, fetched once" =
  let h = H.create () in
  H.type_ h "/sk";
  H.text h ~selector:".popup";
  [%expect
    {|
    Commands ↑↓ Tab Enter Esc
    /skills pick a skill to invoke (/skill:name)
    /skill:<name> [args] send a skill's instructions to the agent, with your arguments
    |}];
  H.key h "ArrowDown";
  H.key h "Tab";
  draft h;
  [%expect
    {|
    (Complete_move 1)
    (Complete_accept (run false))
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s1\
                         \nbackend\
                         \n/work") (purpose Complete))))
    ((m.draft /skill:) (m.cursor 7))
    |}];
  (* Typing on while it is being listed asks nothing more. *)
  H.type_ h "/skill:f";
  H.reply h "list_skills" skills_json;
  H.text h ~selector:".popup";
  [%expect
    {|
    Skills ↑↓ Tab Enter Esc
    frontend-design Distinctive, production-grade web pages
    |}];
  H.type_ h "/skill:rel";
  H.key h "Enter";
  draft h;
  H.text h ~selector:".popup";
  [%expect
    {|
    (Complete_accept (run true))
    ((m.draft "/skill:release ") (m.cursor 15))
    |}];
  (* The arguments are not completed; Enter sends the prompt as typed, for
     the backend to expand. *)
  H.type_ h "/skill:release 2.0 to npm";
  H.text h ~selector:".popup";
  H.key h "Enter";
  [%expect
    {|
    Send
    (Save_history ("/skill:release 2.0 to npm"))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "/skill:release 2.0 to npm")))
     (tag (Sent (text "/skill:release 2.0 to npm") (images ()))))
    |}];
  (* A name typed before arguments that are already there is replaced in
     place. *)
  H.act h (Edit { text = "/skill:fr  the landing page"; cursor = 9 });
  H.key h "Tab" ~target:(Editor { cursor = 9 });
  draft h;
  [%expect
    {|
    (Complete_accept (run false))
    ((m.draft "/skill:frontend-design  the landing page") (m.cursor 22))
    |}]
;;

let%expect_test "/skill:name while running steers; an unknown skill fails back" =
  let h = H.create () in
  set_state h ~running:true ();
  run h "/skill:frontend-design the pricing page";
  [%expect
    {|
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s1\
                         \nbackend\
                         \n/work") (purpose Complete))))
    (Save_history ("/skill:frontend-design the pricing page"))
    Scroll_to_bottom
    (Rpc (method_ steer)
     (params ((text "/skill:frontend-design the pricing page")))
     (tag (Sent (text "/skill:frontend-design the pricing page") (images ()))))
    |}];
  set_state h ();
  run h "/skill:fronted the pricing page";
  H.fail
    h
    "prompt"
    "unknown skill \"fronted\"; did you mean: frontend-design (/skills lists \
     them all)";
  draft h;
  H.text h ~selector:".toasts .error";
  [%expect
    {|
    (Save_history
     ("/skill:fronted the pricing page"
      "/skill:frontend-design the pricing page"))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "/skill:fronted the pricing page")))
     (tag (Sent (text "/skill:fronted the pricing page") (images ()))))
    ((m.draft "/skill:fronted the pricing page") (m.cursor 31))
    Couldn't send: unknown skill "fronted"; did you mean: frontend-design (/skills lists them all). Your message is back in the editor.
    |}]
;;

let%expect_test
    "the skills are listed again for a new session, directory or user"
  =
  let h = H.create () in
  let complete () =
    H.type_ h "/skill:";
    H.type_ h ""
  in
  complete ();
  H.reply h "list_skills" skills_json;
  complete ();
  [%expect
    {|
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s1\
                         \nbackend\
                         \n/work") (purpose Complete))))
    |}];
  (* Another directory has other skills. *)
  set_state h ~cwd:"/other" ();
  complete ();
  [%expect
    {|
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s1\
                         \nbackend\
                         \n/other") (purpose Complete))))
    |}];
  (* A listing for the session we left is ignored. *)
  set_state h ~session:"s2" ();
  H.reply h "list_skills" skills_json;
  H.type_ h "/skill:";
  [%expect
    {|
    (Set_url_session s2)
    (Rpc (method_ get_messages) (params ()) (tag (Messages s2)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s2\
                         \nbackend\
                         \n/work") (purpose Complete))))
    |}];
  print_s [%sexp (Option.is_some (App.Model.popup (H.model h)) : bool)];
  H.reply h "list_skills" {|{"skills":[]}|};
  print_s [%sexp (Option.is_some (App.Model.popup (H.model h)) : bool)];
  [%expect
    {|
    false
    false
    |}];
  (* Acting as another user starts over. *)
  H.type_ h "";
  H.act
    h
    (Reply
       ( User_switched
       , Ok
           (Jsonaf.of_string
              {|{"client_id":"c1","namespace":"bob","user":"alice"}|}) ));
  print_s [%sexp ((H.model h).skills : Skills.t)];
  set_state h ~session:"s2" ();
  H.type_ h "/skill:";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 4000))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    ()
    (Set_url_session s2)
    (Rpc (method_ get_messages) (params ()) (tag (Messages s2)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ list_skills) (params ())
     (tag (Skills (place  "s2\
                         \nbackend\
                         \n/work") (purpose Complete))))
    |}];
  (* An older backend without [list_skills]: no completion, and no request
     at every key either. *)
  H.fail h "list_skills" "unknown method";
  H.type_ h "/skill:x";
  H.text h ~selector:".toasts .error";
  [%expect {| |}]
;;

let skill_text =
  "<skill name=\"frontend-design\" \
   location=\"/work/.claude/skills/frontend-design/SKILL.md\">\n\
   References are relative to /work/.claude/skills/frontend-design.\n\n\
   # Frontend design\n\n\
   Use **bold** typography.\n\
   </skill>\n\n\
   the pricing page"
;;

let%expect_test "an invoked skill: a folded card and the user's arguments" =
  let user text =
    sprintf
      {|{"event":"message_start","message":{"role":"user","text":%s}}|}
      (Jsonaf.to_string (`String text))
  in
  let chat =
    Chat_harness.chat
      [ user skill_text
      ; user
          "<skill name=\"release\" \
           location=\"/home/u/.prigh/skills/release/SKILL.md\">\n\
           References are relative to /home/u/.prigh/skills/release.\n\n\
           Tag it.\n\
           </skill>"
      ]
  in
  Chat_harness.show chat;
  [%expect
    {|
    <div class="entries">
      <div class="msg skill user">
        <details class="skill-card">
          <summary>
            <span class="label"> skill frontend-design </span>
            <span class="preview"> /work/.claude/skills/frontend-design/SKILL.md </span>
          </summary>
          <div class="skill-body">
            <div class="skill-location"> /work/.claude/skills/frontend-design/SKILL.md </div>
            <div class="markdown">
              <p> References are relative to /work/.claude/skills/frontend-design. </p>
              <h1> Frontend design </h1>
              <p>
                Use
                <strong> bold </strong>
                 typography.
              </p>
            </div>
          </div>
        </details>
        <div class="bubble"> the pricing page </div>
      </div>
      <div class="msg skill user">
        <details class="skill-card">
          <summary>
            <span class="label"> skill release </span>
            <span class="preview"> /home/u/.prigh/skills/release/SKILL.md </span>
          </summary>
          <div class="skill-body">
            <div class="skill-location"> /home/u/.prigh/skills/release/SKILL.md </div>
            <div class="markdown">
              <p> References are relative to /home/u/.prigh/skills/release. </p>
              <p> Tag it. </p>
            </div>
          </div>
        </details>
      </div>
    </div>
    |}];
  Chat_harness.text chat;
  [%expect
    {| skill frontend-design /work/.claude/skills/frontend-design/SKILL.md /work/.claude/skills/frontend-design/SKILL.md References are relative to /work/.claude/skills/frontend-design. Frontend design Use bold typography. the pricing page skill release /home/u/.prigh/skills/release/SKILL.md /home/u/.prigh/skills/release/SKILL.md References are relative to /home/u/.prigh/skills/release. Tag it. |}]
;;

let%expect_test "an invoked skill in the page: verbosity classes, /fork" =
  let h = H.create () in
  H.act
    h
    (Reply
       ( Messages "s1"
       , Ok
           (`Array
               [ `Object [ "role", `String "user"; "text", `String skill_text ]
               ]) ));
  H.act h Cycle_verbosity;
  H.act h Cycle_verbosity;
  H.show h ~selector:"#chat";
  [%expect
    {|
    Scroll_to_bottom
    (Expire_toast (id 0) (after_ms 4000))
    (Expire_toast (id 1) (after_ms 4000))
    <div id="chat" class="chat verbosity-quiet">
      <div class="entries">
        <div class="msg skill user">
          <details class="skill-card">
            <summary>
              <span class="label"> skill frontend-design </span>
              <span class="preview"> /work/.claude/skills/frontend-design/SKILL.md </span>
            </summary>
            <div class="skill-body">
              <div class="skill-location"> /work/.claude/skills/frontend-design/SKILL.md </div>
              <div class="markdown">
                <p> References are relative to /work/.claude/skills/frontend-design. </p>
                <h1> Frontend design </h1>
                <p>
                  Use
                  <strong> bold </strong>
                   typography.
                </p>
              </div>
            </div>
          </details>
          <div class="bubble"> the pricing page </div>
        </div>
      </div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </div>
    |}];
  (* /fork offers it as it was typed, and puts that back in the editor. *)
  let entries =
    {|{"head":"e1","entries":[{"id":"e1","parent":null,"kind":"message","message":{"role":"user","text":|}
    ^ Jsonaf.to_string (`String skill_text)
    ^ {|}}]}|}
  in
  run h "/fork";
  H.reply h "get_entries" entries;
  H.text h ~selector:".picker-items";
  H.key h "Enter" ~target:Field;
  draft h;
  [%expect
    {|
    (Save_history (/fork))
    (Rpc (method_ get_entries) (params ()) (tag (Entries Fork)))
    (Focus picker-input)
    /skill:frontend-design the pricing page #1 ✓
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 2) (after_ms 4000))
    (Rpc (method_ fork) (params ((at e1))) (tag Reload_state))
    ((m.draft "/skill:frontend-design the pricing page") (m.cursor 39))
    |}]
;;

let%expect_test "Skill_message.parse" =
  let show text =
    print_s [%sexp (Skill_message.parse text : Skill_message.t option)]
  in
  show skill_text;
  (* The arguments may mention the closing tag, and so may the body. *)
  show
    "<skill name=\"x\" location=\"/s/x/SKILL.md\">\n\
     Write </skill> in HTML.\n\
     </skill>\n\n\
     why does\n\
     </skill>\n\
     close it?";
  show "<skill name=\"x\" location=\"/s/x/SKILL.md\">\n</skill>";
  (* Not an invocation. *)
  show "<skill name=\"x\">\nbody\n</skill>";
  show "<skill name=\"x\" location=\"/s/x/SKILL.md\">\nno end";
  show "look at <skill name=\"x\" location=\"y\">\n</skill>";
  show "<skill name=\"x";
  [%expect
    {|
    (((name frontend-design)
      (location /work/.claude/skills/frontend-design/SKILL.md)
      (body
        "References are relative to /work/.claude/skills/frontend-design.\
       \n\
       \n# Frontend design\
       \n\
       \nUse **bold** typography.")
      (args "the pricing page")))
    (((name x) (location /s/x/SKILL.md) (body "Write </skill> in HTML.")
      (args  "why does\
            \n</skill>\
            \nclose it?")))
    (((name x) (location /s/x/SKILL.md) (body "") (args "")))
    ()
    ()
    ()
    ()
    |}]
;;

let mcp_json =
  {|{"servers":[
     {"name":"fs","source":"/home/u/.prigh/mcp.json","project":false,"status":"ready","tools":[
        {"name":"mcp__fs__read_file","description":"Read a file"},
        {"name":"mcp__fs__list","description":""}]},
     {"name":"web","source":"/home/u/.prigh/mcp.json","project":false,"status":"failed","error":"spawn uvx: not found","tools":[]},
     {"name":"db","source":"/work/.mcp.json","project":true,"status":"needs_approval","tools":[]}],
   "problems":["/work/.mcp.json: server \"x\": give a \"command\" (stdio) or a \"url\" (http)"]}|}
;;

let approved_json =
  {|{"servers":[
     {"name":"fs","source":"/home/u/.prigh/mcp.json","project":false,"status":"ready","tools":[
        {"name":"mcp__fs__read_file","description":"Read a file"},
        {"name":"mcp__fs__list","description":""}]},
     {"name":"web","source":"/home/u/.prigh/mcp.json","project":false,"status":"failed","error":"spawn uvx: not found","tools":[]},
     {"name":"db","source":"/work/.mcp.json","project":true,"status":"ready","tools":[
        {"name":"mcp__db__query","description":"Run SQL"}]}],
   "problems":[]}|}
;;

let%expect_test "/mcp: servers to act on first, problems below; Enter approves" =
  let h = H.create () in
  run h "/mcp";
  H.reply h "list_mcp" mcp_json;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/mcp))
    (Rpc (method_ list_mcp) (params ()) (tag (Mcp Picker)))
    (Focus picker-input)
    MCP servers
    (Close (Esc))
    []
    db needs your approval · Enter starts it · /work/.mcp.json
    web failed: spawn uvx: not found · Enter restarts it · /home/u/.prigh/mcp.json
    fs ready · 2 tools · /home/u/.prigh/mcp.json
    Configuration problems
    /work/.mcp.json: server "x": give a "command" (stdio) or a "url" (http)
    ↑↓ move · Enter choose · Esc close
    |}];
  H.show h ~selector:".picker-item.dimmed";
  [%expect
    {|
    <div role="option" class="dimmed picker-item" @on_click>
      <span class="picker-label"> web </span>
      <span class="picker-detail"> failed: spawn uvx: not found · Enter restarts it · /home/u/.prigh/mcp.json </span>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
    </div>
    |}];
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ mcp_approve) (params ((source /work/.mcp.json) (server db)))
     (tag (Mcp (Refreshed /work/.mcp.json#db))))
    |}];
  H.reply h "mcp_approve" approved_json;
  toasts h;
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 4000))
    (Focus picker-input)
    Approved db: starting it…
    db is ready with 1 tool: Enter lists them
    web failed: spawn uvx: not found · Enter restarts it · /home/u/.prigh/mcp.json
    fs ready · 2 tools · /home/u/.prigh/mcp.json
    db ready · 1 tool · /work/.mcp.json
    |}];
  (* The approved server stays highlighted: Enter shows its tools. *)
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".modal";
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    db: 1 tool
    (Close (Esc))
    /work/.mcp.json
    mcp__db__query Run SQL
    /mcp lists the servers (Done)
    |}];
  H.key h "Escape" ~target:Page;
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  [%expect
    {|
    Close_dialog
    (Focus editor)
    ()
    |}]
;;

let%expect_test "/mcp: Enter restarts a failed server; failures say what next" =
  let h = H.create () in
  run h "/mcp";
  H.reply h "list_mcp" mcp_json;
  H.act h (Picker_query "web");
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    (Save_history (/mcp))
    (Rpc (method_ list_mcp) (params ()) (tag (Mcp Picker)))
    (Focus picker-input)
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ list_mcp) (params ((reconnect true)))
     (tag (Mcp (Refreshed /home/u/.prigh/mcp.json#web))))
    |}];
  (* Something else opened meanwhile stays. *)
  run h "/hotkeys";
  H.reply h "list_mcp" mcp_json;
  toasts h;
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  [%expect
    {|
    (Save_history (/hotkeys /mcp))
    (Focus dialog)
    Restarting the failed MCP servers…
    web failed: spawn uvx: not found. Fix it in /home/u/.prigh/mcp.json, then /mcp reconnect.
    (Hotkeys)
    |}];
  H.act h Close_dialog;
  run h "/mcp";
  H.reply h "list_mcp" mcp_json;
  H.key h "Enter" ~target:Field;
  H.fail h "mcp_approve" "no server \"db\" in /work/.mcp.json";
  H.text h ~selector:".toasts .error";
  [%expect
    {|
    (Focus editor)
    (Save_history (/mcp /hotkeys /mcp))
    (Rpc (method_ list_mcp) (params ()) (tag (Mcp Picker)))
    (Focus picker-input)
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 2) (after_ms 4000))
    (Rpc (method_ mcp_approve) (params ((source /work/.mcp.json) (server db)))
     (tag (Mcp (Refreshed /work/.mcp.json#db))))
    web failed: spawn uvx: not found. Fix it in /home/u/.prigh/mcp.json, then /mcp reconnect.
    Couldn't start the MCP server: no server "db" in /work/.mcp.json. /mcp reconnect retries.
    |}]
;;

let%expect_test "/mcp reconnect reports; no servers says where to add them" =
  let h = H.create () in
  run h "/mcp reconnect";
  H.reply h "list_mcp" mcp_json;
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Save_history ("/mcp reconnect"))
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ list_mcp) (params ((reconnect true))) (tag (Mcp Reconnect)))
    Restarting the failed MCP servers…
    MCP servers: 1 ready (2 tools), 1 failed (web: spawn uvx: not found), 1 awaiting approval, 1 configuration problem — /mcp shows them
    |}];
  let last_toast () = print_endline (List.last_exn (H.model h).toasts).text in
  H.quiet h (fun () ->
    run h "/mcp reconnect";
    H.reply
      h
      "list_mcp"
      {|{"servers":[{"name":"fs","source":"/home/u/.prigh/mcp.json","project":false,"status":"ready","tools":[{"name":"mcp__fs__read_file","description":"Read a file"}]}],"problems":[]}|};
    last_toast ();
    run h "/mcp reconnect";
    H.reply h "list_mcp" {|{"servers":[],"problems":[]}|};
    last_toast ();
    run h "/mcp";
    H.reply h "list_mcp" {|{"servers":[],"problems":[]}|};
    last_toast ();
    run h "/mcp restart";
    last_toast ();
    run h "/mcp";
    H.fail h "list_mcp" "unknown method list_mcp";
    last_toast ());
  [%expect
    {|
    MCP servers: 1 ready (1 tool)
    No MCP servers: configure them under "mcpServers" in ~/.prigh/mcp.json, or in a project's .mcp.json.
    No MCP servers: configure them under "mcpServers" in ~/.prigh/mcp.json, or in a project's .mcp.json.
    Unknown /mcp argument "restart": /mcp lists the servers, /mcp reconnect restarts the failed ones.
    Couldn't list the MCP servers: unknown method list_mcp
    |}]
;;

let%expect_test "/mcp with only problems; the dialog closes with the session" =
  let h = H.create () in
  run h "/mcp";
  H.reply
    h
    "list_mcp"
    {|{"servers":[],"problems":["/home/u/.prigh/mcp.json: expected an object \"mcpServers\""]}|};
  H.text h ~selector:".modal";
  [%expect
    {|
    (Save_history (/mcp))
    (Rpc (method_ list_mcp) (params ()) (tag (Mcp Picker)))
    (Focus picker-input)
    MCP servers
    (Close (Esc))
    []
    No MCP servers: configure them under "mcpServers" in ~/.prigh/mcp.json, or in a project's .mcp.json.
    Configuration problems
    /home/u/.prigh/mcp.json: expected an object "mcpServers"
    ↑↓ move · Enter choose · Esc close
    |}];
  set_state h ~session:"s2" ();
  print_s [%sexp ((H.model h).dialog : Dialog.t option)];
  [%expect
    {|
    (Set_url_session s2)
    (Rpc (method_ get_messages) (params ()) (tag (Messages s2)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    ()
    |}]
;;

let%expect_test "/help /skill: and /help mcp" =
  let h = H.create () in
  run h "/help skill:";
  run h "/help mcp";
  toasts h;
  [%expect
    {|
    (Save_history ("/help skill:"))
    (Expire_toast (id 0) (after_ms 4000))
    (Save_history ("/help mcp" "/help skill:"))
    (Expire_toast (id 1) (after_ms 4000))
    /skill:<name> [args] — send a skill's instructions to the agent, with your arguments
    /mcp [reconnect] — MCP servers: their tools, approve a project's, or restart failed ones
    |}]
;;
