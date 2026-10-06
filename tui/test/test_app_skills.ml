open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

let skills_json =
  {|{"skills":[{"name":"frontend-design","description":"Build distinctive, production-grade UIs","path":"/work/.claude/skills/frontend-design/SKILL.md","model_invocable":true},{"name":"release","description":"Cut a release","path":"/home/u/.prigh/skills/release/SKILL.md","model_invocable":false},{"name":"review","description":"Review a diff","path":"/work/.prigh/skills/review/SKILL.md","model_invocable":true}]}|}
;;

let key = "abc123 backend:/work"

let expanded ?(args = "") () =
  "<skill name=\"frontend-design\" \
   location=\"/work/.claude/skills/frontend-design/SKILL.md\">\n\
   References are relative to /work/.claude/skills/frontend-design.\n\n\
   # Frontend design\n\
   Pick a bold aesthetic direction.\n\
   </skill>"
  ^ if String.is_empty args then "" else "\n\n" ^ args
;;

let connected ?width () =
  let h = Test_app.connected ?width ~height:16 () in
  H.step ~quiet:true h (Set_home "/home/u");
  h
;;

let state_for ?(running = false) ?(cwd = "/work") session =
  Or_error.ok_exn
    (Or_error.bind
       (P.Json.parse
          (state_json ~running ~cwd ~session:(session, "/s/" ^ session) ()))
       ~f:P.State.of_json)
;;

let%expect_test "/skills: a fuzzy picker; Enter puts /skill:NAME in the editor" =
  let h = connected () in
  H.keys h "/skills";
  H.enter h;
  [%expect {| (Rpc (method_ list_skills) (params ()) (tag Skills_picker)) |}];
  H.reply h Skills_picker skills_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Skills  (3)
    / ▏
    ▸  frontend-design  Build distinctive, production-grade UIs…
       release          Cut a release  ~/.prigh/skills/release …
       review           Review a diff  /work/.prigh/skills/revi…
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter puts /skill:NAME in t…
    |}];
  H.keys h "rel";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Skills  (3)
    / rel▏
    ▸  release          Cut a release  ~/.prigh/skills/release …
       review           Review a diff  /work/.prigh/skills/revi…
       frontend-design  Build distinctive, production-grade UIs…
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter puts /skill:NAME in t…
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /skill:release ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "v1.2";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params ((text "/skill:release v1.2")))
      (tag (Skill_prompt "/skill:release v1.2")))
    (Append_history "/skill:release v1.2")
    |}]
;;

let%expect_test "/skills: Esc closes without side effects; none says where to \
                 add them"
  =
  let h = connected () in
  H.keys h "/skills";
  H.enter h;
  H.reply h Skills_picker skills_json;
  H.esc h;
  H.mode h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_skills) (params ()) (tag Skills_picker))
    editing









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  (* [/skill:] without a name is the picker too. *)
  H.keys h "/skill:";
  H.enter h;
  H.reply h Skills_picker {|{"skills":[]}|};
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "abc123 backend:/work")))
    (Rpc (method_ list_skills) (params ()) (tag Skills_picker))






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      no skills here; add one as .prigh/skills/<name>/SKILL.md
      (or .claude/skills/<name>/) in the project, or under
      ~/.prigh/skills/
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/skills";
  H.enter h;
  H.reply_error h Skills_picker "unknown method list_skills";
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_skills) (params ()) (tag Skills_picker))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      no skills here; add one as .prigh/skills/<name>/SKILL.md
      (or .claude/skills/<name>/) in the project, or under
      ~/.prigh/skills/
      unknown method list_skills
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/skill: completes skill names, fetched once per session" =
  let h = connected () in
  H.keys h "/sk";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /sk▏
    ▸ /skills             pick a skill to run (Enter puts /skil…
      /skill:NAME [args]  run a skill, with what follows as its…
    …deepseek-flash  ctx:0.1%/1.0M  Tab/Enter accept · Esc close
    |}];
  (* Accepting [/skill:] goes straight on to the skill names. *)
  H.key h (Key.plain Down);
  H.key h (Key.plain Tab);
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "abc123 backend:/work")))
    |}];
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /skill:▏
    …deepseek-flash  Tab accepts · Enter runs as typed · Esc
    |}];
  H.reply h (Skills_for_autocomplete key) skills_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /skill:▏
    ▸ frontend-design  Build distinctive, production-grade UIs
      release          Cut a release
      review           Review a diff
    …deepseek-flash  Tab accepts · Enter runs as typed · Esc
    |}];
  (* Typing filters without asking again. *)
  H.keys h "fr";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /skill:fr▏
    ▸ frontend-design  Build distinctive, production-grade UIs
    …deepseek-flash  ctx:0.1%/1.0M  Tab/Enter accept · Esc close
    |}];
  H.key h (Key.plain Tab);
  H.keys h "make it bold";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /skill:frontend-design make it bold▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params ((text "/skill:frontend-design make it bold")))
      (tag (Skill_prompt "/skill:frontend-design make it bold")))
    (Append_history "/skill:frontend-design make it bold")
    |}];
  (* Enter on a completion sends the skill without arguments. *)
  H.keys h "/skill:rev";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params ((text /skill:review)))
      (tag (Skill_prompt /skill:review)))
    (Append_history /skill:review)
    |}];
  (* Another session (or directory, or tool host) fetches them again; a late
     reply for the old one is dropped. *)
  H.event h (State (state_for "s2"));
  H.keys h "/skill:";
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "s2 backend:/work")))
    |}];
  H.reply h (Skills_for_autocomplete key) skills_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /skill:▏
    …deepseek-flash  Tab accepts · Enter runs as typed · Esc
    |}];
  H.reply h (Skills_for_autocomplete "s2 backend:/work") {|{"skills":[]}|};
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /skill:▏
    …deepseek-flash  Tab accepts · Enter runs as typed · Esc
    |}];
  (* None here: Enter is the picker, which says where to add them. *)
  H.enter h;
  H.reply h Skills_picker {|{"skills":[]}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_skills) (params ()) (tag Skills_picker))






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      no skills here; add one as .prigh/skills/<name>/SKILL.md
      (or .claude/skills/<name>/) in the project, or under
      ~/.prigh/skills/
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  H.event h (State (state_for ~cwd:"/elsewhere" "s2"));
  H.keys h "/skill:";
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "s2 backend:/elsewhere")))
    |}]
;;

let%expect_test "/skill: cache is reset by switching sessions, users and \
                 reconnecting"
  =
  let h = connected () in
  let fetch () =
    H.keys h "/skill:";
    H.reply ~quiet:true h (Skills_for_autocomplete key) skills_json;
    H.key h (Key.ctrl 'u')
  in
  fetch ();
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "abc123 backend:/work")))
    |}];
  H.keys h "/skill:";
  H.key h (Key.ctrl 'u');
  [%expect {| |}];
  (* /new, /switch, /fork, ... *)
  H.reply ~quiet:true h Reload_messages {|{}|};
  fetch ();
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "abc123 backend:/work")))
    |}];
  H.reply
    ~quiet:true
    h
    User_switched
    {|{"client_id":"c2","namespace":"alice","user":"me"}|};
  fetch ();
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "abc123 backend:/work")))
    |}];
  H.step ~quiet:true h Backend_closed;
  H.reply ~quiet:true h (Reconnect 1) {|{"client_id":"c3"}|};
  fetch ();
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "abc123 backend:/work")))
    |}];
  (* A failed fetch is not retried on every key. *)
  H.event h (State (state_for "s3"));
  H.keys h "/skill:";
  H.reply_error
    h
    (Skills_for_autocomplete "s3 backend:/work")
    "unknown method list_skills";
  H.keys h "x";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "s3 backend:/work")))












      reconnected to the backend
    ────────────────────────────────────────────────────────────
    > /skill:x▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "an unknown skill: the backend's error, and the text is back \
                 in the editor"
  =
  let h = connected () in
  H.keys h "/skill:fronted make it bold";
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ list_skills)
      (params ())
      (tag (Skills_for_autocomplete "abc123 backend:/work")))
    (Rpc
      (method_ prompt)
      (params ((text "/skill:fronted make it bold")))
      (tag (Skill_prompt "/skill:fronted make it bold")))
    (Append_history "/skill:fronted make it bold")
    |}];
  H.reply_error
    h
    (Skill_prompt "/skill:fronted make it bold")
    "unknown skill \"fronted\"; did you mean: frontend-design (/skills lists \
     them all)";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      unknown skill "fronted"; did you mean: frontend-design
      (/skills lists them all)
    ────────────────────────────────────────────────────────────
    > /skill:fronted make it bold▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  (* While a turn runs it is a steer (or, with Alt+Enter, a follow-up); a
     failure takes it off the queue. Text typed since stays. *)
  H.key h (Key.ctrl 'u');
  H.event h (State (state_for ~running:true "abc123"));
  H.keys h "/skill:revew now";
  H.enter h;
  H.keys h "/skill:review later";
  H.key h (Key.alt Enter);
  H.event h (Queue_update { steer = 1; follow_up = 1 });
  H.keys h "draft";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ steer)
      (params ((text "/skill:revew now")))
      (tag (Skill_prompt "/skill:revew now")))
    (Append_history "/skill:revew now")
    (Rpc
      (method_ follow_up)
      (params ((text "/skill:review later")))
      (tag (Skill_prompt "/skill:review later")))
    (Append_history "/skill:review later")




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      unknown skill "fronted"; did you mean: frontend-design
      (/skills lists them all)
    ────────────────────────────────────────────────────────────
    queued (2) · Alt+Up edits the last
      steer      /skill:revew now
      follow-up  /skill:review later
    > draft▏
    …deepseek-flash  ⠋ working · Esc aborts · Enter steers
    |}];
  H.reply_error
    h
    (Skill_prompt "/skill:revew now")
    "unknown skill \"revew\"; did you mean: review (/skills lists them all)";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      unknown skill "fronted"; did you mean: frontend-design
      (/skills lists them all)
      unknown skill "revew"; did you mean: review (/skills lists
      them all)
    ────────────────────────────────────────────────────────────
    queued (2) · Alt+Up edits the last
      follow-up  /skill:review later
      +1 more
    > draft▏
    …deepseek-flash  ⠋ working · Esc aborts · Enter steers
    |}]
;;

let%expect_test "skill messages render compactly; verbose shows the skill file" =
  let h = connected () in
  H.event h (Message_start (user (expanded ~args:"make it bold\nand blue" ())));
  H.event h (Message_start (user (expanded ())));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ skill frontend-design
    ▌ make it bold
    ▌ and blue

    ▌ skill frontend-design
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    ▌ # Frontend design
    ▌ Pick a bold aesthetic direction.
    ▌ make it bold
    ▌ and blue

    ▌ skill frontend-design
    ▌ /work/.claude/skills/frontend-design/SKILL.md
    ▌ References are relative to
    ▌ /work/.claude/skills/frontend-design.
    ▌
    ▌ # Frontend design
    ▌ Pick a bold aesthetic direction.
      view: verbose — everything is shown
    ────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ skill frontend-design
    ▌ make it bold
    ▌ and blue

    ▌ skill frontend-design
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  view:quiet  ctx:0.1%/1.0M  $0.01
    |}];
  (* Ctrl+Up jumps to them like to any user message. *)
  H.key h (Key.ctrl 'o');
  H.key h (Key.ctrl 'o');
  H.key h { (Key.plain Up) with ctrl = true };
  H.show h;
  [%expect
    {|
    ▌ skill frontend-design
    ▌ /work/.claude/skills/frontend-design/SKILL.md
    ▌ References are relative to
    ▌ /work/.claude/skills/frontend-design.
    ▌
    ▌ # Frontend design
    ▌ Pick a bold aesthetic direction.
    ▌ make it bold
    ▌ and blue

    ▌ skill frontend-design
    ▌ /work/.claude/skills/frontend-design/SKILL.md
    ▌ References are relative to
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ↑ scrolled
    |}];
  H.key h { (Key.plain Up) with ctrl = true };
  H.show h;
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ skill frontend-design
    ▌ /work/.claude/skills/frontend-design/SKILL.md
    ▌ References are relative to
    ▌ /work/.claude/skills/frontend-design.
    ▌
    ▌ # Frontend design
    ▌ Pick a bold aesthetic direction.
    ▌ make it bold
    ▌ and blue

    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01  ↑ scrolled
    |}]
;;

let%expect_test "/fork lists and restores a skill message as /skill:NAME ARGS" =
  let h = connected () in
  H.keys h "/fork";
  H.enter h;
  H.reply
    h
    Entries_for_fork
    (sprintf
       {|{"head":"u2","entries":[{"id":"u1","parent":null,"kind":"message","message":{"role":"user","text":"hello"}},{"id":"u2","parent":"u1","kind":"message","message":{"role":"user","text":%s}}]}|}
       (P.Json.to_string (P.Json.str (expanded ~args:"make it bold" ()))));
  H.show h;
  [%expect
    {|
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_fork))






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    Fork at  (2)
    / ▏
       hello                           #1
    ▸* /skill:frontend-design make it bold  #2
    ────────────────────────────────────────────────────────────
    …deepseek-flash  ctx:0.1%/1.0M  Enter selects · Esc closes
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ fork) (params ((at u2))) (tag Reload_messages))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────
    > /skill:frontend-design make it bold▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;

let mcp_json =
  {|{"servers":[{"name":"fs","source":"/home/u/.prigh/mcp.json","project":false,"status":"ready","tools":[{"name":"mcp__fs__read_file","description":"Read a file"},{"name":"mcp__fs__list","description":"List a directory\nwith details"}]},{"name":"gh","source":"/work/.mcp.json","project":true,"status":"failed","error":"exited with 1: gh-mcp: command not found","tools":[]},{"name":"db","source":"/work/.mcp.json","project":true,"status":"needs_approval","tools":[]}],"problems":["/work/.mcp.json: server \"x\": give a \"command\" (stdio) or a \"url\" (http)"]}|}
;;

let%expect_test "/mcp: servers with their status; Enter lists tools, explains \
                 failures, approves"
  =
  let h = connected ~width:80 () in
  H.keys h "/mcp";
  H.enter h;
  [%expect {| |}];
  (* The first Enter completes [/mcp] and offers its argument, as for [/model]. *)
  H.enter h;
  [%expect {| (Rpc (method_ list_mcp) (params ()) (tag Mcp_picker)) |}];
  H.reply h Mcp_picker mcp_json;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
    MCP servers  (3)
    / ▏
    ▸  fs  ready, 2 tools  /home/u/.prigh/mcp.json
       gh  failed: exited with 1: gh-mcp: command not found  /work/.mcp.json
       db  needs approval  /work/.mcp.json
    ────────────────────────────────────────────────────────────────────────────────
    …deepseek-flash  $0.01  Enter approves a server or lists its tools · Esc closes
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      fs  2 tools  /home/u/.prigh/mcp.json
        mcp__fs__read_file  Read a file
        mcp__fs__list       List a directory
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/mcp";
  H.enter h;
  H.enter h;
  H.reply ~quiet:true h Mcp_picker mcp_json;
  H.keys h "gh";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_mcp) (params ()) (tag Mcp_picker))
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      fs  2 tools  /home/u/.prigh/mcp.json
        mcp__fs__read_file  Read a file
        mcp__fs__list       List a directory
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      gh failed: exited with 1: gh-mcp: command not found; check its entry in
      /work/.mcp.json, then /mcp reconnect
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/mcp";
  H.enter h;
  H.enter h;
  H.reply ~quiet:true h Mcp_picker mcp_json;
  H.keys h "db";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_mcp) (params ()) (tag Mcp_picker))
    (Rpc
      (method_ mcp_approve)
      (params (
        (source /work/.mcp.json)
        (server db)))
      (tag (Mcp_approved db)))
      earlier answer
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      fs  2 tools  /home/u/.prigh/mcp.json
        mcp__fs__read_file  Read a file
        mcp__fs__list       List a directory
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      gh failed: exited with 1: gh-mcp: command not found; check its entry in
      /work/.mcp.json, then /mcp reconnect
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      approving db from /work/.mcp.json…
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  (* The reply is the new list: the picker comes back with it. *)
  H.reply
    h
    (Mcp_approved "db")
    (String.substr_replace_all
       mcp_json
       ~pattern:{|"status":"needs_approval","tools":[]|}
       ~with_:
         {|"status":"ready","tools":[{"name":"mcp__db__query","description":"Run SQL"}]|});
  H.show h;
  [%expect
    {|
        mcp__fs__list       List a directory
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      gh failed: exited with 1: gh-mcp: command not found; check its entry in
      /work/.mcp.json, then /mcp reconnect
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      approving db from /work/.mcp.json…
      approved db: ready, 1 tool
    MCP servers  (3)
    / ▏
    ▸  fs  ready, 2 tools  /home/u/.prigh/mcp.json
       gh  failed: exited with 1: gh-mcp: command not found  /work/.mcp.json
       db  ready, 1 tool  /work/.mcp.json
    ────────────────────────────────────────────────────────────────────────────────
    …deepseek-flash  $0.01  Enter approves a server or lists its tools · Esc closes
    |}];
  H.esc h;
  H.mode h;
  [%expect {| editing |}];
  (* Approval failing, or the server then failing to start, says so; the list
     does not come back over what the user is typing. *)
  H.keys h "draft";
  H.reply_error h (Mcp_approved "db") "no MCP server \"db\" in /work/.mcp.json";
  H.reply
    h
    (Mcp_approved "db")
    (String.substr_replace_all
       mcp_json
       ~pattern:{|"status":"needs_approval","tools":[]|}
       ~with_:{|"status":"failed","error":"connection refused","tools":[]|});
  H.show h;
  [%expect
    {|
        mcp__fs__read_file  Read a file
        mcp__fs__list       List a directory
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      gh failed: exited with 1: gh-mcp: command not found; check its entry in
      /work/.mcp.json, then /mcp reconnect
      MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url"
      (http)
      approving db from /work/.mcp.json…
      approved db: ready, 1 tool
      approving MCP server db failed: no MCP server "db" in /work/.mcp.json
      approved db, but it failed: connection refused; check its entry in
      /work/.mcp.json, then /mcp reconnect
    ────────────────────────────────────────────────────────────────────────────────
    > draft▏
    /work  deepseek-flash  think:off  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/mcp reconnect reports every server; no servers says where to \
                 configure them"
  =
  let h = connected () in
  H.keys h "/mcp reconnect";
  H.enter h;
  H.reply h Mcp_reconnected mcp_json;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_mcp) (params ((reconnect true))) (tag Mcp_reconnected))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      reconnecting MCP servers…
      MCP config: /work/.mcp.json: server "x": give a "command"
      (stdio) or a "url" (http)
      fs  ready, 2 tools  /home/u/.prigh/mcp.json
      gh  failed: exited with 1: gh-mcp: command not found
      /work/.mcp.json
      db  needs approval  /work/.mcp.json
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}];
  H.keys h "/mcp";
  H.enter h;
  H.enter h;
  H.reply h Mcp_picker {|{"servers":[],"problems":[]}|};
  H.keys h "/mcp restart";
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_mcp) (params ()) (tag Mcp_picker))

    ▌ earlier question
      earlier answer
      reconnecting MCP servers…
      MCP config: /work/.mcp.json: server "x": give a "command"
      (stdio) or a "url" (http)
      fs  ready, 2 tools  /home/u/.prigh/mcp.json
      gh  failed: exited with 1: gh-mcp: command not found
      /work/.mcp.json
      db  needs approval  /work/.mcp.json
      no MCP servers; configure servers under "mcpServers" in
      ~/.prigh/mcp.json or a project's .mcp.json
      usage: /mcp [reconnect]
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/help lists the skill and MCP commands" =
  let h = connected () in
  H.keys h "/help skill:";
  H.enter h;
  H.keys h "/help mcp";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      /skill:NAME [args]  run a skill, with what follows as its
      arguments
      /mcp [reconnect]  list MCP servers (Enter approves one or
      lists its tools); reconnect restarts failed ones
    ────────────────────────────────────────────────────────────
    > ▏
    …deepseek-flash  think:off  ctx:0.1%/1.0M  $0.01
    |}]
;;
