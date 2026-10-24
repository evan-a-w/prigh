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
    prigh  new session                                     /work






    ╭─ Skills (3) ─────────────────────────────────────────────╮
    │ / ▏                                                      │
    │ ▸  frontend-design  Build distinctive, production-grade… │
    │    release          Cut a release  ~/.prigh/skills/rele… │
    │    review           Review a diff  /work/.prigh/skills/… │
    ╰──────────────────────────────────────────────────────────╯
    ── Enter puts /skill:NAME in the editor · Esc closes ───────
    >
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h "rel";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work






    ╭─ Skills (3) ─────────────────────────────────────────────╮
    │ / rel▏                                                   │
    │ ▸  release          Cut a release  ~/.prigh/skills/rele… │
    │    review           Review a diff  /work/.prigh/skills/… │
    │    frontend-design  Build distinctive, production-grade… │
    ╰──────────────────────────────────────────────────────────╯
    ── Enter puts /skill:NAME in the editor · Esc closes ───────
    >
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > /skill:release ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work







    ╭─ Notice ─────────────────────────────────────────────────╮
    │ no skills here; add one as .prigh/skills/<name>/SKILL.md │
    │ (or .claude/skills/<name>/) in the project, or under     │
    │ ~/.prigh/skills/                                         │
    ╰────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.keys h "/skills";
  H.enter h;
  H.reply_error h Skills_picker "unknown method list_skills";
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_skills) (params ()) (tag Skills_picker))
    prigh  new session                                     /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands ────── unknown method list_skills ──
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/skill: completes skill names, fetched once per session" =
  let h = connected () in
  H.keys h "/sk";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work








    ╭─ Commands ───────────────────────────────────────────────╮
    │ ▸ /skills             pick a skill to run (Enter puts /… │
    │   /skill:NAME [args]  run a skill, with what follows as… │
    ╰──────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────
    > /sk▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work










    ╭─ Skills ─────────────────────────────────────────────────╮
    ╰──────────────────────────────────────────────────────────╯
    ── Tab accepts · Enter as typed · Esc hides ────────────────
    > /skill:▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.reply h (Skills_for_autocomplete key) skills_json;
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work







    ╭─ Skills ─────────────────────────────────────────────────╮
    │ ▸ frontend-design  Build distinctive, production-grade … │
    │   release          Cut a release                         │
    │   review           Review a diff                         │
    ╰──────────────────────────────────────────────────────────╯
    ── Tab accepts · Enter as typed · Esc hides ────────────────
    > /skill:▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  (* Typing filters without asking again. *)
  H.keys h "fr";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work









    ╭─ Skills ─────────────────────────────────────────────────╮
    │ ▸ frontend-design  Build distinctive, production-grade … │
    ╰──────────────────────────────────────────────────────────╯
    ── Tab/Enter accept · Alt+S as typed ───────────────────────
    > /skill:fr▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.key h (Key.plain Tab);
  H.keys h "make it bold";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > /skill:frontend-design make it bold▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work










    ╭─ Skills ─────────────────────────────────────────────────╮
    ╰──────────────────────────────────────────────────────────╯
    ── Tab accepts · Enter as typed · Esc hides ────────────────
    > /skill:▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.reply h (Skills_for_autocomplete "s2 backend:/work") {|{"skills":[]}|};
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work










    ╭─ Skills ─────────────────────────────────────────────────╮
    ╰──────────────────────────────────────────────────────────╯
    ── Tab accepts · Enter as typed · Esc hides ────────────────
    > /skill:▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  (* None here: Enter is the picker, which says where to add them. *)
  H.enter h;
  H.reply h Skills_picker {|{"skills":[]}|};
  H.show h;
  [%expect
    {|
    (Rpc (method_ list_skills) (params ()) (tag Skills_picker))
    prigh  new session                                     /work







    ╭─ Notice ─────────────────────────────────────────────────╮
    │ no skills here; add one as .prigh/skills/<name>/SKILL.md │
    │ (or .claude/skills/<name>/) in the project, or under     │
    │ ~/.prigh/skills/                                         │
    ╰────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work












    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > /skill:x▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work








    ╭─ Error ──────────────────────────────────────────────────╮
    │ unknown skill "fronted"; did you mean: frontend-design   │
    │ (/skills lists them all)                                 │
    ╰────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > /skill:fronted make it bold▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  (* While a turn runs it is a steer (or, with Alt+Enter, a follow-up); a
     failure takes it off the queue. Text typed since stays. *)
  H.key h (Key.ctrl 'u');
  H.event h (State (state_for ~running:true "abc123"));
  H.keys h "/skill:revew now";
  H.enter h;
  H.keys h "/skill:review later";
  H.key h (Key.alt Enter);
  H.event
    h
    (Queue_update
       { steer = [ "/skill:revew now" ]; follow_up = [ "/skill:review later" ] });
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
    prigh  new session                                     /work








    ╭─ Queued (2) ─────────────────────────────────────────────╮
    │ steer      /skill:revew now                              │
    │ follow-up  /skill:review later                           │
    ╰──────── Alt+Up edits the last · /queue edits or removes ─╯
    ── ⠋ working · Esc aborts · Enter steers ───────────────────
    > draft▏
    deepseek-flash          queued:2  ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.reply_error
    h
    (Skill_prompt "/skill:revew now")
    "unknown skill \"revew\"; did you mean: review (/skills lists them all)";
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work





    ╭─ Queued (1) ─────────────────────────────────────────────╮
    │ follow-up  /skill:review later                           │
    ╰──────── Alt+Up edits the last · /queue edits or removes ─╯
    ╭─ Error ──────────────────────────────────────────────────╮
    │ unknown skill "revew"; did you mean: review (/skills     │
    │ lists them all)                                          │
    ╰────────────────────────────────────────── Esc dismisses ─╯
    ── ⠋ working · Esc aborts · Enter steers ───────────────────
    > draft▏
    deepseek-flash          queued:1  ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "skill messages render compactly; verbose shows the skill file" =
  let h = connected () in
  H.event h (Message_start (user (expanded ~args:"make it bold\nand blue" ())));
  H.event h (Message_start (user (expanded ())));
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work




    ▌ earlier question
      earlier answer

    ▌ skill frontend-design
    ▌ make it bold
    ▌ and blue

    ▌ skill frontend-design
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work
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
    ── /help ──────────── view: verbose — everything is shown ──
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.key h (Key.ctrl 'o');
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work




    ▌ earlier question
      earlier answer

    ▌ skill frontend-design
    ╭─ Notice ─────────────────────────────────────────────────╮
    │ view: quiet — intermediate output and thinking are       │
    │ hidden                                                   │
    ╰────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  (* Ctrl+Up jumps to them like to any user message. *)
  H.key h (Key.ctrl 'o');
  H.key h (Key.ctrl 'o');
  H.key h { (Key.plain Up) with ctrl = true };
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work
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
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash        ↑ scrolled  ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.key h { (Key.plain Up) with ctrl = true };
  H.show h;
  [%expect
    {|
    prigh  new session                                     /work
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
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > ▏
    deepseek-flash        ↑ scrolled  ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work







    ╭─ Fork at (2) ────────────────────────────────────────────╮
    │ / ▏                                                      │
    │    hello                         #1                      │
    │ ▸* /skill:frontend-design make it bold  #2               │
    ╰──────────────────────────────────────────────────────────╯
    ── Enter selects · Esc closes ──────────────────────────────
    >
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ fork) (params ((at u2))) (tag Reload_messages))
    prigh  new session                                     /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands · Alt+Enter follow-up ──────────────
    > /skill:frontend-design make it bold▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                         /work


    ╭─ Warning ────────────────────────────────────────────────────────────────────╮
    │ MCP config: /work/.mcp.json: server "x": give a "command" (stdio) or a "url" │
    │ (http)                                                                       │
    ╰────────────────────────────────────────────────────────────── Esc dismisses ─╯
    ╭─ MCP servers (3) ────────────────────────────────────────────────────────────╮
    │ / ▏                                                                          │
    │ ▸  fs  ready, 2 tools  /home/u/.prigh/mcp.json                               │
    │    gh  failed: exited with 1: gh-mcp: command not found  /work/.mcp.json     │
    │    db  needs approval  /work/.mcp.json                                       │
    ╰──────────────────────────────────────────────────────────────────────────────╯
    ── Enter approves a server or lists its tools · Esc closes ─────────────────────
    >
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}];
  H.enter h;
  H.show h;
  [%expect
    {|
    prigh  new session                                                         /work







    ╭─ MCP: fs ────────────────────────────────────────────────────────────────────╮
    │ fs  2 tools  /home/u/.prigh/mcp.json                                         │
    │   mcp__fs__read_file  Read a file                                            │
    │   mcp__fs__list       List a directory                                       │
    ╰───────────────────────────────────────────────────────────────── Esc closes ─╯
    ── ↑↓ scroll · Esc closes · typing goes to the editor ──────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                         /work








    ╭─ Error ──────────────────────────────────────────────────────────────────────╮
    │ gh failed: exited with 1: gh-mcp: command not found; check its entry in      │
    │ /work/.mcp.json, then /mcp reconnect                                         │
    ╰────────────────────────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                         /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands ────────────────── approving db from /work/.mcp.json… ──
    > ▏
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                         /work






    ╭─ MCP servers (3) ────────────────────────────────────────────────────────────╮
    │ / ▏                                                                          │
    │ ▸  fs  ready, 2 tools  /home/u/.prigh/mcp.json                               │
    │    gh  failed: exited with 1: gh-mcp: command not found  /work/.mcp.json     │
    │    db  ready, 1 tool  /work/.mcp.json                                        │
    ╰──────────────────────────────────────────────────────────────────────────────╯
    ── Enter approves a server or lists its tools ──── approved db: ready, 1 tool ──
    >
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                                         /work








    ╭─ Error ──────────────────────────────────────────────────────────────────────╮
    │ approved db, but it failed: connection refused; check its entry in           │
    │ /work/.mcp.json, then /mcp reconnect                                         │
    ╰────────────────────────────────────────────────────────────── Esc dismisses ─╯
    ── /help · Tab commands · Alt+Enter follow-up · Ctrl+C twice quits ─────────────
    > draft▏
    deepseek-flash  think:off  mode:auto  view:normal     ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work


    ╭─ Warning ────────────────────────────────────────────────╮
    │ MCP config: /work/.mcp.json: server "x": give a          │
    │ "command" (stdio) or a "url" (http)                      │
    ╰────────────────────────────────────────── Esc dismisses ─╯
    ╭─ MCP servers ────────────────────────────────────────────╮
    │ fs  ready, 2 tools  /home/u/.prigh/mcp.json              │
    │ gh  failed: exited with 1: gh-mcp: command not found     │
    │ /work/.mcp.json                                          │
    │ db  needs approval  /work/.mcp.json                      │
    ╰───────────────────────────────────────────── Esc closes ─╯
    ── ↑↓ scroll · Esc closes · typing goes to the editor ──────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work










    ▌ earlier question
      earlier answer
    ── /help · Tab commands ───────── usage: /mcp [reconnect] ──
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
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
    prigh  new session                                     /work








    ╭─ /mcp ───────────────────────────────────────────────────╮
    │ /mcp [reconnect]  list MCP servers (Enter approves one   │
    │ or lists its tools); reconnect restarts failed ones      │
    ╰───────────────────────────────────────────── Esc closes ─╯
    ── ↑↓ scroll · Esc closes · typing goes to the editor ──────
    > ▏
    deepseek-flash  mode:auto         ctx ▱▱▱▱▱ 0.1%/1.0M  $0.01
    |}]
;;
