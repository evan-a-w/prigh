open! Core
open! Expect_test_helpers_core
open Prigh_ui
module H = Test_app.H

let run = Test_app.run
let connected () = Test_app.connected ~width:80 ~height:16 ()

let retry ?(command = "/commit -m Fix the parser") then_ : App.Reply_tag.t =
  Retry_in_editor { command; hint = ""; then_ }
;;

let status_json =
  {|{"root":"/work","branch":"main","head":"1a2b3c4d5e","upstream":"origin/main","ahead":1,"behind":0,"files":[]}|}
;;

let commit_json ?(subject = "Fix the parser") () =
  sprintf
    {|{"sha":"1a2b3c4d5e6f","subject":%S,"status":%s}|}
    subject
    status_json
;;

let generated_json ?(kind = "commit_message") ~gen_id text =
  sprintf
    {|{"gen_id":%S,"kind":%S,"text":%S,"model":"deepseek/deepseek-flash","cost_usd":0.0004}|}
    gen_id
    kind
    text
;;

let%expect_test "/commit's arguments" =
  List.iter
    [ "-m Fix the parser"
    ; "-m   \"Fix \\\"it\\\"\n\nBody \\\\ x\"  "
    ; "-m 'single \"quoted\"'"
    ; "-a -m Fix"
    ; "-am Fix"
    ; "--amend"
    ; "--amend -m"
    ; "--amend -m New subject"
    ; "-a --generate"
    ; "--generate --amend"
    ; ""
    ; "-a"
    ; "-m"
    ; "-m \"open"
    ; "-m \"closed\" then more"
    ; "-m Fix --generate"
    ; "--generate -m Fix"
    ; "fix the parser"
    ; "-x"
    ]
    ~f:(fun rest ->
      print_s
        [%sexp
          (rest : string)
        , (Commit_command.parse rest : (Commit_command.t, string) Result.t)]);
  [%expect
    {|
    ("-m Fix the parser" (
      Ok (
        (stage_tracked false)
        (amend         false)
        (message (Text "Fix the parser")))))
    ("-m   \"Fix \\\"it\\\"\n\nBody \\\\ x\"  " (
      Ok (
        (stage_tracked false)
        (amend         false)
        (message (Text "Fix \"it\"\n\nBody \\ x")))))
    ("-m 'single \"quoted\"'" (
      Ok (
        (stage_tracked false)
        (amend         false)
        (message (Text "single \"quoted\"")))))
    ("-a -m Fix" (
      Ok (
        (stage_tracked true)
        (amend         false)
        (message (Text Fix)))))
    ("-am Fix" (
      Ok (
        (stage_tracked true)
        (amend         false)
        (message (Text Fix)))))
    (--amend (
      Ok (
        (stage_tracked false)
        (amend         true)
        (message (Text "")))))
    ("--amend -m" (
      Ok (
        (stage_tracked false)
        (amend         true)
        (message (Text "")))))
    ("--amend -m New subject" (
      Ok (
        (stage_tracked false)
        (amend         true)
        (message (Text "New subject")))))
    ("-a --generate" (
      Ok (
        (stage_tracked true)
        (amend         false)
        (message       Generate))))
    ("--generate --amend" (
      Ok (
        (stage_tracked false)
        (amend         true)
        (message       Generate))))
    ("" (
      Error
      "usage: /commit [-a] [--amend] -m MESSAGE, or /commit [-a] --generate to have the model draft the message"))
    (-a (
      Error
      "usage: /commit [-a] [--amend] -m MESSAGE, or /commit [-a] --generate to have the model draft the message"))
    (-m (Error "/commit -m needs a message: /commit -m \"Fix the parser\""))
    ("-m \"open" (
      Error "the message's closing \" is missing: /commit -m \"Fix the parser\""))
    ("-m \"closed\" then more" (
      Error
      "there is text after the message's closing quote: quote all of the message or none of it"))
    ("-m Fix --generate" (
      Ok (
        (stage_tracked false)
        (amend         false)
        (message (Text "Fix --generate")))))
    ("--generate -m Fix" (
      Error "/commit takes -m MESSAGE or --generate, not both"))
    ("fix the parser" (
      Error "put the message after -m: /commit -m \"fix the parser\""))
    (-x (
      Error
      "unknown /commit flag -x; usage: /commit [-a] [--amend] -m MESSAGE, or /commit [-a] --generate to have the model draft the message"))
    |}];
  (* What [command] writes, [parse] reads back. *)
  List.iter
    [ "Fix the parser"; "Say \"hi\"\n\n- a \\ b\n- c"; "" ]
    ~f:(fun message ->
      let command = Commit_command.command ~amend:false message in
      let back =
        Commit_command.parse (String.chop_prefix_exn command ~prefix:"/commit ")
      in
      print_s
        [%sexp (command : string), (back : (Commit_command.t, string) Result.t)]);
  print_endline (Commit_command.command ~amend:true "Fix");
  [%expect
    {|
    ("/commit -m \"Fix the parser\"" (
      Ok (
        (stage_tracked false)
        (amend         false)
        (message (Text "Fix the parser")))))
    ("/commit -m \"Say \\\"hi\\\"\n\n- a \\\\ b\n- c\"" (
      Ok (
        (stage_tracked false)
        (amend         false)
        (message (Text "Say \"hi\"\n\n- a \\ b\n- c")))))
    ("/commit -m \"\"" (
      Error "/commit -m needs a message: /commit -m \"Fix the parser\""))
    /commit --amend -m "Fix"
    |}];
  List.iter
    [ ""; "-"; "-a "; "-a --gen"; "--generate "; "-m "; "-m Fix"; "fix" ]
    ~f:(fun rest ->
      print_s
        [%sexp
          (rest : string)
        , (Commit_command.complete rest : (int * (string * string) list) option)]);
  [%expect
    {|
    ("" ((
      0 (
        (-a "stage every change to tracked files first")
        (-m "MESSAGE: the commit message (the rest of the line, or quoted)")
        (--generate "have the model draft the message from the staged changes")
        (--amend "replace the last commit (no message: keep its own)")))))
    (- ((
      0 (
        (-a "stage every change to tracked files first")
        (-m "MESSAGE: the commit message (the rest of the line, or quoted)")
        (--generate "have the model draft the message from the staged changes")
        (--amend "replace the last commit (no message: keep its own)")))))
    ("-a " ((
      3 (
        (-m "MESSAGE: the commit message (the rest of the line, or quoted)")
        (--generate "have the model draft the message from the staged changes")
        (--amend "replace the last commit (no message: keep its own)")))))
    ("-a --gen" ((
      3 (
        (-m "MESSAGE: the commit message (the rest of the line, or quoted)")
        (--generate "have the model draft the message from the staged changes")
        (--amend "replace the last commit (no message: keep its own)")))))
    ("--generate " ((
      11 (
        (-a "stage every change to tracked files first")
        (--amend "replace the last commit (no message: keep its own)")))))
    ("-m " ())
    ("-m Fix" ())
    (fix ())
    |}]
;;

let%expect_test "/commit -m: committed, logged; a refusal puts the command back"
  =
  let h = connected () in
  run h "/commit -m Fix the parser";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ git_commit)
      (params ((message "Fix the parser")))
      (tag (
        Retry_in_editor
        (command "/commit -m Fix the parser")
        (hint    "")
        (then_ (Committed (amend false))))))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────── committing… ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.reply h (retry (Committed { amend = false })) (commit_json ());
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      committed 1a2b3c4 Fix the parser on main
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  let command = "/commit -m \"Two\n\nlines\"" in
  run h command;
  H.reply_error
    h
    (retry ~command (Committed { amend = false }))
    "nothing is staged to commit: tick the files to commit (or stage them with \
     git add)";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ git_commit)
      (params ((message "Two\n\nlines")))
      (tag (
        Retry_in_editor
        (command "/commit -m \"Two\n\nlines\"")
        (hint    "")
        (then_ (Committed (amend false))))))


      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      committed 1a2b3c4 Fix the parser on main
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ nothing is staged to commit: tick the files to commit (or stage them with    │
    │ git add)                                                                     │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /commit -m "Two

      lines"▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/commit -a stages the tracked changes first; --amend" =
  let h = connected () in
  run h "/commit -a -m Fix the parser";
  [%expect
    {|
    (Rpc
      (method_ git_stage)
      (params ((paths ()) (tracked true)))
      (tag (
        Retry_in_editor
        (command "/commit -a -m Fix the parser")
        (hint    "")
        (then_ (
          Commit_staged
          (command "/commit -a -m Fix the parser")
          (commit (
            (stage_tracked true)
            (amend         false)
            (message (Text "Fix the parser")))))))))
    |}];
  let commit =
    { Commit_command.stage_tracked = true
    ; amend = false
    ; message = Text "Fix the parser"
    }
  in
  let command = "/commit -a -m Fix the parser" in
  H.reply h (retry ~command (Commit_staged { command; commit })) status_json;
  [%expect
    {|
    (Rpc
      (method_ git_commit)
      (params ((message "Fix the parser")))
      (tag (
        Retry_in_editor
        (command "/commit -a -m Fix the parser")
        (hint    "")
        (then_ (Committed (amend false))))))
    |}];
  H.reply h (retry ~command (Committed { amend = false })) (commit_json ());
  run h "/commit --amend";
  [%expect
    {|
    (Rpc
      (method_ git_commit)
      (params (
        (message "")
        (amend   true)))
      (tag (
        Retry_in_editor
        (command "/commit --amend")
        (hint    "")
        (then_ (Committed (amend true))))))
    |}];
  H.reply
    h
    (retry ~command:"/commit --amend" (Committed { amend = true }))
    (commit_json ~subject:"Fix the parser properly" ());
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      committed 1a2b3c4 Fix the parser on main
      amended 1a2b3c4 Fix the parser properly on main
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/commit and /push usage errors say what to type" =
  let h = connected () in
  List.iter
    [ "/commit"; "/commit fix it"; "/commit -x"; "/push origin" ]
    ~f:(fun command ->
      run h command;
      H.show h;
      H.step ~quiet:true h (Intent Kill_to_start));
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ usage: /commit [-a] [--amend] -m MESSAGE, or /commit [-a] --generate to have │
    │ the model draft the message                                                  │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ▸ /commit [-a] [-m MSG|--generate]  commit the staged changes of the session's …
    ────────────────────────────────────────────────────────────────────────────────
    > /commit▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────── put the message after -m: /commit -m "fix it" ───
    > /commit fix it▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ unknown /commit flag -x; usage: /commit [-a] [--amend] -m MESSAGE, or        │
    │ /commit [-a] --generate to have the model draft the message                  │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /commit -x▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01








      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ /push  push the session's worktree's branch to its upstream (set to the remot…
    ─── usage: /push (it pushes the session's worktree's branch to its upstream) ───
    > /push▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed
    |}]
;;

let%expect_test "/commit completes its flags" =
  let h = connected () in
  H.keys h "/commit ";
  H.show h;
  H.keys h "-a --g";
  H.show h;
  H.key h (Key.plain Tab);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ -a          stage every change to tracked files first
      -m          MESSAGE: the commit message (the rest of the line, or quoted)
      --generate  have the model draft the message from the staged changes
      --amend     replace the last commit (no message: keep its own)
    ────────────────────────────────────────────────────────────────────────────────
    > /commit ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  Tab accepts · Enter as typed · Esc hides








      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ▸ --generate  have the model draft the message from the staged changes
    ────────────────────────────────────────────────────────────────────────────────
    > /commit -a --g▏
    deepseek-flash  think:off  ctx:0.1%/1.0M  Tab/Enter accept · Alt+S as typed









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────
    > /commit -a --generate▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/commit --generate: the draft streams into a panel, then the editor holds \
     the command; Enter commits"
  =
  let h = connected () in
  run h "/commit --generate";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ generate_text)
      (params (
        (gen_id gen-1)
        (kind   commit_message)))
      (tag (Commit_drafted gen-1)))






      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ commit message ─────────────────────────────────────────────────────────────┐
    │ drafting from the staged changes… · Esc stops                                │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  drafting the commit message · Esc stops
    |}];
  H.event h (Generate_delta { gen_id = "gen-1"; delta = "Fix the \"parser\"" });
  H.event h (Generate_delta { gen_id = "gen-1"; delta = "\n\nIt read past" });
  (* Someone else's draft is not ours. *)
  H.event h (Generate_delta { gen_id = "gen-9"; delta = "nope" });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ commit message ─────────────────────────────────────────────────────────────┐
    │ Fix the "parser"                                                             │
    │                                                                              │
    │ It read past                                                                 │
    │ drafting from the staged changes… · Esc stops                                │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  ctx:0.1%/1.0M  $0.01  drafting the commit message · Esc stops
    |}];
  let message = "Fix the \"parser\"\n\nIt read past the end." in
  H.reply h (Commit_drafted "gen-1") (generated_json ~gen_id:"gen-1" message);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ commit message drafted by deepseek/deepseek-flash ($0.0004): Enter commits   │
    │ it, or edit it first                                                         │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /commit -m "Fix the \"parser\"

      It read past the end."▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ git_commit)
      (params ((message "Fix the \"parser\"\n\nIt read past the end.")))
      (tag (
        Retry_in_editor
        (command "/commit -m \"Fix the \\\"parser\\\"\n\nIt read past the end.\"")
        (hint "")
        (then_ (Committed (amend false))))))
    |}]
;;

let%expect_test
    "/commit -a --generate: Esc stops the draft; text typed meanwhile is one \
     undo away; failures"
  =
  let h = connected () in
  run h "/commit -a --amend --generate";
  let command = "/commit -a --amend --generate" in
  let commit =
    { Commit_command.stage_tracked = true; amend = true; message = Generate }
  in
  [%expect
    {|
    (Rpc
      (method_ git_stage)
      (params ((paths ()) (tracked true)))
      (tag (
        Retry_in_editor
        (command "/commit -a --amend --generate")
        (hint    "")
        (then_ (
          Commit_staged
          (command "/commit -a --amend --generate")
          (commit (
            (stage_tracked true)
            (amend         true)
            (message       Generate))))))))
    |}];
  H.reply h (retry ~command (Commit_staged { command; commit })) status_json;
  [%expect
    {|
    (Rpc
      (method_ generate_text)
      (params (
        (gen_id gen-1)
        (kind   commit_message)))
      (tag (Commit_drafted gen-1)))
    |}];
  H.event h (Generate_delta { gen_id = "gen-1"; delta = "Half a" });
  H.esc h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ generate_cancel) (params ((gen_id gen-1))) (tag Ignore))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────── stopped drafting the commit message ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  (* The backend answers the cancelled request: nothing more to say. *)
  H.reply_error h (Commit_drafted "gen-1") "cancelled";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────── stopped drafting the commit message ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/commit --generate";
  H.keys h "meanwhile";
  H.reply
    h
    (Commit_drafted "gen-2")
    (generated_json ~gen_id:"gen-2" "Add the thing");
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ generate_text)
      (params (
        (gen_id gen-2)
        (kind   commit_message)))
      (tag (Commit_drafted gen-2)))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ─────────────────────────────────────────────────────────────────────┐
    │ commit message drafted by deepseek/deepseek-flash ($0.0004): Enter commits   │
    │ it, or edit it first; Ctrl+_ brings back what you had typed                  │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > /commit -m "Add the thing"▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.step h (Intent Undo);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ────────────────────────────────────────────────────────────────────────────────
    > meanwhile▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.step ~quiet:true h (Intent Kill_to_start);
  run h "/commit --generate";
  H.reply_error
    h
    (Commit_drafted "gen-3")
    "nothing is staged to describe: tick the files to commit (or stage them \
     with git add)";
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ generate_text)
      (params (
        (gen_id gen-3)
        (kind   commit_message)))
      (tag (Commit_drafted gen-3)))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ could not draft the commit message: nothing is staged to describe: tick the  │
    │ files to commit (or stage them with git add)                                 │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "/push: pushed, or the backend's reason" =
  let h = connected () in
  run h "/push";
  H.show h;
  [%expect
    {|
    (Rpc (method_ git_push) (params ()) (tag Pushed))









      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ─────────────────────────────────────────────────────────────────── pushing… ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  H.reply
    h
    Pushed
    (sprintf {|{"remote":"origin","branch":"main","status":%s}|} status_json);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      pushed main to origin
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}];
  run h "/push";
  H.reply_error
    h
    Pushed
    "git push origin main was refused: ! [rejected] main -> main (fetch first) \
     (the remote has commits this branch does not: pull first, then push)";
  H.show h;
  [%expect
    {|
    (Rpc (method_ git_push) (params ()) (tag Pushed))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
      pushed main to origin
    ┌─ Error ──────────────────────────────────────────────────────────────────────┐
    │ git push origin main was refused: ! [rejected] main -> main (fetch first)    │
    │ (the remote has commits this branch does not: pull first, then push)         │
    └────────────────────────────────────────────────────────────── Esc dismisses ─┘
    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let defaults_json =
  {|{"repo":{"host":"github.com","kind":"github","owner":"evan-a-w","name":"prigh","remote":"origin","url":"https://github.com/evan-a-w/prigh"},"head":"feat/x","base":"main","title":"Add x","body":"- Add x\n- Test x","commits":["Add x","Test x"],"pushed":true}|}
;;

let%expect_test
    "/pr new --generate: the draft streams into the form; Ctrl+_ undoes it; \
     Ctrl+G again, Esc stops, Esc closes"
  =
  let h = Test_app.connected ~width:80 ~height:22 () in
  run h "/pr new --generate";
  [%expect
    {|
    (Rpc
      (method_ pr_defaults)
      (params ())
      (tag (Pr_form_defaults (title ()) (generate true))))
    |}];
  H.reply h (Pr_form_defaults { title = None; generate = true }) defaults_json;
  [%expect
    {|
    (Rpc
      (method_ generate_text)
      (params (
        (gen_id gen-1)
        (kind   pr)
        (base   main)))
      (tag (Pr_drafted gen-1)))
    |}];
  H.event h (Generate_delta { gen_id = "gen-1"; delta = "Add x to the " });
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Add x to the▏                                                         │
    │ body                                                                         │
    │ base   main                                                                  │
    │ [ ] draft                                                                    │
    │ [ ] push feat/x first  (it is on the remote)                                 │
    │ feat/x → main · 2 commits                                                    │
    │ drafting with the model… (Esc stops)                                         │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Enter creates · Esc cancels                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────── reading the branch… ───
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  drafting… · Esc stops
    |}];
  H.event h (Generate_delta { gen_id = "gen-1"; delta = "parser\n\nIt adds" });
  (* Typing waits for the draft, and so does Enter. *)
  H.keys h "zz";
  H.enter h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Add x to the parser▏                                                  │
    │ body   It adds                                                               │
    │ base   main                                                                  │
    │ [ ] draft                                                                    │
    │ [ ] push feat/x first  (it is on the remote)                                 │
    │ feat/x → main · 2 commits                                                    │
    │ drafting with the model… (Esc stops)                                         │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Enter creates · Esc cancels                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  drafting… · Esc stops
    |}];
  H.reply
    h
    (Pr_drafted "gen-1")
    (generated_json
       ~kind:"pr"
       ~gen_id:"gen-1"
       "Add x to the parser\n\nIt adds x.\n\n- Tested");
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Add x to the parser▏                                                  │
    │ body   It adds x.                                                            │
    │                                                                              │
    │        - Tested                                                              │
    │ base   main                                                                  │
    │ [ ] draft                                                                    │
    │ [ ] push feat/x first  (it is on the remote)                                 │
    │ feat/x → main · 2 commits                                                    │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Ctrl+_ undoes the draft · Enter creates · Esc cancels    │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.step h (Intent Undo);
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Add x▏                                                                │
    │ body   - Add x                                                               │
    │        - Test x                                                              │
    │ base   main                                                                  │
    │ [ ] draft                                                                    │
    │ [ ] push feat/x first  (it is on the remote)                                 │
    │ feat/x → main · 2 commits                                                    │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Ctrl+_ undoes the draft · Enter creates · Esc cancels    │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.key h (Key.ctrl 'g');
  [%expect
    {|
    (Rpc
      (method_ generate_text)
      (params (
        (gen_id gen-2)
        (kind   pr)
        (base   main)))
      (tag (Pr_drafted gen-2)))
    |}];
  H.event h (Generate_delta { gen_id = "gen-2"; delta = "Partial" });
  H.esc h;
  H.show h;
  [%expect
    {|
    (Rpc (method_ generate_cancel) (params ((gen_id gen-2))) (tag Ignore))





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Partial▏                                                              │
    │ body                                                                         │
    │ base   main                                                                  │
    │ [ ] draft                                                                    │
    │ [ ] push feat/x first  (it is on the remote)                                 │
    │ feat/x → main · 2 commits                                                    │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Ctrl+_ undoes the draft · Enter creates · Esc cancels    │
    └──────────────────────────────────────────────────────────────────────────────┘
    ────────────────────────────────────────────────────────────────────────────────
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  H.esc h;
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────── no pull request created ───
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "/pr new --generate TITLE keeps the title; a failed draft keeps the text"
  =
  let h = Test_app.connected ~width:80 ~height:22 () in
  run h "/pr new --generate Make it faster";
  H.reply
    ~quiet:true
    h
    (Pr_form_defaults { title = Some "Make it faster"; generate = true })
    defaults_json;
  H.event h (Generate_delta { gen_id = "gen-1"; delta = "Drafted title\nBody" });
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ pr_defaults)
      (params ())
      (tag (Pr_form_defaults (title ("Make it faster")) (generate true))))




      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Make it faster▏                                                       │
    │ body   Body                                                                  │
    │ base   main                                                                  │
    │ [ ] draft                                                                    │
    │ [ ] push feat/x first  (it is on the remote)                                 │
    │ feat/x → main · 2 commits                                                    │
    │ drafting with the model… (Esc stops)                                         │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Enter creates · Esc cancels                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────── reading the branch… ───
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  drafting… · Esc stops
    |}];
  H.reply_error
    h
    (Pr_drafted "gen-1")
    "generate_model: unknown model \"nope\"; did you mean: deepseek-flash";
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ New pull request in evan-a-w/prigh ─────────────────────────────────────────┐
    │ title  Make it faster▏                                                       │
    │ body   - Add x                                                               │
    │        - Test x                                                              │
    │ base   main                                                                  │
    │ [ ] draft                                                                    │
    │ [ ] push feat/x first  (it is on the remote)                                 │
    │ feat/x → main · 2 commits                                                    │
    │ could not draft it: generate_model: unknown model "nope"; did you mean:      │
    │ deepseek-flash                                                               │
    │ Tab/↑↓ fields · Space toggles · Ctrl+J new line in the body · Ctrl+G drafts  │
    │ it with the model · Enter creates · Esc cancels                              │
    └──────────────────────────────────────────────────────────────────────────────┘
    ──────────────────────────────────────────────────────── reading the branch… ───
    >
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  Enter creates · Esc cancels
    |}];
  (* Closing the form while drafting stops the draft. *)
  H.key h (Key.ctrl 'g');
  H.key h (Key.ctrl 'c');
  H.mode h;
  [%expect
    {|
    (Rpc
      (method_ generate_text)
      (params (
        (gen_id gen-2)
        (kind   pr)
        (base   main)))
      (tag (Pr_drafted gen-2)))
    (Rpc (method_ generate_cancel) (params ((gen_id gen-2))) (tag Ignore))
    editing
    |}]
;;

let%expect_test "switching sessions stops the drafts" =
  let h = Test_app.connected ~width:80 ~height:22 () in
  run h "/commit --generate";
  run h "/pr new --generate";
  H.reply h (Pr_form_defaults { title = None; generate = true }) defaults_json;
  H.reply h Reload_messages {|null|};
  H.mode h;
  print_s [%sexp (h.model.commit_draft : Commit_draft.t option)];
  (* A late delta finds nothing to update. *)
  H.event h (Generate_delta { gen_id = "gen-1"; delta = "late" });
  H.show h;
  [%expect
    {|
    (Rpc
      (method_ generate_text)
      (params (
        (gen_id gen-1)
        (kind   commit_message)))
      (tag (Commit_drafted gen-1)))
    (Rpc
      (method_ pr_defaults)
      (params ())
      (tag (Pr_form_defaults (title ()) (generate true))))
    (Rpc
      (method_ generate_text)
      (params (
        (gen_id gen-2)
        (kind   pr)
        (base   main)))
      (tag (Pr_drafted gen-2)))
    (Rpc (method_ generate_cancel) (params ((gen_id gen-1))) (tag Ignore))
    (Rpc (method_ generate_cancel) (params ((gen_id gen-2))) (tag Ignore))
    (Rpc (method_ get_state) (params ()) (tag Initial_state))
    (Rpc (method_ get_messages) (params ()) (tag Initial_messages))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    editing
    ()



















    ────────────────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  view:normal  ctx:0.1%/1.0M  $0.01
    |}]
;;
