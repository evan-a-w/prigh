open! Core
module H = Harness

(* The repository's worktrees: the sidebar's section, its dialogs and
   [/worktree]. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let toasts h = H.text h ~selector:".toast"
let dialog h = H.text h ~selector:".modal"
let section h = H.text h ~selector:".worktree"

let worktree
      ?(branch = Some "main")
      ?(main = false)
      ?(changes = 0)
      ?(ahead = 0)
      ?(behind = 0)
      ?(current = false)
      ?(prunable = false)
      ?(sessions = "[]")
      path
  =
  sprintf
    {|{"path":"%s","branch":%s,"head":"0123456789abcdef","main":%b,"managed":%b,"locked":false,"prunable":%b,"changes":%d,"upstream":%s,"ahead":%d,"behind":%d,"current":%b,"sessions":%s}|}
    path
    (Option.value_map branch ~default:"null" ~f:(sprintf "%S"))
    main
    (not main)
    prunable
    changes
    (Option.value_map branch ~default:"null" ~f:(sprintf "\"origin/%s\""))
    ahead
    behind
    current
    sessions
;;

let dir = "/home/u/.prigh/worktrees/work"
let login_path = dir ^ "/feat-login"
let docs_path = dir ^ "/docs"

let listing ?(current = "/work") () =
  sprintf
    {|{"root":"/work","project_dir":"%s","worktrees":[%s]}|}
    dir
    (String.concat
       ~sep:","
       [ worktree
           "/work"
           ~main:true
           ~current:(String.equal current "/work")
           ~sessions:
             {|[{"id":"s1","path":"/sessions/s1.jsonl","name":null,"live":true,"running":false}]|}
       ; worktree
           login_path
           ~branch:(Some "feat/login")
           ~changes:3
           ~ahead:2
           ~behind:1
           ~current:(String.equal current login_path)
           ~sessions:
             {|[{"id":"s2","path":"/sessions/s2.jsonl","name":"Login fix","live":true,"running":true}]|}
       ; worktree docs_path ~branch:(Some "docs")
       ; worktree (dir ^ "/old") ~branch:(Some "old") ~prunable:true
       ])
;;

let sessions =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json
           ~name:"Main work"
           ~cwd:"/work"
           ~repo:("/work", "/work", Some "main")
           ~live:true
           "s1"
       ; H.session_json
           ~name:"Login fix"
           ~cwd:login_path
           ~repo:("/work", login_path, Some "feat/login")
           ~live:true
           ~running:true
           "s2"
       ])
;;

let listed () =
  let h = H.create ~sessions () in
  H.reply h "list_worktrees" (listing ());
  h
;;

let state ?(session = "s1") ?(cwd = "/work") ?(branch = "main") () =
  H.state_json
    ~fields:
      [ "session_id", `String session
      ; "cwd", `String cwd
      ; "git_branch", `String branch
      ]
    ()
;;

let state_event h ?session ?cwd ?branch () =
  H.event
    h
    (sprintf {|{"event":"state","state":%s}|} (state ?session ?cwd ?branch ()))
;;

let%expect_test "not a repository: no listing, no error" =
  let h = H.create ~sessions () in
  H.fail
    h
    "list_worktrees"
    "not a git repository: /work (git init starts one, or /cd into one)";
  section h;
  toasts h;
  (* The same place is not asked again at every event. *)
  state_event h ();
  [%expect
    {|
    (feat/login) (New session in this worktree) (Remove this worktree (/worktree rm))
    (Login fix 2m ago) (Archive (A)) (Delete session (Delete))
    |}]
;;

let%expect_test "the listing follows the directory and resets with the session" =
  let h = listed () in
  (* Another directory of this session: listed again, the old one showing
     meanwhile with the lamp. *)
  state_event h ~cwd:login_path ~branch:"feat/login" ();
  H.elements h ~selector:".worktree.here";
  H.reply h "list_worktrees" (listing ~current:login_path ());
  H.text h ~selector:".worktree.here";
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees
       (place  "s1\
              \nbackend\
              \n/home/u/.prigh/worktrees/work/feat-login")
       (purpose (Sidebar 2)))))
    (feat/login 3 changed ↑2 ↓1) (New session in this worktree)
    (Login fix 2m ago) (Archive (A)) (Delete session (Delete))
    |}];
  (* Another session: the old one's listing (its changes, sync) goes; the
     tree keeps the sessions' worktrees. *)
  state_event h ~session:"s2" ~cwd:"/elsewhere" ();
  section h;
  [%expect
    {|
    (Set_url_session s2)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s2.jsonl) (limit 60)))
     (tag (Page (path /sessions/s2.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s2\
                            \nbackend\
                            \n/elsewhere") (purpose (Sidebar 3)))))
    (feat/login) (New session in this worktree)
    (Login fix 2m ago) (Archive (A)) (Delete session (Delete))
    |}];
  H.reply h "list_worktrees" (listing ());
  section h;
  [%expect
    {|
    (feat/login 3 changed ↑2 ↓1) (New session in this worktree) (Remove this worktree (/worktree rm))
    (Login fix 2m ago) (Archive (A)) (Delete session (Delete))
    (docs) (New session in this worktree) (Remove this worktree (/worktree rm))
    (old gone) (Remove this worktree (/worktree rm))
    |}];
  H.elements h ~selector:".worktree";
  [%expect
    {|
    <div class="dirty here worktree">
    <div class="worktree">
    <div class="gone worktree">
    |}]
;;

let%expect_test "sessions moving in or out list the worktrees again" =
  let h = listed () in
  (* The same sessions where they were: nothing. *)
  H.act h (Event Sessions_changed);
  H.reply h "list_sessions" sessions;
  [%expect {| (Rpc (method_ list_sessions) (params ()) (tag Sessions)) |}];
  H.act h (Event Sessions_changed);
  H.reply
    h
    "list_sessions"
    (sprintf
       "[%s]"
       (H.session_json ~name:"Main work" ~cwd:"/work" ~live:true "s1"
        ^ ","
        ^ H.session_json ~name:"Login fix" ~cwd:docs_path "s2"));
  [%expect
    {|
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    |}]
;;

let%expect_test "acting as another user forgets the worktrees" =
  let h = listed () in
  H.quiet h (fun () -> state_event h ~cwd:login_path ());
  H.act
    h
    (Reply
       ( User_switched
       , Ok
           (Jsonaf.of_string
              {|{"client_id":"c1","namespace":"bob","user":"alice"}|}) ));
  section h;
  H.quiet h (fun () -> H.reply h "list_worktrees" (listing ()));
  section h;
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    |}];
  (* Bob's session is listed afresh. *)
  H.reply h "get_state" (state ~session:"b1" ());
  [%expect
    {|
    (Set_url_session b1)
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/b1.jsonl) (limit 60)))
     (tag (Page (path /sessions/b1.jsonl) (before ()))))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "b1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 3)))))
    |}]
;;

let%expect_test "removing asks first; Esc sends nothing; changes need force" =
  let h = listed () in
  H.click h ~selector:".worktree-row .delete" ~nth:0;
  dialog h;
  [%expect
    {|
    (Ask_remove_worktree /home/u/.prigh/worktrees/work/feat-login)
    (Focus dialog)
    Remove worktree with changes?
    (Close (Esc))
    Remove the worktree feat/login at /home/u/.prigh/worktrees/work/feat-login; it has 3 uncommitted changes; sessions working there: Login fix? (the branch stays)
    Enter removes, Esc cancels (Cancel) (Discard changes and remove) (Remove)
    |}];
  H.key h "Escape" ~target:Control;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  (* Removing without force is refused by the backend: the dialog says how to
     force it. *)
  H.click h ~selector:".worktree-row .delete" ~nth:0;
  H.key h "Enter" ~target:Page;
  H.text h ~selector:".modal-buttons";
  H.fail
    h
    "remove_worktree"
    (sprintf
       "%s has uncommitted changes (3 files): commit or stash them first, or \
        force the removal"
       login_path);
  dialog h;
  [%expect
    {|
    (Ask_remove_worktree /home/u/.prigh/worktrees/work/feat-login)
    (Focus dialog)
    Dialog_accept
    (Rpc (method_ remove_worktree)
     (params ((path /home/u/.prigh/worktrees/work/feat-login)))
     (tag (Worktree_removed (target feat/login) (force false))))
    Enter removes, Esc cancels (Cancel) (Discard changes and remove) (Removing…)
    Remove worktree with changes?
    (Close (Esc))
    Remove the worktree feat/login at /home/u/.prigh/worktrees/work/feat-login; it has 3 uncommitted changes; sessions working there: Login fix? (the branch stays)
    /home/u/.prigh/worktrees/work/feat-login has uncommitted changes (3 files): commit or stash them first, or force the removal: “Discard changes and remove” removes it anyway.
    Enter removes, Esc cancels (Cancel) (Discard changes and remove) (Remove)
    |}];
  H.click h ~selector:".modal .btn.danger";
  H.reply
    h
    "remove_worktree"
    (sprintf
       {|{"worktree":%s}|}
       (worktree login_path ~branch:(Some "feat/login")));
  dialog h;
  toasts h;
  [%expect
    {|
    (Remove_worktree (force true))
    (Rpc (method_ remove_worktree)
     (params ((path /home/u/.prigh/worktrees/work/feat-login) (force true)))
     (tag (Worktree_removed (target feat/login) (force true))))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 5260))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    Removed the worktree feat/login at /home/u/.prigh/worktrees/work/feat-login (the branch stays)
    |}]
;;

let%expect_test "a clean worktree is removed with Enter" =
  let h = listed () in
  H.click h ~selector:".worktree-row .delete" ~nth:1;
  dialog h;
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Ask_remove_worktree /home/u/.prigh/worktrees/work/docs)
    (Focus dialog)
    Remove worktree?
    (Close (Esc))
    Remove the worktree docs at /home/u/.prigh/worktrees/work/docs? (the branch stays)
    Enter removes, Esc cancels (Cancel) (Remove)
    Dialog_accept
    (Rpc (method_ remove_worktree)
     (params ((path /home/u/.prigh/worktrees/work/docs)))
     (tag (Worktree_removed (target docs) (force false))))
    |}];
  (* Closed while it works: the backend's answer is a toast. *)
  H.key h "Escape" ~target:Control;
  H.fail
    h
    "remove_worktree"
    (sprintf
       "%s is locked: git worktree unlock %s, or force the removal"
       docs_path
       docs_path);
  toasts h;
  print_endline (H.model h).draft;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    /home/u/.prigh/worktrees/work/docs is locked: git worktree unlock /home/u/.prigh/worktrees/work/docs, or force the removal: /worktree rm -f docs removes it anyway. (Dismiss)
    /worktree rm -f docs
    |}]
;;

let%expect_test
    "the new worktree dialog: Enter creates and moves there, Esc sends nothing"
  =
  let h = listed () in
  H.act h (New_worktree { repo = None });
  dialog h;
  H.show h ~selector:"#worktree-base";
  [%expect
    {|
    (Focus worktree-branch)
    New worktree
    (Close (Esc))
    Branch [] A new branch, or an existing one to check out
    Base [] Where a new branch starts (empty: main, where this session is)
    Created in ~/.prigh/worktrees/work, then this session works there.
    Enter creates, Esc cancels (Cancel) (Create and switch)
    <input id="worktree-base"
           type="text"
           placeholder="main"
           autocomplete="off"
           autocapitalize="off"
           spellcheck="false"
           class="text-input"
           #value=""
           @on_input/>
    |}];
  H.key h "Escape" ~target:Field;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  H.act h (New_worktree { repo = None });
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".dialog-error";
  [%expect
    {|
    (Focus worktree-branch)
    Dialog_accept
    Type a branch name (Esc cancels).
    |}];
  H.input h ~selector:"#worktree-branch" "feat/search";
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".modal-buttons";
  [%expect
    {|
    (New_worktree_input (field Branch) (text feat/search))
    Dialog_accept
    (Rpc (method_ create_worktree) (params ((branch feat/search)))
     (tag
      (Worktree_created (command "/worktree new feat/search")
       (new_session false))))
    Enter creates, Esc cancels (Cancel) (Creating…)
    |}];
  H.reply
    h
    "create_worktree"
    (sprintf
       {|{"worktree":%s,"new_branch":true}|}
       (worktree (dir ^ "/feat-search") ~branch:(Some "feat/search")));
  dialog h;
  toasts h;
  [%expect
    {|
    (Focus editor)
    (Expire_toast (id 0) (after_ms 4620))
    (Rpc (method_ set_cwd)
     (params ((path /home/u/.prigh/worktrees/work/feat-search)))
     (tag
      (Worktree_entered
       "Working in the worktree feat/search (/home/u/.prigh/worktrees/work/feat-search)")))
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    Worktree feat/search at /home/u/.prigh/worktrees/work/feat-search (new branch)
    |}];
  H.reply h "set_cwd" (sprintf {|{"cwd":"%s/feat-search"}|} dir);
  toasts h;
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 4660))
    Worktree feat/search at /home/u/.prigh/worktrees/work/feat-search (new branch)
    Working in the worktree feat/search (/home/u/.prigh/worktrees/work/feat-search)
    |}];
  (* With a base; a refusal stays in the dialog, to fix. *)
  H.act h (New_worktree { repo = None });
  H.input h ~selector:"#worktree-branch" "fix/crash";
  H.input h ~selector:"#worktree-base" "release-1";
  H.key h "Enter" ~target:Field;
  H.fail
    h
    "create_worktree"
    "unknown base release-1: a branch, tag or commit (git branch -a lists them)";
  dialog h;
  [%expect
    {|
    (Focus worktree-branch)
    (New_worktree_input (field Branch) (text fix/crash))
    (New_worktree_input (field Base) (text release-1))
    Dialog_accept
    (Rpc (method_ create_worktree) (params ((branch fix/crash) (base release-1)))
     (tag
      (Worktree_created (command "/worktree new fix/crash release-1")
       (new_session false))))
    New worktree
    (Close (Esc))
    Branch [fix/crash] A new branch, or an existing one to check out
    Base [release-1] Where a new branch starts (empty: main, where this session is)
    Created in ~/.prigh/worktrees/work, then this session works there.
    Unknown base release-1: a branch, tag or commit (git branch -a lists them).
    Enter creates, Esc cancels (Cancel) (Create and switch)
    |}]
;;

let%expect_test "/worktree picks one: Enter, Alt+Enter, Delete" =
  let h = listed () in
  run h "/worktree";
  dialog h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /worktree) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose Picker))))
    Worktrees
    (Close (Esc))
    Loading…
    Esc closes
    |}];
  H.reply h "list_worktrees" (listing ());
  dialog h;
  [%expect
    {|
    (Focus picker-input)
    Worktrees of /work
    (Close (Esc))
    []
    main /work · main worktree · here · 1 session ✓
    feat/login /home/u/.prigh/worktrees/work/feat-login · 3 changed · ↑2 ↓1 · 1 session
    docs /home/u/.prigh/worktrees/work/docs
    old /home/u/.prigh/worktrees/work/old · gone
    Enter works there, Alt+Enter in a new session, Delete removes, Esc closes
    |}];
  H.key h "ArrowDown" ~target:Field;
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    (Dialog_move 1)
    Dialog_accept
    (Focus editor)
    (Rpc (method_ set_cwd)
     (params ((path /home/u/.prigh/worktrees/work/feat-login)))
     (tag
      (Worktree_entered
       "Working in the worktree feat/login (/home/u/.prigh/worktrees/work/feat-login)")))
    |}];
  run h "/worktree list";
  H.reply h "list_worktrees" (listing ());
  H.type_ h "";
  H.act h (Picker_query "docs");
  H.key h "Enter" ~alt:true ~target:Field;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree list") (max 500))) (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose Picker))))
    (Focus picker-input)
    (Open_worktree (path /home/u/.prigh/worktrees/work/docs) (new_session true))
    (Focus editor)
    (Rpc (method_ new_session) (params ())
     (tag
      (Worktree_session (path /home/u/.prigh/worktrees/work/docs)
       (notice
        "New session in the worktree docs (/home/u/.prigh/worktrees/work/docs)"))))
    |}];
  run h "/worktree";
  H.reply h "list_worktrees" (listing ());
  H.act h (Picker_query "docs");
  H.key h "Delete" ~target:Field;
  dialog h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /worktree) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose Picker))))
    (Focus picker-input)
    (Ask_remove_worktree /home/u/.prigh/worktrees/work/docs)
    (Focus dialog)
    Remove worktree?
    (Close (Esc))
    Remove the worktree docs at /home/u/.prigh/worktrees/work/docs? (the branch stays)
    Enter removes, Esc cancels (Cancel) (Remove)
    |}];
  (* Esc while it loads: no picker afterwards. *)
  H.key h "Escape" ~target:Control;
  run h "/worktree";
  H.key h "Escape" ~target:Field;
  H.reply h "list_worktrees" (listing ());
  dialog h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Rpc (method_ push_pref) (params ((key history) (value /worktree) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose Picker))))
    Close_dialog
    (Focus editor)
    |}]
;;

let%expect_test "/worktree's errors say what to do" =
  let h = listed () in
  List.iter
    [ "/worktree open"; "/worktree frobnicate"; "/worktree new a b c" ]
    ~f:(fun command ->
      run h command;
      toasts h;
      print_endline (H.model h).draft;
      H.type_ h "");
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree open") (max 500))) (tag Ignore))
    Usage: /worktree open [-n] TARGET (TARGET: a branch or path, as /worktree lists them). (Dismiss)
    /worktree open
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree frobnicate") (max 500)))
     (tag Ignore))
    Unknown subcommand "frobnicate": /worktree [list|new BRANCH [BASE]|open [-n] TARGET|rm [-f] TARGET]. (Dismiss)
    /worktree frobnicate
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree new a b c") (max 500)))
     (tag Ignore))
    Usage: /worktree new BRANCH [BASE]: a new branch from BASE (default: HEAD), or an existing branch. (Dismiss)
    /worktree new a b c
    |}];
  run h "/worktree open dcos";
  H.reply h "list_worktrees" (listing ());
  toasts h;
  print_endline (H.model h).draft;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree open dcos") (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s1\
                        \nbackend\
                        \n/work")
       (purpose
        (Target (command "/worktree open dcos")
         (action (Open (target dcos) (new_session false))))))))
    (Focus editor)
    No worktree "dcos" (closest: docs, main, old); /worktree lists them. (Dismiss)
    /worktree open dcos
    |}];
  H.type_ h "";
  run h "/worktree rm main";
  H.reply h "list_worktrees" (listing ());
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree rm main") (max 500))) (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s1\
                        \nbackend\
                        \n/work")
       (purpose
        (Target (command "/worktree rm main")
         (action (Remove (target main) (force false))))))))
    (Focus editor)
    /work is the repository's main worktree: it cannot be removed. (Dismiss)
    |}];
  H.type_ h "";
  run h "/worktree";
  H.fail
    h
    "list_worktrees"
    "not a git repository: /work (git init starts one, or /cd into one)";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /worktree) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose Picker))))
    (Focus editor)
    Couldn't list the worktrees: not a git repository: /work (git init starts one, or /cd into one). (Dismiss)
    |}]
;;

let%expect_test "/worktree open [-n], rm [-f], new" =
  let h = listed () in
  run h "/worktree open feat-login";
  H.reply h "list_worktrees" (listing ());
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree open feat-login") (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s1\
                        \nbackend\
                        \n/work")
       (purpose
        (Target (command "/worktree open feat-login")
         (action (Open (target feat-login) (new_session false))))))))
    (Rpc (method_ set_cwd)
     (params ((path /home/u/.prigh/worktrees/work/feat-login)))
     (tag
      (Worktree_entered
       "Working in the worktree feat/login (/home/u/.prigh/worktrees/work/feat-login)")))
    (Focus editor)
    |}];
  run h "/worktree open -n docs";
  H.reply h "list_worktrees" (listing ());
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree open -n docs") (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s1\
                        \nbackend\
                        \n/work")
       (purpose
        (Target (command "/worktree open -n docs")
         (action (Open (target docs) (new_session true))))))))
    (Rpc (method_ new_session) (params ())
     (tag
      (Worktree_session (path /home/u/.prigh/worktrees/work/docs)
       (notice
        "New session in the worktree docs (/home/u/.prigh/worktrees/work/docs)"))))
    (Focus editor)
    |}];
  run h "/worktree rm -f feat/login";
  H.reply h "list_worktrees" (listing ());
  dialog h;
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree rm -f feat/login") (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s1\
                        \nbackend\
                        \n/work")
       (purpose
        (Target (command "/worktree rm -f feat/login")
         (action (Remove (target feat/login) (force true))))))))
    (Focus dialog)
    Remove worktree with changes?
    (Close (Esc))
    Remove the worktree feat/login at /home/u/.prigh/worktrees/work/feat-login; its 3 uncommitted changes are lost; sessions working there: Login fix? (the branch stays)
    Enter removes it anyway, Esc cancels (Cancel) (Discard changes and remove)
    Dialog_accept
    (Rpc (method_ remove_worktree)
     (params ((path /home/u/.prigh/worktrees/work/feat-login) (force true)))
     (tag (Worktree_removed (target feat/login) (force true))))
    |}];
  H.key h "Escape" ~target:Control;
  run h "/worktree rm docs";
  H.reply h "list_worktrees" (listing ());
  H.key h "Escape" ~target:Control;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree rm docs") (max 500))) (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s1\
                        \nbackend\
                        \n/work")
       (purpose
        (Target (command "/worktree rm docs")
         (action (Remove (target docs) (force false))))))))
    (Focus dialog)
    Close_dialog
    (Focus editor)
    |}];
  run h "/worktree new feat/x main";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree new feat/x main") (max 500)))
     (tag Ignore))
    (Expire_toast (id 0) (after_ms 2740))
    (Rpc (method_ create_worktree) (params ((branch feat/x) (base main)))
     (tag
      (Worktree_created (command "/worktree new feat/x main")
       (new_session false))))
    Creating the worktree feat/x…
    |}];
  H.fail
    h
    "create_worktree"
    "feat/x is already checked out at /work/x: /worktree open feat/x works \
     there";
  toasts h;
  print_endline (H.model h).draft;
  [%expect
    {|
    Couldn't create the worktree: feat/x is already checked out at /work/x: /worktree open feat/x works there. (Dismiss)
    /worktree new feat/x main
    |}]
;;

let%expect_test "/worktree completes subcommands, worktrees and bases" =
  let h = listed () in
  let popup () = H.text h ~selector:".popup" in
  H.type_ h "/worktree ";
  popup ();
  [%expect
    {|
    Worktrees ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    list pick a worktree of this repository to work in
    new new BRANCH [BASE]: create a worktree and work in it
    open open [-n] TARGET: work in a worktree (-n: in a new session)
    rm rm [-f] TARGET: remove a worktree (-f: with uncommitted changes)
    |}];
  H.type_ h "/worktree op";
  H.key h "Enter";
  print_s [%sexp ((H.model h).draft : string)];
  popup ();
  [%expect
    {|
    (Complete_accept (run true))
    (Set_caret 15)
    "/worktree open "
    Worktrees ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    -n in a new session
    main /work · main worktree · here · 1 session
    feat/login /home/u/.prigh/worktrees/work/feat-login · 3 changed · ↑2 ↓1 · 1 session
    docs /home/u/.prigh/worktrees/work/docs
    old /home/u/.prigh/worktrees/work/old · gone
    |}];
  H.type_ h "/worktree rm ";
  popup ();
  [%expect
    {|
    Worktrees ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    -f also with uncommitted changes
    feat/login /home/u/.prigh/worktrees/work/feat-login · 3 changed · ↑2 ↓1 · 1 session
    docs /home/u/.prigh/worktrees/work/docs
    old /home/u/.prigh/worktrees/work/old · gone
    |}];
  H.type_ h "/worktree new fix/a ";
  popup ();
  [%expect
    {|
    Worktrees ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    main base: /work · main worktree · here · 1 session
    feat/login base: /home/u/.prigh/worktrees/work/feat-login · 3 changed · ↑2 ↓1 · 1 session
    docs base: /home/u/.prigh/worktrees/work/docs
    old base: /home/u/.prigh/worktrees/work/old · gone
    |}];
  H.type_ h "/worktree open fe";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/worktree open feat/login") (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ list_worktrees) (params ())
     (tag
      (Worktrees (place  "s1\
                        \nbackend\
                        \n/work")
       (purpose
        (Target (command "/worktree open feat/login")
         (action (Open (target feat/login) (new_session false))))))))
    |}]
;;

let%expect_test "a turn's end checks them again, in the background" =
  let h = listed () in
  let running r =
    H.event
      h
      (sprintf
         {|{"event":"state","state":%s}|}
         (H.state_json ~fields:[ ("running", if r then `True else `False) ] ()))
  in
  running true;
  running false;
  [%expect
    {|
    (Rpc (method_ list_worktrees) (params ())
     (tag (Worktrees (place  "s1\
                            \nbackend\
                            \n/work") (purpose (Sidebar 2)))))
    |}];
  H.reply h "list_worktrees" (listing ~current:"" ());
  H.elements h ~selector:".worktree.here";
  [%expect {| |}]
;;
