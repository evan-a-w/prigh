open! Core
open! Prigh

let parse s =
  match Approval_rule.of_string s with
  | Ok rule -> printf "%-28s -> %s\n" s (Approval_rule.to_string rule)
  | Error e -> printf "%-28s -> error: %s\n" s (Error.to_string_hum e)
;;

let%expect_test "parsing rules" =
  List.iter
    [ "bash"
    ; "Bash(git status:*)"
    ; "  edit(src/**)  "
    ; "mcp__github__*"
    ; "*"
    ; "bash()"
    ; "bash(ls"
    ; "(ls)"
    ; "rm -rf"
    ]
    ~f:parse;
  [%expect
    {|
    bash                         -> bash
    Bash(git status:*)           -> bash(git status:*)
      edit(src/**)               -> edit(src/**)
    mcp__github__*               -> mcp__github__*
    *                            -> *
    bash()                       -> error: rule "bash()": empty pattern; write "bash" to match every call
    bash(ls                      -> error: rule "bash(ls": the pattern must end with ")"
    (ls)                         -> error: rule "(ls)": missing the tool name, e.g. bash(ls:*)
    rm -rf                       -> error: rule "rm -rf": "rm -rf" is not a tool name (letters, digits, _, -, . and *)
    |}]
;;

let%expect_test "splitting commands" =
  List.iter
    [ "git status"
    ; "cd src && make -j4 || echo failed; ls | wc -l"
    ; "echo 'a && b' \"c; d\""
    ; "dune build 2>&1 | tail -5"
    ; "ls > /dev/null 2>/dev/null"
    ; "echo hi > out.txt"
    ; "echo $(rm -rf /)"
    ; "echo `date`"
    ; "echo '$(not substituted)'"
    ; "diff <(ls a) <(ls b)"
    ; "sleep 1 & wait"
    ; "line one\nline two"
    ]
    ~f:(fun command ->
      let parts, kind = Approval_rule.segments command in
      print_s
        [%message
          command
            (parts : string list)
            ~_:
              ((match kind with
                | `Simple -> "simple"
                | `Complex -> "complex")
               : string)]);
  [%expect
    {|
    ("git status" (parts ("git status")) simple)
    ("cd src && make -j4 || echo failed; ls | wc -l"
     (parts ("cd src" "make -j4" "echo failed" ls "wc -l")) simple)
    ("echo 'a && b' \"c; d\"" (parts ("echo 'a && b' \"c; d\"")) simple)
    ("dune build 2>&1 | tail -5" (parts ("dune build 2>&1" "tail -5")) simple)
    ("ls > /dev/null 2>/dev/null" (parts ("ls > /dev/null 2>/dev/null")) simple)
    ("echo hi > out.txt" (parts ("echo hi > out.txt")) complex)
    ("echo $(rm -rf /)" (parts ("echo $(rm -rf /)")) complex)
    ("echo `date`" (parts ("echo `date`")) complex)
    ("echo '$(not substituted)'" (parts ("echo '$(not substituted)'")) simple)
    ("diff <(ls a) <(ls b)" (parts ("diff <(ls a) <(ls b)")) complex)
    ("sleep 1 & wait" (parts ("sleep 1" wait)) simple)
    ( "line one\
     \nline two" (parts ("line one" "line two")) simple)
    |}]
;;

let%expect_test "globs" =
  List.iter
    [ "/p/src/**", "/p/src/a/b.ml"
    ; "/p/src/**", "/p/src"
    ; "/p/src/**", "/p/srcx/a"
    ; "/p/src/*.ml", "/p/src/a.ml"
    ; "/p/src/*.ml", "/p/src/a/b.ml"
    ; "/p/**/*.ml", "/p/a.ml"
    ; "/p/**/*.ml", "/p/a/b/c.ml"
    ; "/p/?.txt", "/p/a.txt"
    ; "/p/?.txt", "/p/ab.txt"
    ]
    ~f:(fun (pattern, path) ->
      printf
        "%-14s %-14s %b\n"
        pattern
        path
        (Approval_rule.glob_matches ~pattern path));
  [%expect
    {|
    /p/src/**      /p/src/a/b.ml  true
    /p/src/**      /p/src         false
    /p/src/**      /p/srcx/a      false
    /p/src/*.ml    /p/src/a.ml    true
    /p/src/*.ml    /p/src/a/b.ml  false
    /p/**/*.ml     /p/a.ml        true
    /p/**/*.ml     /p/a/b/c.ml    true
    /p/?.txt       /p/a.txt       true
    /p/?.txt       /p/ab.txt      false
    |}]
;;

let call tool args =
  Approval_rule.Call.create ~cwd:"/work" ~tool (Jsonaf.of_string args)
;;

let check rule (call : Approval_rule.Call.t) =
  let rule = Or_error.ok_exn (Approval_rule.of_string rule) in
  let covers =
    Approval_rule.covers [ rule ] ~cwd:"/work" ~home:"/home/u" call
  in
  let touches = Approval_rule.touches rule ~cwd:"/work" ~home:"/home/u" call in
  printf
    "%-22s %-40s covers=%b touches=%b\n"
    (Approval_rule.to_string rule)
    (Option.value call.command ~default:(String.concat ~sep:"," call.paths))
    covers
    touches
;;

let%expect_test "which calls rules cover" =
  let bash command =
    call "bash" (Jsonaf.to_string (`Object [ "command", `String command ]))
  in
  check "bash(git status:*)" (bash "git status");
  check "bash(git status:*)" (bash "git   status -s");
  check "bash(git status:*)" (bash "git statusx");
  check "bash(git status:*)" (bash "git status && git push");
  check "bash(git:*)" (bash "git status && git push");
  check "bash(rm:*)" (bash "cd /tmp && rm -rf x");
  check "bash(echo:*)" (bash "echo $(rm -rf x)");
  check "bash(echo:*)" (bash "echo hi > notes.txt");
  check "bash(npm run test*)" (bash "npm run test:unit");
  check "bash" (bash "anything at all");
  check "edit(src/**)" (call "edit" {|{"path": "src/a/b.ml"}|});
  check "edit(src/**)" (call "edit" {|{"path": "src/../../etc/passwd"}|});
  check "edit(src/**)" (call "write" {|{"path": "/work/src/new.ml"}|});
  check "edit(~/notes/*)" (call "write" {|{"path": "/home/u/notes/a.md"}|});
  check
    "edit"
    (call
       "apply_patch"
       {|{"input": "*** Begin Patch\n*** Add File: x\n+y\n*** End Patch"}|});
  check
    "edit(src/**)"
    (call
       "apply_patch"
       {|{"input": "*** Begin Patch\n*** Add File: src/x\n+y\n*** Add File: y\n+y\n*** End Patch"}|});
  check "read(/etc/**)" (call "grep" {|{"pattern": "x", "path": "/etc/ssl"}|});
  check "read" (call "ls" {|{}|});
  check "write(src/**)" (call "edit" {|{"path": "src/a.ml"}|});
  check "mcp__github__*" (call "mcp__github__create_issue" {|{}|});
  check "mcp__github__*" (call "mcp__gitlab__create_issue" {|{}|});
  check "subagent(x)" (call "subagent" {|{"task": "x"}|});
  (* Rules cover a command together, a part each. *)
  let rules =
    List.map [ "bash(git status:*)"; "bash(echo:*)" ] ~f:(fun r ->
      Or_error.ok_exn (Approval_rule.of_string r))
  in
  List.iter [ "git status -s; echo ok"; "git status && rm x" ] ~f:(fun c ->
    printf
      "both rules: %-24s covers=%b\n"
      c
      (Approval_rule.covers rules ~cwd:"/work" ~home:"/home/u" (bash c)));
  [%expect
    {|
    bash(git status:*)     git status                               covers=true touches=true
    bash(git status:*)     git   status -s                          covers=true touches=true
    bash(git status:*)     git statusx                              covers=false touches=false
    bash(git status:*)     git status && git push                   covers=false touches=true
    bash(git:*)            git status && git push                   covers=true touches=true
    bash(rm:*)             cd /tmp && rm -rf x                      covers=false touches=true
    bash(echo:*)           echo $(rm -rf x)                         covers=false touches=true
    bash(echo:*)           echo hi > notes.txt                      covers=false touches=true
    bash(npm run test*)    npm run test:unit                        covers=true touches=true
    bash                   anything at all                          covers=true touches=true
    edit(src/**)           /work/src/a/b.ml                         covers=true touches=true
    edit(src/**)           /etc/passwd                              covers=false touches=false
    edit(src/**)           /work/src/new.ml                         covers=true touches=true
    edit(~/notes/*)        /home/u/notes/a.md                       covers=true touches=true
    edit                   /work/x                                  covers=true touches=true
    edit(src/**)           /work/src/x,/work/y                      covers=false touches=true
    read(/etc/**)          /etc/ssl                                 covers=true touches=true
    read                   /work                                    covers=true touches=true
    write(src/**)          /work/src/a.ml                           covers=false touches=false
    mcp__github__*                                                  covers=true touches=true
    mcp__github__*                                                  covers=false touches=false
    subagent(x)                                                     covers=false touches=false
    both rules: git status -s; echo ok   covers=true
    both rules: git status && rm x       covers=false
    |}]
;;

let%expect_test "suggested session rules" =
  let suggest tool args =
    let call = call tool args in
    print_s
      [%sexp
        (List.map (Approval_rule.suggest call) ~f:Approval_rule.to_string
         : string list)]
  in
  suggest "bash" {|{"command": "git status -s"}|};
  suggest "bash" {|{"command": "git --no-pager log"}|};
  suggest "bash" {|{"command": "cd src && make -j4 && ./run.sh --fast"}|};
  suggest "bash" {|{"command": "ls; ls -la"}|};
  suggest "bash" {|{"command": "echo $(date)"}|};
  suggest "write" {|{"path": "a"}|};
  suggest "apply_patch" {|{"input": ""}|};
  suggest "mcp__github__create_issue" {|{}|};
  [%expect
    {|
    ("bash(git status:*)")
    ("bash(git:*)")
    ("bash(cd:*)" "bash(make:*)" "bash(./run.sh:*)")
    ("bash(ls:*)")
    ()
    (edit)
    (edit)
    (mcp__github__create_issue)
    |}]
;;

let%expect_test "decisions" =
  let rules : Approval.Rules.t =
    { allow = [ Or_error.ok_exn (Approval_rule.of_string "bash(git status:*)") ]
    ; ask = [ Or_error.ok_exn (Approval_rule.of_string "read(secrets/**)") ]
    ; deny = [ Or_error.ok_exn (Approval_rule.of_string "bash(rm:*)") ]
    }
  in
  let calls =
    [ "bash", {|{"command": "git status"}|}, true
    ; "bash", {|{"command": "make"}|}, true
    ; "bash", {|{"command": "make && rm -rf build"}|}, true
    ; "edit", {|{"path": "a.ml"}|}, true
    ; "read", {|{"path": "a.ml"}|}, false
    ; "read", {|{"path": "secrets/key"}|}, false
    ; "mcp__x__y", {|{}|}, true
    ]
  in
  List.iter
    [ Approval_mode.Ask, false; Auto_edit, false; Auto, false; Auto, true ]
    ~f:(fun (mode, plan) ->
      printf
        "%s%s:\n"
        (Approval_mode.to_string mode)
        (if plan then " + plan" else "");
      List.iter calls ~f:(fun (tool, args, destructive) ->
        let decision =
          Approval.decide
            ~mode
            ~plan
            ~rules
            ~session:[]
            ~cwd:"/work"
            ~home:"/home/u"
            ~destructive
            (call tool args)
        in
        printf
          "  %-10s %-36s %s\n"
          tool
          args
          (match decision with
           | Allow -> "allow"
           | Ask { rules } ->
             sprintf
               "ask [%s]"
               (String.concat
                  ~sep:" "
                  (List.map rules ~f:Approval_rule.to_string))
           | Deny text -> "deny " ^ String.prefix text 40)));
  [%expect
    {|
    ask:
      bash       {"command": "git status"}            allow
      bash       {"command": "make"}                  ask [bash(make:*)]
      bash       {"command": "make && rm -rf build"}  deny [denied by the rule bash(rm:*) in permis
      edit       {"path": "a.ml"}                     ask [edit]
      read       {"path": "a.ml"}                     allow
      read       {"path": "secrets/key"}              ask []
      mcp__x__y  {}                                   ask [mcp__x__y]
    auto-edit:
      bash       {"command": "git status"}            allow
      bash       {"command": "make"}                  ask [bash(make:*)]
      bash       {"command": "make && rm -rf build"}  deny [denied by the rule bash(rm:*) in permis
      edit       {"path": "a.ml"}                     allow
      read       {"path": "a.ml"}                     allow
      read       {"path": "secrets/key"}              ask []
      mcp__x__y  {}                                   ask [mcp__x__y]
    auto:
      bash       {"command": "git status"}            allow
      bash       {"command": "make"}                  allow
      bash       {"command": "make && rm -rf build"}  deny [denied by the rule bash(rm:*) in permis
      edit       {"path": "a.ml"}                     allow
      read       {"path": "a.ml"}                     allow
      read       {"path": "secrets/key"}              ask []
      mcp__x__y  {}                                   allow
    auto + plan:
      bash       {"command": "git status"}            allow
      bash       {"command": "make"}                  ask [bash(make:*)]
      bash       {"command": "make && rm -rf build"}  deny [denied by the rule bash(rm:*) in permis
      edit       {"path": "a.ml"}                     deny [plan mode] Files cannot be changed whil
      read       {"path": "a.ml"}                     allow
      read       {"path": "secrets/key"}              ask []
      mcp__x__y  {}                                   ask [mcp__x__y]
    |}]
;;
