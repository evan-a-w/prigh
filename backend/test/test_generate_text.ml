open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf
module H = Test_rpc.H

let call = Test_rpc.call

let sh (t : Tool_test_helpers.t) script =
  let command =
    sprintf
      "cd %s && export GIT_CONFIG_GLOBAL=/dev/null GIT_CONFIG_NOSYSTEM=1 \
       GIT_AUTHOR_NAME=t GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t \
       GIT_COMMITTER_EMAIL=t@t && (%s) > /dev/null 2>&1"
      (Filename.quote t.dir)
      script
  in
  match Sys_unix.command command with
  | 0 -> ()
  | n -> raise_s [%message "setup failed" (script : string) (n : int)]
;;

let streamed chunks =
  { Reply.events = List.map chunks ~f:(fun c -> Assistant_event.Text_delta c)
  ; stop_reason = End_turn
  ; usage = { input = 100; output = 10; cache_read = 0 }
  }
;;

let print_request (r : Provider.Request.t) =
  printf
    "model=%s thinking=%s tools=%d max_tokens=%s\n"
    (Model.key r.model)
    (Sexp.to_string [%sexp (r.thinking : Thinking.t)])
    (List.length r.tools)
    (Option.value_map r.max_tokens ~default:"-" ~f:Int.to_string);
  printf "system: %s\n" (Option.value r.system ~default:"");
  List.iter r.messages ~f:(function
    | User u -> printf "user:\n%s\n" u.text
    | Assistant _ | Tool_result _ -> print_endline "(other)")
;;

let with_server ?delay replies f =
  Core_unix.putenv ~key:"GIT_CONFIG_GLOBAL" ~data:"/dev/null";
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let requests = Queue.create () in
  let h = ref None in
  let provider =
    Faux_provider.create
      ~on_request:(Queue.enqueue requests)
      ?delay_between_events:
        (Option.map delay ~f:(fun f () -> Option.iter !h ~f:(f t)))
      replies
  in
  let agent, server =
    Test_rpc.make_server t ~sw ~provider ~tools:(Plan.tool :: Tools.all)
  in
  h := Some server;
  f t agent server requests
;;

let deltas t (h : H.t) =
  Queue.to_list h.sent
  |> List.iter ~f:(fun json ->
    match Json.member "event" json with
    | Some (`String "generate_delta") ->
      print_endline (mask t (Json.to_string json))
    | _ -> ());
  Queue.clear h.sent
;;

let repo t =
  sh
    t
    "git init -q -b main . && git config user.name t && git config user.email \
     t@t && printf 'sessions/\\n' > .gitignore && printf 'one\\n' > a.txt && \
     git add . && git commit -qm 'web: add a' && printf 'two\\n' >> a.txt && \
     git commit -qam 'backend: grow a'"
;;

let%expect_test "commit_message: the staged diff, recent commits, the goal" =
  with_server
    [ Reply.tool_call
        ~id:"p1"
        ~name:"update_plan"
        ~arguments:
          {|{"plan": [{"step": "Grow a", "status": "in_progress"}, {"step": "Test it", "status": "pending"}]}|}
        ()
    ; Reply.text "On it."
    ; streamed [ "web: "; "grow a "; "again\n" ]
    ]
  @@ fun t agent h requests ->
  call t h ~params:{|{"kind": "commit_message"}|} "generate_text";
  [%expect
    {| {"type":"response","id":"r1","ok":false,"error":"not a git repository: $DIR (name a worktree of the repository: /worktree lists them)"} |}];
  repo t;
  call t h ~params:{|{"kind": "commit_message"}|} "generate_text";
  [%expect
    {| {"type":"response","id":"r1","ok":false,"error":"nothing is staged to describe: tick (stage) the files to commit first"} |}];
  write t "a.txt" "one\ntwo\nthree\n";
  sh t "git add a.txt";
  write t "AGENTS.md" "# Rules\n## Commits\nPrefix the area.\n## Other\nx\n";
  ignore
    (Agent.set_config
       agent
       { (Agent.config agent) with goal_max_continuations = 0 }
     : unit Or_error.t);
  ignore (Agent.set_goal agent "Make a longer" : unit Or_error.t);
  Agent.wait_idle agent;
  Queue.clear requests;
  Queue.clear h.sent;
  call
    t
    h
    ~params:{|{"kind": "commit_message", "gen_id": "g1"}|}
    "generate_text";
  deltas t h;
  Queue.iter requests ~f:print_request;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{"gen_id":"g1","kind":"commit_message","text":"web: grow a again","model":"deepseek/deepseek-flash","cost_usd":4.2e-05}}
    {"type":"event","event":"generate_delta","gen_id":"g1","delta":"web: "}
    {"type":"event","event":"generate_delta","gen_id":"g1","delta":"grow a "}
    {"type":"event","event":"generate_delta","gen_id":"g1","delta":"again\n"}
    model=deepseek/deepseek-flash thinking=Off tools=0 max_tokens=1000
    system: Write a git commit message for the staged changes. Follow the style of the repository's recent commits (their subjects are listed): their tense, capitalisation, prefixes such as "fix:" or "web:", and length, and its commit guidance if given. Format: a subject line of at most 72 characters; then, when the change needs explaining, a blank line and a body wrapped at 72 columns saying what changed and why. Reply with the message only: no code fences, no preamble.
    user:
    <branch>
    main
    </branch>

    <recent_commits>
    backend: grow a
    web: add a
    </recent_commits>

    <commit_guidance>
    From AGENTS.md:
    ## Commits
    Prefix the area.
    </commit_guidance>

    <session_goal>
    Make a longer
    </session_goal>

    <session_plan>
    - [in_progress] Grow a
    - [pending] Test it
    </session_plan>

    <staged_diff>
    diff --git a/a.txt b/a.txt
    index 814f4a4..4cb29ea 100644
    --- a/a.txt
    +++ b/a.txt
    @@ -1,2 +1,3 @@
     one
     two
    +three
    </staged_diff>
    |}]
;;

let%expect_test "pr: commits and diff against the base, guidance, template" =
  with_server [ streamed [ "# Grow a\n"; "\nIt grows.\n" ] ]
  @@ fun t _agent h requests ->
  repo t;
  sh
    t
    "git switch -q -c feat/b && printf 'b\\n' > b.txt && git add b.txt && git \
     commit -qm 'Add b' -m 'Because b.' && mkdir .github && printf '## \
     What\\n\\n## Testing\\n' > .github/pull_request_template.md && printf '# \
     Project\\n## Pull requests\\nSay how it was tested.\\n' > CONTRIBUTING.md";
  call t h ~params:{|{"kind": "pr", "base": "nope"}|} "generate_text";
  call t h ~params:{|{"kind": "pr", "base": "feat/b"}|} "generate_text";
  call
    t
    h
    ~params:{|{"kind": "pr", "base": "main", "gen_id": "p"}|}
    "generate_text";
  deltas t h;
  Queue.iter requests ~f:print_request;
  [%expect
    {|
    {"type":"response","id":"r1","ok":false,"error":"unknown base \"nope\": name a branch of the repository (e.g. main)"}
    {"type":"response","id":"r1","ok":false,"error":"feat/b has no commits that feat/b lacks: commit the changes first"}
    {"type":"response","id":"r1","ok":true,"result":{"gen_id":"p","kind":"pr","text":"Grow a\n\nIt grows.","title":"Grow a","body":"It grows.","model":"deepseek/deepseek-flash","cost_usd":4.2e-05}}
    {"type":"event","event":"generate_delta","gen_id":"p","delta":"# Grow a\n"}
    {"type":"event","event":"generate_delta","gen_id":"p","delta":"\nIt grows.\n"}
    model=deepseek/deepseek-flash thinking=Off tools=0 max_tokens=2500
    system: Write a pull request for the branch: its title on the first line (at most 72 characters, no "Title:"), a blank line, then the body in Markdown saying what changed and why, how it was tested when the commits say, and anything left for later. Follow the repository's pull request guidance, and fill in its template when one is given. Reply with the title and body only: no code fence around them, no preamble.
    user:
    <branch>
    feat/b, into main
    </branch>

    <commits>
    - Add b

    Because b.
    </commits>

    <pull_request_guidance>
    From CONTRIBUTING.md:
    ## Pull requests
    Say how it was tested.
    </pull_request_guidance>

    <pull_request_template>
    .github/pull_request_template.md:
    ## What

    ## Testing
    </pull_request_template>

    <diff>
    diff --git a/b.txt b/b.txt
    new file mode 100644
    index 0000000..6178079
    --- /dev/null
    +++ b/b.txt
    @@ -0,0 +1 @@
    +b
    </diff>
    |}]
;;

let%expect_test "branch_name and session_title; errors" =
  with_server
    [ streamed [ "`Feat/Add  Dark"; " mode!`\n" ]
    ; Reply.text "Sure."
    ; Reply.text "\"Dark mode.\""
    ]
  @@ fun t _agent h requests ->
  call t h ~params:{|{"kind": "haiku"}|} "generate_text";
  call t h ~params:{|{"kind": "branch_name"}|} "generate_text";
  call t h ~params:{|{"kind": "session_title"}|} "generate_text";
  call
    t
    h
    ~params:{|{"kind": "branch_name", "task": "add a dark mode"}|}
    "generate_text";
  deltas t h;
  Queue.iter requests ~f:print_request;
  Queue.clear requests;
  call t h ~params:{|{"text": "make it dark"}|} "prompt";
  Agent.wait_idle (Test_rpc.current h);
  Queue.clear requests;
  Queue.clear h.sent;
  call t h ~params:{|{"kind": "session_title"}|} "generate_text";
  Queue.iter requests ~f:print_request;
  [%expect
    {|
    {"type":"response","id":"r1","ok":false,"error":"unknown kind \"haiku\": generate_text drafts commit_message, pr, branch_name, session_title"}
    {"type":"response","id":"r1","ok":false,"error":"say what the branch is for first: type the task, then Generate"}
    {"type":"response","id":"r1","ok":false,"error":"the session has no conversation to name yet: send a message first"}
    {"type":"response","id":"r1","ok":true,"result":{"gen_id":"gen-3","kind":"branch_name","text":"feat/add-dark-mode","model":"deepseek/deepseek-flash","cost_usd":4.2e-05}}
    {"type":"event","event":"generate_delta","gen_id":"gen-3","delta":"`Feat/Add  Dark"}
    {"type":"event","event":"generate_delta","gen_id":"gen-3","delta":" mode!`\n"}
    model=deepseek/deepseek-flash thinking=Off tools=0 max_tokens=60
    system: Name a git branch for the task: lowercase words joined by hyphens, at most 40 characters, with a prefix such as feat/, fix/ or docs/ when one fits. Reply with the name only.
    user:
    <task>
    add a dark mode
    </task>
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{"gen_id":"gen-4","kind":"session_title","text":"Dark mode","model":"deepseek/deepseek-flash","cost_usd":9e-06}}
    model=deepseek/deepseek-flash thinking=Off tools=0 max_tokens=60
    system: Describe the conversation below in one short line (at most eight words) saying what the user is working on, e.g. "Fixing flaky session tests" or "Adding OAuth login to the CLI". Reply with the line only: no quotes, no trailing period, no preamble.
    user:
    USER:
    make it dark

    ASSISTANT:
    Sure.
    |}]
;;

let%expect_test "generate_model, cancelling" =
  let cancelled = ref false in
  with_server
    ~delay:(fun t h ->
      if not !cancelled
      then (
        cancelled := true;
        call t h ~params:{|{"gen_id": "slow"}|} "generate_cancel"))
    [ streamed [ "a"; "b"; "c" ]; Reply.text "fix/quick" ]
  @@ fun t agent h requests ->
  call
    t
    h
    ~params:{|{"config": {"generate_model": "no-such-model"}}|}
    "set_config";
  call
    t
    h
    ~params:{|{"kind": "branch_name", "task": "x", "gen_id": "slow"}|}
    "generate_text";
  call t h ~params:{|{"gen_id": "slow"}|} "generate_cancel";
  deltas t h;
  ignore
    (Agent.set_config
       agent
       { (Agent.config agent) with generate_model = Some "deepseek-v4-pro" }
     : unit Or_error.t);
  printf
    "generate_model: %s\n"
    (Option.value ~default:"-" (Agent.config agent).generate_model);
  call t h ~params:{|{"kind": "branch_name", "task": "x"}|} "generate_text";
  Queue.iter requests ~f:(fun r -> print_endline (Model.key r.model));
  [%expect
    {|
    {"type":"response","id":"r1","ok":false,"error":"generate_model: unknown model \"no-such-model\"; did you mean: openai/gpt-5.3-codex (GPT-5.3 Codex), openai/gpt-5.6-sol (GPT-5.6 Sol), openai/gpt-6.1-sol (GPT-6.1 Sol)"}
    {"type":"response","id":"r1","ok":true,"result":{"cancelled":true}}
    {"type":"response","id":"r1","ok":false,"error":"cancelled"}
    {"type":"response","id":"r1","ok":true,"result":{"cancelled":false}}
    generate_model: deepseek/deepseek-v4-pro
    {"type":"response","id":"r1","ok":true,"result":{"gen_id":"gen-1","kind":"branch_name","text":"fix/quick","model":"deepseek/deepseek-v4-pro","cost_usd":3.3e-05}}
    deepseek/deepseek-flash
    deepseek/deepseek-v4-pro
    |}]
;;

let%expect_test "cleaning replies" =
  List.iter
    [ Generate_text.Kind.Pr, "```\nTitle: Add b\n\nBody here.\n```"
    ; Pr, "## Add b"
    ; Commit_message, "```text\nfix: x\n\nwhy\n```"
    ; Branch_name, "Branch: feat/x"
    ; Branch_name, "--Feat//Über..fix--"
    ; Session_title, "\"Fixing things.\"\nmore"
    ]
    ~f:(fun (kind, text) ->
      printf
        "%s %S -> %S\n"
        (Generate_text.Kind.to_string kind)
        text
        (Generate_text.clean kind text));
  [%expect
    {|
    pr "```\nTitle: Add b\n\nBody here.\n```" -> "Add b\n\nBody here."
    pr "## Add b" -> "Add b"
    commit_message "```text\nfix: x\n\nwhy\n```" -> "fix: x\n\nwhy"
    branch_name "Branch: feat/x" -> "feat/x"
    branch_name "--Feat//\195\156ber..fix--" -> "feat/ber.fix"
    session_title "\"Fixing things.\"\nmore" -> "Fixing things"
    |}]
;;
