open! Core
open! Prigh
open Tool_test_helpers
open! Test_rpc

let%expect_test "git_diff, review and init" =
  with_agent [ Reply.text "No findings." ]
  @@ fun t _agent h ->
  call t h "git_diff";
  call t h ~params:{|{"args": "main"}|} "review";
  let sh script =
    assert (
      Sys_unix.command
        (sprintf
           "cd %s && export GIT_CONFIG_GLOBAL=/dev/null GIT_AUTHOR_NAME=t \
            GIT_AUTHOR_EMAIL=t@t GIT_COMMITTER_NAME=t GIT_COMMITTER_EMAIL=t@t \
            GIT_AUTHOR_DATE=2026-01-01T00:00:00Z \
            GIT_COMMITTER_DATE=2026-01-01T00:00:00Z && (%s) >/dev/null 2>&1"
           (Filename.quote t.dir)
           script)
      = 0)
  in
  sh
    "git init -q -b main . && echo sessions > .gitignore && git add -A && git \
     commit -q -m init";
  write t "new.txt" "hello\n";
  call t h ~params:{|{"path": "new.txt"}|} "git_diff";
  call t h ~params:{|{"path": 3}|} "git_diff";
  call t h ~params:{|{"rev": "main"}|} "git_diff";
  call t h "git_refs";
  call t h ~params:{|{"rev": "main", "path": ".gitignore"}|} "git_show";
  call t h ~params:{|{"rev": "main", "path": "new.txt"}|} "git_show";
  call t h "review";
  Agent.wait_idle (current h);
  call t h "init";
  Agent.wait_idle (current h);
  [%expect
    {|
    {"type":"response","id":"r1","ok":false,"error":"not a git repository: $DIR (git init starts one, or /cd into one)"}
    {"type":"response","id":"r1","ok":false,"error":"not a git repository: $DIR (git init starts one, or /cd into one)"}
    {"type":"response","id":"r1","ok":true,"result":{"root":"$DIR","against":"HEAD","diff":"diff --git a/new.txt b/new.txt\nnew file mode 100644\nindex 0000000..ce01362\n--- /dev/null\n+++ b/new.txt\n@@ -0,0 +1 @@\n+hello\n","truncated":false,"branch":"main","base":"HEAD","main":true,"branches":["main"],"remote_branches":[],"tags":[],"commits":[{"sha":"<id><id>6a1a1555","subject":"init"}],"direct":false}}
    {"type":"response","id":"r1","ok":false,"error":"param \"path\" must be a string"}
    {"type":"response","id":"r1","ok":false,"error":"nothing to compare main with: name a base (e.g. --base main)"}
    {"type":"response","id":"r1","ok":true,"result":{"branches":["main"],"remote_branches":[],"tags":[]}}
    {"type":"response","id":"r1","ok":true,"result":{"text":"sessions\n","truncated":false}}
    {"type":"response","id":"r1","ok":false,"error":"main has no file new.txt"}
    {"type":"response","id":"r1","ok":true,"result":{}}
    {"type":"response","id":"r1","ok":true,"result":{}}
    |}]
;;

let%expect_test "a transient provider error: the retry event, then the reply" =
  with_agent [ Reply.error "HTTP 529: Overloaded"; Reply.text "ok" ]
  @@ fun _t agent _h ->
  Agent.subscribe agent ~f:(fun e ->
    match e with
    | Loop (Retry _ | Message_start (Assistant _) | Message_end (Assistant _))
      -> print_endline (Json.to_string (Rpc_json.event e))
    | _ -> ());
  Or_error.ok_exn (Agent.prompt agent "hi");
  Agent.wait_idle agent;
  [%expect
    {|
    {"type":"event","event":"message_start","message":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}}
    {"type":"event","event":"retry","attempt":2,"max_attempts":6,"error":"HTTP 529: Overloaded","delay_ms":1000}
    {"type":"event","event":"message_start","message":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"deepseek-flash"}}
    {"type":"event","event":"message_end","message":{"role":"assistant","content":[{"type":"text","text":"ok"}],"stop_reason":{"type":"end_turn"},"usage":{"input":10,"output":5,"cache_read":0},"model":"deepseek-flash"}}
    |}]
;;
