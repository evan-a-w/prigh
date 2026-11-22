open! Core
open! Prigh
open! Test_job_input

let%expect_test "job_input on a tool host that predates it" =
  host_scenario ~features:false;
  [%expect
    {|
    {"type":"response","id":"r1","ok":true,"result":{}}
    (host answered $instructions)
    {"type":"event","event":"tool_exec","host":"client-2","session_id":"<id>","exec_id":"<id>/j1-1","call_id":"j1","name":"bash","arguments":{"command":"cat","timeout":2592000},"cwd":"$DIR"}
    (no input reached the host)
    {"type":"response","id":"r2","ok":true,"result":{}}
    user: [Client: the user is on another client, which cannot display components: do not call render_ui; answer in text (markdown is fine).] |  | go
    assistant:  bash{"command":"cat","background":true,"stdin":true}
    tool_result bash: started job j1: cat; its output is being recorded; you will be notified when it exits. Use job_output to peek, job_kill to stop it.
    assistant:  job_input{"id":"j1","input":"one\n","wait_ms":10000}
    tool_result job_input: cannot write to job j1: tool host "laptop" runs an older prigh that cannot pass input to jobs: update prigh there and restart its TUI or tool-host, or start the job again with its input in the command (e.g. a here-document)
    assistant:  job_input{"id":"j1","input":"","close":true,"wait_ms":10000}
    tool_result job_input: cannot write to job j1: tool host "laptop" runs an older prigh that cannot pass input to jobs: update prigh there and restart its TUI or tool-host, or start the job again with its input in the command (e.g. a here-document)
    assistant: done
    user: [job j1 exited 0] cat | one
    assistant: noted
    |}]
;;

(* The worker side: an exec with [stdin] keeps its stdin open for [input]
   lines. *)
