open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let seconds_re = Re.compile (Re.Perl.re {|\b[0-9]+s ago\b|})
let clean t s = Re.replace_string seconds_re ~by:"Ns ago" (mask t s)

(* Runs [tool] as the backend's executor would for session [scope] (none:
   an older backend). *)
let call t ?scope tool args =
  let args =
    match scope with
    | None -> args
    | Some scope -> Seen_files.with_scope args ~scope
  in
  let result =
    Tool.execute tool (Tool.Context.create ~env:t.env ~cwd:t.dir ()) args
  in
  printf
    "%s %s\n"
    (if result.is_error then "error:" else "ok:")
    (clean t (String.prefix result.text 400))
;;

let write t ?scope path content =
  call
    t
    ?scope
    Tool_write.tool
    (`Object [ "path", `String path; "content", `String content ])
;;

let read t ?scope path =
  call t ?scope Tool_read.tool (`Object [ "path", `String path ])
;;

let edit t ?scope path old_text new_text =
  call
    t
    ?scope
    Tool_edit.tool
    (`Object
        [ "path", `String path
        ; ( "edits"
          , `Array
              [ `Object
                  [ "old_text", `String old_text; "new_text", `String new_text ]
              ] )
        ])
;;

let patch t ?scope text =
  call t ?scope Tool_apply_patch.tool (`Object [ "input", `String text ])
;;

let%expect_test "write: an existing file must have been seen as it is now" =
  with_sandbox
  @@ fun t ->
  let scope = "session-1" in
  Tool_test_helpers.write t "notes.txt" "old notes\n";
  write t ~scope "new.txt" "fresh\n";
  write t ~scope "new.txt" "written twice\n";
  write t ~scope "notes.txt" "clobbered\n";
  print_endline (Tool_test_helpers.read t "notes.txt");
  read t ~scope "notes.txt";
  write t ~scope "notes.txt" "replaced\n";
  (* Changed behind the agent's back. *)
  Tool_test_helpers.write t "notes.txt" "edited by someone else\n";
  write t ~scope "notes.txt" "clobbered\n";
  (* Touched but the same contents: fine. *)
  Tool_test_helpers.write t "new.txt" "written twice\n";
  write t ~scope "new.txt" "third\n";
  (* Another session has seen nothing; no scope (an older backend): no
     check. *)
  write t ~scope:"session-2" "new.txt" "other session\n";
  write t "new.txt" "unchecked\n";
  [%expect
    {|
    ok: wrote 1 line to $DIR/new.txt
    ok: overwrote 1 line to $DIR/new.txt
    error: refusing to overwrite $DIR/notes.txt: it already exists and you have not read it in this session. Read $DIR/notes.txt first, then write it again (or edit it to change parts of it)
    old notes

    ok: old notes

    ok: overwrote 1 line to $DIR/notes.txt
    error: refusing to overwrite $DIR/notes.txt: it changed since you last read or wrote it (Ns ago). Read $DIR/notes.txt again, then write it again (or edit it to change parts of it)
    ok: overwrote 1 line to $DIR/new.txt
    error: refusing to overwrite $DIR/new.txt: it already exists and you have not read it in this session. Read $DIR/new.txt first, then write it again (or edit it to change parts of it)
    ok: overwrote 1 line to $DIR/new.txt
    |}]
;;

let%expect_test "edits are not blocked, but say when the file changed" =
  with_sandbox
  @@ fun t ->
  let scope = "session-edit" in
  Tool_test_helpers.write t "a.ml" "let x = 1\nlet y = 2\n";
  edit t ~scope "a.ml" "let x = 1" "let x = 10";
  Tool_test_helpers.write t "a.ml" "let x = 10\nlet y = 3\n";
  edit t ~scope "a.ml" "let y = 3" "let y = 30";
  (* The edit counts as seeing it: a write is now allowed. *)
  write t ~scope "a.ml" "let z = 0\n";
  [%expect
    {|
    ok: --- a/a.ml
    +++ b/a.ml
    @@ -1,2 +1,2 @@
    -let x = 1
    +let x = 10
     let y = 2

    ok: note: $DIR/a.ml changed since you last read or wrote it (Ns ago); the edit was matched against its current contents
    --- a/a.ml
    +++ b/a.ml
    @@ -1,2 +1,2 @@
     let x = 10
    -let y = 3
    +let y = 30

    ok: overwrote 1 line to $DIR/a.ml
    |}]
;;

let%expect_test "apply_patch: Add File over an existing file, moves, updates" =
  with_sandbox
  @@ fun t ->
  let scope = "session-patch" in
  Tool_test_helpers.write t "keep.txt" "precious\n";
  Tool_test_helpers.write t "src.txt" "source\n";
  patch
    t
    ~scope
    "*** Begin Patch\n\
     *** Add File: keep.txt\n\
     +replaced\n\
     *** Add File: other.txt\n\
     +new\n\
     *** End Patch";
  print_s
    [%sexp
      (Tool_test_helpers.read t "keep.txt" : string)
    , (Sys_unix.file_exists_exn (Filename.concat t.dir "other.txt") : bool)];
  patch
    t
    ~scope
    "*** Begin Patch\n\
     *** Update File: src.txt\n\
     *** Move to: keep.txt\n\
     @@\n\
     -source\n\
     +moved\n\
     *** End Patch";
  read t ~scope "keep.txt";
  patch
    t
    ~scope
    "*** Begin Patch\n*** Add File: keep.txt\n+replaced\n*** End Patch";
  Tool_test_helpers.write t "keep.txt" "changed\n";
  patch
    t
    ~scope
    "*** Begin Patch\n\
     *** Update File: keep.txt\n\
     @@\n\
     -changed\n\
     +updated\n\
     *** End Patch";
  patch
    t
    ~scope
    "*** Begin Patch\n*** Add File: keep.txt\n+again\n*** End Patch";
  [%expect
    {|
    error: refusing to overwrite $DIR/keep.txt with *** Add File: it already exists and you have not read it in this session. Read $DIR/keep.txt first, then send the patch again (or change it with *** Update File)
    ("precious\n" false)
    error: refusing to move src.txt onto $DIR/keep.txt: it already exists and you have not read it in this session. Read $DIR/keep.txt first, then send the patch again (or change it with *** Update File)
    ok: precious

    ok: Success. Updated the following files:
    M keep.txt

    --- a/keep.txt
    +++ b/keep.txt
    @@ -1,1 +1,1 @@
    -precious
    +replaced

    ok: Success. Updated the following files:
    M keep.txt
    note: $DIR/keep.txt changed since you last read or wrote it (Ns ago); the edit was matched against its current contents

    --- a/keep.txt
    +++ b/keep.txt
    @@ -1,1 +1,1 @@
    -changed
    +updated

    ok: Success. Updated the following files:
    M keep.txt

    --- a/keep.txt
    +++ b/keep.txt
    @@ -1,1 +1,1 @@
    -updated
    +again
    |}]
;;

let%expect_test "the record is bounded; losing an entry asks to read again" =
  with_sandbox
  @@ fun t ->
  let scope = "session-bounded" in
  Tool_test_helpers.write t "first.txt" "1\n";
  read t ~scope "first.txt";
  for i = 1 to Seen_files.max_files do
    Tool_test_helpers.write t (sprintf "f%d" i) "x";
    call t ~scope Tool_read.tool (`Object [ "path", `String (sprintf "f%d" i) ])
    |> ignore
  done;
  [%expect.output] |> ignore;
  write t ~scope "first.txt" "2\n";
  write t ~scope (sprintf "f%d" Seen_files.max_files) "y";
  [%expect
    {|
    error: refusing to overwrite $DIR/first.txt: it already exists and you have not read it in this session. Read $DIR/first.txt first, then write it again (or edit it to change parts of it)
    ok: overwrote 1 line to $DIR/f2000
    |}]
;;

(* Through an agent: the scope is the session, so subagents share it. *)
let%expect_test "an agent's reads let it write; its subagents share what it saw"
  =
  let call ~id name args =
    Reply.tool_call ~id ~name ~arguments:(Json.to_string (`Object args)) ()
  in
  Test_background_jobs.with_agent
    ~main:
      [ call
          ~id:"w1"
          "write"
          [ "path", `String "plan.md"; "content", `String "mine\n" ]
      ; call ~id:"r1" "read" [ "path", `String "plan.md" ]
      ; call ~id:"s1" "subagent" [ "task", `String "rewrite the plan" ]
      ; call ~id:"s2" "subagent_wait" []
      ; call ~id:"w2" "bash" [ "command", `String "echo theirs > plan.md" ]
      ; call
          ~id:"w3"
          "write"
          [ "path", `String "plan.md"; "content", `String "mine again\n" ]
      ; Reply.text "done"
      ]
    [ ( "rewrite the plan"
      , [ call
            ~id:"c1"
            "write"
            [ "path", `String "plan.md"; "content", `String "the subagent's\n" ]
        ; Reply.text "rewrote it"
        ] )
    ]
  @@ fun t agent ->
  Tool_test_helpers.write t "plan.md" "the user's\n";
  Or_error.ok_exn (Agent.prompt agent "go");
  Agent.wait_idle agent;
  print_endline (Tool_test_helpers.read t "plan.md");
  Option.iter (Agent.subagent agent "a1") ~f:(fun (_, messages) ->
    List.iter messages ~f:(fun { message; _ } ->
      print_endline (clean t (Test_background_subagents.message_line message))));
  [%expect
    {|
    state: running=true subagents=[] jobs=[]
    agent_start
    user: go
    assistant:  write{"path":"plan.md","content":"mine\n"}
    tool_result write: refusing to overwrite $DIR/plan.md: it already exists and you have not read it in this session. Read $DIR/plan.md first, then write it again (or edit it to change parts of it)
    assistant:  read{"path":"plan.md"}
    tool_result read: the user's
    assistant:  subagent{"task":"rewrite the plan"}
    state: running=true subagents=[a1 running] jobs=[]
    tool_result subagent: started agent a1 (rewrite the plan); its result will be delivered to you when it finishes; use subagent_wait to block on it
    assistant:  subagent_wait{}
    state: running=true subagents=[a1 finished] jobs=[]
    state: running=true subagents=[] jobs=[]
    tool_result subagent_wait: [subagent a1 finished] rewrite the plan | rewrote it | [subagent: 2 turns, 30 in / 13 out tokens, $0.0000]
    assistant:  bash{"command":"echo theirs > plan.md"}
    tool_result bash:
    assistant:  write{"path":"plan.md","content":"mine again\n"}
    tool_result write: refusing to overwrite $DIR/plan.md: it changed since you last read or wrote it (Ns ago). Read $DIR/plan.md again, then write it again (or edit it to change parts of it)
    assistant: done
    agent_end
    state: running=false subagents=[] jobs=[]
    theirs

    user: rewrite the plan
    assistant:  write{"path":"plan.md","content":"the subagent's\n"}
    tool_result write: overwrote 1 line to $DIR/plan.md
    assistant: rewrote it
    |}]
;;

(* The record lives in the tool host's process: a restarted [prigh
   tool-host] has none, so the agent is asked to read again. *)
let%expect_test "a restarted tool host has forgotten: read again" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  Tool_test_helpers.write t "config.toml" "a = 1\n";
  let exe = Filename.concat (Sys_unix.getcwd ()) "../bin/main.exe" in
  let start () =
    let stdin_r, stdin_w = Eio_unix.pipe sw in
    let stdout_r, stdout_w = Eio_unix.pipe sw in
    let child =
      Eio.Process.spawn
        ~sw
        (Eio.Stdenv.process_mgr t.env)
        ~stdin:stdin_r
        ~stdout:stdout_w
        [ exe; "tool-host" ]
    in
    Eio.Flow.close stdin_r;
    Eio.Flow.close stdout_w;
    let reader = Eio.Buf_read.of_flow stdout_r ~max_size:(1024 * 1024) in
    let exec name arguments =
      Eio.Flow.copy_string
        (Json.to_string
           (`Object
               [ "type", `String "exec"
               ; "exec_id", `String name
               ; "name", `String name
               ; ( "arguments"
                 , Seen_files.with_scope arguments ~scope:"session-host" )
               ; "cwd", `String t.dir
               ])
         ^ "\n")
        stdin_w;
      let rec result () =
        let json = Json.of_string (Eio.Buf_read.line reader) in
        match Json.member "type" json with
        | Some (`String "result") ->
          print_endline
            (clean
               t
               (Json.string_exn (Option.value_exn (Json.member "text" json))))
        | _ -> result ()
      in
      result ()
    in
    let stop () =
      Eio.Flow.close stdin_w;
      ignore (Eio.Process.await child : Eio.Process.exit_status)
    in
    exec, stop
  in
  let write_args =
    `Object [ "path", `String "config.toml"; "content", `String "a = 2\n" ]
  in
  let exec, stop = start () in
  exec "read" (`Object [ "path", `String "config.toml" ]);
  exec "write" write_args;
  stop ();
  let exec, stop = start () in
  exec "write" write_args;
  stop ();
  [%expect
    {|
    a = 1

    overwrote 1 line to $DIR/config.toml
    refusing to overwrite $DIR/config.toml: it already exists and you have not read it in this session. Read $DIR/config.toml first, then write it again (or edit it to change parts of it)
    |}]
;;
