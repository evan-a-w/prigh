open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

(* Stop ([abort], what the frontends send for Stop and Esc) while a tool
   call runs on a tool host: the turn ends at once with the call cancelled,
   whether or not the host answers, and the host kills the command's process
   group. *)

let member_string json name =
  match Json.member name json with
  | Some (`String s) -> s
  | _ -> ""
;;

let alive pid =
  match Signal_unix.send Signal.zero (`Pid (Pid.of_int pid)) with
  | `Ok -> true
  | `No_such_process -> false
;;

let exists t name = Sys_unix.file_exists_exn (Filename.concat t.dir name)

let read_pid t name =
  Int.of_string (String.strip (In_channel.read_all (Filename.concat t.dir name)))
;;

(* Writes the shell's and its child's pids next to [DIR], and [survived]
   only if the shell outlives its child. *)
let sleeper ?(tag = "") () =
  sprintf
    {|{"command":"echo $$ > $DIR/shell%s.pid; sleep 30 & echo $! > $DIR/sleep%s.pid; wait; touch $DIR/survived%s"}|}
    tag
    tag
    tag
;;

let tool_call ?tag t ~id =
  Reply.tool_call
    ~id
    ~name:"bash"
    ~arguments:
      (String.substr_replace_all (sleeper ?tag ()) ~pattern:"$DIR" ~with_:t.dir)
    ()
;;

(* The main client's run events since last time, with tool results. *)
let show_events t (h : Test_rpc.H.t) =
  Queue.iter h.sent ~f:(fun json ->
    match member_string json "event" with
    | "tool_end" ->
      let field name =
        Option.value_map (Json.member "result" json) ~default:"" ~f:(fun r ->
          match Json.member name r with
          | Some (`String s) -> s
          | Some json -> Json.to_string json
          | None -> "")
      in
      print_endline
        (mask
           t
           (sprintf
              "tool_end %s is_error=%s"
              (Json.to_string (`String (field "text")))
              (field "is_error")))
    | ( "message_start"
      | "message_end"
      | "turn_start"
      | "turn_end"
      | "agent_start"
      | "agent_end"
      | "tool_start" ) as e -> print_endline e
    | _ -> ());
  Queue.clear h.sent
;;

(* Aborts once [ready] holds, then prints whether the run ended within a
   second, the events and whether the commands' processes are gone. *)
let abort_and_show ?(tags = [ "" ]) t (h : Test_rpc.H.t) agent =
  printf
    "started: %b\n"
    (Test_tool_host.eventually t (fun () -> exists t "sleep.pid"));
  Queue.clear h.sent;
  Test_rpc.call t h "abort";
  let clock = Eio.Stdenv.clock t.env in
  let ended =
    Eio.Fiber.first
      (fun () ->
         Agent.wait_idle agent;
         true)
      (fun () ->
         Eio.Time.sleep clock 1.;
         false)
  in
  printf "turn ended within 1s: %b\n" ended;
  show_events t h;
  List.iter tags ~f:(fun tag ->
    if exists t ("sleep" ^ tag ^ ".pid")
    then (
      let shell = read_pid t ("shell" ^ tag ^ ".pid") in
      let sleep = read_pid t ("sleep" ^ tag ^ ".pid") in
      printf
        "processes%s killed: %b\n"
        tag
        (Test_tool_host.eventually t (fun () ->
           (not (alive shell)) && not (alive sleep)));
      printf "survived%s: %b\n" tag (exists t ("survived" ^ tag)))
    else printf "command%s never started\n" tag)
;;

let hello (h : Test_rpc.H.t) client params =
  ignore
    (Rpc_server.handle
       h.server
       client
       (Json.of_string
          (sprintf {|{"id": "r", "method": "hello", "params": %s}|} params))
     : Json.t)
;;

let with_server replies f =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent, h =
    Test_rpc.make_server t ~sw ~provider:(Faux_provider.create (replies t))
  in
  hello h h.client "{}";
  f t ~sw agent h
;;

(* [prigh tool-host], or [prigh web]'s own: {!Tool_host.connect}. *)
let with_network_host replies f =
  with_server replies
  @@ fun t ~sw agent h ->
  let host_dir = Filename.concat t.dir "host" in
  Core_unix.mkdir_p host_dir;
  let listener = Test_tool_host.Listener.start t ~sw h.server in
  let host =
    Test_tool_host.Host.start
      t
      ~sw
      ~port:listener.port
      ~host_id:"host-box"
      ~token:None
      ~cwd:host_dir
  in
  Test_tool_host.Host.wait_logs t host 1;
  Test_rpc.call t h ~params:{|{"host": "host-box"}|} "set_active_host";
  f t agent h
;;

let%expect_test "network tool host: abort ends the turn and kills the command" =
  with_network_host (fun t -> [ tool_call t ~id:"c1"; Reply.text "unreachable" ])
  @@ fun t agent h ->
  Test_rpc.call t h ~params:{|{"text": "sleep"}|} "prompt";
  abort_and_show t h agent;
  [%expect {| |}]
;;

let%expect_test "network tool host: abort skips the reply's other calls" =
  with_network_host (fun t ->
    let args tag =
      String.substr_replace_all (sleeper ~tag ()) ~pattern:"$DIR" ~with_:t.dir
    in
    [ Reply.tool_calls [ "c1", "bash", args ""; "c2", "bash", args "2" ]
    ; Reply.text "unreachable"
    ])
  @@ fun t agent h ->
  Test_rpc.call t h ~params:{|{"text": "sleep twice"}|} "prompt";
  abort_and_show ~tags:[ ""; "2" ] t h agent;
  [%expect {| |}]
;;

(* The backend's own tool host runs the command in-process. *)
let%expect_test "backend tool host: abort ends the turn and kills the command" =
  with_server (fun t -> [ tool_call t ~id:"c1"; Reply.text "unreachable" ])
  @@ fun t ~sw:_ agent h ->
  Test_rpc.call t h ~params:{|{"text": "sleep"}|} "prompt";
  abort_and_show t h agent;
  [%expect {| |}]
;;

(* A host that never answers the cancel (hung, or its connection is dead but
   not yet noticed): the turn ends anyway, and its late result is refused. *)
let%expect_test "silent tool host: abort ends the turn without its answer" =
  with_server (fun t -> [ tool_call t ~id:"c1"; Reply.text "unreachable" ])
  @@ fun t ~sw:_ agent h ->
  let sent = Queue.create () in
  let host = Rpc_server.connect h.server ~send:(Queue.enqueue sent) in
  hello
    h
    host
    {|{"name": "mute", "cwd": "/mute", "tools": true, "host_id": "host-mute"}|};
  Test_rpc.call t h ~params:{|{"host": "host-mute"}|} "set_active_host";
  Test_rpc.call t h ~params:{|{"text": "sleep"}|} "prompt";
  let exec = Test_rpc.wait_for_exec h host sent in
  let exec_id = member_string exec "exec_id" in
  printf "host runs: %s\n" (Test_rpc.exec_name exec);
  Queue.clear sent;
  Queue.clear h.sent;
  Test_rpc.call t h "abort";
  let clock = Eio.Stdenv.clock t.env in
  printf
    "turn ended within 1s: %b\n"
    (Eio.Fiber.first
       (fun () ->
          Agent.wait_idle agent;
          true)
       (fun () ->
          Eio.Time.sleep clock 1.;
          false));
  show_events t h;
  Queue.iter sent ~f:(fun json ->
    match member_string json "event" with
    | "tool_exec_cancel" ->
      printf
        "host told: tool_exec_cancel (same exec: %b)\n"
        (String.equal (member_string json "exec_id") exec_id)
    | _ -> ());
  print_endline
    (mask
       t
       (Json.to_string
          (Rpc_server.handle
             h.server
             host
             (Json.of_string
                (sprintf
                   {|{"id": "late", "method": "tool_exec_result", "params": {"exec_id": "%s", "text": "late"}}|}
                   exec_id)))));
  print_s
    [%sexp
      (List.filter_map (Agent.messages agent) ~f:(function
         | Message.Tool_result r -> Some r.text
         | _ -> None)
       : string list)];
  [%expect {| |}]
;;
