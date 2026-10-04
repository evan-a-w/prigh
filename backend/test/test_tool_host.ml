open! Core
open! Prigh
open Tool_test_helpers
module Reply = Faux_provider.Reply
module Json = Jsonaf

let eventually ?(timeout = 10.) t f =
  let clock = Eio.Stdenv.clock t.env in
  let deadline = Eio.Time.now clock +. timeout in
  let rec go () =
    if f ()
    then true
    else if Float.( > ) (Eio.Time.now clock) deadline
    then false
    else (
      Eio.Time.sleep clock 0.01;
      go ())
  in
  go ()
;;

(* A [-listen]-style JSON-lines port; [drop] closes every connection from
   the server side. *)
module Listener = struct
  type t =
    { port : int
    ; flows : (unit -> unit) list ref
    }

  let start t ~sw server =
    let socket =
      Eio.Net.listen
        ~sw
        ~backlog:16
        ~reuse_addr:true
        (Eio.Stdenv.net t.env)
        (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
    in
    let port =
      match Eio.Net.listening_addr socket with
      | `Tcp (_, port) -> port
      | `Unix _ -> assert false
    in
    let flows = ref [] in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      while true do
        let flow, _ = Eio.Net.accept ~sw socket in
        flows
        := (fun () ->
             try Eio.Flow.shutdown flow `All with
             | _ -> ())
           :: !flows;
        Eio.Fiber.fork_daemon ~sw (fun () ->
          Rpc_server.serve_connection server ~input:flow ~output:flow;
          `Stop_daemon)
      done;
      `Stop_daemon);
    { port; flows }
  ;;

  let drop t =
    List.iter !(t.flows) ~f:(fun shutdown -> shutdown ());
    t.flows := []
  ;;
end

module Host = struct
  type t = { logs : string Queue.t }

  (* Runs [Tool_host.connect] until [sw] ends; logs are kept, with the port
     masked. *)
  let start ?terminals t ~sw ~port ~token ~cwd =
    let logs = Queue.create () in
    Eio.Fiber.fork_daemon ~sw (fun () ->
      Tool_host.connect
        ~env:t.env
        ?terminals
        ~log:(fun line ->
          Queue.enqueue
            logs
            (String.substr_replace_all
               line
               ~pattern:(sprintf ":%d" port)
               ~with_:":PORT"))
        ~initial_backoff:(Time_ns.Span.of_sec 0.05)
        ~max_backoff:(Time_ns.Span.of_sec 0.2)
        ~host:"127.0.0.1"
        ~port
        ~token
        ~name:"box"
        ~cwd
        ());
    { logs }
  ;;

  let wait_logs t host n =
    ignore (eventually t (fun () -> Queue.length host.logs >= n) : bool);
    List.iter (List.take (Queue.to_list host.logs) n) ~f:print_endline;
    for _ = 1 to n do
      ignore (Queue.dequeue host.logs : string option)
    done
  ;;
end

let call t (h : Test_rpc.H.t) meth params =
  print_endline
    (mask
       t
       (Json.to_string
          (Rpc_server.handle
             h.server
             h.client
             (Json.of_string
                (sprintf
                   {|{"id": "r", "method": "%s", "params": %s}|}
                   meth
                   params)))))
;;

let show_hosts t agent =
  printf
    "active=%s hosts=%s\n"
    (Agent.active_host agent)
    (mask t (Sexp.to_string [%sexp (Agent.hosts agent : Agent.Host.t list)]))
;;

let tool_results t agent =
  List.iter (Agent.messages agent) ~f:(function
    | Message.Tool_result r ->
      print_endline (mask t (sprintf "tool_result: %S" r.text))
    | _ -> ())
;;

let%expect_test "network tool host: hello, run tools, reconnect, bad token" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let host_dir = Filename.concat t.dir "host" in
  Core_unix.mkdir_p host_dir;
  let agent, h =
    Test_rpc.make_server
      ~token:"sekrit"
      t
      ~sw
      ~provider:
        (Faux_provider.create
           [ Reply.tool_call
               ~id:"c1"
               ~name:"bash"
               ~arguments:{|{"command":"pwd; echo from-host"}|}
               ()
           ; Reply.text "done"
           ])
  in
  call t h "hello" {|{"token": "sekrit"}|};
  let listener = Listener.start t ~sw h.server in
  let host =
    Host.start t ~sw ~port:listener.port ~token:(Some "sekrit") ~cwd:host_dir
  in
  Host.wait_logs t host 1;
  show_hosts t agent;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"client_id":"client-1","state":{"session_id":"<id>","session_path":"$DIR/sessions/<stamp>_<id>.jsonl","session_name":null,"session_description":null,"cwd":"$DIR","git_branch":null,"model":{"id":"deepseek-flash","provider":"deepseek","key":"deepseek/deepseek-flash","name":"DeepSeek V4.1 Flash","context_window":1000000,"max_output":384000,"supports_thinking":true,"cost":{"input":0.3,"output":1.2,"cache_read":0.006}},"thinking":"off","running":false,"message_count":0,"usage":{"input":0,"output":0,"cache_read":0},"cost_usd":0,"context_tokens":0,"active_host":"backend","hosts":[{"id":"backend","name":"<host>","cwd":"$DIR","session_id":null,"session_name":null}]}}}
    connected to 127.0.0.1:PORT as client-2
    active=backend hosts=(((id backend)(name <host>)(cwd $DIR)(session_id())(session_name()))((id client-2)(name box)(cwd $DIR/host)(session_id(<id>))(session_name())))
    |}];
  (* Switching resolves the directory on the host; the bash call runs there. *)
  call t h "set_active_host" {|{"host": "client-2"}|};
  call t h "prompt" {|{"text": "run it"}|};
  Agent.wait_idle agent;
  show_hosts t agent;
  tool_results t agent;
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{}}
    {"type":"response","id":"r","ok":true,"result":{}}
    active=client-2 hosts=(((id backend)(name <host>)(cwd $DIR)(session_id())(session_name()))((id client-2)(name box)(cwd $DIR/host)(session_id(<id>))(session_name())))
    tool_result: "$DIR/host\nfrom-host\n"
    |}];
  (* The server dropping the connection: the host comes back as a new
     client. *)
  Listener.drop listener;
  Host.wait_logs t host 3;
  ignore
    (eventually t (fun () ->
       List.exists (Agent.hosts agent) ~f:(fun h ->
         String.equal h.id "client-3"))
     : bool);
  show_hosts t agent;
  [%expect
    {|
    disconnected from 127.0.0.1:PORT
    retrying in 50ms
    connected to 127.0.0.1:PORT as client-3
    active=client-2 hosts=(((id backend)(name <host>)(cwd $DIR)(session_id())(session_name()))((id client-3)(name box)(cwd $DIR/host)(session_id(<id>))(session_name())))
    |}];
  (* A wrong token is refused, and retried with a growing delay. *)
  Eio.Switch.run (fun sw ->
    let bad =
      Host.start t ~sw ~port:listener.port ~token:(Some "nope") ~cwd:host_dir
    in
    Host.wait_logs t bad 8);
  [%expect
    {|
    hello failed: unauthorised: bad or missing token
    retrying in 50ms
    hello failed: unauthorised: bad or missing token
    retrying in 100ms
    hello failed: unauthorised: bad or missing token
    retrying in 200ms
    hello failed: unauthorised: bad or missing token
    retrying in 200ms
    |}];
  (* Nothing listening. *)
  let closed_port =
    Eio.Switch.run (fun sw -> (Listener.start t ~sw h.server).port)
  in
  Eio.Switch.run (fun sw ->
    let host = Host.start t ~sw ~port:closed_port ~token:None ~cwd:host_dir in
    ignore (eventually t (fun () -> Queue.length host.logs >= 2) : bool);
    List.iter
      (List.take (Queue.to_list host.logs) 2)
      ~f:(fun line ->
        print_endline
          (match String.substr_index line ~pattern:"PORT: " with
           | Some i -> String.prefix line (i + 4) ^ " ..."
           | None -> line)));
  [%expect
    {|
    cannot connect to 127.0.0.1:PORT ...
    retrying in 50ms
    |}]
;;

(* The stdio worker, driven over pipes like a frontend does. *)
module Worker = struct
  type t =
    { input : Eio_unix.sink_ty Eio.Resource.t
    ; output : Eio.Buf_read.t
    }

  let start ?terminals (sandbox : Tool_test_helpers.t) ~sw =
    let input_r, input_w = Eio_unix.pipe sw in
    let output_r, output_w = Eio_unix.pipe sw in
    Eio.Fiber.fork ~sw (fun () ->
      Tool_host.run
        ~env:sandbox.env
        ?terminals
        ~input:input_r
        ~output:output_w
        ();
      Eio.Resource.close output_w;
      print_endline "(worker finished)");
    { input = (input_w :> Eio_unix.sink_ty Eio.Resource.t)
    ; output = Eio.Buf_read.of_flow output_r ~max_size:(1024 * 1024)
    }
  ;;

  let send t line = Eio.Flow.copy_string (line ^ "\n") t.input

  let read_line t =
    match Eio.Buf_read.line t.output with
    | line -> Some line
    | exception End_of_file -> None
  ;;

  let close t = Eio.Resource.close t.input
end

let%expect_test "stdio worker: exec, output, cancel" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let w = Worker.start t ~sw in
  Worker.send
    w
    (sprintf
       {|{"type":"exec","exec_id":"e1","name":"bash","arguments":{"command":"echo one $PWD"},"cwd":"%s"}|}
       t.dir);
  let rec until_result () =
    match Worker.read_line w with
    | None -> ()
    | Some line ->
      print_endline (mask t line);
      if not (String.is_substring line ~substring:{|"type":"result"|})
      then until_result ()
  in
  until_result ();
  Worker.send
    w
    {|{"type":"exec","exec_id":"e2","name":"bash","arguments":{"command":"sleep 30"}}|};
  Worker.send w {|{"type":"cancel","exec_id":"e2"}|};
  Option.iter (Worker.read_line w) ~f:print_endline;
  Worker.send w "not json";
  Worker.close w;
  print_s [%sexp (Worker.read_line w : string option)];
  [%expect
    {|
    {"type":"output","exec_id":"e1","chunk":"one $DIR\n"}
    {"type":"result","exec_id":"e1","text":"one $DIR\n","is_error":false}
    {"type":"result","exec_id":"e2","text":"[cancelled]","is_error":true}
    (worker finished)
    ()
    |}]
;;
