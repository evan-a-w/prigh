open! Core
open! Prigh
open Tool_test_helpers
open! Test_file_http

let%expect_test "file browser on a network tool host, and without any" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent, h =
    Test_rpc.make_server
      ~token:"sekrit"
      ~backend_host:false
      t
      ~sw
      ~provider:(Faux_provider.create [])
  in
  ignore (call t h "hello" {|{"token": "sekrit"}|} : Json.t);
  let (_ : string) = [%expect.output] in
  (* Without the backend host, nothing is reachable until a host connects:
     the backend's own files never are. *)
  write t "backend-secret.txt" "no";
  ignore (call t h "fs_list" "{}" : Json.t);
  ignore (call t h "fs_download" {|{"path": "backend-secret.txt"}|} : Json.t);
  [%expect
    {|
    {"type":"response","id":"r","ok":false,"error":"no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host, or talk without one in chat mode (/mode chat)"}
    {"type":"response","id":"r","ok":false,"error":"no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host, or talk without one in chat mode (/mode chat)"}
    |}];
  let host_dir = Filename.concat t.dir "host" in
  write t "host/readme.md" "# hi\n";
  let listener = Test_tool_host.Listener.start t ~sw h.server in
  let host =
    Test_tool_host.Host.start
      t
      ~sw
      ~port:listener.port
      ~token:(Some "sekrit")
      ~host_id:"host-box"
      ~cwd:host_dir
  in
  Test_tool_host.Host.wait_logs t host 1;
  ignore
    (Test_tool_host.eventually t (fun () ->
       String.equal (Agent.active_host agent) "host-box")
     : bool);
  [%expect {| connected to 127.0.0.1:PORT as client-2 |}];
  let port = http_port t ~sw h in
  ignore (call t h "fs_list" "{}" : Json.t);
  (* Several windows' worth of chunks each way. *)
  let size = (Host_files.chunk_size * 20) + 17 in
  let link =
    call
      t
      h
      "fs_upload"
      (sprintf {|{"name": "big.bin", "size": %d, "dir": "."}|} size)
  in
  show t (put t ~sw ~port (result_string link "url") (binary size));
  print_s [%sexp (String.equal (read t "host/big.bin") (binary size) : bool)];
  let link = call t h "fs_download" {|{"path": "big.bin"}|} in
  let r = get t ~sw ~port (result_string link "url") in
  print_endline r.status;
  print_s [%sexp (String.equal r.body (binary size) : bool)];
  ignore (call t h "fs_preview" {|{"path": "readme.md"}|} : Json.t);
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"path":"$DIR/host","parent":"$DIR","home":"$HOME","entries":[{"name":"readme.md","kind":"file","size":5,"mtime":<ms>}],"total":1,"hidden":0}}
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/upload?ticket=<ticket>","path":"./big.bin"}}
    HTTP/1.1 200 OK
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: <n>
      Content-Type: application/json
      X-Content-Type-Options: nosniff
      body: {\"path\":\"$DIR/host/big.bin\",\"bytes\":3932177}
    true
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/download?ticket=<ticket>","name":"big.bin"}}
    HTTP/1.1 200 OK
    true
    {"type":"response","id":"r","ok":true,"result":{"path":"readme.md","name":"readme.md","size":5,"mtime":<ms>,"kind":"text","text":"# hi\n","truncated":false,"md5":"<id><id>"}}
    |}]
;;
