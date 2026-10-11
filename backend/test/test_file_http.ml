open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* The file browser end to end: its RPC methods, and the web server's
   ticketed transfers, on the backend host and on a network tool host. *)

let ticket_re = Re.compile (Re.Perl.re {|ticket=[0-9a-f]{32}|})
let mtime_re = Re.compile (Re.Perl.re {|"mtime":[0-9]+|})

let mask t s =
  let home = Option.value (Sys.getenv "HOME") ~default:"/" in
  Re.replace_string ticket_re ~by:"ticket=<ticket>" s
  |> mask t
  |> String.substr_replace_all
       ~pattern:(sprintf {|"parent":"%s"|} (Filename.dirname t.dir))
       ~with_:{|"parent":"$PARENT"|}
  |> String.substr_replace_all
       ~pattern:(sprintf {|"home":"%s"|} home)
       ~with_:{|"home":"$HOME"|}
  |> Re.replace_string mtime_re ~by:{|"mtime":<ms>|}
;;

let call t (h : Test_rpc.H.t) meth params =
  Rpc_server.handle
    h.server
    h.client
    (Json.of_string
       (sprintf {|{"id": "r", "method": "%s", "params": %s}|} meth params))
  |> fun reply ->
  print_endline (mask t (Json.to_string reply));
  reply
;;

let result_string reply key =
  match Json.member "result" reply with
  | Some result ->
    (match Json.member key result with
     | Some (`String s) -> s
     | _ -> "")
  | None -> ""
;;

let http_port t ~sw (h : Test_rpc.H.t) =
  Web_server.listen
    ~env:t.env
    ~sw
    ~addr:Eio.Net.Ipaddr.V4.loopback
    ~port:0
    ~root:None
    ~websockets:[ "/ws", Web_server.serve_rpc (Rpc_router.single h.server) ]
    ~on_lines:(Rpc_router.serve_lines (Rpc_router.single h.server))
;;

module Response = struct
  type t =
    { status : string
    ; headers : string list
    ; body : string
    }
end

(* One request on its own connection: [head] without the blank line. *)
let request t ~sw ~port ?(body = "") head =
  let flow =
    Eio.Net.connect
      ~sw
      (Eio.Stdenv.net t.env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, port))
  in
  Eio.Flow.copy_string (head ^ "\r\n\r\n" ^ body) flow;
  let reader = Eio.Buf_read.of_flow flow ~max_size:(64 * 1024 * 1024) in
  let rec headers acc =
    match Eio.Buf_read.line reader with
    | "" -> List.rev acc
    | line -> headers (line :: acc)
  in
  let status = Eio.Buf_read.line reader in
  let headers = headers [] in
  let body = Eio.Buf_read.take_all reader in
  Eio.Flow.close flow;
  { Response.status; headers; body }
;;

let show ?(body = true) t (r : Response.t) =
  print_endline r.status;
  (* The length of a body that names the sandbox varies with it. *)
  let varies = String.is_substring r.body ~substring:t.dir in
  List.iter (List.sort r.headers ~compare:String.compare) ~f:(fun h ->
    if varies && String.is_prefix h ~prefix:"Content-Length:"
    then print_endline "  Content-Length: <n>"
    else printf "  %s\n" (mask t h));
  if body && not (String.is_empty r.body)
  then printf "  body: %s\n" (mask t (String.escaped r.body))
;;

let get t ~sw ~port url =
  request t ~sw ~port (sprintf "GET %s HTTP/1.1\r\nHost: x" url)
;;

let put t ~sw ~port ?length url body =
  request
    t
    ~sw
    ~port
    ~body
    (sprintf
       "PUT %s HTTP/1.1\r\nHost: x\r\nContent-Length: %d"
       url
       (Option.value length ~default:(String.length body)))
;;

let binary n = String.init n ~f:(fun i -> Char.of_int_exn (i * 7 % 256))

let%expect_test "file browser over RPC and HTTP on the backend host" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let _agent, h =
    Test_rpc.make_server
      ~token:"sekrit"
      t
      ~sw
      ~provider:(Faux_provider.create [])
  in
  ignore (call t h "hello" {|{"token": "sekrit"}|} : Json.t);
  let port = http_port t ~sw h in
  write t "notes.txt" "hello\n";
  write t "src/a.ml" "let a = 1\n";
  write t "shot.png" (Image_fixtures.bytes Image_fixtures.png_3x2);
  write t "blob.bin" (binary 1000);
  let (_ : string) = [%expect.output] in
  ignore (call t h "fs_list" "{}" : Json.t);
  ignore (call t h "fs_list" {|{"path": "src", "hidden": true}|} : Json.t);
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"path":"$DIR","parent":"$PARENT","home":"$HOME","entries":[{"name":"blob.bin","kind":"file","size":1000,"mtime":<ms>},{"name":"notes.txt","kind":"file","size":6,"mtime":<ms>},{"name":"shot.png","kind":"file","size":84,"mtime":<ms>},{"name":"src","kind":"dir","mtime":<ms>}],"total":4,"hidden":0}}
    {"type":"response","id":"r","ok":true,"result":{"path":"$DIR/src","parent":"$DIR","home":"$HOME","entries":[{"name":"a.ml","kind":"file","size":10,"mtime":<ms>}],"total":1,"hidden":0}}
    |}];
  (* Previews: text inline, an image by a reusable inline link, a binary
     file only named. *)
  ignore (call t h "fs_preview" {|{"path": "notes.txt"}|} : Json.t);
  let image = call t h "fs_preview" {|{"path": "shot.png"}|} in
  ignore (call t h "fs_preview" {|{"path": "blob.bin"}|} : Json.t);
  let url = result_string image "url" in
  show ~body:false t (get t ~sw ~port url);
  print_s [%sexp (String.length (get t ~sw ~port url).body : int)];
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"path":"notes.txt","name":"notes.txt","size":6,"mtime":<ms>,"kind":"text","text":"hello\n","truncated":false}}
    {"type":"response","id":"r","ok":true,"result":{"path":"shot.png","name":"shot.png","size":84,"mtime":<ms>,"kind":"image","mime":"image/png","url":"/files/download?ticket=<ticket>"}}
    {"type":"response","id":"r","ok":true,"result":{"path":"blob.bin","name":"blob.bin","size":1000,"mtime":<ms>,"kind":"binary"}}
    HTTP/1.1 200 OK
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Disposition: inline; filename="shot.png"; filename*=UTF-8''shot.png
      Content-Length: 84
      Content-Security-Policy: sandbox; default-src 'none'
      Content-Type: image/png
      X-Content-Type-Options: nosniff
    84
    |}];
  (* A download link works once. *)
  let link = call t h "fs_download" {|{"path": "blob.bin"}|} in
  let url = result_string link "url" in
  let r = get t ~sw ~port url in
  show ~body:false t r;
  print_s [%sexp (String.equal r.body (binary 1000) : bool)];
  show t (get t ~sw ~port url);
  show t (get t ~sw ~port "/files/download?ticket=0123");
  ignore (call t h "fs_download" {|{"path": "src"}|} : Json.t);
  ignore
    (call t h "fs_download" {|{"path": "notes.txt", "archive": true}|} : Json.t);
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/download?ticket=<ticket>","name":"blob.bin"}}
    HTTP/1.1 200 OK
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Disposition: attachment; filename="blob.bin"; filename*=UTF-8''blob.bin
      Content-Length: 1000
      Content-Security-Policy: sandbox; default-src 'none'
      Content-Type: application/octet-stream
      X-Content-Type-Options: nosniff
    true
    HTTP/1.1 410 Gone
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: 90
      Content-Type: text/plain; charset=utf-8
      X-Content-Type-Options: nosniff
      body: this link has expired or was used already: start the transfer again from the file browser\n
    HTTP/1.1 410 Gone
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: 90
      Content-Type: text/plain; charset=utf-8
      X-Content-Type-Options: nosniff
      body: this link has expired or was used already: start the transfer again from the file browser\n
    {"type":"response","id":"r","ok":false,"error":"$DIR/src is a directory: download it as an archive"}
    {"type":"response","id":"r","ok":false,"error":"not a directory: $DIR/notes.txt"}
    |}];
  (* A directory as a .tar.gz. *)
  let link = call t h "fs_download" {|{"path": "src", "archive": true}|} in
  let r = get t ~sw ~port (result_string link "url") in
  show ~body:false t r;
  let archive = Filename.concat t.dir "got.tar.gz" in
  Out_channel.write_all archive ~data:r.body;
  print_s
    [%sexp
      (Core_unix.open_process_in
         (sprintf "tar -tzf %s" (Filename.quote archive))
       |> In_channel.input_lines
       : string list)];
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/download?ticket=<ticket>","name":"src.tar.gz"}}
    HTTP/1.1 200 OK
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Disposition: attachment; filename="src.tar.gz"; filename*=UTF-8''src.tar.gz
      Content-Security-Policy: sandbox; default-src 'none'
      Content-Type: application/gzip
      X-Content-Type-Options: nosniff
    (src/ src/a.ml)
    |}];
  (* Uploads: into a directory, refusing to overwrite unless asked; into the
     chat's uploads directory under a free name. *)
  let upload params body =
    let link = call t h "fs_upload" params in
    match result_string link "url" with
    | "" -> ()
    | url -> show t (put t ~sw ~port url body)
  in
  upload {|{"name": "new.bin", "size": 300000, "dir": "src"}|} (binary 300000);
  print_s [%sexp (String.equal (read t "src/new.bin") (binary 300000) : bool)];
  upload {|{"name": "a.ml", "size": 3, "dir": "src"}|} "new";
  upload {|{"name": "a.ml", "size": 3, "dir": "src", "overwrite": true}|} "new";
  print_endline (read t "src/a.ml");
  upload {|{"name": "pasted.txt", "size": 2}|} "hi";
  upload {|{"name": "pasted.txt", "size": 2}|} "ho";
  print_s
    [%sexp
      (Sys_unix.readdir (Filename.concat t.dir ".prigh/uploads")
       |> Array.to_list
       |> List.sort ~compare:String.compare
       : string list)];
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/upload?ticket=<ticket>","path":"src/new.bin"}}
    HTTP/1.1 200 OK
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: <n>
      Content-Type: application/json
      X-Content-Type-Options: nosniff
      body: {\"path\":\"$DIR/src/new.bin\",\"bytes\":300000}
    true
    {"type":"response","id":"r","ok":false,"error":"a.ml already exists in src: overwrite it or rename the upload"}
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/upload?ticket=<ticket>","path":"src/a.ml"}}
    HTTP/1.1 200 OK
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: <n>
      Content-Type: application/json
      X-Content-Type-Options: nosniff
      body: {\"path\":\"$DIR/src/a.ml\",\"bytes\":3}
    new
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/upload?ticket=<ticket>","path":"$DIR/.prigh/uploads/pasted.txt"}}
    HTTP/1.1 200 OK
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: <n>
      Content-Type: application/json
      X-Content-Type-Options: nosniff
      body: {\"path\":\"$DIR/.prigh/uploads/pasted.txt\",\"bytes\":2}
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/upload?ticket=<ticket>","path":"$DIR/.prigh/uploads/pasted.txt"}}
    HTTP/1.1 200 OK
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: <n>
      Content-Type: application/json
      X-Content-Type-Options: nosniff
      body: {\"path\":\"$DIR/.prigh/uploads/pasted-2.txt\",\"bytes\":2}
    (.gitignore pasted-2.txt pasted.txt)
    |}];
  (* What the endpoints refuse. *)
  let link =
    call t h "fs_upload" {|{"name": "x.txt", "size": 5, "dir": "."}|}
  in
  let url = result_string link "url" in
  show t (put t ~sw ~port ~length:4 url "abcd");
  show t (put t ~sw ~port url "abcde");
  ignore
    (call t h "fs_upload" {|{"name": "../x", "size": 1, "dir": "."}|} : Json.t);
  ignore
    (call t h "fs_upload" {|{"name": "big", "size": 99999999999, "dir": "."}|}
     : Json.t);
  let link = call t h "fs_download" {|{"path": "notes.txt"}|} in
  show
    t
    (put
       t
       ~sw
       ~port
       (String.substr_replace_first
          (result_string link "url")
          ~pattern:"download"
          ~with_:"upload")
       "x");
  show t (request t ~sw ~port "DELETE /files/download HTTP/1.1");
  show t (request t ~sw ~port "OPTIONS /files/upload HTTP/1.1");
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/upload?ticket=<ticket>","path":"./x.txt"}}
    HTTP/1.1 400 Bad Request
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: 68
      Content-Type: application/json
      X-Content-Type-Options: nosniff
      body: {\"error\":\"the upload was announced as 5 bytes but has 4: try again\"}
    HTTP/1.1 410 Gone
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: 101
      Content-Type: application/json
      X-Content-Type-Options: nosniff
      body: {\"error\":\"this link has expired or was used already: start the transfer again from the file browser\"}
    {"type":"response","id":"r","ok":false,"error":"\"../x\": a name cannot contain /"}
    {"type":"response","id":"r","ok":false,"error":"big is too large to upload (99999999999 bytes; at most 2147483648): copy it to the host another way (scp, git)"}
    {"type":"response","id":"r","ok":true,"result":{"url":"/files/download?ticket=<ticket>","name":"notes.txt"}}
    HTTP/1.1 400 Bad Request
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: 35
      Content-Type: application/json
      X-Content-Type-Options: nosniff
      body: {\"error\":\"this is a download link\"}
    HTTP/1.1 405 Method Not Allowed
      Access-Control-Allow-Origin: *
      Cache-Control: no-store
      Connection: close
      Content-Length: 41
      Content-Type: text/plain; charset=utf-8
      X-Content-Type-Options: nosniff
      body: GET /files/download or PUT /files/upload\n
    HTTP/1.1 204 No Content
      Access-Control-Allow-Headers: Content-Type
      Access-Control-Allow-Methods: GET, PUT, POST, OPTIONS
      Access-Control-Allow-Origin: *
      Access-Control-Max-Age: 600
      Cache-Control: no-store
      Connection: close
      Content-Length: 0
      X-Content-Type-Options: nosniff
    |}];
  (* mkdir, rename, delete. *)
  ignore (call t h "fs_mkdir" {|{"path": "made"}|} : Json.t);
  ignore (call t h "fs_rename" {|{"from": "made", "to": "renamed"}|} : Json.t);
  ignore (call t h "fs_delete" {|{"path": "renamed"}|} : Json.t);
  ignore (call t h "fs_delete" {|{"path": "renamed"}|} : Json.t);
  ignore (call t h "fs_rename" {|{"from": "notes.txt"}|} : Json.t);
  [%expect
    {|
    {"type":"response","id":"r","ok":true,"result":{"path":"$DIR/made"}}
    {"type":"response","id":"r","ok":true,"result":{"path":"$DIR/renamed"}}
    {"type":"response","id":"r","ok":true,"result":{"path":"$DIR/renamed"}}
    {"type":"response","id":"r","ok":false,"error":"$DIR/renamed does not exist"}
    {"type":"response","id":"r","ok":false,"error":"missing param \"to\""}
    |}]
;;

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
    {"type":"response","id":"r","ok":false,"error":"no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host"}
    {"type":"response","id":"r","ok":false,"error":"no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host"}
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
    {"type":"response","id":"r","ok":true,"result":{"path":"readme.md","name":"readme.md","size":5,"mtime":<ms>,"kind":"text","text":"# hi\n","truncated":false}}
    |}]
;;

(* A tool host from before the file browser, played by a test client. *)
let%expect_test "file browser on a tool host that predates it" =
  with_sandbox
  @@ fun t ->
  Eio.Switch.run
  @@ fun sw ->
  let agent, h =
    Test_rpc.make_server
      ~backend_host:false
      t
      ~sw
      ~provider:(Faux_provider.create [])
  in
  let sent = Queue.create () in
  let laptop = Rpc_server.connect h.server ~send:(Queue.enqueue sent) in
  ignore
    (Rpc_server.handle
       h.server
       laptop
       (Json.of_string
          (sprintf
             {|{"id": 1, "method": "hello", "params": %s}|}
             (Test_rpc.hello_params
                ~session:agent
                (sprintf
                   {|"name": "laptop", "tools": true, "cwd": "%s", "features": ["exec_input"]|}
                   t.dir))))
     : Json.t);
  Eio.Fiber.fork ~sw (fun () ->
    let exec = Test_rpc.wait_for_exec h laptop sent in
    let exec_id =
      Json.string_exn (Option.value_exn (Json.member "exec_id" exec))
    in
    ignore
      (Rpc_server.handle
         h.server
         laptop
         (Json.of_string
            (sprintf
               {|{"id": 2, "method": "tool_exec_result", "params": {"exec_id": "%s", "text": "unknown host tool \"$fs_list\"", "is_error": true}}|}
               exec_id))
       : Json.t));
  ignore (call t h "fs_list" "{}" : Json.t);
  [%expect
    {| {"type":"response","id":"r","ok":false,"error":"the file browser needs a newer prigh on tool host laptop: update prigh there and restart its TUI or tool-host"} |}]
;;
