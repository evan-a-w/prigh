open! Core
open! Prigh
open Tool_test_helpers
module Json = Jsonaf

(* The file browser: host operations ([Host_files] through [Host_ops]), the
   RPC methods and the web server's transfers, on the backend host and on a
   network tool host. *)

let mtime_re = Re.compile (Re.Perl.re {|"mtime":[0-9]+|})
let ticket_re = Re.compile (Re.Perl.re {|ticket=[0-9a-f]{32}|})
let temp_re = Re.compile (Re.Perl.re {|prigh-upload-[0-9]+-[0-9]+|})

let mask t s =
  let home = Option.value (Sys.getenv "HOME") ~default:"/" in
  mask t s
  |> Re.replace_string mtime_re ~by:{|"mtime":<ms>|}
  |> Re.replace_string ticket_re ~by:"ticket=<ticket>"
  |> Re.replace_string temp_re ~by:"prigh-upload-<n>"
  |> String.substr_replace_all
       ~pattern:(sprintf {|"parent":"%s"|} (Filename.dirname t.dir))
       ~with_:{|"parent":"$PARENT"|}
  |> String.substr_replace_all
       ~pattern:(sprintf {|"home":"%s"|} home)
       ~with_:{|"home":"$HOME"|}
  |> String.substr_replace_all
       ~pattern:(sprintf "delete %s" home)
       ~with_:"delete $HOME"
;;

let run_op
      ?stdin
      ?(cancel = Cancellation.never)
      ?(on_output = ignore)
      t
      name
      args
  =
  Host_ops.execute
    ~mcp:None
    ~env:t.env
    ?stdin
    ~cancel
    ~on_output
    ~cwd:t.dir
    ~name
    ~arguments:(Json.of_string args)
    ()
;;

let show t (result : Tool.Result.t) =
  print_endline
    (mask t ((if result.is_error then "ERROR: " else "") ^ result.text))
;;

let op ?stdin ?cancel ?on_output t name args =
  show t (run_op ?stdin ?cancel ?on_output t name args)
;;

let settle t = Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.05

let%expect_test "host ops: list, stat and read" =
  with_sandbox
  @@ fun t ->
  write t "b.txt" "bee\n";
  write t "a.ml" "let x = 1\n";
  write t ".hidden" "h";
  write t "sub/inner.txt" "in";
  Core_unix.symlink ~target:"sub" ~link_name:(Filename.concat t.dir "to-sub");
  Core_unix.symlink ~target:"gone" ~link_name:(Filename.concat t.dir "dangling");
  op t Host_files.list_op {|{"path": "."}|};
  [%expect
    {| {"path":"$DIR","parent":"$PARENT","home":"$HOME","entries":[{"name":"a.ml","kind":"file","size":10,"mtime":<ms>},{"name":"b.txt","kind":"file","size":4,"mtime":<ms>},{"name":"dangling","kind":"link","mtime":<ms>,"target":"missing"},{"name":"sub","kind":"dir","mtime":<ms>},{"name":"to-sub","kind":"link","mtime":<ms>,"target":"dir"}],"total":5,"hidden":1} |}];
  (* Dot files on request; a file lists its directory, selecting it. *)
  op t Host_files.list_op {|{"path": "sub/inner.txt", "hidden": true}|};
  op t Host_files.list_op {|{"path": "to-sub", "hidden": true}|};
  op t Host_files.list_op {|{"path": "nope"}|};
  op t Host_files.list_op {|{"path": ""}|};
  [%expect
    {|
    {"path":"$DIR/sub","parent":"$DIR","home":"$HOME","entries":[{"name":"inner.txt","kind":"file","size":2,"mtime":<ms>}],"total":1,"hidden":0,"select":"inner.txt"}
    {"path":"$DIR/sub","parent":"$DIR","home":"$HOME","entries":[{"name":"inner.txt","kind":"file","size":2,"mtime":<ms>}],"total":1,"hidden":0}
    ERROR: $DIR/nope does not exist
    ERROR: path is empty
    |}];
  op t Host_files.stat_op {|{"path": "a.ml"}|};
  op t Host_files.read_op {|{"path": "a.ml", "offset": 4, "length": 3}|};
  op t Host_files.read_op {|{"path": "a.ml", "offset": 4}|};
  op t Host_files.read_op {|{"path": "sub"}|};
  [%expect
    {|
    {"path":"$DIR/a.ml","kind":"file","size":10,"mtime":<ms>}
    {"size":10,"mtime":<ms>,"data":"eCA9","eof":false}
    {"size":10,"mtime":<ms>,"data":"eCA9IDEK","eof":true}
    ERROR: $DIR/sub is a directory
    |}]
;;

let%expect_test "host ops: mkdir, rename and delete, and what they refuse" =
  with_sandbox
  @@ fun t ->
  write t "f.txt" "x";
  write t "g.txt" "y";
  write t "tree/a/b.txt" "deep";
  op t Host_files.mkdir_op {|{"path": "new"}|};
  op t Host_files.mkdir_op {|{"path": "new"}|};
  op
    t
    Host_files.mkdir_op
    {|{"path": "up/loads", "parents": true, "ignore_all": true}|};
  print_endline (read t "up/loads/.gitignore");
  op t Host_files.mkdir_op {|{"path": "x/.."}|};
  [%expect
    {|
    {"path":"$DIR/new"}
    ERROR: $DIR/new already exists
    {"path":"$DIR/up/loads"}
    *

    ERROR: ".." is not a name
    |}];
  op t Host_files.rename_op {|{"from": "f.txt", "to": "g.txt"}|};
  op
    t
    Host_files.rename_op
    {|{"from": "f.txt", "to": "g.txt", "overwrite": true}|};
  op t Host_files.rename_op {|{"from": "g.txt", "to": "new/h.txt"}|};
  op t Host_files.rename_op {|{"from": "missing", "to": "m"}|};
  print_endline (read t "new/h.txt");
  [%expect
    {|
    ERROR: $DIR/g.txt already exists
    {"path":"$DIR/g.txt"}
    {"path":"$DIR/new/h.txt"}
    ERROR: $DIR/missing does not exist
    x
    |}];
  Core_unix.symlink ~target:t.dir ~link_name:(Filename.concat t.dir "tree/loop");
  op t Host_files.delete_op {|{"path": "tree"}|};
  op t Host_files.delete_op {|{"path": "tree"}|};
  op t Host_files.delete_op {|{"path": "/"}|};
  op t Host_files.delete_op {|{"path": "~"}|};
  (* The symlink went, not what it pointed to. *)
  print_s
    [%sexp
      (Sys_unix.readdir t.dir
       |> Array.to_list
       |> List.sort ~compare:String.compare
       : string list)];
  [%expect
    {|
    {"path":"$DIR/tree"}
    ERROR: $DIR/tree does not exist
    ERROR: refusing to delete /
    ERROR: refusing to delete $HOME
    (new up)
    |}]
;;

let%expect_test "host ops: downloads stream within the credits granted" =
  with_sandbox
  @@ fun t ->
  let chunk = Host_files.chunk_size in
  write
    t
    "big.bin"
    (String.init ((chunk * 4) + 10) ~f:(fun i -> Char.of_int_exn (i % 256)));
  Eio.Switch.run
  @@ fun sw ->
  let feed = Stdin_feed.create () in
  let chunks = Queue.create () in
  let result = ref None in
  Eio.Fiber.fork ~sw (fun () ->
    result
    := Some
         (run_op
            ~stdin:feed
            ~on_output:(Queue.enqueue chunks)
            t
            Host_files.download_op
            {|{"path": "big.bin", "window": 2}|}));
  let progress () =
    settle t;
    printf
      "%d chunks, %d bytes, done: %b\n"
      (Queue.length chunks)
      (Queue.sum
         (module Int)
         chunks
         ~f:(fun c -> String.length (Base64.decode_exn c)))
      (Option.is_some !result)
  in
  progress ();
  ignore (Stdin_feed.write feed "+" ~close:false : unit Or_error.t);
  progress ();
  ignore (Stdin_feed.write feed "+++" ~close:false : unit Or_error.t);
  progress ();
  Option.iter !result ~f:(show t);
  print_s
    [%sexp
      (String.equal
         (String.concat (List.map (Queue.to_list chunks) ~f:Base64.decode_exn))
         (read t "big.bin")
       : bool)];
  [%expect
    {|
    2 chunks, 393216 bytes, done: false
    3 chunks, 589824 bytes, done: false
    5 chunks, 786442 bytes, done: true
    {"bytes":786442}
    true
    |}];
  (* The reader going away (end of credits) stops it. *)
  let feed = Stdin_feed.create () in
  ignore (Stdin_feed.write feed "" ~close:true : unit Or_error.t);
  op ~stdin:feed t Host_files.download_op {|{"path": "big.bin", "window": 1}|};
  op t Host_files.download_op {|{"path": "big.bin"}|};
  op ~stdin:(Stdin_feed.create ()) t Host_files.download_op {|{"path": "."}|};
  [%expect
    {|
    ERROR: [cancelled]
    ERROR: $fs_download needs a stdin feed for its credits
    ERROR: $DIR is a directory: download it as an archive
    |}]
;;

let%expect_test "host ops: a directory downloads as a .tar.gz" =
  with_sandbox
  @@ fun t ->
  write t "proj/a.txt" "a";
  write t "proj/src/b.ml" "b";
  let feed = Stdin_feed.create () in
  let chunks = Queue.create () in
  let result =
    run_op
      ~stdin:feed
      ~on_output:(Queue.enqueue chunks)
      t
      Host_files.download_op
      {|{"path": "proj", "archive": true, "window": 256}|}
  in
  (* The size of a .tar.gz varies with tar and gzip. *)
  print_s [%sexp (result.is_error : bool)];
  let archive = Filename.concat t.dir "out.tar.gz" in
  Out_channel.write_all
    archive
    ~data:(String.concat (List.map (Queue.to_list chunks) ~f:Base64.decode_exn));
  let listing =
    Core_unix.open_process_in (sprintf "tar -tzf %s" (Filename.quote archive))
    |> In_channel.input_lines
    |> List.sort ~compare:String.compare
  in
  print_s [%sexp (listing : string list)];
  op
    ~stdin:feed
    t
    Host_files.download_op
    {|{"path": "proj/a.txt", "archive": true}|};
  op ~stdin:feed t Host_files.download_op {|{"path": "/", "archive": true}|};
  [%expect
    {|
    false
    (proj/ proj/a.txt proj/src/ proj/src/b.ml)
    ERROR: not a directory: $DIR/proj/a.txt
    ERROR: refusing to archive /
    |}]
;;

let upload t ?(chunks = []) ?(close = true) ?cancel args =
  let feed = Stdin_feed.create () in
  List.iter chunks ~f:(fun c ->
    ignore
      (Stdin_feed.write feed (Base64.encode_string c) ~close:false
       : unit Or_error.t));
  if close then ignore (Stdin_feed.write feed "" ~close:true : unit Or_error.t);
  let acks = ref 0 in
  let result =
    run_op
      ~stdin:feed
      ?cancel
      ~on_output:(fun s -> acks := !acks + String.count s ~f:(Char.equal '+'))
      t
      Host_files.upload_op
      args
  in
  printf "acks: %d  " !acks;
  show t result
;;

let ls t dir =
  print_s
    [%sexp
      (Sys_unix.readdir (Filename.concat t.dir dir)
       |> Array.to_list
       |> List.sort ~compare:String.compare
       |> List.map ~f:(mask t)
       : string list)]
;;

let%expect_test "host ops: uploads, their conflicts and cancelling" =
  with_sandbox
  @@ fun t ->
  write t "in/notes.txt" "old";
  upload t ~chunks:[ "hello "; "world" ] {|{"path": "in/new.txt", "size": 11}|};
  print_endline (read t "in/new.txt");
  upload t ~chunks:[ "x" ] {|{"path": "in/notes.txt"}|};
  upload
    t
    ~chunks:[ "new" ]
    {|{"path": "in/notes.txt", "conflict": "overwrite"}|};
  upload t ~chunks:[ "2" ] {|{"path": "in/notes.txt", "conflict": "rename"}|};
  upload t ~chunks:[ "3" ] {|{"path": "in/notes.txt", "conflict": "rename"}|};
  upload t ~chunks:[ "4" ] {|{"path": "in/.env", "conflict": "rename"}|};
  upload t ~chunks:[ "4" ] {|{"path": "in/.env", "conflict": "rename"}|};
  print_endline (read t "in/notes.txt");
  [%expect
    {|
    acks: 2  {"path":"$DIR/in/new.txt","bytes":11}
    hello world
    acks: 0  ERROR: $DIR/in/notes.txt already exists
    acks: 1  {"path":"$DIR/in/notes.txt","bytes":3}
    acks: 1  {"path":"$DIR/in/notes-2.txt","bytes":1}
    acks: 1  {"path":"$DIR/in/notes-3.txt","bytes":1}
    acks: 1  {"path":"$DIR/in/.env","bytes":1}
    acks: 1  {"path":"$DIR/in/.env-2","bytes":1}
    new
    |}];
  (* A short body, a cancelled upload: nothing is left behind. *)
  upload t ~chunks:[ "abc" ] {|{"path": "in/short.txt", "size": 10}|};
  let cancel = Cancellation.create () in
  Eio.Switch.run (fun sw ->
    Eio.Fiber.fork ~sw (fun () ->
      settle t;
      Cancellation.cancel cancel);
    upload t ~chunks:[ "part" ] ~close:false ~cancel {|{"path": "in/cut.txt"}|});
  upload t {|{"path": "missing/x.txt"}|};
  upload t {|{"path": "in/x", "conflict": "sometimes"}|};
  ls t "in";
  [%expect
    {|
    acks: 1  ERROR: $DIR/in/short.txt: got 3 of 10 bytes
    acks: 1  ERROR: [cancelled]
    acks: 0  ERROR: not a directory: $DIR/missing
    acks: 0  ERROR: unknown conflict "sometimes": fail, overwrite or rename
    (.env .env-2 new.txt notes-2.txt notes-3.txt notes.txt)
    |}]
;;

let%expect_test "attachments: a binary file is named, not inlined" =
  with_sandbox
  @@ fun t ->
  write t "report.pdf" "%PDF-1.7\000\001\002 binary";
  write t "notes.txt" "plain\n";
  op t Host_ops.read_file_op {|{"path": "report.pdf"}|};
  op t Host_ops.read_file_op {|{"path": "notes.txt"}|};
  [%expect
    {|
    [binary file, 18 bytes: not shown; inspect it with tools]
    plain
    |}]
;;
