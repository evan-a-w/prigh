open! Core
open! Prigh

let with_dir f =
  let dir = Filename_unix.temp_dir "prigh-session-cache" "" in
  f dir
;;

let saved ~dir prompts =
  let t = Session.create ~dir ~cwd:"/proj" () in
  List.iter prompts ~f:(fun p ->
    ignore (Session.append_message t (Message.user p) : Session.Entry.t));
  t
;;

let cache_files ~dir =
  match Sys_unix.ls_dir (Session.cache_dir ~dir) with
  | files -> List.sort files ~compare:String.compare
  | exception _ -> []
;;

(* A listing as a new process makes it: which files it parsed, and what it
   listed. *)
let list_fresh ~dir =
  Session.forget_cached_summaries ();
  let parsed = ref [] in
  let summaries =
    Session.list_with
      ~live:(fun _ -> None)
      ~load:(fun path ->
        parsed := Filename.basename path :: !parsed;
        Session.load path)
      ~dir
  in
  printf
    "parsed %d; listed %s; cache %s\n"
    (List.length !parsed)
    (List.map summaries ~f:(fun (s : Session.Summary.t) ->
       sprintf
         "%s/%d"
         (Option.value s.first_prompt ~default:"-")
         s.message_count)
     |> List.sort ~compare:String.compare
     |> String.concat ~sep:",")
    (String.concat ~sep:"," (cache_files ~dir))
;;

let%expect_test
    "summaries are kept on disk: a new process parses only what changed"
  =
  with_dir
  @@ fun dir ->
  let a = saved ~dir [ "alpha" ] in
  let b = saved ~dir [ "beta"; "more beta" ] in
  ignore (saved ~dir [ "gamma" ] : Session.t);
  (* The first listing parses them all, and writes the cache. *)
  list_fresh ~dir;
  (* A new process: nothing changed, nothing parsed. *)
  list_fresh ~dir;
  (* One grew (its size and mtime changed): only it is parsed. *)
  ignore
    (Session.append_message a (Message.user "alpha again") : Session.Entry.t);
  list_fresh ~dir;
  list_fresh ~dir;
  (* One went: it is no longer listed, nor kept in the cache. *)
  Session.delete_files (Session.path b);
  list_fresh ~dir;
  let cache = Filename.concat (Session.cache_dir ~dir) "summaries-v1" in
  let sexp = In_channel.read_all cache in
  printf
    "b in the cache: %b\n"
    (String.is_substring sexp ~substring:(Session.id b));
  [%expect
    {|
    parsed 3; listed alpha/1,beta/2,gamma/1; cache summaries-v1
    parsed 0; listed alpha/1,beta/2,gamma/1; cache summaries-v1
    parsed 1; listed alpha/2,beta/2,gamma/1; cache summaries-v1
    parsed 0; listed alpha/2,beta/2,gamma/1; cache summaries-v1
    parsed 0; listed alpha/2,gamma/1; cache summaries-v1
    b in the cache: false
    |}]
;;

let cache_file ~dir = Filename.concat (Session.cache_dir ~dir) "summaries-v1"

let%expect_test "the same size with a new mtime is parsed again" =
  with_dir
  @@ fun dir ->
  let a = saved ~dir [ "alpha" ] in
  list_fresh ~dir;
  let path = Session.path a in
  let data = In_channel.read_all path in
  Out_channel.write_all
    path
    ~data:(String.substr_replace_all data ~pattern:"alpha" ~with_:"omega");
  let mtime = (Core_unix.stat path).st_mtime +. 5. in
  Core_unix.utimes path ~access:mtime ~modif:mtime;
  list_fresh ~dir;
  [%expect
    {|
    parsed 1; listed alpha/1; cache summaries-v1
    parsed 1; listed omega/1; cache summaries-v1
    |}]
;;

let%expect_test "a cache file that cannot be used is rebuilt" =
  with_dir
  @@ fun dir ->
  ignore (saved ~dir [ "alpha" ] : Session.t);
  ignore (saved ~dir [ "beta" ] : Session.t);
  list_fresh ~dir;
  let good = In_channel.read_all (cache_file ~dir) in
  let try_with what data =
    printf "%s: " what;
    Out_channel.write_all (cache_file ~dir) ~data;
    list_fresh ~dir;
    printf
      "  rewritten as it was: %b\n"
      (String.equal (In_channel.read_all (cache_file ~dir)) good)
  in
  (* prigh stopped while writing it (it is written atomically, but a disk
     may still lose its end). *)
  try_with "torn" (String.prefix good (String.length good / 2));
  try_with "garbage" "\000\001 not a sexp ((";
  try_with
    "another version's, under this one's name"
    (String.substr_replace_first
       good
       ~pattern:"(version 1)"
       ~with_:"(version 2)");
  try_with
    "entries of another shape"
    "((version 1)(summaries((x.jsonl(1 2 3)))))";
  try_with "empty" "";
  (* A directory where the file should be: listing still works, uncached. *)
  Core_unix.unlink (cache_file ~dir);
  Core_unix.mkdir (cache_file ~dir);
  list_fresh ~dir;
  list_fresh ~dir;
  [%expect
    {|
    parsed 2; listed alpha/1,beta/1; cache summaries-v1
    torn: parsed 2; listed alpha/1,beta/1; cache summaries-v1
      rewritten as it was: true
    garbage: parsed 2; listed alpha/1,beta/1; cache summaries-v1
      rewritten as it was: true
    another version's, under this one's name: parsed 2; listed alpha/1,beta/1; cache summaries-v1
      rewritten as it was: true
    entries of another shape: parsed 2; listed alpha/1,beta/1; cache summaries-v1
      rewritten as it was: true
    empty: parsed 2; listed alpha/1,beta/1; cache summaries-v1
      rewritten as it was: true
    parsed 2; listed alpha/1,beta/1; cache summaries-v1
    parsed 2; listed alpha/1,beta/1; cache summaries-v1
    |}]
;;

let%expect_test "a session's disk usage is counted again when its data changes" =
  with_dir
  @@ fun dir ->
  let a = saved ~dir [ "alpha" ] in
  let path = Session.path a in
  let extra () =
    let s = List.hd_exn (Session.list ~dir) in
    s.bytes - Int64.to_int_exn (Core_unix.stat path).st_size
  in
  printf "%d\n" (extra ());
  let data = Session.data_dir path in
  Core_unix.mkdir_p (Filename.concat data "tool-output");
  Out_channel.write_all (Filename.concat data "tool-output/1.txt") ~data:"12345";
  printf "%d\n" (extra ());
  (* A file added in a directory in it. *)
  Out_channel.write_all (Filename.concat data "tool-output/2.txt") ~data:"123";
  printf "%d\n" (extra ());
  Session.forget_cached_summaries ();
  printf "%d\n" (extra ());
  [%expect
    {|
    0
    5
    8
    8
    |}]
;;

let%expect_test
    "a file is read a chunk at a time: lines across chunks, a torn end, offsets"
  =
  with_dir
  @@ fun dir ->
  (* Lines shorter and longer than a chunk (64 KiB), and ones that end just
     past one. *)
  let sizes = [ 10; 70_000; 3; 200_000; 65_400; 65_536; 1 ] in
  let t =
    saved
      ~dir
      (List.mapi sizes ~f:(fun i n -> String.make n (Char.of_int_exn (97 + i))))
  in
  let path = Session.path t in
  let show (s : Session.t) =
    let lengths =
      List.filter_map (Session.messages s) ~f:(function
        | Message.User u -> Some (String.length (Message.User.body u))
        | _ -> None)
    in
    print_s [%sexp (lengths : int list), (Session.skipped s : string list)]
  in
  let loaded = Session.load path |> Or_error.ok_exn in
  show loaded;
  (* The offsets recorded as it was written and as it was read agree, and
     lead to the entries. *)
  let entries = Session.active_path t in
  let agree =
    List.for_all entries ~f:(fun (e : Session.Entry.t) ->
      let offset = Session.entry_offset t e.id in
      Option.equal Int.equal offset (Session.entry_offset loaded e.id)
      &&
      match
        Option.bind offset ~f:(fun offset -> Session.read_entry_at path ~offset)
      with
      | Some read -> String.equal read.id e.id
      | None -> false)
  in
  printf "offsets agree: %b of %d\n" agree (List.length entries);
  print_s
    [%sexp
      (Session.read_entry_at path ~offset:1 |> Option.is_some : bool)
    , (Session.read_entry_at path ~offset:100_000_000 |> Option.is_some : bool)];
  (* prigh stopped while writing a line: the rest is read, and the next
     line starts on a line of its own. *)
  Out_channel.with_file path ~append:true ~f:(fun oc ->
    Out_channel.output_string oc {|{"type":"entry","id":"x",|});
  let torn = Session.load path |> Or_error.ok_exn in
  show torn;
  ignore (Session.append_message torn (Message.user "after") : Session.Entry.t);
  let again = Session.load path |> Or_error.ok_exn in
  show again;
  let last = List.last_exn (Session.active_path again) in
  printf
    "last offset leads to it: %b\n"
    (Option.bind (Session.entry_offset again last.id) ~f:(fun offset ->
       Session.read_entry_at path ~offset)
     |> Option.exists ~f:(fun (e : Session.Entry.t) ->
       String.equal e.id last.id));
  [%expect
    {|
    ((10 70000 3 200000 65400 65536 1) ())
    offsets agree: true of 7
    (false false)
    ((10 70000 3 200000 65400 65536 1)
     ("line 9: skipped an incomplete last line (prigh probably stopped while writing it)"))
    ((10 70000 3 200000 65400 65536 1 5)
     ("line 9: skipped a line that is not JSON"))
    last offset leads to it: true
    |}]
;;
