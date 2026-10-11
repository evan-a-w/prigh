open! Core
open! Prigh

let day = 86400.
let now = Core_unix.gettimeofday ()

let with_dir f =
  let dir = Filename_unix.temp_dir "prigh-session" "" in
  f dir
;;

let summary t ~dir =
  List.find_exn (Session.list ~dir) ~f:(fun (s : Session.Summary.t) ->
    String.equal s.id (Session.id t))
;;

(* The archived state of [t]'s summary after 7 days, with its explicit
   marker and how many days ago its last activity was. *)
let show ?(active = false) t ~dir =
  let s = summary t ~dir in
  printf
    "marker=%s activity=%d days ago archived=%b\n"
    (Option.value_map s.archived ~default:"none" ~f:Bool.to_string)
    (Float.iround_nearest_exn ((now -. s.last_activity) /. day))
    (Session.Summary.is_archived s ~now ~archive_after_days:7. ~active)
;;

let%expect_test "archived by age, by hand; unarchived by hand or a new message" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  let append ~days_ago text =
    ignore
      (Session.append_message
         t
         ~at:(now -. (days_ago *. day))
         (Message.user text)
       : Session.Entry.t)
  in
  append ~days_ago:30. "old";
  append ~days_ago:10. "less old";
  show t ~dir;
  show t ~dir ~active:true;
  [%expect
    {|
    marker=none activity=10 days ago archived=true
    marker=none activity=10 days ago archived=false
    |}];
  (* Unarchiving counts as activity: another 7 days before it goes again. *)
  ignore (Session.set_archived t ~archived:false : Session.Entry.t);
  show t ~dir;
  ignore (Session.set_archived t ~archived:true : Session.Entry.t);
  show t ~dir;
  show t ~dir ~active:true;
  [%expect
    {|
    marker=false activity=0 days ago archived=false
    marker=true activity=10 days ago archived=true
    marker=true activity=10 days ago archived=false
    |}];
  (* Recorded in the file: a reload agrees, and a new message clears it. *)
  let loaded = Or_error.ok_exn (Session.load (Session.path t)) in
  print_s [%sexp (List.length (Session.messages loaded) : int)];
  append ~days_ago:1. "new";
  show t ~dir;
  [%expect
    {|
    2
    marker=none activity=1 days ago archived=false
    |}];
  let s = summary t ~dir in
  List.iter [ 0.; 0.5; 2. ] ~f:(fun archive_after_days ->
    printf
      "archive_after_days=%g: %b\n"
      archive_after_days
      (Session.Summary.is_archived s ~now ~archive_after_days ~active:false));
  [%expect
    {|
    archive_after_days=0: false
    archive_after_days=0.5: true
    archive_after_days=2: false
    |}]
;;

let%expect_test "a session without message times falls back to the mtime" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  ignore (Session.set_name t ~name:"named only" : Session.Entry.t);
  let old = now -. (8. *. day) in
  Core_unix.utimes (Session.path t) ~access:old ~modif:old;
  show t ~dir;
  [%expect {| marker=none activity=8 days ago archived=true |}]
;;

let%expect_test "archive entries are not copied by fork, nor messages" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  ignore (Session.append_message t (Message.user "hi") : Session.Entry.t);
  ignore (Session.set_archived t ~archived:true : Session.Entry.t);
  ignore (Session.append_message t (Message.user "again") : Session.Entry.t);
  ignore (Session.set_archived t ~archived:true : Session.Entry.t);
  let forked = Or_error.ok_exn (Session.fork t ~dir) in
  let kinds t =
    List.map (Session.active_path t) ~f:(fun (e : Session.Entry.t) ->
      match e.payload with
      | Message (User { text; _ }) -> text
      | Archived { archived } -> sprintf "archived=%b" archived
      | _ -> "?")
  in
  print_s [%sexp (kinds t : string list), (kinds forked : string list)];
  print_s
    [%sexp
      (List.length (Session.messages t) : int)
    , ((summary forked ~dir).archived : bool option)];
  [%expect
    {|
    ((hi archived=true again archived=true) (hi again))
    (2 ())
    |}]
;;

let%expect_test "a session's files: its log and its directory" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  ignore (Session.append_message t (Message.user "hi") : Session.Entry.t);
  let path = Session.path t in
  let size () = Int64.to_int_exn (Core_unix.stat path).st_size in
  let show () =
    printf
      "files=%s disk_usage=log+%d listed=log+%d\n"
      (String.concat
         ~sep:","
         (List.map (Session.files path) ~f:(fun f ->
            String.chop_prefix_exn f ~prefix:(Session.data_dir path)
            |> function
            | "" -> "dir"
            | rest -> rest)))
      (Session.disk_usage path - size ())
      ((summary t ~dir).bytes - size ())
  in
  show ();
  let data = Session.data_dir path in
  Core_unix.mkdir_p (Filename.concat data "tool-output");
  Out_channel.write_all (Filename.concat data "tool-output/1.txt") ~data:"12345";
  Out_channel.write_all (Filename.concat data "notes") ~data:"abc";
  show ();
  Session.delete_files path;
  print_s
    [%sexp
      (Sys_unix.file_exists_exn path : bool)
    , (Sys_unix.file_exists_exn data : bool)
    , (Session.list ~dir |> List.length : int)];
  [%expect
    {|
    files=.jsonl disk_usage=log+0 listed=log+0
    files=.jsonl,dir disk_usage=log+8 listed=log+8
    (false false 0)
    |}]
;;

(* Unchanged files are not parsed again; a changed one is. *)
let%expect_test "list reuses summaries of unchanged files" =
  with_dir
  @@ fun dir ->
  let t = Session.create ~dir ~cwd:"/proj" () in
  ignore (Session.append_message t (Message.user "hi") : Session.Entry.t);
  let counts () =
    List.map (Session.list ~dir) ~f:(fun (s : Session.Summary.t) ->
      s.message_count)
  in
  print_s [%sexp (counts () : int list)];
  ignore (Session.append_message t (Message.user "more") : Session.Entry.t);
  (* A whole second, which [utimes] can set exactly. *)
  let mtime = Float.round_down now in
  let set_mtime mtime =
    Core_unix.utimes (Session.path t) ~access:mtime ~modif:mtime
  in
  set_mtime mtime;
  print_s [%sexp (counts () : int list)];
  (* The same size and mtime: the cached summary is used. *)
  let data = In_channel.read_all (Session.path t) in
  Out_channel.write_all
    (Session.path t)
    ~data:(String.substr_replace_all data ~pattern:{|"hi"|} ~with_:{|"HO"|});
  let first_prompts () =
    print_s
      [%sexp
        (List.map (Session.list ~dir) ~f:(fun s -> s.first_prompt)
         : string option list)]
  in
  set_mtime mtime;
  first_prompts ();
  set_mtime (mtime +. 1.);
  first_prompts ();
  [%expect
    {|
    (1)
    (2)
    ((hi))
    ((HO))
    |}]
;;
