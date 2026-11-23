open! Core
open! Prigh
open Tool_test_helpers
open! Test_file_index

let%expect_test "a large tree: every file, quickly" =
  with_sandbox
  @@ fun t ->
  for d = 0 to 99 do
    let dir = sprintf "d%02d" d in
    Core_unix.mkdir_p (Filename.concat t.dir dir);
    for f = 0 to 199 do
      Out_channel.write_all
        (Filename.concat t.dir (sprintf "%s/f%03d.ml" dir f))
        ~data:""
    done
  done;
  let r = File_index.list ~env:t.env ~root:t.dir () in
  printf
    "%d files, truncated %b, first %s, last %s\n"
    (List.length r.files)
    r.truncated
    (List.hd_exn r.files)
    (List.last_exn r.files);
  [%expect
    {| 20000 files, truncated false, first d00/f000.ml, last d99/f199.ml |}];
  git' t [ "init"; "-q" ];
  let r = File_index.list ~env:t.env ~root:t.dir () in
  printf "in git: %d files\n" (List.length r.files);
  [%expect {| in git: 20000 files |}];
  let r = File_index.list ~limit:500 ~env:t.env ~root:t.dir () in
  printf "limited: %d files, truncated %b\n" (List.length r.files) r.truncated;
  [%expect {| limited: 500 files, truncated true |}]
;;
