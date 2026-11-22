open! Core
open! Prigh
open Tool_test_helpers
open! Test_seen_files

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
