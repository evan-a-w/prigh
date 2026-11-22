open! Core
open! Prigh
open Tool_test_helpers
open! Test_tools

let%expect_test "bash: timeout, cancellation, truncation" =
  with_sandbox
  @@ fun t ->
  run
    t
    Tool_bash.tool
    {|{"command": "echo start; sleep 5; echo never", "timeout": 1}|};
  [%expect
    {|
    ERROR: start
    [timed out after 1s]
    |}];
  let cancel = Cancellation.create () in
  Eio.Fiber.both
    (fun () -> run t ~cancel Tool_bash.tool {|{"command": "echo go; sleep 5"}|})
    (fun () ->
       Eio.Time.sleep (Eio.Stdenv.clock t.env) 0.1;
       Cancellation.cancel cancel);
  [%expect
    {|
    ERROR: go
    [cancelled]
    |}];
  run t Tool_bash.tool {|{"command": "seq 1 3000"}|};
  let output = [%expect.output] in
  let lines = String.split_lines output in
  let saved =
    Re.exec (Re.Perl.compile_pat {|in (\S+\.log)|}) (List.hd_exn lines)
    |> fun groups -> Re.Group.get groups 1
  in
  print_s
    [%sexp
      { first =
          (Re.replace_string
             (Re.Perl.compile_pat {|\S+/prigh-output-[0-9]+/\S+\.log|})
             ~by:"<file>"
             (List.hd_exn lines)
           : string)
      ; second = (List.nth_exn lines 1 : string)
      ; around_the_cut =
          (List.filter lines ~f:(fun l ->
             String.is_prefix l ~prefix:"[..."
             || String.equal l "666"
             || String.equal l "667"
             || String.equal l "1667")
           : string list)
      ; last = (List.last_exn lines : string)
      ; count = (List.length lines : int)
      ; saved_lines = (Truncate.count_lines (In_channel.read_all saved) : int)
      }];
  [%expect
    {|
    ((first
      "[output truncated: 3000 lines / 13893 bytes, showing the beginning and the end; the full output is in <file> (read it with offset/limit, or grep it)]")
     (second 1)
     (around_the_cut (666 "[... 1000 lines (4667 bytes) omitted ...]" 1667))
     (last 3000) (count 2002) (saved_lines 3000))
    |}]
;;
