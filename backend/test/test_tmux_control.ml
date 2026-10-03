open! Core
open! Prigh

let visible s =
  String.concat_map s ~f:(function
    | '\027' -> "^["
    | '\r' -> "^M"
    | '\n' -> "^J\n"
    | c -> String.of_char c)
;;

let%expect_test "control mode: output, our replies, others' blocks, exit" =
  let parser = Tmux_control.create () in
  let feed chunk =
    List.iter (Tmux_control.feed parser chunk) ~f:(fun event ->
      print_s [%sexp (event : Tmux_control.Event.t)])
  in
  (* The attach command's own block (flags 0) is not a reply to us. *)
  feed "%begin 1 10 0\n%end 1 10 0\n%session-changed $0 t1\n";
  feed "%output %0 a\\033[31mred\\015\\012back\\134slash\n";
  (* Lines split across reads. *)
  feed "%begin 2 11 1\nline one\nline ";
  feed "two\n%end 2 11 1\n";
  (* A pane line that merely looks like an end marker stays in the block. *)
  feed "%begin 3 12 1\n%end 9 9 9\n%end 3 12 1\n";
  feed "%begin 4 13 1\nunknown command: x\n%error 4 13 1\n";
  feed "%layout-change @0 a,1x1,0,0,0\n%exit\n";
  [%expect
    {|
    (Output  "a\027[31mred\r\
            \nback\\slash")
    (Reply (Ok ("line one" "line two")))
    (Reply (Ok ("%end 9 9 9")))
    (Reply (Error ("unknown command: x")))
    Exit
    |}]
;;

let%expect_test "send-keys: hex, at most 256 bytes per command" =
  List.iter
    (Tmux_control.send_keys ~target:"t1-x" "ls\r\003\xc3\xa9")
    ~f:print_endline;
  let long = Tmux_control.send_keys ~target:"t" (String.make 600 'a') in
  print_s
    [%sexp
      (List.map long ~f:(fun line ->
         List.length (String.split line ~on:' ') - 4)
       : int list)];
  [%expect
    {|
    send-keys -t t1-x -H 6c 73 0d 03 c3 a9
    (256 256 88)
    |}]
;;

let%expect_test "replay: screen, cursor and modes" =
  let replay modes ~screen ~saved =
    print_endline (visible (Terminal.For_testing.replay ~modes ~screen ~saved));
    print_endline "--"
  in
  replay "2 1 0 1 0 0 0 0 0" ~screen:[ "old"; "$"; "" ] ~saved:[];
  (* An application on the alternate screen, hidden cursor, keypad and SGR
     mouse: the normal screen goes first, then the switch. *)
  replay "0 0 1 0 1 1 0 0 1" ~screen:[ "vim" ] ~saved:[ "$ vim" ];
  replay "garbage" ~screen:[ "a"; "b" ] ~saved:[];
  [%expect
    {|
    old^M^J
    $^M^J
    ^[[0m^[[2;3H
    --
    $ vim^[[?1049h^[[Hvim^[[0m^[[1;1H^[[?25l^[[?1h^[[?1000h^[[?1006h
    --
    a^M^J
    b^[[0m
    --
    |}]
;;
