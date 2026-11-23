open! Core
open Prigh_web
module H = Harness

(* Scale tests, in @runtest-full only: the default run has the same
   behaviour on small inputs (test_code_editor.ml, test_highlight.ml). *)

module Lang = Highlight.Lang

let%expect_test "a 256 KiB line does not overflow the stack" =
  let line = String.concat (List.init 40_000 ~f:(fun i -> sprintf "%d," i)) in
  List.iter [ Lang.Json; Javascript; Html; Markdown; Css ] ~f:(fun lang ->
    printf "%d\n" (List.length (Highlight.tokens lang line)));
  [%expect
    {|
    80000
    80000
    1
    1
    80000
    |}]
;;

let state () =
  H.state_json
    ~fields:[ "session_id", `String "s1"; "cwd", `String "/home/me/proj" ]
    ()
;;

let source = "let x = 1\nlet y = 2\nlet z = 3\n"

let preview ?(md5 = "m1") ?(path = "/home/me/proj/src/a.ml") text =
  sprintf
    {|{"path":%S,"name":%S,"size":%d,"mtime":1791100000000,"kind":"text","text":%S,"truncated":false,"md5":%S}|}
    path
    (Filename.basename path)
    (String.length text)
    text
    md5
;;

let entered ?(narrow = false) () =
  let h = H.create ~state:(state ()) () in
  if narrow then H.act h (Set_narrow true);
  H.quiet h (fun () ->
    H.act h (Code (Enter (Some "src/a.ml")));
    H.reply
      h
      "fs_list"
      {|{"path":"/home/me/proj","parent":"/home/me","home":"/home/me","entries":[
      {"name":"src","kind":"dir","mtime":1791190000000},
      {"name":"README.md","kind":"file","size":20,"mtime":1791190000000}],"total":2,"hidden":0}|};
    H.reply h "fs_preview" (preview source));
  h
;;

let code (h : H.t) = (H.model h).code

let%expect_test "Go to file: a big repository, every match, a window of rows" =
  let h = entered () in
  H.quiet h (fun () -> H.act h (Code Find_file));
  let paths =
    List.init 100_000 ~f:(fun i ->
      sprintf "pkg%03d/src/file_%05d.ml" (i / 100) i)
  in
  (* Given as parsed: the harness's JSON parser is not the page's. *)
  H.quiet h (fun () ->
    H.act
      h
      (Reply
         ( Code (Files "/home/me/proj")
         , Ok
             (`Object
                 [ "root", `String "/home/me/proj"
                 ; "files", `String (String.concat ~sep:"\n" paths)
                 ; "truncated", `False
                 ]) ));
    H.act h (Code (Filter "file")));
  let rows () =
    let t = code h in
    printf
      "%d matches, %d rows in the page\n"
      (Array.length t.matches)
      (List.length (H.node h ~selector:".code-matches .code-row" ()))
  in
  rows ();
  H.text h ~selector:".code-section-title";
  [%expect
    {|
    100000 matches, 60 rows in the page
    100,000 files of 100,000
    |}];
  (* Scrolled far down: the rows there, not the first ones. *)
  H.quiet h (fun () -> H.act h (Code (List_scrolled 80_000)));
  rows ();
  H.text h ~selector:".code-matches .code-row:first-child";
  [%expect
    {|
    100000 matches, 60 rows in the page
    (pkg799/src/ file _79980.ml)
    |}]
;;
