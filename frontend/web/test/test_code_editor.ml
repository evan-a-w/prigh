open! Core
module H = Harness

(* The editor mode: files of the session's host opened, edited, saved (and
   refused when the host's copy changed), asked about, and the agent's
   changes to an open file shown to keep or undo; on a wide window and on a
   phone. *)

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

let print_buffer h =
  match Prigh_web.Code_editor.current (code h) with
  | None -> print_endline "no file"
  | Some b ->
    print_s
      [%sexp
        { path = (b.path : string)
        ; dirty = (Prigh_web.Code_editor.Buffer.dirty b : bool)
        ; saving = (b.saving : bool)
        ; conflict = (b.conflict : Prigh_web.Code_editor.Conflict.t option)
        ; hunks = (b.hunks : Prigh_web.Code_editor.Hunk.t list)
        ; kept = (b.kept : Prigh_web.Code_editor.Hunk.t list)
        ; marks = (Prigh_web.Code_editor.Buffer.marks b : (int * int) list)
        }]
;;

let%expect_test "/edit opens the mode on the file, Esc leaves it" =
  let h = H.create ~state:(state ()) () in
  H.type_ h "/edit src/a.ml";
  H.key h "Enter";
  [%expect
    {|
    (Rpc (method_ list_files) (params ()) (tag (Code (Files /home/me/proj))))
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "/edit src/a.ml") (max 500))) (tag Ignore))
    (Rpc (method_ fs_list) (params ((hidden false))) (tag (Code Listed)))
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/src/a.ml)))
     (tag (Code (Opened /home/me/proj/src/a.ml))))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    |}];
  H.text h ~selector:".code-strip";
  [%expect
    {|
    (Show or hide the files (Ctrl+B)) No file open
    (Show or hide the chat (Alt+C))
    (More)
    (Back to the chat (Esc))
    |}];
  H.text h ~selector:".code-none";
  [%expect {| Opening src/a.ml… |}];
  H.quiet h (fun () -> H.reply h "fs_preview" (preview source));
  H.text h ~selector:".code-strip";
  [%expect
    {|
    (Show or hide the files (Ctrl+B)) src/ a.ml
    (Show or hide the chat (Alt+C))
    (More)
    (Back to the chat (Esc))
    |}];
  H.key h "Escape" ~target:Page;
  [%expect {| (Code Leave) |}];
  print_s [%sexp ((code h).open_ : bool)];
  [%expect {| false |}]
;;

let%expect_test "typing makes it dirty; Ctrl+S saves with the md5 it read" =
  let h = entered () in
  H.act
    h
    (Code (Edited { path = "/home/me/proj/src/a.ml"; text = "let x = 10\n" }));
  [%expect {| |}];
  H.text h ~selector:".code-strip";
  [%expect
    {|
    (Show or hide the files (Ctrl+B)) src/ a.ml
    (Save) (Show or hide the chat (Alt+C))
    (More)
    (Back to the chat (Esc))
    |}];
  H.key h "s" ~ctrl:true ~target:Code;
  [%expect
    {|
    (Code Save)
    (Rpc (method_ fs_write)
     (params ((path /home/me/proj/src/a.ml) (text "let x = 10\n") (expected m1)))
     (tag (Code (Saved /home/me/proj/src/a.ml))))
    |}];
  H.reply h "fs_write" {|{"md5":"m2"}|};
  print_buffer h;
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    ((path /home/me/proj/src/a.ml) (dirty false) (saving false) (conflict ())
     (hunks ()) (kept ()) (marks ()))
    |}]
;;

let%expect_test "a save the host refuses (changed there): reload or overwrite" =
  let h = entered () in
  H.quiet h (fun () ->
    H.act h (Code (Edited { path = "/home/me/proj/src/a.ml"; text = "mine\n" }));
    H.act h (Code Save));
  H.fail h "fs_write" "src/a.ml changed since you opened it";
  [%expect {| |}];
  print_buffer h;
  [%expect
    {|
    ((path /home/me/proj/src/a.ml) (dirty true) (saving false)
     (conflict (Changed_on_disk)) (hunks ()) (kept ()) (marks ()))
    |}];
  H.text h ~selector:".code-notice";
  [%expect
    {| src/a.ml changed on the host since you opened it: reload it (losing your edits) or keep yours and overwrite it. (Reload) (Overwrite) |}];
  H.click h ~selector:".code-notice .btn.danger";
  [%expect
    {|
    (Code Overwrite)
    (Rpc (method_ fs_write)
     (params ((path /home/me/proj/src/a.ml) (text "mine\n")))
     (tag (Code (Saved /home/me/proj/src/a.ml))))
    |}]
;;

let%expect_test "the agent's edit of a clean file: shown, then undone" =
  let h = entered () in
  H.quiet h (fun () ->
    H.event h (Agent_events.tool_end ~id:"e1" ~name:"edit" "Edited src/a.ml"));
  H.reply
    h
    "fs_preview"
    (preview ~md5:"m2" "let x = 1\nlet y = 20\nlet w = 0\nlet z = 3\n");
  print_buffer h;
  [%expect
    {|
    ((path /home/me/proj/src/a.ml) (dirty false) (saving false) (conflict ())
     (hunks
      (((line 2) (removed ("let y = 2")) (added ("let y = 20" "let w = 0")))))
     (kept ()) (marks ((2 3))))
    |}];
  H.text h ~selector:".code-notice";
  [%expect {| The agent changed this file: 1 change, +2 −1 (Keep) (Undo) |}];
  H.click h ~selector:".code-notice .btn:not(.primary)";
  [%expect
    {|
    (Code (Undo 0))
    (Rpc (method_ fs_write)
     (params
      ((path /home/me/proj/src/a.ml)
       (text  "let x = 1\
             \nlet y = 2\
             \nlet z = 3\
             \n")
       (expected m2)))
     (tag (Code (Saved /home/me/proj/src/a.ml))))
    |}];
  print_buffer h;
  [%expect
    {|
    ((path /home/me/proj/src/a.ml) (dirty true) (saving true) (conflict ())
     (hunks ()) (kept ()) (marks ()))
    |}]
;;

let long = String.concat (List.init 12 ~f:(fun i -> sprintf "line %d\n" (i + 1)))

let%expect_test "several changes: each kept or undone on its own, a kept one reopened" =
  let h = entered () in
  H.quiet h (fun () ->
    H.act h (Code Reload);
    H.reply h "fs_preview" (preview ~md5:"m1" long);
    H.event h (Agent_events.tool_end ~id:"e1" ~name:"edit" "Edited src/a.ml"));
  let after =
    String.substr_replace_all long ~pattern:"line 2\n" ~with_:"line two\n"
    |> String.substr_replace_all ~pattern:"line 6\n" ~with_:"line six\nline 6b\n"
    |> String.substr_replace_all ~pattern:"line 10\n" ~with_:""
  in
  H.reply h "fs_preview" (preview ~md5:"m2" after);
  print_buffer h;
  H.text h ~selector:".code-notice";
  [%expect
    {|
    ((path /home/me/proj/src/a.ml) (dirty false) (saving false) (conflict ())
     (hunks
      (((line 2) (removed ("line 2")) (added ("line two")))
       ((line 6) (removed ("line 6")) (added ("line six" "line 6b")))
       ((line 11) (removed ("line 10")) (added ()))))
     (kept ()) (marks ((2 2) (6 7))))
    The agent changed this file: 3 changes, +3 −3 (Keep all) (Undo all)
    |}];
  (* Keep the first: a badge in the gutter now. *)
  H.act h (Code (Keep 0));
  print_buffer h;
  [%expect
    {|
    ((path /home/me/proj/src/a.ml) (dirty false) (saving false) (conflict ())
     (hunks
      (((line 6) (removed ("line 6")) (added ("line six" "line 6b")))
       ((line 11) (removed ("line 10")) (added ()))))
     (kept (((line 2) (removed ("line 2")) (added ("line two")))))
     (marks ((2 2) (6 7))))
    |}];
  (* Undo the (now) first, the one in the middle: the last moves up. *)
  H.act h (Code (Undo 0));
  print_buffer h;
  [%expect
    {|
    (Rpc (method_ fs_write)
     (params
      ((path /home/me/proj/src/a.ml)
       (text
         "line 1\
        \nline two\
        \nline 3\
        \nline 4\
        \nline 5\
        \nline 6\
        \nline 7\
        \nline 8\
        \nline 9\
        \nline 11\
        \nline 12\
        \n")
       (expected m2)))
     (tag (Code (Saved /home/me/proj/src/a.ml))))
    ((path /home/me/proj/src/a.ml) (dirty true) (saving true) (conflict ())
     (hunks (((line 10) (removed ("line 10")) (added ()))))
     (kept (((line 2) (removed ("line 2")) (added ("line two")))))
     (marks ((2 2))))
    |}];
  H.quiet h (fun () -> H.reply h "fs_write" {|{"md5":"m3"}|});
  (* The kept one's badge shows it again, with its Undo. *)
  H.act h (Code (Reopen 0));
  H.text h ~selector:".code-widget";
  [%expect
    {|
    ((path /home/me/proj/src/a.ml) (lang Ocaml) (revision 3) (lines 11)
     (hunks
      (((line 2) (removed ("line 2")) (added ("line two")))
       ((line 10) (removed ("line 10")) (added ()))))
     (kept ()) (ask ()))
    |}];
  H.act h (Code Keep_all);
  print_buffer h;
  [%expect
    {|
    ((path /home/me/proj/src/a.ml) (dirty false) (saving false) (conflict ())
     (hunks ())
     (kept
      (((line 2) (removed ("line 2")) (added ("line two")))
       ((line 10) (removed ("line 10")) (added ()))))
     (marks ((2 2))))
    |}];
  (* Typing above a change moves it down with its lines. *)
  let b = Option.value_exn (Prigh_web.Code_editor.current (code h)) in
  H.act
    h
    (Code (Edited { path = b.path; text = "new first line\n" ^ b.text }));
  print_s
    [%sexp
      (List.map (Option.value_exn (Prigh_web.Code_editor.current (code h))).kept
         ~f:(fun h -> h.line)
       : int list)];
  [%expect {| (3 11) |}]
;;

let%expect_test
    "the agent's edit of a file with unsaved edits: a conflict, nothing lost"
  =
  let h = entered () in
  H.quiet h (fun () ->
    H.act h (Code (Edited { path = "/home/me/proj/src/a.ml"; text = "mine\n" }));
    H.event h (Agent_events.tool_end ~id:"e1" ~name:"edit" "Edited src/a.ml"));
  H.reply h "fs_preview" (preview ~md5:"m2" "theirs\n");
  [%expect {| |}];
  print_buffer h;
  [%expect
    {|
    ((path /home/me/proj/src/a.ml) (dirty true) (saving false)
     (conflict (Changed_on_disk)) (hunks ()) (kept ()) (marks ()))
    |}];
  print_s
    [%sexp
      ((Option.value_exn (Prigh_web.Code_editor.current (code h))).text
       : string)];
  [%expect {| "mine\n" |}]
;;

let%expect_test "Ctrl+L asks about the selection: a chip, then the prompt" =
  let h = entered () in
  H.quiet h (fun () ->
    H.act
      h
      (Code
         (Selected (Some { first = 2; last = 3; text = "let y = 2\nlet z = 3" }))));
  H.key h "l" ~ctrl:true ~target:Code;
  [%expect
    {|
    (Code Ask)
    (Focus editor)
    |}];
  H.text h ~selector:".code-chips";
  [%expect {| src/a.ml:2–3 2 lines (Remove) |}];
  H.quiet h (fun () ->
    H.type_ h "why 2?";
    H.key h "Enter");
  H.text h ~selector:"#chat .user";
  [%expect
    {|
    Send
    [src/a.ml:2–3] why 2?
    Sending…
    |}];
  print_s [%sexp ((code h).chip : Prigh_web.Code_editor.Chip.t option)];
  [%expect {| () |}]
;;

let files =
  {|{"root":"/home/me/proj","files":"README.md\nlib/main.ml\nsrc/a.ml\nsrc/compact.ml\nsrc/compact.mli\ntest/test_compact.ml\nalpha.txt","truncated":false}|}
;;

let%expect_test "Go to file: fuzzy over every file, listed once" =
  let h = entered () in
  H.key h "p" ~ctrl:true ~target:Code;
  [%expect
    {|
    (Code Find_file)
    (Focus code-filter)
    |}];
  H.input h ~selector:"#code-filter" "cmpml";
  [%expect {| (Code (Filter cmpml)) |}];
  H.text h ~selector:".code-tree-scroll";
  [%expect {| Listing the files… |}];
  H.reply h "list_files" files;
  H.text h ~selector:".code-tree-scroll";
  [%expect
    {|
    3 files of 7
    (src/ c o mp act. ml) (src/ c o mp act. ml i) (test/ test_ c o mp act. ml)
    |}];
  (* The letters matched are marked. *)
  H.show h ~selector:".code-matches .code-row:first-child .code-path-name";
  [%expect
    {|
    <span class="code-path-name">
      <mark> c </mark>
      o
      <mark> mp </mark>
      act.
      <mark> ml </mark>
    </span>
    |}];
  (* Typing on narrows the last matches; Enter opens the best. *)
  H.input h ~selector:"#code-filter" "cmpmli";
  H.text h ~selector:".code-tree-scroll";
  [%expect
    {|
    (Code (Filter cmpmli))
    1 file of 7
    (src/ c o mp act. mli)
    |}];
  H.keydown h ~selector:"#code-filter" "Enter";
  [%expect {|
    (Code (Open /home/me/proj/src/compact.mli))
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/src/compact.mli)))
     (tag (Code (Opened /home/me/proj/src/compact.mli))))
    |}]
;;

let%expect_test "Go to file: a big repository, every match, a window of rows" =
  let h = entered () in
  H.quiet h (fun () -> H.act h (Code Find_file));
  let paths =
    List.init 100_000 ~f:(fun i -> sprintf "pkg%03d/src/file_%05d.ml" (i / 100) i)
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
  [%expect {|
    100000 matches, 60 rows in the page
    100,000 files of 100,000
    |}];
  (* Scrolled far down: the rows there, not the first ones. *)
  H.quiet h (fun () -> H.act h (Code (List_scrolled 80_000)));
  rows ();
  H.text h ~selector:".code-matches .code-row:first-child";
  [%expect {|
    100000 matches, 60 rows in the page
    (pkg799/src/ file _79980.ml)
    |}]
;;

let%expect_test "a phone: the drawer, the keys, the peek" =
  let h = entered ~narrow:true () in
  H.text h ~selector:".code-page";
  [%expect
    {|
    (Files) src/ a.ml
    (More)
    ((path /home/me/proj/src/a.ml) (lang Ocaml) (revision 0) (lines 3) (hunks ())
     (kept ()) (ask ()))
    (Ask)
    (⇥) ({) (}) (() ()) ([) (]) (;) (=) (|) (->) (←) (→) (↶)
    |}];
  H.click h ~selector:".code-tree-toggle";
  H.text h ~selector:".code-tree";
  [%expect
    {|
    (Code Toggle_drawer)
    Open
    (src/ a.ml)
    proj/
    (..) (src/) (README.md)
    []
    |}];
  H.click h ~selector:".code-tree .code-row.file";
  print_s [%sexp ((code h).drawer : bool)];
  [%expect
    {|
    (Code (Open /home/me/proj/README.md))
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/README.md)))
     (tag (Code (Opened /home/me/proj/README.md))))
    false
    |}];
  H.click h ~selector:".code-key[aria-label=Indent]";
  [%expect
    {|
    (Code (Command Indent))
    (Code_editor Indent)
    |}]
;;

let%expect_test
    "a wide window: Alt+C the chat beside, Ctrl+B the files, Alt+E out and back"
  =
  let h = entered () in
  let show () =
    let t = code h in
    print_s
      [%sexp
        { open_ = (t.open_ : bool)
        ; chat = (t.chat : bool)
        ; tree = (t.tree : bool)
        }]
  in
  show ();
  H.key h "c" ~alt:true ~code:"KeyC" ~target:Code;
  H.key h "b" ~ctrl:true ~code:"KeyB" ~target:Code;
  show ();
  H.key h "e" ~alt:true ~code:"KeyE" ~target:Code;
  show ();
  H.key h "e" ~alt:true ~code:"KeyE";
  show ();
  [%expect
    {|
    ((open_ true) (chat true) (tree true))
    (Code Toggle_chat)
    (Code Toggle_tree)
    ((open_ true) (chat false) (tree false))
    (Code Leave)
    ((open_ false) (chat false) (tree false))
    (Code (Enter ()))
    ((open_ true) (chat false) (tree false))
    |}]
;;

let%expect_test "Ctrl+I: the ask box under the lines, sent as a prompt about them" =
  let h = entered () in
  H.quiet h (fun () ->
    H.type_ h "a draft I was writing";
    H.act
      h
      (Code
         (Selected (Some { first = 2; last = 3; text = "let y = 2\nlet z = 3" }))));
  H.key h "i" ~ctrl:true ~target:Code;
  H.text h ~selector:".code-widget";
  [%expect
    {|
    (Code Ask_inline)
    ((path /home/me/proj/src/a.ml) (lang Ocaml) (revision 0) (lines 3) (hunks ())
     (kept ())
     (ask (((path src/a.ml) (first 2) (last 3) (text  "let y = 2\
                                                     \nlet z = 3")))))
    |}];
  (* Empty: says what to do. *)
  H.act h (Code (Ask_send "  "));
  [%expect
    {| |}];
  H.text h ~selector:".toast";
  [%expect {| Type a question or an instruction first (Dismiss) |}];
  H.act h (Code (Ask_send "rename y to why"));
  H.text h ~selector:"#chat .user";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params
      ((key history) (value "[src/a.ml:2\226\128\1473] rename y to why")
       (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params
      ((text
         "[src/a.ml:2\226\128\1473] rename y to why\
        \n\
        \n<pasted_text name=\"Pasted text #1\" lines=\"3\">\
        \nsrc/a.ml, lines 2\226\128\1473:\
        \nlet y = 2\
        \nlet z = 3\
        \n</pasted_text>")))
     (tag
      (Sent_with_files (text "[src/a.ml:2\226\128\1473] rename y to why")
       (images ()) (attachments ())
       (pasted ( "src/a.ml, lines 2\226\128\1473:\
                \nlet y = 2\
                \nlet z = 3")))))
    [src/a.ml:2–3] rename y to why
    Sending…
    |}];
  (* The box is gone; the composer kept its draft. *)
  print_s
    [%sexp
      { asking = ((code h).asking : Prigh_web.Code_editor.Chip.t option)
      ; draft = ((H.model h).draft : string)
      }];
  [%expect {| ((asking ()) (draft "a draft I was writing")) |}]
;;

let%expect_test "the ask box: Esc (Cancel) closes it, back in the text" =
  let h = entered ~narrow:true () in
  H.click h ~selector:".code-ask";
  print_s [%sexp (Option.is_some (code h).asking : bool)];
  H.act h (Code Ask_cancel);
  print_s [%sexp (Option.is_some (code h).asking : bool)];
  [%expect
    {|
    (Code Ask_inline)
    true
    (Code_editor Focus)
    false
    |}]
;;

let%expect_test "a phone: swiping goes through the open files" =
  let h = entered ~narrow:true () in
  H.quiet h (fun () ->
    H.act h (Code (Open "/home/me/proj/README.md"));
    H.reply h "fs_preview" (preview ~path:"/home/me/proj/README.md" "# hi\n"));
  let current () = print_s [%sexp ((code h).current : string option)] in
  current ();
  H.act h (Code (Swiped 1));
  current ();
  H.act h (Code (Swiped (-1)));
  current ();
  H.text h ~selector:".code-dots";
  [%expect
    {|
    (/home/me/proj/README.md)
    (/home/me/proj/src/a.ml)
    (/home/me/proj/README.md)
    |}]
;;

let%expect_test "/edit completes any file, fuzzily, from the same list" =
  let h = H.create ~state:(state ()) () in
  H.type_ h "/edit cmpml";
  [%expect
    {| (Rpc (method_ list_files) (params ()) (tag (Code (Files /home/me/proj)))) |}];
  H.reply h "list_files" files;
  H.text h ~selector:".popup-items";
  [%expect {|
    src/compact.ml
    src/compact.mli
    test/test_compact.ml
    |}];
  H.key h "Enter";
  [%expect {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/edit src/compact.ml") (max 500)))
     (tag Ignore))
    (Rpc (method_ fs_list) (params ((hidden false))) (tag (Code Listed)))
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/src/compact.ml)))
     (tag (Code (Opened /home/me/proj/src/compact.ml))))
    (Rpc (method_ git_diff) (params ()) (tag (Review Diff)))
    |}]
;;
