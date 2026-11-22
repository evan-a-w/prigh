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
        ; agent = (Option.is_some b.agent : bool)
        ; hunks = (b.hunks : Prigh_web.Code_editor.Hunk.t list)
        ; marks = (b.marks : (int * int) list)
        }]
;;

let%expect_test "/edit opens the mode on the file, Esc leaves it" =
  let h = H.create ~state:(state ()) () in
  H.type_ h "/edit src/a.ml";
  H.key h "Enter";
  [%expect
    {|
    (Rpc (method_ list_paths) (params ((prefix src/a.ml)))
     (tag (Paths src/a.ml)))
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
     (agent false) (hunks ()) (marks ()))
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
     (conflict (Changed_on_disk)) (agent false) (hunks ()) (marks ()))
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
     (agent true) (hunks (((line 2) (removed ("let y = 2")) (added 2))))
     (marks ((2 3))))
    |}];
  H.text h ~selector:".code-notice";
  [%expect {| The agent changed this file: +2 −1 (Keep) (Undo) |}];
  H.click h ~selector:".code-notice .btn:not(.primary)";
  [%expect
    {|
    (Code Undo_agent)
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
     (agent false) (hunks ()) (marks ()))
    |}]
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
     (conflict (Changed_on_disk)) (agent false) (hunks ()) (marks ()))
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

let%expect_test "Go to file: fuzzy over what the host finds" =
  let h = entered () in
  H.key h "p" ~ctrl:true ~target:Code;
  [%expect
    {|
    (Code Find_file)
    (Focus code-filter)
    |}];
  H.input h ~selector:"#code-filter" "am";
  [%expect
    {|
    (Code (Filter am))
    (Rpc (method_ list_paths) (params ((prefix a))) (tag (Code (Paths a))))
    (Rpc (method_ list_paths) (params ((prefix am))) (tag (Code (Paths am))))
    |}];
  H.reply h "list_paths" {|["src/a.ml","src/","lib/main.ml","alpha.txt"]|};
  H.reply h "list_paths" {|[]|};
  H.text h ~selector:".code-tree-scroll";
  [%expect
    {|
    Files
    (src/ a.ml) (lib/ main.ml) (README.md)
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
     (marks ()))
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
