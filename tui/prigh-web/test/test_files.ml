open! Core
module H = Harness

(* The file browser and the files attached to a prompt, on the session's
   active tool host. *)

let hosts =
  {|[{"id":"backend","name":"backend","cwd":"/work"},
     {"id":"c7","name":"laptop","cwd":"/home/me/proj","session_id":"s1","session_name":null}]|}
;;

let state ?(session = "s1") ?(active_host = "c7") ?(cwd = "/home/me/proj") () =
  H.state_json
    ~fields:
      [ "session_id", `String session
      ; "session_path", `String (sprintf "/sessions/%s.jsonl" session)
      ; "active_host", `String active_host
      ; "cwd", `String cwd
      ; "hosts", Jsonaf.of_string hosts
      ]
    ()
;;

let listing =
  {|{"path":"/home/me/proj","parent":"/home/me","home":"/home/me","entries":[
      {"name":"README.md","kind":"file","size":2048,"mtime":1791100000000},
      {"name":"src","kind":"dir","mtime":1791190000000},
      {"name":"big.bin","kind":"file","size":5000000,"mtime":1791000000000},
      {"name":"docs","kind":"link","target":"dir","mtime":1791000000000},
      {"name":"gone","kind":"link","target":"missing","mtime":1791000000000}],
    "total":5,"hidden":2}|}
;;

let src_listing =
  {|{"path":"/home/me/proj/src","parent":"/home/me/proj","home":"/home/me","entries":[
      {"name":"main.ml","kind":"file","size":120,"mtime":1791190000000}],
    "total":1,"hidden":0}|}
;;

let opened () =
  let h = H.create ~state:(state ()) () in
  H.act h (Files (Set_base "http://box:7790"));
  H.quiet h (fun () ->
    H.act h (Files Toggle);
    H.reply h "fs_list" listing);
  h
;;

let%expect_test
    "Ctrl+Shift+F opens the browser at the session's cwd on its host"
  =
  let h = H.create ~state:(state ()) () in
  H.key h "F" ~ctrl:true ~shift:true ~code:"KeyF";
  [%expect
    {|
    (Files Toggle)
    (Rpc (method_ fs_list) (params ((hidden false))) (tag (Files (Listed ""))))
    (Files (Focus files-list))
    |}];
  (* Acknowledged at once: where, and that it is listing. *)
  H.text h ~selector:".files-panel";
  [%expect
    {|
    laptop
    (Up (Backspace)) (Refresh) (Show dot files) (New folder) (Upload files here) (Close (Ctrl+Shift+F))
    /home/me/proj
    Listing…
    |}];
  H.reply h "fs_list" listing;
  H.text h ~selector:".files-panel";
  [%expect
    {|
    laptop
    (Up (Backspace)) (Refresh) (Show dot files) (New folder) (Upload files here) (Close (Ctrl+Shift+F))
    (~) / proj (Type a path)
    (Name ↑) (Size) (Modified)
    docs/ 3 Oct 04:00
    src/ 08:46
    big.bin 4.8 MB 3 Oct 04:00
    gone 3 Oct 04:00
    README.md 2.0 KB Yesterday 07:46
    5 items  2 hidden
    |}];
  H.text h ~selector:".files-toggle";
  [%expect {| (Files (Ctrl+Shift+F)) |}];
  (* Again: closed, back to the editor. *)
  H.key h "F" ~ctrl:true ~shift:true ~code:"KeyF" ~target:Files;
  [%expect
    {|
    (Files Toggle)
    (Focus editor)
    |}];
  H.text h ~selector:".files-panel";
  [%expect {| |}]
;;

let%expect_test "sorting, dot files, and the list's keys" =
  let h = opened () in
  H.click h ~selector:".files-sort.col-size";
  H.text h ~selector:".files-list";
  [%expect
    {|
    (Files (Sort_by Size))
    src/ 08:46
    docs/ 3 Oct 04:00
    big.bin 4.8 MB 3 Oct 04:00
    README.md 2.0 KB Yesterday 07:46
    gone 3 Oct 04:00
    |}];
  H.click h ~selector:".files-sort.col-size";
  H.text h ~selector:".files-columns";
  [%expect
    {|
    (Files (Sort_by Size))
    (Name) (Size ↑) (Modified)
    |}];
  H.click h ~selector:".files-tool:nth-child(3)";
  [%expect
    {|
    (Files Toggle_hidden)
    (Rpc (method_ fs_list) (params ((path /home/me/proj) (hidden true)))
     (tag (Files (Listed /home/me/proj))))
    |}];
  H.reply h "fs_list" listing;
  (* ↓ selects (a file shows), Enter opens a directory, Backspace goes up to
     it again. *)
  H.key h "ArrowDown" ~target:Files;
  H.key h "ArrowDown" ~target:Files;
  H.key h "Enter" ~target:Files;
  [%expect
    {|
    (Files (Move 1))
    (Files (Move 1))
    (Files Open_selected)
    (Rpc (method_ fs_list) (params ((path /home/me/proj/src) (hidden true)))
     (tag (Files (Listed /home/me/proj/src))))
    |}];
  H.reply h "fs_list" src_listing;
  H.text h ~selector:".files-crumbs";
  H.key h "Backspace" ~target:Files;
  H.reply h "fs_list" listing;
  H.text h ~selector:".files-row.selected";
  [%expect
    {|
    (~) / (proj) / src (Type a path)
    (Files Up)
    (Rpc (method_ fs_list) (params ((path /home/me/proj) (hidden true)))
     (tag (Files (Listed /home/me/proj))))
    src/ 08:46
    |}];
  (* Esc closes the panel; Ctrl+Shift+F brings it back where it was. *)
  H.key h "Escape" ~target:Files;
  H.key h "F" ~ctrl:true ~shift:true ~code:"KeyF";
  [%expect
    {|
    (Files Close)
    (Focus editor)
    (Files Toggle)
    (Files (Focus files-list))
    |}]
;;

let%expect_test "typing a path; a path that fails says what to do" =
  let h = opened () in
  H.click h ~selector:".files-edit-path";
  [%expect
    {|
    (Files (Edit_path (~/proj)))
    (Files (Focus files-path))
    |}];
  H.input h ~selector:"#files-path" "src";
  H.act h (Files Submit_path);
  H.reply h "fs_list" src_listing;
  [%expect
    {|
    (Files (Edit_path (src)))
    (Files (Focus files-path))
    (Rpc (method_ fs_list) (params ((path /home/me/proj/src) (hidden false)))
     (tag (Files (Listed /home/me/proj/src))))
    |}];
  H.act h (Files (Go "/nope"));
  H.fail h "fs_list" "/nope does not exist";
  [%expect
    {|
    (Rpc (method_ fs_list) (params ((path /nope) (hidden false)))
     (tag (Files (Listed /nope))))
    (Files (Focus files-path))
    |}];
  H.text h ~selector:".files-body";
  [%expect
    {|
    /nope does not exist
    Check the path (the pencil types one), go up, or try again.
    (Try again)
    |}];
  (* A late answer for a path left behind is ignored. *)
  H.act h (Files (Go "~/proj"));
  H.act h (Files (Go "/srv"));
  H.reply h "fs_list" listing;
  H.text h ~selector:".files-crumbs";
  [%expect
    {|
    (Rpc (method_ fs_list) (params ((path ~/proj) (hidden false)))
     (tag (Files (Listed ~/proj))))
    (Rpc (method_ fs_list) (params ((path /srv) (hidden false)))
     (tag (Files (Listed /srv))))
    (~) / (proj) / src (Type a path)
    |}]
;;

let%expect_test
    "previews: text with line numbers, images by link, binaries named"
  =
  let h = opened () in
  H.click h ~selector:".files-row[data-name='README.md']";
  [%expect
    {|
    (Files (Select README.md))
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/README.md)))
     (tag (Files (Previewed /home/me/proj/README.md))))
    |}];
  H.text h ~selector:".files-preview";
  [%expect
    {|
    README.md (Close the preview (Esc))
    Reading…
    |}];
  H.reply
    h
    "fs_preview"
    {|{"path":"/home/me/proj/README.md","name":"README.md","size":300000,"mtime":1791100000000,"kind":"text","text":"# proj\n\nhello\n","truncated":true}|};
  H.text h ~selector:".files-preview";
  H.text h ~selector:".files-actions";
  [%expect
    {|
    README.md 293.0 KB Yesterday 07:46 (Close the preview (Esc))
    1
    2
    3
    # proj hello
    Showing the first 14 B of 293.0 KB: download it for the rest.
    (Attach) (Download) (Mention) (Copy path) (Rename) (Delete)
    |}];
  H.key h "ArrowUp" ~target:Files;
  H.reply
    h
    "fs_preview"
    {|{"path":"/home/me/proj/shot.png","name":"shot.png","size":81,"mtime":null,"kind":"image","mime":"image/png","url":"/files/download?ticket=abc"}|};
  H.show h ~selector:".files-image";
  [%expect
    {|
    (Files (Move -1))
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/gone)))
     (tag (Files (Previewed /home/me/proj/gone))))
    <div class="files-image">
      <img src="http://box:7790/files/download?ticket=abc" alt="shot.png (image/png)"/>
    </div>
    |}];
  H.act h (Files (Select "big.bin"));
  H.reply
    h
    "fs_preview"
    {|{"path":"/home/me/proj/big.bin","name":"big.bin","size":5000000,"mtime":null,"kind":"binary"}|};
  H.text h ~selector:".files-preview-body";
  [%expect
    {|
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/big.bin)))
     (tag (Files (Previewed /home/me/proj/big.bin))))
    A binary file: nothing to show here. Download it, or attach it for the agent to inspect with its tools.
    |}];
  H.key h "Escape" ~target:Files;
  H.text h ~selector:".files-preview";
  [%expect
    {|
    (Files Close_preview)
    (Files (Focus files-list))
    |}]
;;

let%expect_test "downloads: a file, and a directory as .tar.gz" =
  let h = opened () in
  H.act h (Files (Download "README.md"));
  H.text h ~selector:".files-transfers";
  H.text h ~selector:".status .transfers";
  H.reply
    h
    "fs_download"
    {|{"url":"/files/download?ticket=t1","name":"README.md"}|};
  [%expect
    {|
    (Rpc (method_ fs_download) (params ((path /home/me/proj/README.md)))
     (tag (Files Download_link)))
    Preparing README.md…
    Preparing README.md…
    (Files
     (Download (url http://box:7790/files/download?ticket=t1) (name README.md)))
    (Expire_toast (id 0) (after_ms 2500))
    |}];
  H.act h (Files (Download "src"));
  H.text h ~selector:".files-transfers";
  H.fail h "fs_download" "tar exited with 2";
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Rpc (method_ fs_download) (params ((path /home/me/proj/src) (archive true)))
     (tag (Files Download_link)))
    Packing src as .tar.gz…
    Downloading "README.md"
    tar exited with 2
    |}]
;;

let%expect_test "uploads into the directory, asking before replacing a file" =
  let h = opened () in
  H.act
    h
    (Files
       (Chosen
          { dest = Here
          ; files =
              [ { file = 1; name = "new.txt"; size = 1000 }
              ; { file = 2; name = "README.md"; size = 10 }
              ]
          }));
  [%expect
    {|
    (Rpc (method_ fs_upload)
     (params ((name new.txt) (size 1000) (dir /home/me/proj)))
     (tag (Files (Upload_link (file 1) (overwrite false)))))
    (Rpc (method_ fs_upload)
     (params ((name README.md) (size 10) (dir /home/me/proj)))
     (tag (Files (Upload_link (file 2) (overwrite false)))))
    |}];
  H.reply
    h
    "fs_upload"
    {|{"url":"/files/upload?ticket=u1","path":"/home/me/proj/new.txt"}|};
  H.fail
    h
    "fs_upload"
    "README.md already exists in /home/me/proj: overwrite it or rename the \
     upload";
  [%expect
    {|
    (Files (Upload (file 1) (url http://box:7790/files/upload?ticket=u1)))
    (Files (Focus files-dialog))
    |}];
  H.text h ~selector:".modal";
  [%expect
    {|
    Replace README.md?
    (Close (Esc))
    ~/proj already has a README.md. Replace it with the one you are uploading (10 B)?
    (Skip) (Replace)
    |}];
  (* The dialog has the keyboard: Enter replaces. *)
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    (Files Ask_accept)
    (Rpc (method_ fs_upload)
     (params ((name README.md) (size 10) (dir /home/me/proj) (overwrite true)))
     (tag (Files (Upload_link (file 2) (overwrite true)))))
    |}];
  H.act h (Files (Upload_progress { file = 1; sent = 400 }));
  H.text h ~selector:".files-transfers";
  (* Visible outside the panel too: in the status line. *)
  H.text h ~selector:".status .transfers";
  [%expect
    {|
    new.txt 400 B of 1000 B
    README.md waiting…
    Uploading 2 files 39%
    |}];
  H.act
    h
    (Files
       (Upload_done
          { file = 1
          ; result =
              Ok
                (Jsonaf.of_string
                   {|{"path":"/home/me/proj/new.txt","bytes":1000}|})
          }));
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    (Rpc (method_ fs_list) (params ((path /home/me/proj) (hidden false)))
     (tag (Files (Listed /home/me/proj))))
    |}];
  H.act
    h
    (Files
       (Upload_done
          { file = 2
          ; result =
              Error {|{"error":"no space left for /home/me/proj/README.md"}|}
          }));
  H.text h ~selector:".toasts";
  H.elements h ~selector:".status .transfers";
  [%expect
    {|
    Uploaded "new.txt"
    Could not upload README.md: no space left for /home/me/proj/README.md
    |}];
  H.reply
    h
    "fs_upload"
    {|{"url":"/files/upload?ticket=u2","path":"/home/me/proj/README.md"}|};
  (* Esc skips a conflicting file: nothing is sent. *)
  H.act
    h
    (Files
       (Chosen { dest = Here; files = [ { file = 3; name = "src"; size = 1 } ] }));
  H.fail
    h
    "fs_upload"
    "src already exists in /home/me/proj: overwrite it or rename the upload";
  H.key h "Escape" ~target:Page;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Files (Forget 2))
    (Rpc (method_ fs_upload) (params ((name src) (size 1) (dir /home/me/proj)))
     (tag (Files (Upload_link (file 3) (overwrite false)))))
    (Files (Focus files-dialog))
    (Files Ask_cancel)
    (Files (Forget 3))
    |}]
;;

let%expect_test
    "new folder, rename and delete: dialogs that say what went wrong"
  =
  let h = opened () in
  H.click h ~selector:".files-tool:nth-child(4)";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Files New_folder)
    (Files (Focus files-ask-input))
    New folder
    (Close (Esc))
    In ~/proj
    []
    (Cancel) (Create)
    |}];
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".files-ask-error";
  H.input h ~selector:"#files-ask-input" "a/b";
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".files-ask-error";
  [%expect
    {|
    (Files Ask_accept)
    Type a name.
    (Files (Ask_input a/b))
    (Files Ask_accept)
    A name cannot contain /: type it without.
    |}];
  H.input h ~selector:"#files-ask-input" "notes";
  H.key h "Enter" ~target:Field;
  H.text h ~selector:".modal-buttons";
  H.fail h "fs_mkdir" "/home/me/proj/notes already exists";
  H.text h ~selector:".modal";
  [%expect
    {|
    (Files (Ask_input notes))
    (Files Ask_accept)
    (Rpc (method_ fs_mkdir) (params ((path /home/me/proj/notes)))
     (tag (Files (Changed (notice "Created folder \"notes\"") (select (notes))))))
    (Cancel) (Creating…)
    New folder
    (Close (Esc))
    In ~/proj
    [notes]
    /home/me/proj/notes already exists
    (Cancel) (Create)
    |}];
  (* Esc closes without doing anything. *)
  H.key h "Escape" ~target:Field;
  H.text h ~selector:".modal";
  [%expect
    {|
    (Files Ask_cancel)
    (Files (Focus files-list))
    |}];
  H.act h (Files (Select "README.md"));
  H.key h "F2" ~target:Files;
  H.input h ~selector:"#files-ask-input" "READ.md";
  H.key h "Enter" ~target:Field;
  H.reply h "fs_rename" {|{"path":"/home/me/proj/READ.md"}|};
  H.reply h "fs_list" listing;
  [%expect
    {|
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/README.md)))
     (tag (Files (Previewed /home/me/proj/README.md))))
    (Files (Rename README.md))
    (Files (Focus files-ask-input))
    (Files (Ask_input READ.md))
    (Files Ask_accept)
    (Rpc (method_ fs_rename)
     (params ((from /home/me/proj/README.md) (to /home/me/proj/READ.md)))
     (tag
      (Files
       (Changed (notice "Renamed \"README.md\" to \"READ.md\"")
        (select (READ.md))))))
    (Expire_toast (id 0) (after_ms 2780))
    (Rpc (method_ fs_list) (params ((path /home/me/proj) (hidden false)))
     (tag (Files (Listed /home/me/proj))))
    (Files (Focus files-list))
    |}];
  H.act h (Files (Select "src"));
  H.reply h "fs_list" src_listing;
  H.act h (Files Up);
  H.reply h "fs_list" listing;
  H.key h "Delete" ~target:Files;
  H.text h ~selector:".modal";
  H.key h "Enter" ~target:Page;
  H.reply h "fs_delete" {|{"path":"/home/me/proj/src"}|};
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Rpc (method_ fs_list) (params ((path /home/me/proj/src) (hidden false)))
     (tag (Files (Listed /home/me/proj/src))))
    (Rpc (method_ fs_list) (params ((path /home/me/proj) (hidden false)))
     (tag (Files (Listed /home/me/proj))))
    (Files (Delete src))
    (Files (Focus files-dialog))
    Delete src?
    (Close (Esc))
    ~/proj/src and everything in it will be deleted from laptop. This cannot be undone.
    (Cancel) (Delete)
    (Files Ask_accept)
    (Rpc (method_ fs_delete) (params ((path /home/me/proj/src)))
     (tag (Files (Changed (notice "Deleted \"src\"") (select ())))))
    (Expire_toast (id 1) (after_ms 2500))
    (Rpc (method_ fs_list) (params ((path /home/me/proj) (hidden false)))
     (tag (Files (Listed /home/me/proj))))
    (Files (Focus files-list))
    Renamed "README.md" to "READ.md"
    Deleted "src"
    |}]
;;

let%expect_test "paths into the composer, files attached from the browser" =
  let h = opened () in
  H.act h (Files (Select "README.md"));
  H.quiet h (fun () -> H.click h ~selector:".files-act:nth-child(3)");
  H.quiet h (fun () -> H.click h ~selector:".files-act:nth-child(4)");
  H.click h ~selector:".files-act.attach";
  print_endline (H.model h).draft;
  H.text h ~selector:".attach-chips";
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Rpc (method_ fs_preview) (params ((path /home/me/proj/README.md)))
     (tag (Files (Previewed /home/me/proj/README.md))))
    (Files (Insert_path /home/me/proj/README.md))
    (Files (Copy_path /home/me/proj/README.md))
    (Files (Attach README.md))
    (Expire_toast (id 1) (after_ms 3100))
    @README.md
    README.md 2.0 KB (Remove README.md)
    Copied the path ~/proj/README.md
    Attached "README.md" to the next message
    |}];
  H.act h (Files (Attach "src"));
  H.text h ~selector:".toasts .error";
  [%expect
    {| src is a directory: attach its files, or insert its path (@) for the agent to look at. |}];
  H.key h "Enter";
  [%expect
    {|
    Send
    (Save_history (@README.md))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text @README.md) (attachments (README.md))))
     (tag
      (Sent_with_files (text @README.md) (images ())
       (attachments
        (((file ()) (name README.md) (size (2048))
          (status (Ready /home/me/proj/README.md))))))))
    |}];
  (* Sent: the chips are gone; a failure brings them back. *)
  H.text h ~selector:".attach-chips";
  H.fail h "prompt" "the backend is busy";
  H.text h ~selector:".attach-chips";
  [%expect {| README.md 2.0 KB (Remove README.md) |}]
;;

let%expect_test "files dropped on the composer upload into the chat's uploads" =
  let h = H.create ~state:(state ()) () in
  H.act h (Files (Set_base "http://box:7790"));
  H.act
    h
    (Files
       (Chosen
          { dest = Chat
          ; files =
              [ { file = 1; name = "report.pdf"; size = 2_000_000 }
              ; { file = 2; name = "notes.txt"; size = 10 }
              ]
          }));
  [%expect
    {|
    (Rpc (method_ fs_upload) (params ((name report.pdf) (size 2000000)))
     (tag (Files (Upload_link (file 1) (overwrite false)))))
    (Rpc (method_ fs_upload) (params ((name notes.txt) (size 10)))
     (tag (Files (Upload_link (file 2) (overwrite false)))))
    |}];
  H.reply
    h
    "fs_upload"
    {|{"url":"/files/upload?ticket=a","path":"/home/me/proj/.prigh/uploads/report.pdf"}|};
  H.fail
    h
    "fs_upload"
    "the file browser needs a newer prigh on tool host laptop: update prigh \
     there and restart its TUI or tool-host";
  H.act h (Files (Upload_progress { file = 1; sent = 500_000 }));
  H.text h ~selector:".status .transfers";
  H.show h ~selector:".attach-chip.uploading";
  H.text h ~selector:".attach-chips";
  [%expect
    {|
    (Files (Upload (file 1) (url http://box:7790/files/upload?ticket=a)))
    (Files (Forget 2))
    Uploading report.pdf 25%
    <div title="Uploading report.pdf…" class="attach-chip uploading" custom-css-vars=((--done 25%))>
      <span class="files-lamp">  </span>
      <span class="attach-chip-name"> report.pdf </span>
      <span class="attach-chip-detail"> 25% </span>
      <button type="button"
              title="Remove report.pdf"
              aria-label="Remove report.pdf"
              class="attach-chip-remove btn ghost icon"
              @on_click>
        <icon class="close"> </icon>
      </button>
    </div>
    report.pdf 25% (Remove report.pdf)
    notes.txt the file browser needs a newer prigh on tool host laptop: update prigh there and restart its TUI or tool-host (Remove notes.txt)
    |}];
  (* Not sent while uploading or failed: the reason, and what to do. *)
  H.type_ h "summarise";
  H.key h "Enter";
  H.act h (Files (Remove_attachment 1));
  H.key h "Enter";
  [%expect
    {|
    Send
    Send
    |}];
  H.text h ~selector:".toasts";
  [%expect {| Wait for the attachment to upload (or remove it with ×). |}];
  H.act
    h
    (Files
       (Upload_done
          { file = 1
          ; result =
              Ok
                (Jsonaf.of_string
                   {|{"path":"/home/me/proj/.prigh/uploads/report.pdf","bytes":2000000}|})
          }));
  H.text h ~selector:".attach-chips";
  H.key h "Enter";
  [%expect
    {|
    report.pdf 1.9 MB (Remove report.pdf)
    Send
    (Save_history (summarise))
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params ((text summarise) (attachments (.prigh/uploads/report.pdf))))
     (tag
      (Sent_with_files (text summarise) (images ())
       (attachments
        (((file (1)) (name report.pdf) (size (2000000))
          (status (Ready /home/me/proj/.prigh/uploads/report.pdf))))))))
    |}]
;;

let%expect_test "another session or host resets the browser and drops the chips"
  =
  let h = opened () in
  H.act
    h
    (Files
       (Chosen
          { dest = Chat; files = [ { file = 9; name = "a.txt"; size = 5 } ] }));
  H.event h (sprintf {|{"event":"state","state":%s}|} (state ~session:"s2" ()));
  [%expect
    {|
    (Rpc (method_ fs_upload) (params ((name a.txt) (size 5)))
     (tag (Files (Upload_link (file 9) (overwrite false)))))
    (Set_url_session s2)
    (Rpc (method_ get_messages) (params ()) (tag (Messages s2)))
    (Rpc (method_ get_pending) (params ()) (tag Pending))
    (Rpc (method_ list_sessions) (params ()) (tag Sessions))
    (Rpc (method_ list_subagents) (params ()) (tag Subagents))
    (Rpc (method_ list_jobs) (params ()) (tag Jobs))
    (Files (Abort_upload 9))
    (Rpc (method_ fs_list) (params ((hidden false))) (tag (Files (Listed ""))))
    |}];
  H.text h ~selector:".attach-chips";
  H.text h ~selector:".files-panel .files-crumbs";
  [%expect {| /home/me/proj |}]
;;

let%expect_test "on a phone the browser is a sheet that Esc closes" =
  let h = opened () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.elements h ~selector:".app";
  H.key h "Escape" ~target:Control;
  [%expect
    {|
    <div class="app files-open narrow">
    (Files Close)
    (Focus editor)
    |}]
;;
