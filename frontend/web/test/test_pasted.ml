open! Core
open! Prigh_web
module H = Harness

(* Long pastes into the composer are chips, sent whole with the message as
   pasted texts; the transcript shows them folded. *)

let hello ?(features = [ "pasted_text" ]) h =
  H.act
    h
    (Hello
       { client_id = "c1"
       ; namespace = None
       ; user = None
       ; superuser = false
       ; features
       ; deployed = Prigh_protocol.Deployed_engines.harness_only
       })
;;

(* Long by its lines (over 40), short to print. *)
let long = String.concat ~sep:"\n" (List.init 41 ~f:(fun i -> sprintf "l%d" i))
let chips h = H.text h ~selector:".pasted-chips"

let%expect_test "is_long: 2,000 characters or 40 lines" =
  List.iter
    [ String.make 2000 'x'
    ; String.make 2001 'x'
    ; String.make 1000 '\xc3' (* not characters: bytes *)
    ; String.concat ~sep:"\n" (List.init 40 ~f:(fun _ -> "a"))
    ; long
    ]
    ~f:(fun s ->
      printf
        "%d bytes, %d lines: %b\n"
        (String.length s)
        (Prigh_ui.Pasted_text.count_lines s)
        (Prigh_ui.Pasted_text.is_long s));
  [%expect
    {|
    2000 bytes, 1 lines: false
    2001 bytes, 1 lines: true
    1000 bytes, 1 lines: false
    79 bytes, 40 lines: false
    153 bytes, 41 lines: true
    |}]
;;

let%expect_test "a long paste is a chip; opened, removed, inserted as text" =
  let h = H.create () in
  hello h;
  H.type_ h "why?";
  H.act h (Paste_text long);
  H.act h (Paste_text (String.make 3000 'y'));
  print_s [%sexp ((H.model h).draft : string)];
  chips h;
  [%expect
    {|
    (Focus editor)
    (Focus editor)
    why?
    (Pasted text #1 41 lines · 153 B) (Remove Pasted text #1)
    (Pasted text #2 1 line · 2.9 KB) (Remove Pasted text #2)
    |}];
  H.click h ~selector:".pasted-chip-open";
  H.text h ~selector:".pasted-dialog";
  [%expect
    {|
    (View_pasted 0)
    (Focus dialog)
    Pasted text #1 · 41 lines · 153 B
    (Close (Esc))
    l0
    l1
    l2
    l3
    l4
    l5
    l6
    l7
    l8
    l9
    l10
    l11
    l12
    l13
    l14
    l15
    l16
    l17
    l18
    l19
    l20
    l21
    l22
    l23
    l24
    l25
    l26
    l27
    l28
    l29
    l30
    l31
    l32
    l33
    l34
    l35
    l36
    l37
    l38
    l39
    l40
    Sent whole with the message, not in the box (Remove) (Insert as text) (Done)
    |}];
  H.act h Dialog_accept;
  H.click ~nth:1 h ~selector:".attach-chip-remove";
  chips h;
  [%expect
    {|
    (Focus editor)
    (Remove_pasted 1)
    (Focus editor)
    (Pasted text #1 41 lines · 153 B) (Remove Pasted text #1)
    |}];
  H.click h ~selector:".pasted-chip-open";
  H.act h (Insert_pasted 0);
  print_s [%sexp ((H.model h).draft : string)];
  chips h;
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect
    {|
    (View_pasted 0)
    (Focus dialog)
    (Focus editor)
    (Set_caret 157)
     "why?l0\
    \nl1\
    \nl2\
    \nl3\
    \nl4\
    \nl5\
    \nl6\
    \nl7\
    \nl8\
    \nl9\
    \nl10\
    \nl11\
    \nl12\
    \nl13\
    \nl14\
    \nl15\
    \nl16\
    \nl17\
    \nl18\
    \nl19\
    \nl20\
    \nl21\
    \nl22\
    \nl23\
    \nl24\
    \nl25\
    \nl26\
    \nl27\
    \nl28\
    \nl29\
    \nl30\
    \nl31\
    \nl32\
    \nl33\
    \nl34\
    \nl35\
    \nl36\
    \nl37\
    \nl38\
    \nl39\
    \nl40"
    false
    |}]
;;

let%expect_test "too large: refused, saying the limit and what to do" =
  let h = H.create () in
  H.act h (Paste_text (String.make (Prigh_ui.Pasted_text.max_bytes + 1) 'z'));
  chips h;
  H.text h ~selector:".toast";
  [%expect
    {| That paste is 2,097,153 bytes, over the 2.0 MB (2,097,152 bytes) a pasted text may be: save it to a file and attach that instead. (Dismiss) |}]
;;

let%expect_test
    "sending carries the pasted texts; an old backend gets them inline"
  =
  let h = H.create () in
  hello h;
  H.type_ h "why?";
  H.act h (Paste_text long);
  H.act h Send;
  chips h;
  [%expect
    {|
    (Focus editor)
    (Rpc (method_ push_pref) (params ((key history) (value why?) (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params
      ((text why?)
       (pasted
        ( "l0\
         \nl1\
         \nl2\
         \nl3\
         \nl4\
         \nl5\
         \nl6\
         \nl7\
         \nl8\
         \nl9\
         \nl10\
         \nl11\
         \nl12\
         \nl13\
         \nl14\
         \nl15\
         \nl16\
         \nl17\
         \nl18\
         \nl19\
         \nl20\
         \nl21\
         \nl22\
         \nl23\
         \nl24\
         \nl25\
         \nl26\
         \nl27\
         \nl28\
         \nl29\
         \nl30\
         \nl31\
         \nl32\
         \nl33\
         \nl34\
         \nl35\
         \nl36\
         \nl37\
         \nl38\
         \nl39\
         \nl40"))))
     (tag
      (Sent_with_files (text why?) (images ()) (attachments ())
       (pasted
        ( "l0\
         \nl1\
         \nl2\
         \nl3\
         \nl4\
         \nl5\
         \nl6\
         \nl7\
         \nl8\
         \nl9\
         \nl10\
         \nl11\
         \nl12\
         \nl13\
         \nl14\
         \nl15\
         \nl16\
         \nl17\
         \nl18\
         \nl19\
         \nl20\
         \nl21\
         \nl22\
         \nl23\
         \nl24\
         \nl25\
         \nl26\
         \nl27\
         \nl28\
         \nl29\
         \nl30\
         \nl31\
         \nl32\
         \nl33\
         \nl34\
         \nl35\
         \nl36\
         \nl37\
         \nl38\
         \nl39\
         \nl40")))))
    |}];
  (* It failed: the chips are back. *)
  H.fail h "prompt" "boom";
  chips h;
  [%expect {| (Pasted text #1 41 lines · 153 B) (Remove Pasted text #1) |}];
  let h = H.create () in
  hello ~features:[] h;
  H.act h (Paste_text "a\nb");
  H.act h Send;
  [%expect
    {|
    (Focus editor)
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params
      ((text
         "<pasted_text name=\"Pasted text #1\" lines=\"2\">\
        \na\
        \nb\
        \n</pasted_text>")))
     (tag
      (Sent_with_files (text "") (images ()) (attachments ())
       (pasted ( "a\
                \nb")))))
    |}]
;;

let%expect_test "the transcript shows a chip, its text only once opened" =
  let h = H.create () in
  let pasted = "first line\nsecond\nthird" in
  let text =
    sprintf
      "why?\n\n\
       <pasted_text name=\"Pasted text #1\" lines=\"3\">\n\
       %s\n\
       </pasted_text>"
      pasted
  in
  let start =
    String.length text - String.length "\n</pasted_text>" - String.length pasted
  in
  H.event
    h
    (sprintf
       {|{"type":"event","event":"message_start","message":{"role":"user","text":%s,"origin":{"body":"why?","pasted":[{"name":"Pasted text #1","lines":3,"start":%d,"length":%d}]},"at":1791194280000}}|}
       (Jsonaf.to_string (`String text))
       start
       (String.length pasted));
  H.show h ~selector:".msg.user";
  [%expect
    {|
    <div data-entry="0" data-message-entry="0" id="msg-0" tabindex="-1" class="msg user">
      <div dir="auto" class="bubble"> why? </div>
      <div class="pasted-texts">
        <div class="copyable pasted">
          <button type="button"
                  data-fold="pasted:1791194280000:0"
                  aria-expanded="false"
                  title="Show it all"
                  class="pasted-head">
            <span class="chev"> </span>
            <span class="pasted-name"> Pasted text #1 </span>
            <span class="pasted-detail"> 3 lines · 23 B </span>
            <span class="pasted-preview"> first line </span>
          </button>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
      </div>
      <time title="Monday 5 October 2026, 09:58:00" datetime="2026-10-05T09:58:00Z" class="time"> 09:58 </time>
    </div>
    |}];
  (* The page's listener turns a click on [data-fold] into this. *)
  H.act h (Toggle_fold "pasted:1791194280000:0");
  H.show h ~selector:".pasted";
  [%expect
    {|
    <div class="copyable open pasted">
      <button type="button"
              data-fold="pasted:1791194280000:0"
              aria-expanded="true"
              title="Fold it"
              class="pasted-head">
        <span class="chev"> </span>
        <span class="pasted-name"> Pasted text #1 </span>
        <span class="pasted-detail"> 3 lines · 23 B </span>
        <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      </button>
      <div class="pasted-body">
        <button type="button" title="Copy" class="copy"> Copy </button>
        <pre class="copy-text pasted-text"> first line
    second
    third </pre>
      </div>
    </div>
    |}]
;;

let%expect_test "Stop gives queued pasted texts back as chips" =
  let h = H.create () in
  hello h;
  H.act h Abort;
  H.reply h "abort" {|{"restored":["look"],"pasted":[["log one\nlog two"]]}|};
  print_s [%sexp ((H.model h).draft : string)];
  chips h;
  [%expect
    {|
    (Rpc (method_ abort) (params ()) (tag Restored))
    (Expire_toast (id 0) (after_ms 3260))
    (Focus editor)
    look
    (Pasted text #1 2 lines · 15 B) (Remove Pasted text #1)
    |}]
;;

let%expect_test "Ctrl+Z undoes a chip coming or going; typed text is the box's" =
  let h = H.create () in
  hello h;
  H.type_ h "why?";
  H.act h (Paste_text long);
  chips h;
  H.key h ~ctrl:true ~code:"KeyZ" "z";
  chips h;
  print_endline "--- redo: Ctrl+Shift+Z, then undo and Ctrl+Y, and ⌘ on a Mac";
  H.key h ~ctrl:true ~shift:true ~code:"KeyZ" "Z";
  chips h;
  H.key h ~meta:true ~code:"KeyZ" "z";
  H.key h ~ctrl:true ~code:"KeyY" "y";
  H.key h ~meta:true ~shift:true ~code:"KeyZ" "z";
  chips h;
  print_endline "--- nothing more to redo: the box's own";
  H.key h ~ctrl:true ~code:"KeyY" "y";
  print_endline "--- removing a chip with its ×, then Ctrl+Z";
  H.act h (Paste_text (String.make 3000 'y'));
  H.click h ~selector:".pasted-chip .attach-chip-remove";
  chips h;
  H.key h ~ctrl:true ~code:"KeyZ" "z";
  chips h;
  print_endline "--- typed text after it: Ctrl+Z is the box's";
  H.type_ h "why? and";
  H.key h ~ctrl:true ~code:"KeyZ" "z";
  H.key h ~ctrl:true ~shift:true ~code:"KeyZ" "z";
  chips h;
  [%expect
    {|
    (Focus editor)
    (Pasted text #1 41 lines · 153 B) (Remove Pasted text #1)
    Undo_paste
    (Focus editor)
    --- redo: Ctrl+Shift+Z, then undo and Ctrl+Y, and ⌘ on a Mac
    Redo_paste
    (Focus editor)
    (Pasted text #1 41 lines · 153 B) (Remove Pasted text #1)
    Undo_paste
    (Focus editor)
    Redo_paste
    (Focus editor)
    (browser default)
    (Pasted text #1 41 lines · 153 B) (Remove Pasted text #1)
    --- nothing more to redo: the box's own
    (browser default)
    --- removing a chip with its ×, then Ctrl+Z
    (Focus editor)
    (Remove_pasted 0)
    (Focus editor)
    (Pasted text #1 1 line · 2.9 KB) (Remove Pasted text #1)
    Undo_paste
    (Focus editor)
    (Pasted text #1 41 lines · 153 B) (Remove Pasted text #1)
    (Pasted text #2 1 line · 2.9 KB) (Remove Pasted text #2)
    --- typed text after it: Ctrl+Z is the box's
    (browser default)
    (browser default)
    (Pasted text #1 41 lines · 153 B) (Remove Pasted text #1)
    (Pasted text #2 1 line · 2.9 KB) (Remove Pasted text #2)
    |}]
;;

(* A user message as the backend has it: what was typed, then each pasted
   text's block, with the origin saying where the texts are. *)
let message_with_pasted ~id ~parent ~body pasted =
  let text, spans =
    List.foldi pasted ~init:(body, []) ~f:(fun i (text, spans) p ->
      let lines = Prigh_ui.Pasted_text.count_lines p in
      let header =
        sprintf
          "\n\n<pasted_text name=\"Pasted text #%d\" lines=\"%d\">\n"
          (i + 1)
          lines
      in
      let start = String.length text + String.length header in
      ( text ^ header ^ p ^ "\n</pasted_text>"
      , `Object
          [ "name", `String (sprintf "Pasted text #%d" (i + 1))
          ; "lines", `Number (Int.to_string lines)
          ; "start", `Number (Int.to_string start)
          ; "length", `Number (Int.to_string (String.length p))
          ]
        :: spans ))
  in
  `Object
    [ "id", `String id
    ; "parent", parent
    ; "kind", `String "message"
    ; ( "message"
      , `Object
          [ "role", `String "user"
          ; "text", `String text
          ; ( "origin"
            , `Object
                [ "body", `String body; "pasted", `Array (List.rev spans) ] )
          ] )
    ]
;;

let%expect_test "/fork gives a message's pasted texts back as chips" =
  let h = H.create () in
  hello h;
  H.type_ h "/fork";
  H.quiet h (fun () -> H.act h Send);
  H.reply
    h
    "get_entries"
    (Jsonaf.to_string
       (`Object
           [ "head", `String "u2"
           ; ( "entries"
             , `Array
                 [ message_with_pasted
                     ~id:"u1"
                     ~parent:`Null
                     ~body:"why?"
                     [ "trace A" ]
                 ; message_with_pasted
                     ~id:"u2"
                     ~parent:(`String "u1")
                     ~body:"and these?"
                     [ "trace B\nline 2"; "trace C" ]
                 ] )
           ]));
  H.key h "Enter" ~target:Field;
  print_s [%sexp ((H.model h).draft : string)];
  chips h;
  H.key h "Enter";
  [%expect
    {|
    (Focus picker-input)
    Dialog_accept
    (Focus editor)
    (Expire_toast (id 0) (after_ms 3700))
    (Rpc (method_ fork) (params ((at u2))) (tag Reload_state))
    "and these?"
    (Pasted text #1 2 lines · 14 B) (Remove Pasted text #1)
    (Pasted text #2 1 line · 7 B) (Remove Pasted text #2)
    Send
    (Rpc (method_ push_pref)
     (params ((key history) (value "and these?") (max 500))) (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt)
     (params ((text "and these?") (pasted ( "trace B\
                                           \nline 2" "trace C"))))
     (tag
      (Sent_with_files (text "and these?") (images ()) (attachments ())
       (pasted ( "trace B\
                \nline 2" "trace C")))))
    |}]
;;
