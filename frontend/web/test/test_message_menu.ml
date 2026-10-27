open! Core
open Prigh_web
module H = Harness

(* A message's context menu: how it opens (right-click, its ⋯ button,
   Shift+F10 or the Menu key on the focused message), what it offers
   (Copy, Trim before, Trim after, Delete; disabled with the reason where
   they do not apply), what each does, the preview of the highlighted
   item, and the keyboard. *)

let base_ms = 1791194400000.
let at_ms n = base_ms +. Float.of_int (n * 60_000)

let user n =
  sprintf {|{"role":"user","text":"question %d","at":%.0f}|} n (at_ms n)
;;

let assistant text =
  sprintf
    {|{"role":"assistant","content":[{"type":"text","text":"%s"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    text
;;

let messages l = "[" ^ String.concat ~sep:"," l ^ "]"

let exchanges ns =
  messages
    (List.concat_map ns ~f:(fun n ->
       [ user n; assistant (sprintf "answer %d" n) ]))
;;

let state ?(running = false) () =
  H.state_json ~fields:[ ("running", if running then `True else `False) ] ()
;;

let newest h messages =
  H.quiet h (fun () ->
    H.act h (Reply (H.newest "s1", Ok (Jsonaf.of_string (H.page messages)))))
;;

(* Session s1 with exchanges 1 to 3: entries 0 to 5. *)
let session ?running () =
  let h = H.create ~state:(state ?running ()) () in
  newest h (exchanges [ 1; 2; 3 ]);
  h
;;

let menu h = H.text h ~selector:".context-menu"

(* What the page's listeners do ({!Chat_listeners}): a right-click (or a
   long press) opens the message's menu at the pointer; its ⋯, under it. *)
let right_click h n =
  H.act
    h
    (Open_menu { target = Message n; anchor = Point { x = 300; y = 120 } })
;;

let more h n =
  H.act
    h
    (Open_menu { target = Message n; anchor = Element (Chat_view.more_id n) })
;;

let items h = H.names h ~selector:".context-menu [role=menuitem]"
let toasts h = H.text h ~selector:".toast"
let dialog h = H.text h ~selector:".modal"

(* The ⋯ buttons, as [H.elements] shows them, and the preview they carry. *)
let mores h = H.elements h ~selector:".msg-more"

let%expect_test "a right-click on a message opens its menu there" =
  let h = session () in
  right_click h 2;
  H.show h ~selector:".context-menu";
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    <div id="dialog"
         role="menu"
         aria-label="Your message"
         tabindex="-1"
         data-x="300"
         data-y="120"
         class="context-menu menu-surface">
      <div class="context-menu-head">
        <span class="context-menu-title"> Your message </span>
        <span class="context-menu-sub preview"> question 2 </span>
      </div>
      <div class="menu-section">
        <div role="menuitem"
             data-item="copy_message"
             class="menu-item selected"
             @on_click
             @on_mouseenter>
          <icon class="copy"> </icon>
          <span class="menu-label"> Copy </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
      </div>
      <div class="menu-section">
        <div role="menuitem" data-item="trim_before" class="menu-item" @on_click @on_mouseenter>
          <icon class="scissors"> </icon>
          <span class="menu-label"> Trim everything before this </span>
          <kbd class="menu-command"> /trim </kbd>
        </div>
        <div role="menuitem" data-item="trim_after" class="menu-item" @on_click @on_mouseenter>
          <icon class="scissors"> </icon>
          <span class="menu-label"> Trim everything after this </span>
          <kbd class="menu-command"> /trim after </kbd>
        </div>
      </div>
      <div class="menu-section">
        <div role="menuitem" data-item="delete" class="danger menu-item" @on_click @on_mouseenter>
          <icon class="trash"> </icon>
          <span class="menu-label"> Delete </span>
          <kbd class="menu-command"> /delete </kbd>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "the ⋯ button opens it under itself, and says it is open" =
  let h = session () in
  mores h;
  [%expect
    {|
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    |}];
  more h 1;
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Element msg-more-1))
    |}];
  H.show h ~selector:".msg[data-message-entry=\"1\"]";
  [%expect
    {|
    <div data-message-entry="1" tabindex="-1" class="assistant msg">
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> answer 1 </p>
        </div>
      </div>
      <button type="button"
              id="msg-more-1"
              data-message-menu="1"
              title="More (right-click, Shift+F10)"
              aria-label="More for this message"
              aria-haspopup="menu"
              aria-expanded="true"
              class="msg-more open">
        <icon class="more"> </icon>
      </button>
    </div>
    |}];
  (* Esc closes it, nothing else. *)
  H.key h "Escape" ~target:Page;
  mores h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    |}]
;;

let%expect_test "Shift+F10 or the Menu key on the focused message" =
  let h = session () in
  H.key h "F10" ~shift:true ~target:(Message 4);
  menu h;
  [%expect
    {|
    (Open_menu ((target (Message 4)) (anchor (Element msg-more-4))))
    (Focus dialog)
    (Place_menu (Element msg-more-4))
    Your message question 3
    Copy
    Trim everything before this /trim
    Trim everything after this /trim after
    Delete /delete
    |}];
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  H.key h "ContextMenu" ~target:(Message 5);
  menu h;
  [%expect
    {|
    (Open_menu ((target (Message 5)) (anchor (Element msg-more-5))))
    (Focus dialog)
    (Place_menu (Element msg-more-5))
    Reply answer 3
    Copy
    Trim everything before this /trim
    Trim everything after this Nothing after it: the newest message
    Delete /delete
    |}];
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  (* Elsewhere, F10 is the browser's. *)
  H.key h "F10" ~shift:true ~target:Page;
  [%expect {| (browser default) |}];
  H.key h "F10" ~target:(Message 4);
  [%expect {| (browser default) |}]
;;

let%expect_test "what does not apply is disabled, saying why" =
  let h = session () in
  (* The first message: nothing before it. *)
  right_click h 0;
  items h;
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    [menuitem] div "Copy"
    [menuitem] div "Trim everything before this The context starts here"
    [menuitem] div "Trim everything after this /trim after"
    [menuitem] div "Delete /delete"
    |}];
  H.key h "Escape" ~target:Page;
  (* The newest: nothing after it. *)
  right_click h 5;
  items h;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    [menuitem] div "Copy"
    [menuitem] div "Trim everything before this /trim"
    [menuitem] div "Trim everything after this Nothing after it: the newest message"
    [menuitem] div "Delete /delete"
    |}];
  (* Accepting a disabled item does nothing; the menu stays. *)
  H.key h "ArrowDown" ~target:Page;
  H.key h "ArrowDown" ~target:Page;
  H.key h "Enter" ~target:Page;
  mores h;
  H.elements h ~selector:".context-menu .menu-item.selected";
  [%expect
    {|
    (Dialog_move 1)
    (Dialog_move 1)
    Dialog_accept
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more open">
    <div class="disabled menu-item selected">
    |}];
  H.key h "Escape" ~target:Page;
  (* During a run, all but Copy. *)
  let h = session ~running:true () in
  right_click h 2;
  H.text h ~selector:".context-menu";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    Your message question 2
    Copy
    Trim everything before this Wait for the run to end, or stop it (Esc)
    Trim everything after this Wait for the run to end, or stop it (Esc)
    Delete Wait for the run to end, or stop it (Esc)
    |}]
;;

let%expect_test "the keyboard: arrows skip nothing, Enter accepts" =
  let h = session () in
  H.key h "F10" ~shift:true ~target:(Message 2);
  H.key h "ArrowDown" ~target:Page;
  H.elements h ~selector:".context-menu .menu-item.selected";
  mores h;
  [%expect
    {|
    (Open_menu ((target (Message 2)) (anchor (Element msg-more-2))))
    (Focus dialog)
    (Place_menu (Element msg-more-2))
    (Dialog_move 1)
    <div class="menu-item selected">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more open preview-trim-before">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    |}];
  H.key h "ArrowDown" ~target:Page;
  H.elements h ~selector:".context-menu .menu-item.selected";
  mores h;
  [%expect
    {|
    (Dialog_move 1)
    <div class="menu-item selected">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more open preview-trim-after">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    |}];
  H.key h "ArrowDown" ~target:Page;
  H.elements h ~selector:".context-menu .menu-item.selected";
  mores h;
  [%expect
    {|
    (Dialog_move 1)
    <div class="danger menu-item selected">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more open preview-delete">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    |}];
  (* Enter: Delete, at once. *)
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    Dialog_accept
    (Focus editor)
    (Rpc (method_ get_message_points) (params ())
     (tag
      (Message_points (Delete (entry 2) (index 2) (at ("2026-10-05 10:02:00Z"))))))
    |}]
;;

let%expect_test "Copy: the message's text, acknowledged" =
  let h = session () in
  right_click h 3;
  H.click h ~selector:".context-menu [data-item=copy_message]";
  toasts h;
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    (Picker_choose copy_message)
    (Focus editor)
    (Copy "answer 2")
    (Expire_toast (id 0) (after_ms 2820))
    Copied the reply to the clipboard
    |}]
;;

let%expect_test "Copy: a reply with math copies its TeX as written" =
  let h = H.create ~state:(state ()) () in
  newest
    h
    (messages
       [ user 1
       ; assistant
           {|Area $\\pi r^2$, not $5 or $10:\n\n$$\n\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}\n$$|}
       ]);
  H.show h ~selector:".msg.assistant .markdown";
  right_click h 1;
  H.click h ~selector:".context-menu [data-item=copy_message]";
  [%expect
    {|
    <div class="markdown">
      <div class="md-part">
        <p dir="auto">
          Area
          <span class="math math-inline"> \pi r^2 </span>
          , not $5 or $10:
        </p>
      </div>
      <div class="md-part">
        <span class="math math-display"> \begin{pmatrix} a & b \\ c & d \end{pmatrix} </span>
      </div>
    </div>
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    (Picker_choose copy_message)
    (Focus editor)
    (Copy
      "Area $\\pi r^2$, not $5 or $10:\
     \n\
     \n$$\
     \n\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}\
     \n$$")
    (Expire_toast (id 0) (after_ms 2820))
    |}]
;;

let%expect_test "Trim before and after: the confirmation, as before" =
  let h = session () in
  right_click h 2;
  H.click h ~selector:".context-menu [data-item=trim_before]";
  H.elements h ~selector:".context-menu";
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    (Picker_choose trim_before)
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag (Message_points (Trim_before (index 2) (at ("2026-10-05 10:02:00Z"))))))
    |}];
  right_click h 2;
  H.click h ~selector:".context-menu [data-item=trim_after]";
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    (Picker_choose trim_after)
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag
      (Message_points
       (Trim_after (index 2) (last 2) (at ("2026-10-05 10:02:00Z"))))))
    |}]
;;

let%expect_test "a tool call's menu is its reply's" =
  let h = H.create ~state:(state ()) () in
  newest
    h
    (messages
       [ user 1
       ; {|{"role":"assistant","content":[{"type":"text","text":"Listing."},{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"ls\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
       ; {|{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"a b","is_error":false}|}
       ; assistant "done"
       ]);
  H.show h ~selector:".msg.assistant";
  [%expect
    {|
    <div data-message-entry="1" tabindex="-1" class="assistant msg">
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> Listing. </p>
        </div>
      </div>
      <div class="ok tool tool-bash">
        <div class="tool-head">
          <span title="Done" role="img" aria-label="Done" class="done mark">
            <icon class="check"> </icon>
          </span>
          <span class="name"> bash </span>
          <span class="arg command"> ls </span>
        </div>
        <div class="tool-body">
          <div class="output">
            <pre> a b </pre>
          </div>
        </div>
      </div>
      <button type="button"
              id="msg-more-1"
              data-message-menu="1"
              title="More (right-click, Shift+F10)"
              aria-label="More for this message"
              aria-haspopup="menu"
              aria-expanded="false"
              class="msg-more">
        <icon class="more"> </icon>
      </button>
    </div>
    <div data-message-entry="2" tabindex="-1" class="assistant msg">
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> done </p>
        </div>
      </div>
      <button type="button"
              id="msg-more-2"
              data-message-menu="2"
              title="More (right-click, Shift+F10)"
              aria-label="More for this message"
              aria-haspopup="menu"
              aria-expanded="false"
              class="msg-more">
        <icon class="more"> </icon>
      </button>
    </div>
    |}];
  right_click h 1;
  menu h;
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    Reply with a tool call Listing.
    Copy
    Trim everything before this /trim
    Trim everything after this /trim after
    Delete /delete
    |}];
  H.click h ~selector:".context-menu [data-item=copy_message]";
  [%expect
    {|
    (Picker_choose copy_message)
    (Focus editor)
    (Copy Listing.)
    (Expire_toast (id 0) (after_ms 2820))
    |}]
;;

let%expect_test "under a filter: only what shows has a menu, acting as before" =
  let h = session () in
  H.quiet h (fun () -> H.act h (Run "/filter -user"));
  H.elements h ~selector:".entries > *";
  H.elements h ~selector:".entries > [tabindex] > .msg-more";
  [%expect
    {|
    <div class="filtered">
    <div class="assistant msg">
    <div class="filtered">
    <div class="assistant msg">
    <div class="filtered">
    <div class="assistant msg">
    <button class="msg-more">
    <button class="msg-more">
    <button class="msg-more">
    |}];
  (* The newest reply's Trim before still trims before it. *)
  right_click h 5;
  H.text h ~selector:".context-menu";
  H.click h ~selector:".context-menu [data-item=trim_before]";
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    Reply answer 3
    Copy
    Trim everything before this /trim
    Trim everything after this Nothing after it: the newest message
    Delete /delete
    (Picker_choose trim_before)
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag (Message_points (Trim_before (index 5) (at ())))))
    |}]
;;
