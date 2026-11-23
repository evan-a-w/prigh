open! Core
module H = Harness

(* A message's context menu: how it opens (right-click or long press at
   the pointer, Shift+F10 or the Menu key on the focused message, under
   it; messages have no ⋯ button), what it offers
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
   long press) opens the message's menu at the pointer. *)
let right_click h n =
  H.act
    h
    (Open_menu { target = Message n; anchor = Point { x = 300; y = 120 } })
;;

let items h = H.names h ~selector:".context-menu [role=menuitem]"
let toasts h = H.text h ~selector:".toast"
let dialog h = H.text h ~selector:".modal"

(* The messages with a menu, as [H.elements] shows them: the open one's,
   and the preview it carries. *)
let mores h = H.elements h ~selector:"[data-message-entry]"

let%expect_test "a right-click on a message opens its menu there" =
  let h = session () in
  right_click h 2;
  H.show h ~selector:".msg[data-message-entry=\"2\"]";
  H.show h ~selector:".context-menu";
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    <div data-entry="2" data-message-entry="2" id="msg-2" tabindex="-1" class="menu-open msg user">
      <div dir="auto" class="bubble"> question 2 </div>
      <time title="Monday 5 October 2026, 10:02:00" datetime="2026-10-05T10:02:00Z" class="time"> 10:02 </time>
    </div>
    <div data-slot="dropdown-menu-content"
         role="menu"
         aria-label="Your message"
         tabindex="-1"
         id="dialog"
         data-x="300"
         data-y="120"
         class="[&:not(.placed)]:opacity-0 [&:not(.placed)]:pointer-events-none [&_.menu-label]:shrink-0 animate-in bg-popover cn-menu-target context-menu duration-100 fade-in-0 fixed loon max-[760px]:[&_.menu-item]:min-h-10 max-[760px]:[&_.menu-item]:text-[15px] max-[760px]:bottom-2 max-[760px]:left-2! max-[760px]:max-w-none max-[760px]:right-2 max-[760px]:top-auto! max-h-[calc(100vh-16px)] max-w-[min(360px,calc(100vw-16px))] menu-surface min-w-32 min-w-[248px] motion-reduce:animate-none narrow:[&_.menu-item]:min-h-11! narrow:animate-[sheet-up_0.18s_ease-out] narrow:border-border narrow:border-t narrow:overscroll-contain narrow:pb-[max(8px,var(--safe-b))] narrow:pl-[max(8px,var(--safe-l))] narrow:pr-[max(8px,var(--safe-r))] narrow:pt-0 narrow:ring-0 narrow:rounded-b-none narrow:rounded-t-xl narrow:shadow-[0_-12px_40px_-12px_var(--edge-shadow)] narrow:z-(--z-menu-surface) overflow-x-hidden overflow-y-auto p-1 ring-1 ring-foreground/10 rounded-lg shadow-md text-popover-foreground zoom-in-95">
      <div class="-mx-1 [&_.session-place]:flex-none border-b border-border context-menu-head flex flex-col gap-px mb-1 pb-1.5 pt-1 px-2.5">
        <span class="context-menu-title flex font-medium gap-1.5 items-center text-[13px] text-foreground truncate"> Your message </span>
        <span class="context-menu-sub font-mono font-sans preview text-muted-foreground text-xs truncate"> question 2 </span>
      </div>
      <div data-slot="dropdown-menu-group"
           role="group"
           class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
        <div data-slot="dropdown-menu-item"
             data-highlighted=""
             role="menuitem"
             data-item="copy_message"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm"
             @on_click
             @on_mouseenter>
          <icon class="copy"> </icon>
          <span class="menu-label truncate"> Copy </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
      </div>
      <div data-slot="dropdown-menu-group"
           role="group"
           class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="trim_before"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="scissors"> </icon>
          <span class="menu-label truncate"> Trim everything before this </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> /trim </kbd>
        </div>
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-item="trim_after"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="scissors"> </icon>
          <span class="menu-label truncate"> Trim everything after this </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> /trim after </kbd>
        </div>
      </div>
      <div data-slot="dropdown-menu-group"
           role="group"
           class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
        <div data-slot="dropdown-menu-item"
             role="menuitem"
             data-variant="destructive"
             data-item="delete"
             class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default danger data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
             @on_click
             @on_mouseenter>
          <icon class="trash"> </icon>
          <span class="menu-label truncate"> Delete </span>
          <kbd data-slot="dropdown-menu-shortcut"
               class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> /delete </kbd>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "messages have no ⋯: they are focusable, the open one marked" =
  let h = session () in
  H.elements h ~selector:".msg-more";
  H.elements h ~selector:"[data-message-menu]";
  H.elements h ~selector:"#chat button[aria-haspopup]";
  mores h;
  [%expect
    {|
    <div class="msg user">
    <div class="assistant msg">
    <div class="msg user">
    <div class="assistant msg">
    <div class="msg user">
    <div class="assistant msg">
    |}];
  H.key h "F10" ~shift:true ~target:(Message 1);
  H.show h ~selector:".msg[data-message-entry=\"1\"]";
  [%expect
    {|
    (Open_menu ((target (Message 1)) (anchor (Element msg-1))))
    (Focus dialog)
    (Place_menu (Element msg-1))
    <div data-message-entry="1" id="msg-1" tabindex="-1" class="assistant menu-open msg">
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> answer 1 </p>
        </div>
      </div>
    </div>
    |}];
  (* Esc closes it, nothing else. *)
  H.key h "Escape" ~target:Page;
  mores h;
  [%expect
    {|
    Close_dialog
    (Focus msg-1)
    <div class="msg user">
    <div class="assistant msg">
    <div class="msg user">
    <div class="assistant msg">
    <div class="msg user">
    <div class="assistant msg">
    |}]
;;

let%expect_test "Shift+F10 or the Menu key on the focused message" =
  let h = session () in
  H.key h "F10" ~shift:true ~target:(Message 4);
  menu h;
  [%expect
    {|
    (Open_menu ((target (Message 4)) (anchor (Element msg-4))))
    (Focus dialog)
    (Place_menu (Element msg-4))
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
    (Focus msg-4)
    |}];
  H.key h "ContextMenu" ~target:(Message 5);
  menu h;
  [%expect
    {|
    (Open_menu ((target (Message 5)) (anchor (Element msg-5))))
    (Focus dialog)
    (Place_menu (Element msg-5))
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
    (Focus msg-5)
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
    (Focus msg-0)
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
    <div class="msg user">
    <div class="assistant msg">
    <div class="msg user">
    <div class="assistant msg">
    <div class="msg user">
    <div class="assistant menu-open msg">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground disabled flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
    |}];
  H.key h "Escape" ~target:Page;
  (* During a run, all but Copy. *)
  let h = session ~running:true () in
  right_click h 2;
  H.text h ~selector:".context-menu";
  [%expect
    {|
    Close_dialog
    (Focus msg-5)
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
    (Open_menu ((target (Message 2)) (anchor (Element msg-2))))
    (Focus dialog)
    (Place_menu (Element msg-2))
    (Dialog_move 1)
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
    <div class="msg user">
    <div class="assistant msg">
    <div class="menu-open msg preview-trim-before user">
    <div class="assistant msg">
    <div class="msg user">
    <div class="assistant msg">
    |}];
  H.key h "ArrowDown" ~target:Page;
  H.elements h ~selector:".context-menu .menu-item.selected";
  mores h;
  [%expect
    {|
    (Dialog_move 1)
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
    <div class="msg user">
    <div class="assistant msg">
    <div class="menu-open msg preview-trim-after user">
    <div class="assistant msg">
    <div class="msg user">
    <div class="assistant msg">
    |}];
  H.key h "ArrowDown" ~target:Page;
  H.elements h ~selector:".context-menu .menu-item.selected";
  mores h;
  [%expect
    {|
    (Dialog_move 1)
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default danger data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
    <div class="msg user">
    <div class="assistant msg">
    <div class="menu-open msg preview-delete user">
    <div class="assistant msg">
    <div class="msg user">
    <div class="assistant msg">
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
    (Copy (text "answer 2") (copied "Copied the reply to the clipboard"))
    (Expire_toast (id 0) (after_ms 2820))
    Copied the reply to the clipboard
    |}]
;;

(* On a plain http address other than localhost the browser has no
   clipboard API, and the fallback may fail too: no "Copied", but what to do
   instead. *)
let%expect_test "Copy: when the browser refuses, an error saying what to do" =
  let h = session () in
  H.block_clipboard h;
  right_click h 3;
  H.click h ~selector:".context-menu [data-item=copy_message]";
  toasts h;
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    (Picker_choose copy_message)
    (Focus editor)
    (Copy (text "answer 2") (copied "Copied the reply to the clipboard"))
    Couldn't copy: the browser blocks the clipboard on this http address; use the backend's https address, or select the text and copy it (Ctrl+C) (Dismiss)
    |}];
  (* A code block's Copy reports the same way; in a secure context the
     browser refused for another reason. *)
  H.act h (Copy_failed { secure = true });
  toasts h;
  [%expect
    {|
    Couldn't copy: the browser blocks the clipboard on this http address; use the backend's https address, or select the text and copy it (Ctrl+C) (Dismiss)
    Couldn't copy: the browser refused the clipboard; select the text and copy it (Ctrl+C) (Dismiss)
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
     (text
       "Area $\\pi r^2$, not $5 or $10:\
      \n\
      \n$$\
      \n\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}\
      \n$$")
     (copied "Copied the reply to the clipboard"))
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
    <div data-message-entry="1" id="msg-1" tabindex="-1" class="assistant msg">
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> Listing. </p>
        </div>
      </div>
      <div data-slot="collapsible" data-state="closed" class="ok tool tool-bash">
        <button data-slot="collapsible-trigger"
                type="button"
                data-fold="c1#open"
                aria-expanded="false"
                class="tool-head">
          <span title="Done" role="img" aria-label="Done" class="done mark">
            <icon class="check"> </icon>
          </span>
          <span aria-hidden="true" class="tool-icon">
            <icon class="terminal"> </icon>
          </span>
          <span class="name"> bash </span>
          <span class="arg command"> ls </span>
          <span class="chev">  </span>
        </button>
      </div>
    </div>
    <div data-message-entry="2" id="msg-2" tabindex="-1" class="assistant msg">
      <div class="markdown">
        <div class="md-part">
          <p dir="auto"> done </p>
        </div>
      </div>
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
    (Copy (text Listing.) (copied "Copied the reply to the clipboard"))
    (Expire_toast (id 0) (after_ms 2820))
    |}]
;;

let%expect_test "under a filter: only what shows has a menu, acting as before" =
  let h = session () in
  H.quiet h (fun () -> H.act h (Run "/filter -user"));
  H.elements h ~selector:".entries > *";
  H.elements h ~selector:".entries > [tabindex]";
  [%expect
    {|
    <div class="filtered">
    <div class="assistant msg">
    <div class="filtered">
    <div class="assistant msg">
    <div class="filtered">
    <div class="assistant msg">
    <div class="assistant msg">
    <div class="assistant msg">
    <div class="assistant msg">
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

let%expect_test
    "on a phone, Select text: a long press opens the menu, not the browser's \
     selection"
  =
  let h = session () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  right_click h 2;
  H.text h ~selector:".context-menu";
  H.click h ~selector:".context-menu [data-item=select_text]";
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    Your message question 2
    Copy
    Select text
    Trim everything before this /trim
    Trim everything after this /trim after
    Delete /delete
    (Picker_choose select_text)
    (Select_message 2)
    |}];
  (* Not on a computer: the mouse selects. *)
  H.quiet h (fun () -> H.act h (Set_narrow false));
  right_click h 2;
  items h;
  [%expect
    {|
    (Focus dialog)
    (Place_menu (Point (x 300) (y 120)))
    [menuitem] div "Copy"
    [menuitem] div "Trim everything before this /trim"
    [menuitem] div "Trim everything after this /trim after"
    [menuitem] div "Delete /delete"
    |}]
;;
