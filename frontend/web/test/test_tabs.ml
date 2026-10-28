open! Core
open Prigh_web
module H = Harness

(* Session tabs (/tabs, Session_tabs): off by default; on, opening a session
   opens or focuses its tab, which shows how it stands; tabs close, move and
   switch from the keyboard; which are open, in order, is the account's. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let sessions specs = "[" ^ String.concat ~sep:"," specs ^ "]"

let three =
  sessions
    [ H.session_json ~name:"Alpha" "s1"
    ; H.session_json ~name:"Beta" "s2"
    ; H.session_json ~name:"Gamma" "s3"
    ]
;;

(* The page as switching gets it: the backend's answers, quietly. *)
let arrive h id =
  H.quiet h (fun () ->
    H.reply h "switch_session" "{}";
    H.reply h "get_state" (H.state_json ~fields:[ "session_id", `String id ] ()))
;;

let switch h id =
  H.quiet h (fun () ->
    H.act h (Switch_session (sprintf "/sessions/%s.jsonl" id)));
  arrive h id
;;

let listed h json =
  H.quiet h (fun () ->
    H.event h {|{"event":"sessions_changed"}|};
    H.reply h "list_sessions" json)
;;

(* Each tab: [*] the active one, its status, its title and tooltip. *)
let tabs h =
  match
    Session_tabs_view.view (H.model h) ~inject:(fun _ ->
      Virtual_dom.Vdom.Effect.Ignore)
  with
  | None -> print_endline "(no tabs)"
  | Some _ -> H.names h ~selector:".session-tab-open"
;;

let with_tabs ?(sessions = three) () =
  let h = H.create ~sessions () in
  H.quiet h (fun () -> run h "/tabs on");
  h
;;

let%expect_test "off by default: the page has no strip, and /tabs says so" =
  let h = H.create ~sessions:three () in
  H.elements h ~selector:".session-tabs";
  tabs h;
  H.quiet h (fun () -> run h "/tabs");
  H.text h ~selector:".toast";
  [%expect
    {|
    (no tabs)
    Session tabs are off: /tabs on shows the open sessions as tabs
    |}];
  (* Its keys are not taken. *)
  H.key h ~alt:true ~shift:true ~code:"BracketRight" "}";
  H.key h ~alt:true ~code:"KeyW" "w";
  [%expect
    {|
    (browser default)
    (browser default)
    |}];
  H.quiet h (fun () -> run h "/tabs sideways");
  H.text h ~selector:".toast";
  [%expect
    {|
    Session tabs are off: /tabs on shows the open sessions as tabs
    Unknown /tabs argument “sideways”: /tabs on or /tabs off (/tabs alone says which) (Dismiss)
    |}]
;;

let%expect_test
    "/tabs on: saved for the account; opening sessions opens and focuses their \
     tabs"
  =
  let h = H.create ~sessions:three () in
  run h "/tabs on";
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/tabs on") (max 500))) (tag Ignore))
    (Cache_prefs (((key web.tabs) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.tabs true)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 5340))
    (Cache_prefs (((key web.open-tabs) (value ((s1))))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.open-tabs (s1))))))
     (tag Show_error))
    |}];
  tabs h;
  H.show h ~selector:".session-tabs";
  [%expect
    {|
    [tab] button "Alpha"
    <nav aria-label="Session tabs" class="session-tabs">
      <div role="tablist" class="session-tabs-list">
        <div id="tab-s1"
             draggable="true"
             class="active session-tab"
             @on_auxclick
             @on_dragend
             @on_dragstart
             @on_drop>
          <button type="button"
                  role="tab"
                  aria-selected="true"
                  title="Alpha"
                  class="session-tab-open"
                  @on_click>
            <Vdom.Node.none-widget> </Vdom.Node.none-widget>
            <span dir="auto" class="session-tab-title"> Alpha </span>
          </button>
          <button type="button"
                  title="Close tab (Alt+W)"
                  aria-label="Close Alpha"
                  class="btn ghost icon session-tab-close"
                  @on_click>
            <icon class="close"> </icon>
          </button>
        </div>
      </div>
      <div class="session-tabs-end" @on_drop>
        <button type="button"
                title="New session (/new)"
                aria-label="New session"
                class="btn ghost icon session-tabs-new"
                @on_click>
          <icon class="plus"> </icon>
        </button>
      </div>
    </nav>
    |}];
  switch h "s2";
  switch h "s3";
  tabs h;
  [%expect
    {|
    [tab] button "Alpha"
    [tab] button "Beta"
    [tab] button "Gamma"
    |}];
  (* Opening one that has a tab focuses it: nothing new. *)
  switch h "s1";
  tabs h;
  [%expect
    {|
    [tab] button "Alpha"
    [tab] button "Beta"
    [tab] button "Gamma"
    |}];
  (* A tab opens its session. *)
  H.click h ~selector:".session-tab-open" ~nth:1;
  [%expect
    {|
    (Switch_session /sessions/s2.jsonl)
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Focus editor)
    |}]
;;

let%expect_test "switching from the keyboard" =
  let h = with_tabs () in
  switch h "s2";
  switch h "s3";
  H.key h ~alt:true ~shift:true ~code:"BracketRight" "}";
  arrive h "s1";
  tabs h;
  [%expect
    {|
    (Step_tab 1)
    (Rpc (method_ switch_session) (params ((path /sessions/s1.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    (Focus editor)
    [tab] button "Alpha"
    [tab] button "Beta"
    [tab] button "Gamma"
    |}];
  H.key h ~alt:true ~shift:true ~code:"BracketLeft" "{";
  arrive h "s3";
  H.key h ~alt:true ~shift:true ~code:"Digit2" "@";
  arrive h "s2";
  tabs h;
  [%expect
    {|
    (Step_tab -1)
    (Rpc (method_ switch_session) (params ((path /sessions/s3.jsonl)))
     (tag Reload_state))
    (Focus editor)
    (Nth_tab 2)
    (Rpc (method_ switch_session) (params ((path /sessions/s2.jsonl)))
     (tag Reload_state))
    (Focus editor)
    [tab] button "Alpha"
    [tab] button "Beta"
    [tab] button "Gamma"
    |}];
  (* Beyond the last tab: nothing. *)
  H.key h ~alt:true ~shift:true ~code:"Digit9" "(";
  [%expect {| (Nth_tab 9) |}];
  (* The agents panel keeps Alt+digits. *)
  H.key h ~alt:true ~code:"Digit1" "1";
  [%expect {| (browser default) |}];
  (* The bindings are in the keymap. *)
  List.iter Keys.help ~f:(fun (key, what) ->
    if String.is_substring key ~substring:"session tabs"
    then printf "%s: %s\n" key what);
  [%expect
    {| Alt+Shift+] Alt+Shift+[ / Alt+Shift+1…9 / Alt+W (session tabs): the next or previous tab, the Nth, close this one (/tabs on; also its ×, a middle click; drag a tab to move it) |}]
;;

let%expect_test
    "closing: ×, a middle click (Close_tab), Alt+W; the last leaves the empty \
     state"
  =
  let h = with_tabs () in
  switch h "s2";
  switch h "s3";
  switch h "s2";
  (* Another tab: it goes, the session stays. *)
  H.click h ~selector:".session-tab-close" ~nth:2;
  tabs h;
  [%expect
    {|
    (Close_tab (s3))
    (Cache_prefs (((key web.open-tabs) (value ((s1 s2))))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.open-tabs (s1 s2))))))
     (tag Show_error))
    [tab] button "Alpha"
    [tab] button "Beta"
    |}];
  (* The active one: its neighbour opens (the next, else the previous). *)
  H.key h ~alt:true ~code:"KeyW" "w";
  arrive h "s1";
  tabs h;
  [%expect
    {|
    (Close_tab ())
    (Rpc (method_ switch_session) (params ((path /sessions/s1.jsonl)))
     (tag Reload_state))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    (Focus editor)
    (Cache_prefs (((key web.open-tabs) (value ((s1))))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.open-tabs (s1))))))
     (tag Show_error))
    [tab] button "Alpha"
    |}];
  H.act h (Close_tab None);
  tabs h;
  [%expect
    {|
    (Rpc (method_ new_session) (params ()) (tag Reload_state))
    (Focus editor)
    (Cache_prefs (((key web.open-tabs) (value (())))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.open-tabs ())))))
     (tag Show_error))
    |}];
  (* The new session stays out of the strip until it is listed: the empty
     state, with only [+]. *)
  H.quiet h (fun () ->
    H.reply h "new_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s9" ] ()));
  H.names h ~selector:".session-tabs button";
  H.text h ~selector:".empty-hints";
  tabs h;
  [%expect {| button "New session" -- New session (/new) |}];
  listed
    h
    (sessions
       [ H.session_json "s1"; H.session_json ~first_prompt:"Hello" "s9" ]);
  tabs h;
  [%expect {| [tab] button "Hello" |}]
;;

let%expect_test "dragging a tab moves it" =
  let h = with_tabs () in
  switch h "s2";
  switch h "s3";
  H.act h (Drag_tab (Some "s3"));
  H.elements h ~selector:".session-tab.dragging";
  H.act h (Drop_tab (Some "s1"));
  tabs h;
  [%expect
    {|
    <div class="active dragging session-tab">
    (Cache_prefs (((key web.open-tabs) (value ((s3 s1 s2))))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.open-tabs (s3 s1 s2))))))
     (tag Show_error))
    [tab] button "Gamma"
    [tab] button "Alpha"
    [tab] button "Beta"
    |}];
  (* To the end. *)
  H.act h (Drag_tab (Some "s3"));
  H.act h (Drop_tab None);
  tabs h;
  [%expect {|
    (Cache_prefs (((key web.open-tabs) (value ((s1 s2 s3))))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.open-tabs (s1 s2 s3))))))
     (tag Show_error))
    [tab] button "Alpha"
    [tab] button "Beta"
    [tab] button "Gamma"
    |}];
  (* Dropped nowhere: nothing moves. *)
  H.act h (Drag_tab (Some "s1"));
  H.act h (Drag_tab None);
  H.act h (Drop_tab (Some "s2"));
  tabs h;
  [%expect
    {|
    [tab] button "Alpha"
    [tab] button "Beta"
    [tab] button "Gamma"
    |}]
;;

let%expect_test "each tab says how its session stands" =
  let h =
    with_tabs
      ~sessions:
        (sessions
           [ H.session_json ~name:"Alpha" "s1"
           ; H.session_json ~name:"Beta" "s2"
           ; H.session_json ~name:"Gamma" "s3"
           ; H.session_json ~name:"Delta" "s4"
           ])
      ()
  in
  List.iter [ "s2"; "s3"; "s4"; "s1" ] ~f:(switch h);
  listed
    h
    (sessions
       [ H.session_json ~name:"Alpha" "s1"
       ; H.session_json ~name:"Beta" ~live:true ~running:true "s2"
       ; H.session_json ~name:"Gamma" ~live:true ~running:true ~waiting:1 "s3"
       ; H.session_json ~name:"Delta" ~failed:true "s4"
       ]);
  tabs h;
  H.elements h ~selector:".session-tab";
  H.show h ~selector:"#tab-s2 .session-tab-open";
  [%expect
    {|
    [tab] button "Alpha"
    [tab] button "Beta" -- Beta — Running
    [tab] button "Gamma" -- Gamma — Waiting for you: a confirmation or a question
    [tab] button "Delta" -- Delta — Failed: its last reply ended in an error
    <div class="active session-tab">
    <div class="running session-tab">
    <div class="session-tab waiting">
    <div class="failed session-tab">
    <button type="button"
            role="tab"
            aria-selected="false"
            title="Beta — Running"
            class="session-tab-open"
            @on_click>
      <span title="Running" role="img" aria-label="Running" class="lamp"> </span>
      <span dir="auto" class="session-tab-title"> Beta </span>
    </button>
    |}];
  (* Beta's run ended out of sight: done since you looked, until you do. *)
  listed
    h
    (sessions
       [ H.session_json ~name:"Alpha" "s1"
       ; H.session_json ~name:"Beta" ~live:true "s2"
       ; H.session_json ~name:"Gamma" ~live:true "s3"
       ; H.session_json ~name:"Delta" ~failed:true "s4"
       ]);
  tabs h;
  [%expect
    {|
    [tab] button "Alpha"
    [tab] button "Beta" -- Beta — Done since you looked
    [tab] button "Gamma" -- Gamma — Done since you looked
    [tab] button "Delta" -- Delta — Failed: its last reply ended in an error
    |}];
  switch h "s2";
  switch h "s1";
  tabs h;
  [%expect
    {|
    [tab] button "Alpha"
    [tab] button "Beta"
    [tab] button "Gamma" -- Gamma — Done since you looked
    [tab] button "Delta" -- Delta — Failed: its last reply ended in an error
    |}]
;;

let%expect_test "deleting or archiving a session closes its tab" =
  let h = with_tabs () in
  switch h "s2";
  switch h "s3";
  listed
    h
    (sessions
       [ H.session_json ~name:"Alpha" "s1"; H.session_json ~name:"Gamma" "s3" ]);
  tabs h;
  [%expect
    {|
    [tab] button "Alpha"
    [tab] button "Gamma"
    |}];
  listed
    h
    (sessions
       [ H.session_json ~name:"Alpha" ~archived:true "s1"
       ; H.session_json ~name:"Gamma" "s3"
       ]);
  tabs h;
  [%expect {| [tab] button "Gamma" |}]
;;

let%expect_test
    "the account's: restored on load, off from another browser, not on a phone"
  =
  let h = H.connecting () in
  H.quiet h (fun () ->
    H.act h Start;
    H.reply h "get_state" (H.state_json ());
    H.reply h "list_sessions" three;
    H.reply
      h
      "get_prefs"
      {|{"prefs":{"web.migrated":true,"web.tabs":true,"web.open-tabs":["s3","s2","gone"]}}|});
  tabs h;
  [%expect
    {|
    [tab] button "Gamma"
    [tab] button "Beta"
    [tab] button "Alpha"
    |}];
  (* Another browser turns them off, and on: the strip follows; its tabs
     do not replace ours. *)
  H.event h {|{"event":"prefs_changed","prefs":{"web.tabs":null}}|};
  tabs h;
  H.event
    h
    {|{"event":"prefs_changed","prefs":{"web.tabs":true,"web.open-tabs":["s1"]}}|};
  tabs h;
  [%expect
    {|
    (Cache_prefs (((key web.tabs) (value ()))))
    (no tabs)
    (Cache_prefs
     (((key web.tabs) (value (true))) ((key web.open-tabs) (value ((s1))))))
    [tab] button "Gamma"
    [tab] button "Beta"
    [tab] button "Alpha"
    |}];
  (* A phone shows none, and leaves the keys alone. *)
  H.quiet h (fun () -> H.act h (Set_narrow true));
  tabs h;
  H.key h ~alt:true ~code:"KeyW" "w";
  [%expect
    {|
    (no tabs)
    (browser default)
    |}];
  (* The settings page's switch. *)
  H.quiet h (fun () ->
    H.act h (Set_narrow false);
    H.act h Open_settings);
  H.names h ~selector:".setting.tabs [role=switch]";
  H.click h ~selector:".setting.tabs [role=switch]";
  tabs h;
  [%expect
    {|
    [switch] button "Tabs" -- Turn session tabs off (/tabs off)
    (Run "/tabs off")
    (Cache_prefs (((key web.tabs) (value ()))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.tabs null)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 3100))
    (no tabs)
    |}]
;;
