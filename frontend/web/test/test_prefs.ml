open! Core
open Prigh_web
module H = Harness
module Key = Pref_sync.Key

(* The user's preferences (Pref_sync): the browser's cache first, then the
   backend's, which win; changes go to both; another browser's arrive live;
   a page from before the backend kept them uploads its own, once. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let change key value = { Prigh_protocol.Prefs.Change.key; value = Some value }
let str s = `String s

let show_prefs h =
  let m = H.model h in
  print_s
    [%message
      ""
        ~theme:(m.theme : Theme.t)
        ~look:(App.Model.look m : Theme.t)
        ~notify:(m.notifications.enabled : bool)
        ~verbosity:(m.verbosity : Prigh_ui.Verbosity.t)
        ~history:(History.to_list m.history : string list)
        ~sidebar:(Resize.preference m.layout Sidebar : int option)
        ~terminal_open:(m.terminal.open_ : bool)
        ~remote:(m.prefs.remote : Pref_sync.Remote.t)]
;;

let restore h ?(legacy = false) prefs =
  H.act h (Restore_prefs { prefs; legacy })
;;

let cached =
  [ change Key.theme (str "dark")
  ; change Key.accent (str "blue")
  ; change Key.history (`Array [ str "newest"; str "oldest" ])
  ; change (Resize.Handle.pref_key Sidebar) (`Number "320")
  ; change Key.terminal_open `True
  ]
;;

let%expect_test "the cache first, then the backend's, which win" =
  let h = H.connecting () in
  H.quiet h (fun () -> H.act h Start);
  restore h cached;
  show_prefs h;
  [%expect
    {|
    (Apply_theme ((name Dark) (accent (Preset Blue))))
    ((theme ((name Dark) (accent (Preset Blue))))
     (look ((name Dark) (accent (Preset Blue)))) (notify true) (verbosity Normal)
     (history (newest oldest)) (sidebar (320)) (terminal_open true)
     (remote Unknown))
    |}];
  (* Another browser changed the theme meanwhile and closed the terminal;
     what the backend does not have is the default. *)
  H.reply
    h
    "get_prefs"
    {|{"prefs":{"web.migrated":true,"web.theme":"light","web.accent":"#3366ff","history":["from the tui"],"web.verbosity":"quiet"}}|};
  show_prefs h;
  [%expect
    {|
    (Apply_theme ((name Light) (accent (Custom #3366ff))))
    (Cache_prefs
     (((key web.theme) (value (light))) ((key web.accent) (value (#3366ff)))
      ((key web.notify) (value ())) ((key web.verbosity) (value (quiet)))
      ((key web.terminal-open) (value ())) ((key web.sidebar-folded) (value ()))
      ((key history) (value (("from the tui"))))
      ((key web.migrated) (value (true))) ((key web.sidebar-width) (value ()))
      ((key web.files-width) (value ())) ((key web.files-split) (value ()))
      ((key web.agents-width) (value ())) ((key web.terminal-height) (value ()))))
    ((theme ((name Light) (accent (Custom #3366ff))))
     (look ((name Light) (accent (Custom #3366ff)))) (notify true)
     (verbosity Quiet) (history ("from the tui")) (sidebar ())
     (terminal_open false) (remote Kept))
    |}]
;;

let%expect_test "a page from before uploads the browser's once" =
  let h = H.connecting () in
  H.quiet h (fun () -> H.act h Start);
  restore
    h
    ~legacy:true
    [ change Key.notify `False
    ; change Key.history (`Array [ str "old prompt" ])
    ];
  (* The TUI's history is already there: it stays. *)
  H.reply h "get_prefs" {|{"prefs":{"history":["tui prompt"]}}|};
  show_prefs h;
  [%expect
    {|
    (Apply_theme ((name Original) (accent (Preset Orange))))
    (Apply_theme ((name Original) (accent (Preset Orange))))
    (Cache_prefs
     (((key web.theme) (value ())) ((key web.accent) (value ()))
      ((key web.notify) (value (false))) ((key web.verbosity) (value ()))
      ((key web.terminal-open) (value ())) ((key web.sidebar-folded) (value ()))
      ((key history) (value (("tui prompt"))))
      ((key web.migrated) (value (true))) ((key web.sidebar-width) (value ()))
      ((key web.files-width) (value ())) ((key web.files-split) (value ()))
      ((key web.agents-width) (value ())) ((key web.terminal-height) (value ()))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.notify false) (web.migrated true)))))
     (tag Show_error))
    ((theme ((name Original) (accent (Preset Orange))))
     (look ((name Original) (accent (Preset Orange)))) (notify false)
     (verbosity Normal) (history ("tui prompt")) (sidebar ())
     (terminal_open false) (remote Kept))
    |}];
  (* Once done, the backend's win: a second browser with a page from before
     uploads nothing. *)
  let h = H.connecting () in
  H.quiet h (fun () -> H.act h Start);
  H.quiet h (fun () -> restore h ~legacy:true [ change Key.notify `False ]);
  H.reply h "get_prefs" {|{"prefs":{"web.migrated":true}}|};
  print_s [%sexp ((H.model h).notifications.enabled : bool)];
  [%expect
    {|
    (Apply_theme ((name Original) (accent (Preset Orange))))
    (Cache_prefs
     (((key web.theme) (value ())) ((key web.accent) (value ()))
      ((key web.notify) (value ())) ((key web.verbosity) (value ()))
      ((key web.terminal-open) (value ())) ((key web.sidebar-folded) (value ()))
      ((key history) (value ())) ((key web.migrated) (value (true)))
      ((key web.sidebar-width) (value ())) ((key web.files-width) (value ()))
      ((key web.files-split) (value ())) ((key web.agents-width) (value ()))
      ((key web.terminal-height) (value ()))))
    true
    |}]
;;

let%expect_test "changes go to the backend; another browser's arrive live" =
  let h = H.create () in
  run h "/theme dark";
  H.type_ h "hello";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/theme dark") (max 500))) (tag Ignore))
    (Cache_prefs
     (((key web.theme) (value (dark))) ((key web.accent) (value (orange)))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.theme dark) (web.accent orange))))) (tag Show_error))
    (Expire_toast (id 0) (after_ms 3620))
    (Apply_theme ((name Dark) (accent (Preset Orange))))
    (Rpc (method_ push_pref) (params ((key history) (value hello) (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text hello)))
     (tag (Sent (text hello) (images ()))))
    |}];
  H.event
    h
    {|{"event":"prefs_changed","prefs":{"web.accent":"pink","history":["there","hello"],"web.sidebar-width":300,"web.terminal-open":true,"web.notify":false}}|};
  show_prefs h;
  [%expect
    {|
    (Cache_prefs
     (((key web.accent) (value (pink))) ((key history) (value ((there hello))))
      ((key web.sidebar-width) (value (300)))
      ((key web.terminal-open) (value (true)))
      ((key web.notify) (value (false)))))
    (Apply_theme ((name Dark) (accent (Preset Pink))))
    ((theme ((name Dark) (accent (Preset Pink))))
     (look ((name Dark) (accent (Preset Pink)))) (notify false)
     (verbosity Normal) (history (there hello)) (sidebar (300))
     (terminal_open false) (remote Kept))
    |}];
  (* Browsing the history is not disturbed. *)
  H.key h "ArrowUp";
  H.event
    h
    {|{"event":"prefs_changed","prefs":{"history":["new","there","hello"]}}|};
  print_s [%sexp (History.to_list (H.model h).history : string list)];
  [%expect
    {|
    History_older
    (Cache_prefs (((key history) (value ((new there hello))))))
    (there hello)
    |}]
;;

let%expect_test
    "acting as another user, or on a backend that keeps none, changes stay in \
     the browser"
  =
  let h = H.create () in
  H.act
    h
    (Hello { client_id = "c1"; namespace = Some "bob"; user = Some "alice" });
  run h "/accent teal";
  [%expect
    {|
    (Cache_prefs (((key history) (value (("/accent teal"))))))
    (Cache_prefs
     (((key web.theme) (value (original))) ((key web.accent) (value (teal)))))
    (Expire_toast (id 0) (after_ms 2500))
    (Apply_theme ((name Original) (accent (Preset Teal))))
    |}];
  (* Switched: theirs are not fetched, ours are not sent. *)
  H.act
    h
    (Reply
       ( User_switched
       , Ok
           (Jsonaf.of_string
              {|{"client_id":"c1","namespace":"bob","user":"alice"}|}) ));
  run h "/accent green";
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 2500))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ forge_status) (params ()) (tag (Forge Status)))
    (Cache_prefs (((key history) (value (("/accent green" "/accent teal"))))))
    (Cache_prefs
     (((key web.theme) (value (original))) ((key web.accent) (value (green)))))
    (Expire_toast (id 2) (after_ms 2500))
    (Apply_theme ((name Original) (accent (Preset Green))))
    |}];
  (* Back: ours are fetched again. *)
  H.act
    h
    (Reply
       ( User_switched
       , Ok
           (Jsonaf.of_string
              {|{"client_id":"c1","namespace":"alice","user":"alice"}|}) ));
  print_s
    [%sexp (List.map (H.model h).toasts ~f:(fun t -> t.text) : string list)];
  [%expect
    {|
    (Expire_toast (id 3) (after_ms 2500))
    (Focus editor)
    (Rpc (method_ get_state) (params ()) (tag State))
    (Rpc (method_ list_models) (params ()) (tag Models))
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Refresh)))
    (Rpc (method_ get_config) (params ()) (tag Config))
    (Rpc (method_ forge_status) (params ()) (tag (Forge Status)))
    (Rpc (method_ get_prefs)
     (params
      ((keys
        (web.theme web.accent web.notify web.verbosity web.terminal-open
         web.sidebar-folded history web.migrated web.sidebar-width
         web.files-width web.files-split web.agents-width web.terminal-height))))
     (tag Prefs))
    ("Accent: green" "Back to alice")
    |}];
  (* An older backend refuses get_prefs: the browser keeps them. *)
  let h = H.connecting () in
  H.quiet h (fun () -> H.act h Start);
  H.fail h "get_prefs" "unknown method get_prefs";
  run h "/theme light";
  print_s [%sexp ((H.model h).prefs.remote : Pref_sync.Remote.t)];
  [%expect
    {|
    (Cache_prefs (((key history) (value (("/theme light"))))))
    (Cache_prefs
     (((key web.theme) (value (light))) ((key web.accent) (value (orange)))))
    (Expire_toast (id 0) (after_ms 2500))
    (Apply_theme ((name Light) (accent (Preset Orange))))
    Local
    |}]
;;

let%expect_test "/theme and /accent: pickers that preview what is highlighted" =
  let h = H.create () in
  run h "/theme";
  H.text h ~selector:".picker-dialog";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /theme) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    Theme
    (Close (Esc))
    []
    Original graphite, warm greys ✓
    Dark neutral greys, only the accent in colour
    Light paper white
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  (* Moving shows it; Esc puts the user's back, saving nothing. *)
  H.act h (Picker_move 1);
  H.act h (Picker_move 1);
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    (Apply_theme ((name Dark) (accent (Preset Orange))))
    (Apply_theme ((name Light) (accent (Preset Orange))))
    Close_dialog
    (Focus editor)
    (Apply_theme ((name Original) (accent (Preset Orange))))
    |}];
  run h "/theme";
  H.act h (Picker_query "dar");
  H.act h Picker_accept;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /theme) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    (Apply_theme ((name Dark) (accent (Preset Orange))))
    (Focus editor)
    (Cache_prefs
     (((key web.theme) (value (dark))) ((key web.accent) (value (orange)))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.theme dark) (web.accent orange))))) (tag Show_error))
    (Expire_toast (id 0) (after_ms 3620))
    |}];
  (* A colour typed into the accent picker is shown, and taken. *)
  run h "/accent";
  H.act h (Picker_query "#c0f");
  H.act h Picker_accept;
  run h "/accent";
  H.text h ~selector:".picker-items";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /accent) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    (Apply_theme ((name Dark) (accent (Custom #cc00ff))))
    (Focus editor)
    (Cache_prefs
     (((key web.theme) (value (dark))) ((key web.accent) (value (#cc00ff)))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.theme dark) (web.accent #cc00ff))))) (tag Show_error))
    (Expire_toast (id 1) (after_ms 2500))
    (Rpc (method_ push_pref) (params ((key history) (value /accent) (max 500)))
     (tag Ignore))
    (Focus picker-input)
    Orange
    Blue
    Green
    Purple
    Pink
    Teal
    Yellow
    Custom #cc00ff ✓
    |}];
  (* Wrong names say what would do. *)
  H.key h "Escape" ~target:Page;
  run h "/theme solarized";
  H.text h ~selector:".toasts";
  run h "/accent mauve";
  H.text h ~selector:".toasts";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/theme solarized") (max 500))) (tag Ignore))
    Accent: #cc00ff
    Unknown theme “solarized”: use original, dark or light (/theme picks one). (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/accent mauve") (max 500))) (tag Ignore))
    Accent: #cc00ff
    Unknown accent “mauve”: use orange, blue, green, purple, pink, teal, yellow or a colour like #3b82f6 (/accent picks one). (Dismiss)
    |}]
;;

let%expect_test
    "the settings' appearance: a click saves, the colour input previews while \
     it is dragged"
  =
  let h = H.create () in
  H.act h Open_settings;
  H.click h ~selector:".theme-choice" ~nth:2;
  H.click h ~selector:".accent-choice" ~nth:1;
  H.input h ~selector:"#accent-input" "#123456";
  H.show h ~selector:".setting.accent .on";
  [%expect
    {|
    (Focus dialog)
    (Run "/theme light")
    (Cache_prefs
     (((key web.theme) (value (light))) ((key web.accent) (value (orange)))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.theme light) (web.accent orange))))) (tag Show_error))
    (Expire_toast (id 0) (after_ms 2500))
    (Apply_theme ((name Light) (accent (Preset Orange))))
    (Run "/accent blue")
    (Cache_prefs
     (((key web.theme) (value (light))) ((key web.accent) (value (blue)))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.theme light) (web.accent blue))))) (tag Show_error))
    (Expire_toast (id 1) (after_ms 2500))
    (Apply_theme ((name Light) (accent (Preset Blue))))
    (Preview_accent #123456)
    (Apply_theme ((name Light) (accent (Custom #123456))))
    (Run "/accent #123456")
    (Cache_prefs
     (((key web.theme) (value (light))) ((key web.accent) (value (#123456)))))
    (Rpc (method_ set_prefs)
     (params ((prefs ((web.theme light) (web.accent #123456)))))
     (tag Show_error))
    (Expire_toast (id 2) (after_ms 2500))
    <label title="Any colour (/accent #rrggbb)" class="accent-choice accent-custom on">
      <span aria-hidden="true" class="swatch" style={ --sodium: #123456; }> </span>
      <input type="color"
             id="accent-input"
             aria-label="Custom accent colour"
             #value="#123456"
             @on_change
             @on_input/>
      <span class="accent-custom-label"> Custom </span>
    </label>
    |}];
  (* Closed without choosing: the saved one is back. *)
  H.key h "Escape" ~target:Page;
  print_s [%sexp (App.Model.look (H.model h) : Theme.t)];
  [%expect
    {|
    Close_dialog
    (Focus editor)
    ((name Light) (accent (Custom #123456)))
    |}]
;;

let%expect_test "accents: names and colours" =
  List.iter
    [ "Blue"
    ; " teal "
    ; "#ABC"
    ; "#a1b2c3"
    ; "#1e3a8a"
    ; "#12345"
    ; "#gggggg"
    ; "red"
    ]
    ~f:(fun s ->
      print_s
        [%message
          s
            (Theme.Accent.of_string s : Theme.Accent.t option)
            ~vars:
              (Option.map (Theme.Accent.of_string s) ~f:(fun accent ->
                 Theme.vars { Theme.default with accent })
               : (string * string) list option)]);
  [%expect
    {|
    (Blue ("Theme.Accent.of_string s" ((Preset Blue))) (vars (())))
    (" teal " ("Theme.Accent.of_string s" ((Preset Teal))) (vars (())))
    (#ABC ("Theme.Accent.of_string s" ((Custom #aabbcc)))
     (vars (((--sodium #aabbcc) (--sodium-ink #15171b)))))
    (#a1b2c3 ("Theme.Accent.of_string s" ((Custom #a1b2c3)))
     (vars (((--sodium #a1b2c3) (--sodium-ink #15171b)))))
    (#1e3a8a ("Theme.Accent.of_string s" ((Custom #1e3a8a)))
     (vars (((--sodium #1e3a8a) (--sodium-ink #ffffff)))))
    (#12345 ("Theme.Accent.of_string s" ()) (vars ()))
    (#gggggg ("Theme.Accent.of_string s" ()) (vars ()))
    (red ("Theme.Accent.of_string s" ()) (vars ()))
    |}]
;;
