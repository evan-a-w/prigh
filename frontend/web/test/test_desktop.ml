open! Core
open Prigh_web
module H = Harness

(* Under [prigh desktop] (the desktop app) the page can quit it: /quit,
   Ctrl+Q (⌘Q), the account menu and, on a phone, the ⋯ sheet. In a browser
   there is nothing to quit: /quit says to close the tab, Ctrl+Q is the
   browser's and nothing offers Quit prigh. *)
let%expect_test "in a browser: /quit says to close the tab, no Quit prigh" =
  let h = H.create () in
  H.type_ h "/quit";
  H.key h "Enter";
  print_endline "-- Ctrl+Q:";
  H.key h "q" ~ctrl:true ~code:"KeyQ";
  H.quiet h (fun () ->
    H.act
      h
      (Hello
         { client_id = "c1"
         ; namespace = Some "alice"
         ; user = Some "alice"
         ; superuser = false
         }));
  H.act h Open_accounts;
  H.text h ~selector:".account-menu";
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref) (params ((key history) (value /quit) (max 500)))
     (tag Ignore))
    (Expire_toast (id 0) (after_ms 4620))
    -- Ctrl+Q:
    (browser default)
    (Focus dialog)
    A
    alice
    signed in
    Add account… sign in as another user, or to another backend
    Turn notifications off stop telling you what happens while the tab is hidden
    Settings Ctrl+,
    Commands and keys /help
    Sign out forget this account in this browser
    ↑↓ to move, Enter to choose, Esc to close
    |}]
;;

let%expect_test "under the desktop app: /quit, Ctrl+Q and ⌘Q quit" =
  let h = H.create () in
  H.act h (Set_desktop true);
  H.type_ h "/quit";
  H.key h "Enter";
  print_endline "-- Ctrl+Q in the editor:";
  H.key h "q" ~ctrl:true ~code:"KeyQ";
  print_endline "-- Cmd+Q:";
  H.key h "q" ~meta:true ~code:"KeyQ";
  print_endline "-- Ctrl+Shift+Q (with Caps Lock) on the page:";
  H.key h "Q" ~ctrl:true ~code:"KeyQ" ~target:Page;
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ push_pref) (params ((key history) (value /quit) (max 500)))
     (tag Ignore))
    Quit_desktop
    -- Ctrl+Q in the editor:
    Quit_desktop
    Quit_desktop
    -- Cmd+Q:
    Quit_desktop
    Quit_desktop
    -- Ctrl+Shift+Q (with Caps Lock) on the page:
    Quit_desktop
    Quit_desktop
    |}]
;;

let%expect_test "under the desktop app: Quit prigh in the account menu" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act
      h
      (Hello
         { client_id = "c1"
         ; namespace = Some "alice"
         ; user = Some "alice"
         ; superuser = false
         });
    H.act h (Set_desktop true));
  H.act h Open_accounts;
  H.text h ~selector:".account-menu";
  H.act h (Picker_choose "quit");
  [%expect
    {|
    (Focus dialog)
    A
    alice
    signed in
    Add account… sign in as another user, or to another backend
    Turn notifications off stop telling you what happens while the tab is hidden
    Settings Ctrl+,
    Commands and keys /help
    Sign out forget this account in this browser
    Quit prigh close the desktop app and its tool host (Ctrl+Q)
    ↑↓ to move, Enter to choose, Esc to close
    (Focus editor)
    Quit_desktop
    |}]
;;

let%expect_test "under the desktop app on a phone: Quit prigh in the ⋯ sheet" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Set_narrow true);
    H.act h (Set_desktop true));
  H.click h ~selector:".chrome-more .more-toggle";
  H.elements h ~selector:"#chrome-menu > .quit-row";
  H.text h ~selector:"#chrome-menu > .quit-row";
  H.click h ~selector:"#chrome-menu > .quit-row";
  [%expect
    {|
    Toggle_status_tray
    <button class="quit-row sheet-row">
    (Quit prigh)
    Toggle_status_tray
    Quit_desktop
    Quit_desktop
    |}]
;;

let%expect_test "/help lists the key and /quit" =
  List.iter Keys.help ~f:(fun (key, text) ->
    if String.is_substring key ~substring:"Ctrl+Q"
    then printf "%s: %s\n" key text);
  Option.iter (Slash.find ~superuser:false "quit") ~f:(fun c ->
    printf "/%s: %s\n" c.name c.help);
  [%expect
    {|
    Ctrl+Q / ⌘+Q (desktop app): quit prigh: its window and this machine's tool host (/quit)
    /quit: leave: quit the desktop app (Ctrl+Q); in a browser, close the tab (/signout signs out)
    |}]
;;

let%expect_test "the sidecar's link and answers" =
  List.iter
    [ "#desktop=0123abcd"; "#signin=x&desktop=k"; "#desktop="; "#signin=x"; "" ]
    ~f:(fun hash ->
      print_s
        [%message hash ~key:(Desktop_link.key_of_hash hash : string option)]);
  print_endline (Desktop_link.status_body ~key:"k");
  print_endline
    (Desktop_link.connect_body
       ~key:"k"
       ~address:"host:7790"
       ~user:None
       ~token:(Some "sekrit")
       ~plaintext:false);
  print_endline
    (Desktop_link.connect_body
       ~key:"k"
       ~address:"host:7790"
       ~user:None
       ~token:None
       ~plaintext:true);
  List.iter
    [ {|{"state":"connect","address":"host:7790","user":null,"error":"refused"}|}
    ; {|{"state":"connect","address":null,"user":null,"error":null}|}
    ; {|{"state":"connected","address":"host:7790","code":"c0de"}|}
    ; {|{"state":"connected","address":"host:7790","code":null}|}
    ; {|{"state":"remote","url":"https://ai.example.com/#app=desktop&host=laptop"}|}
    ; {|{"state":"remote"}|}
    ; {|{"state":"other"}|}
    ; "not json"
    ]
    ~f:(fun reply ->
      print_s
        [%sexp
          (Desktop_link.Status.of_json reply : Desktop_link.Status.t option)]);
  List.iter
    [ {|{"ok":true,"code":"c0de"}|}
    ; {|{"ok":true,"remote":"https://ai.example.com/#app=desktop"}|}
    ; {|{"ok":false,"error":"cannot reach the backend"}|}
    ; {|{"ok":false,"error":"not encrypted","plaintext":true}|}
    ; {|{"ok":false}|}
    ; "[]"
    ]
    ~f:(fun reply ->
      print_s
        [%sexp
          (Desktop_link.Connect_reply.of_json reply
           : Desktop_link.Connect_reply.t option)]);
  [%expect
    {|
    (#desktop=0123abcd (key (0123abcd)))
    (#signin=x&desktop=k (key (k)))
    (#desktop= (key ()))
    (#signin=x (key ()))
    ("" (key ()))
    {"key":"k"}
    {"key":"k","address":"host:7790","user":null,"token":"sekrit"}
    {"key":"k","address":"host:7790","user":null,"token":null,"plaintext":true}
    ((Connect (address (host:7790)) (user ()) (error (refused))))
    ((Connect (address ()) (user ()) (error ())))
    ((Connected (code (c0de))))
    ((Connected (code ())))
    ((Remote https://ai.example.com/#app=desktop&host=laptop))
    ()
    ()
    ()
    ((Connected (c0de)))
    ((Remote https://ai.example.com/#app=desktop))
    ((Refused "cannot reach the backend"))
    ((Plaintext "not encrypted"))
    ((Refused "The backend did not accept the connection."))
    ()
    |}]
;;

let%expect_test "the backend's own site opened by the desktop app" =
  List.iter
    [ "#app=desktop&host=host-laptop"
    ; "#app=desktop&host=my%20laptop%2Fhome"
    ; "#app=desktop&host=a+b%zz%4"
    ; "#app=desktop"
    ; "#app=desktop&host="
    ; "#host=laptop"
    ; "#app=other&host=laptop"
    ; "#desktop=k"
    ; ""
    ]
    ~f:(fun hash ->
      print_s
        [%message
          hash
            ~hint:(Desktop_link.Hint.of_hash hash : Desktop_link.Hint.t option)]);
  print_endline Desktop_link.quit_url;
  [%expect
    {|
    (#app=desktop&host=host-laptop (hint (((host (host-laptop))))))
    (#app=desktop&host=my%20laptop%2Fhome (hint (((host ("my laptop/home"))))))
    (#app=desktop&host=a+b%zz%4 (hint (((host (a+b%zz%4))))))
    (#app=desktop (hint (((host ())))))
    (#app=desktop&host= (hint (((host ())))))
    (#host=laptop (hint ()))
    (#app=other&host=laptop (hint ()))
    (#desktop=k (hint ()))
    ("" (hint ()))
    about:blank#prigh-desktop-quit
    |}]
;;
