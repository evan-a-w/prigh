open! Core
open Prigh_web

(* Which clicks on links the desktop app's browser window hands to the app
   (External_link.binding), to open in the default browser at once, and
   which it leaves to the browser. *)

let own = "http://127.0.0.1:7780"

let click ?(button = 0) ?(ctrl = false) ?(meta = false) ?(shift = false) () =
  { External_link.Click.button; ctrl; meta; shift }
;;

let show ?(target = "_blank") ?(download = false) ?(click = click ()) href =
  print_s
    [%message
      href
        ~_:
          (External_link.decide ~own_origin:own ~href ~target ~download click
           : External_link.t option)]
;;

let%expect_test "links to other sites are handed over; prigh's own are not" =
  List.iter
    ~f:show
    [ "https://github.com/evan-a-w/prigh/pull/1"
    ; "http://example.com/"
    ; "HTTPS://Example.COM/x"
    ; "mailto:me@example.com"
      (* prigh's own site, also written with its default port or in capitals *)
    ; "http://127.0.0.1:7780/?session=s2"
    ; "HTTP://127.0.0.1:7780/files/a.txt"
      (* another port, another scheme: another site *)
    ; "http://127.0.0.1:7781/"
    ; "https://127.0.0.1:7780/"
      (* what the app would refuse, or the page's own *)
    ; "file:///etc/passwd"
    ; "javascript:alert(1)"
    ; "blob:http://127.0.0.1:7780/uuid"
    ; "data:text/plain,hi"
    ; "vscode://file/x"
    ; "about:blank#prigh-desktop-quit"
    ];
  [%expect
    {|
    (https://github.com/evan-a-w/prigh/pull/1
     (((url https://github.com/evan-a-w/prigh/pull/1) (new_window true))))
    (http://example.com/ (((url http://example.com/) (new_window true))))
    (HTTPS://Example.COM/x (((url HTTPS://Example.COM/x) (new_window true))))
    (mailto:me@example.com (((url mailto:me@example.com) (new_window true))))
    (http://127.0.0.1:7780/?session=s2 ())
    (HTTP://127.0.0.1:7780/files/a.txt ())
    (http://127.0.0.1:7781/ (((url http://127.0.0.1:7781/) (new_window true))))
    (https://127.0.0.1:7780/ (((url https://127.0.0.1:7780/) (new_window true))))
    (file:///etc/passwd ())
    ("javascript:alert(1)" ())
    (blob:http://127.0.0.1:7780/uuid ())
    (data:text/plain,hi ())
    (vscode://file/x ())
    (about:blank#prigh-desktop-quit ())
    |}]
;;

let%expect_test "the default port, user info" =
  let decide own_origin href =
    External_link.decide ~own_origin ~href ~target:"" ~download:false (click ())
    |> Option.is_some
  in
  print_s
    [%message
      (decide "https://ai.example.com" "https://ai.example.com:443/x" : bool)
        (decide "http://ai.example.com" "http://AI.example.com:80" : bool)
        (decide "https://ai.example.com" "https://me@ai.example.com/" : bool)
        (decide "https://ai.example.com" "https://ai.example.com@evil.example/"
         : bool)
        (decide "https://ai.example.com" "https://ai.example.com.evil.example/"
         : bool)];
  [%expect
    {|
    (("decide \"https://ai.example.com\" \"https://ai.example.com:443/x\"" false)
     ("decide \"http://ai.example.com\" \"http://AI.example.com:80\"" false)
     ("decide \"https://ai.example.com\" \"https://me@ai.example.com/\"" false)
     ("decide \"https://ai.example.com\" \"https://ai.example.com@evil.example/\""
      true)
     ("decide \"https://ai.example.com\" \"https://ai.example.com.evil.example/\""
      true))
    |}]
;;

let%expect_test "a new window or the same: target, modifiers, the middle button"
  =
  let url = "https://example.com/docs" in
  List.iter
    [ "a link in the window", "", click ()
    ; "_self", "_self", click ()
    ; "_top", "_top", click ()
    ; "a named window", "docs", click ()
    ; "_blank", "_blank", click ()
    ; "Ctrl+click", "", click ~ctrl:true ()
    ; "⌘+click", "", click ~meta:true ()
    ; "Shift+click", "", click ~shift:true ()
    ; "a middle click", "", click ~button:1 ()
    ; "a right click (auxclick)", "_blank", click ~button:2 ()
    ; "the back button (auxclick)", "_blank", click ~button:3 ()
    ]
    ~f:(fun (what, target, click) ->
      print_s
        [%message
          what
            ~_:
              (External_link.decide
                 ~own_origin:own
                 ~href:url
                 ~target
                 ~download:false
                 click
               |> Option.map ~f:(fun t -> t.new_window)
               : bool option)]);
  print_endline "-- a download link:";
  show ~download:true url;
  [%expect
    {|
    ("a link in the window" (false))
    (_self (false))
    (_top (false))
    ("a named window" (true))
    (_blank (true))
    (Ctrl+click (true))
    ("\226\140\152+click" (true))
    (Shift+click (true))
    ("a middle click" (true))
    ("a right click (auxclick)" ())
    ("the back button (auxclick)" ())
    -- a download link:
    (https://example.com/docs ())
    |}]
;;

let%expect_test "what the binding is called with" =
  print_endline External_link.binding;
  print_endline
    (External_link.payload
       { url = "https://example.com/a?b=\"c\""; new_window = true });
  print_endline
    (External_link.payload
       { url = "mailto:me@example.com"; new_window = false });
  [%expect
    {|
    prighOpenExternal
    {"url":"https://example.com/a?b=\"c\"","newWindow":true}
    {"url":"mailto:me@example.com","newWindow":false}
    |}]
;;
