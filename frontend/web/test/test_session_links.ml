open! Core
open Prigh_web
module H = Harness

(* Links to sessions in the transcript: the model's [title](?session=ID),
   and the ids of the user's sessions in text and tool output. *)

let taskbar = "6cad9f6e0123abcd"
let parser = "7b1e2d3c4f5a6b7c"
let twin = "7b1e2d3c99999999"

let sessions =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json ~first_prompt:"hello" "s1"
       ; H.session_json
           ~name:"Fix the taskbar entry"
           ~first_prompt:"the taskbar entry is blank"
           taskbar
       ; H.session_json ~first_prompt:"fix the parser" parser
       ; H.session_json ~first_prompt:"another" twin
       ])
;;

let reply h text = H.event h (Agent_events.text_reply text)

let%expect_test "a markdown link to a session is a session link" =
  let h = H.create ~sessions () in
  reply
    h
    (sprintf
       "See [Fix the taskbar entry](?session=%s), [the same](prigh://session/%s), \
        [by prefix](/?session=6cad9f6e) and [elsewhere](https://example.com/?session=%s)."
       taskbar
       taskbar
       taskbar);
  H.show h ~selector:".chat .markdown p";
  [%expect {||}]
;;

let%expect_test "ids of known sessions are links; other hex is not" =
  let h = H.create ~sessions () in
  reply
    h
    (String.concat
       ~sep:"\n\n"
       [ "Done in 6cad9f6e, see also `6cad9f6e0123abcd`."
       ; (* An unknown id, a commit's hash, a known id inside a word, an
            ambiguous prefix, too short *)
         "Not 0badc0de, a1b2c3d4e5f60718293a4b5c6d7e8f9012345678, x6cad9f6e, \
          7b1e2d3c, 6cad9f6."
       ; "```\nsession 6cad9f6e in a code block\n```"
       ; "[a link around 6cad9f6e](https://example.com)"
       ]);
  H.show h ~selector:".chat .markdown";
  [%expect {||}]
;;

let%expect_test "tool output: the sessions tool's ids are links" =
  let h = H.create ~sessions () in
  H.event
    h
    (Agent_events.call
       ~id:"c1"
       ~name:"sessions"
       {|{"action":"search","query":"taskbar"}|});
  H.event
    h
    (Agent_events.tool_end
       ~id:"c1"
       ~name:"sessions"
       (sprintf
          "%s  2026-11-10  [Fix the taskbar entry](?session=%s)  /work  4 \
           messages\n\
           deadbeefdeadbeef  2026-11-09  [Gone](?session=deadbeefdeadbeef)  \
           /work  2 messages"
          taskbar
          taskbar));
  H.show h ~selector:".chat .tool";
  [%expect {||}]
;;

let%expect_test "the session list arriving later makes ids links" =
  let h = H.create () in
  reply h "Done in 6cad9f6e.";
  H.show h ~selector:".chat .markdown p";
  [%expect {||}];
  H.event h {|{"event":"sessions_changed"}|};
  H.reply h "list_sessions" sessions;
  H.show h ~selector:".chat .markdown p";
  [%expect {||}]
;;

let%expect_test "following a link: in place, in a new window, or a toast" =
  let h = H.create ~sessions () in
  H.act h (Follow_session_link { id = "6cad9f6e"; new_window = false });
  [%expect {||}];
  H.act h (Follow_session_link { id = taskbar; new_window = true });
  [%expect {||}];
  (* Deleted, or another user's; a typo of one; ambiguous *)
  List.iter [ "0badc0de"; "6cad9f7e"; "7b1e2d3c" ] ~f:(fun id ->
    H.act h (Follow_session_link { id; new_window = false });
    H.text h ~selector:".toast:last-child");
  [%expect {||}]
;;

let own = "http://127.0.0.1:7780"

let%expect_test "which clicks the page follows in place" =
  let show ?(button = 0) ?(ctrl = false) ?(session = None) href =
    print_s
      [%message
        href
          ~_:
            (Session_link_click.decide
               ~own_origin:own
               ~own_path:"/"
               ~href
               ~session
               { button; ctrl; meta = false; shift = false }
             : Session_link_click.t option)]
  in
  show ~session:(Some "6cad9f6e") (own ^ "/?session=6cad9f6e");
  show ~session:(Some "6cad9f6e") ~ctrl:true (own ^ "/?session=6cad9f6e");
  show ~session:(Some "6cad9f6e") ~button:1 (own ^ "/?session=6cad9f6e");
  show ~session:(Some "6cad9f6e") ~button:2 (own ^ "/?session=6cad9f6e");
  (* A link to this page with ?session= (written in full) *)
  show (own ^ "/?session=6cad9f6e0123abcd#x");
  show (own ^ "/?theme=dark&session=6cad9f6e");
  (* Other pages and sites *)
  show (own ^ "/files/?session=6cad9f6e");
  show "https://example.com/?session=6cad9f6e";
  show (own ^ "/");
  [%expect {||}]
;;
