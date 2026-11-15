open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H

(* Links to sessions in the transcript: the model's [title](?session=ID) is
   the title underlined in magenta and the short id, dim (no OSC 8: the link
   is a page's, relative); the ids of the user's sessions are underlined in
   magenta. [/switch PREFIX] goes to one. *)

let sessions =
  {|[{"id":"6cad9f6e0123abcd","path":"/home/u/.prigh/sessions/6cad9f6e0123abcd.jsonl","name":"Fix the taskbar entry","cwd":"/work","created_at":"2026-11-10T10:00:00Z","first_prompt":"the taskbar entry is blank","message_count":4},{"id":"7b1e2d3c4f5a6b7c","path":"/home/u/.prigh/sessions/7b1e2d3c4f5a6b7c.jsonl","cwd":"/work","created_at":"2026-11-11T10:00:00Z","first_prompt":"fix the parser","message_count":2},{"id":"7b1e2d3c99999999","path":"/home/u/.prigh/sessions/7b1e2d3c99999999.jsonl","cwd":"/work","created_at":"2026-11-12T10:00:00Z","first_prompt":"another","message_count":2}]|}
;;

let reply =
  "See [Fix the taskbar entry](?session=6cad9f6e0123abcd) and [this \
   one](?session=0badc0de).\n\
   Done in 6cad9f6e, also `6cad9f6e0123abcd`; not 0badc0de, 7b1e2d3c or \
   x6cad9f6e.\n\
   ```\n\
   6cad9f6e in a code block\n\
   ```"
;;

let%expect_test "markdown: session links and the ids of known sessions" =
  let h = Test_app.connected () in
  H.reply ~quiet:true h Sessions_cache sessions;
  let known = h.H.model.known_sessions in
  print_endline (Content.to_styled (Markdown.render ~sessions:known reply));
  [%expect
    {|
    See [magenta][underline]Fix the taskbar entry[/][dim] 6cad9f6e[/] and [magenta][underline]this one[/][dim] 0badc0de[/].
    Done in [magenta][underline]6cad9f6e[/], also [magenta][underline]6cad9f6e0123abcd[/]; not 0badc0de, 7b1e2d3c or x6cad9f6e.
    [dim]──[/]
    [gray]6cad9f6e in a code block[/]
    |}];
  (* Before the session list arrives, links are still links; ids are text *)
  print_endline (Content.to_styled (Markdown.render reply));
  [%expect
    {|
    See [magenta][underline]Fix the taskbar entry[/][dim] 6cad9f6e[/] and [magenta][underline]this one[/][dim] 0badc0de[/].
    Done in 6cad9f6e, also [cyan]6cad9f6e0123abcd[/]; not 0badc0de, 7b1e2d3c or x6cad9f6e.
    [dim]──[/]
    [gray]6cad9f6e in a code block[/]
    |}]
;;

let%expect_test "the session list arriving highlights ids; /switch PREFIX" =
  let h = Test_app.connected ~height:12 () in
  H.event h (Message_start (user "which session fixed the taskbar?"));
  H.event h (Message_update (unnumbered (Text_delta reply)));
  H.event h (Message_end (assistant reply));
  H.show h;
  H.reply h Sessions_cache sessions;
  let transcript () =
    List.iter (Render.screen h.H.model).lines ~f:(fun line ->
      let plain = Content.Line.to_plain line in
      if String.is_substring plain ~substring:"6cad9f6e"
      then print_endline (Content.to_styled [ line ]))
  in
  transcript ();
  [%expect
    {|
    ▌ earlier question
      earlier answer

    ▌ which session fixed the taskbar?
      See Fix the taskbar entry 6cad9f6e and this one 0badc0de.
      Done in 6cad9f6e, also 6cad9f6e0123abcd; not 0badc0de,
      7b1e2d3c or x6cad9f6e.
      ──
      6cad9f6e in a code block
    ────────────────────────────────────────────────────────────
    > ▏
    deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
      See [magenta][underline]Fix the taskbar entry[/][dim] 6cad9f6e[/] and [magenta][underline]this one[/][dim] 0badc0de[/].
      Done in [magenta][underline]6cad9f6e[/], also [magenta][underline]6cad9f6e0123abcd[/]; not 0badc0de,
      7b1e2d3c or x6cad9f6e.
      [gray]6cad9f6e in a code block[/]
    |}];
  (* The backend takes a unique prefix of an id (test_rpc in the backend) *)
  H.keys h "/switch 6cad9f6e";
  H.esc h;
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ switch_session)
      (params ((path 6cad9f6e)))
      (tag (
        Retry_in_editor
        (command "/switch 6cad9f6e")
        (hint "/sessions lists them, or Tab after /switch completes")
        (then_ Reload_messages))))
    |}]
;;
