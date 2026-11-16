open! Core
open! Expect_test_helpers_core
open Prigh_ui
open Fixtures
module H = Test_app.H
module P = Prigh_protocol

(* Long by its lines (over [Pasted_text.long_lines]), short to print. *)
let long_text =
  String.concat ~sep:"\n" (List.init 41 ~f:(fun i -> Int.to_string (i % 10)))
;;

let connected ?(features = [ "pasted_text" ]) () =
  let h = Test_app.connected ~width:70 ~height:12 () in
  H.step
    ~quiet:true
    h
    (Hello
       { client_id = "c1"
       ; namespace = None
       ; user = None
       ; superuser = false
       ; features
       });
  h
;;

let paste h text = H.step h (Intent (Paste text))
let editor h = printf "%S\n" (Editor.text h.H.model.editor)

let%expect_test "a long paste is a token; it goes as a pasted text, in full" =
  let h = connected () in
  H.keys h "why? ";
  paste h long_text;
  editor h;
  H.show h;
  [%expect
    {|
    "why? [Pasted text #1 +41 lines]"





      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ──────────────────────────────────────────────────────────────────────
    > why? [Pasted text #1 +41 lines]▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params (
        (text "why? [Pasted text #1]")
        (pasted (
          "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0"))))
      (tag Show_error))
    (Append_history "why? [Pasted text #1 +41 lines]")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "why? [Pasted text #1 +41 lines]")
        (max   500)))
      (tag Ignore))
    |}]
;;

let%expect_test "short pastes go into the editor as before" =
  let h = connected () in
  paste h "l1\nl2\nl3\nl4";
  editor h;
  paste h (String.make 2000 'x');
  print_s [%sexp (String.length (Editor.text h.H.model.editor) : int)];
  [%expect
    {|
    "l1\nl2\nl3\nl4"
    2011
    |}]
;;

let%expect_test "only a paste: nothing typed is sent; two pastes are numbered" =
  let h = connected () in
  paste h long_text;
  paste h (String.make 2001 'y');
  print_s
    [%sexp
      (List.map
         (Editor.pasted h.H.model.editor)
         ~f:(fun (t : Pasted_text.Token.t) -> t.token, String.length t.text)
       : (string * int) list)];
  H.key h (Key.plain Backspace);
  paste
    h
    "y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y\n\
     y";
  editor h;
  [%expect
    {|
    (("[Pasted text #2 +1 line]"   2001)
     ("[Pasted text #1 +41 lines]" 81))
    "[Pasted text #1 +41 lines][Pasted text #3 +42 lines]"
    |}];
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params (
        (text "")
        (pasted (
          "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0"
          "y\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny\ny"))))
      (tag Show_error))
    (Append_history "[Pasted text #1 +41 lines][Pasted text #3 +42 lines]")
    (Rpc
      (method_ push_pref)
      (params (
        (key history)
        (value "[Pasted text #1 +41 lines][Pasted text #3 +42 lines]")
        (max 500)))
      (tag Ignore))
    |}]
;;

let%expect_test "Backspace and Delete remove a token whole; it is not sent" =
  let h = connected () in
  paste h long_text;
  H.keys h "!";
  H.key h (Key.plain Backspace);
  H.key h (Key.plain Backspace);
  editor h;
  paste h long_text;
  H.key h (Key.plain Home);
  H.key h (Key.plain Delete);
  editor h;
  H.keys h "done";
  H.enter h;
  [%expect
    {|
    ""
    ""
    (Rpc (method_ prompt) (params ((text done))) (tag Show_error))
    (Append_history done)
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value done)
        (max   500)))
      (tag Ignore))
    |}]
;;

let%expect_test "an older backend gets the pasted text inline" =
  let h = connected ~features:[] () in
  H.keys h "see ";
  paste h "a\nb\nc\n";
  paste h long_text;
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params ((
        text
        "see a\nb\nc\n[Pasted text #1]\n\n<pasted_text name=\"Pasted text #1\" lines=\"41\">\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n</pasted_text>")))
      (tag Show_error))
    (Append_history "see a\nb\nc\n[Pasted text #1 +41 lines]")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "see a\nb\nc\n[Pasted text #1 +41 lines]")
        (max   500)))
      (tag Ignore))
    |}]
;;

let%expect_test "a paste over the limit is refused, saying what to do" =
  let h = connected () in
  paste h (String.make (Pasted_text.max_bytes + 1) 'z');
  editor h;
  H.show h;
  [%expect
    {|
    ""

      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer
    ┌─ Notice ───────────────────────────────────────────────────────────┐
    │ That paste is 2,097,153 bytes, over the 2.0 MB (2,097,152 bytes) a │
    │ pasted text may be: save it to a file and attach that instead.     │
    └──────────────────────────────────────────────────── Esc dismisses ─┘
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test "history and Esc's restore bring pasted texts back" =
  let h = connected () in
  H.keys h "fix ";
  paste h long_text;
  H.enter h;
  [%expect
    {|
    (Rpc
      (method_ prompt)
      (params (
        (text "fix [Pasted text #1]")
        (pasted (
          "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0"))))
      (tag Show_error))
    (Append_history "fix [Pasted text #1 +41 lines]")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "fix [Pasted text #1 +41 lines]")
        (max   500)))
      (tag Ignore))
    |}];
  H.key h (Key.plain Up);
  editor h;
  H.enter h;
  [%expect
    {|
    "fix [Pasted text #1 +41 lines]"
    (Rpc
      (method_ prompt)
      (params (
        (text "fix [Pasted text #1]")
        (pasted (
          "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0"))))
      (tag Show_error))
    (Append_history "fix [Pasted text #1 +41 lines]")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "fix [Pasted text #1 +41 lines]")
        (max   500)))
      (tag Ignore))
    |}];
  (* Given back by an abort: tokens again, where the text named them. *)
  H.reply
    h
    Abort_done
    (sprintf
       {|{"restored":["fix [Pasted text #1]","b"],"pasted":[[%s],[]]}|}
       (P.Json.to_string (P.Json.str "p\nq")));
  editor h;
  H.enter h;
  [%expect
    {|
    "fix [Pasted text #2 +2 lines]\n\nb"
    (Rpc
      (method_ prompt)
      (params ((text "fix [Pasted text #1]\n\nb") (pasted ("p\nq"))))
      (tag Show_error))
    (Append_history "fix [Pasted text #2 +2 lines]\n\nb")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "fix [Pasted text #2 +2 lines]\n\nb")
        (max   500)))
      (tag Ignore))
    |}]
;;

let%expect_test "the transcript names pasted texts rather than showing them" =
  let h = connected () in
  let pasted, block =
    ( long_text
    , sprintf
        "<pasted_text name=\"Pasted text #1\" lines=\"41\">\n%s\n</pasted_text>"
        long_text )
  in
  let text = "fix this\n\n" ^ block in
  let origin : P.Origin.t =
    { P.Origin.empty with
      body = Some "fix this"
    ; pasted = [ { name = "Pasted text #1"; lines = 41; text = pasted } ]
    }
  in
  H.event h (Message_start (user ~origin text));
  H.event h (Message_end (user ~origin text));
  H.show h;
  [%expect
    {|
      prigh in /work · /help · Esc aborts · Ctrl+C twice quits

    ▌ earlier question
      earlier answer

    ▌ fix this
    ▌ Pasted text #1  41 lines · 81 B  pasted
    ──────────────────────────────────────────────────────────────────────
    > ▏
    /work  deepseek-flash  think:off  mode:auto  ctx:0.1%/1.0M  $0.01
    |}]
;;

let%expect_test
    "Ctrl+Z undoes a long paste's token going in or out; Ctrl+Y redoes"
  =
  let h = connected () in
  let ctrl c = H.key h (Key.ctrl c) in
  H.keys h "why? ";
  paste h long_text;
  editor h;
  ctrl 'z';
  editor h;
  ctrl 'y';
  editor h;
  print_endline "--- Backspace takes the token out; Ctrl+Z puts it back";
  H.key h (Key.plain Backspace);
  editor h;
  ctrl 'z';
  editor h;
  ctrl 'y';
  editor h;
  ctrl 'z';
  editor h;
  print_endline "--- typed text: Ctrl+Z suspends, Ctrl+Y yanks as before";
  H.keys h "ok";
  ctrl 'z';
  editor h;
  ctrl 'w';
  ctrl 'y';
  editor h;
  [%expect
    {|
    "why? [Pasted text #1 +41 lines]"
    "why? "
    "why? [Pasted text #1 +41 lines]"
    --- Backspace takes the token out; Ctrl+Z puts it back
    "why? "
    "why? [Pasted text #1 +41 lines]"
    "why? "
    "why? [Pasted text #1 +41 lines]"
    --- typed text: Ctrl+Z suspends, Ctrl+Y yanks as before
    Suspend
    "why? [Pasted text #1 +41 lines]ok"
    "why? [Pasted text #1 +41 lines]ok"
    |}]
;;

(* A user message as the backend has it: what was typed, then each pasted
   text's block, with the origin saying where the texts are. *)
let message_with_pasted ~id ~parent ~body pasted =
  let text, spans =
    List.foldi pasted ~init:(body, []) ~f:(fun i (text, spans) p ->
      let header =
        sprintf
          "\n\n<pasted_text name=\"Pasted text #%d\" lines=\"%d\">\n"
          (i + 1)
          (Pasted_text.count_lines p)
      in
      let start = String.length text + String.length header in
      ( text ^ header ^ p ^ "\n</pasted_text>"
      , `Object
          [ "name", `String (sprintf "Pasted text #%d" (i + 1))
          ; "lines", `Number (Int.to_string (Pasted_text.count_lines p))
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

let%expect_test "/fork gives a message's pasted texts back as tokens" =
  let h = connected () in
  let entries =
    `Object
      [ ( "entries"
        , `Array
            [ message_with_pasted
                ~id:"u1"
                ~parent:`Null
                ~body:"why? [Pasted text #1]"
                [ long_text ]
            ; message_with_pasted
                ~id:"u2"
                ~parent:(`String "u1")
                ~body:"and these?"
                [ long_text ^ "\nmore"; long_text ]
            ] )
      ]
    |> Jsonaf.to_string
  in
  H.keys h "/fork";
  H.enter h;
  H.reply ~quiet:true h Entries_for_fork entries;
  H.key h (Key.plain Up);
  H.enter h;
  editor h;
  H.enter h;
  [%expect
    {|
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_fork))
    (Rpc (method_ fork) (params ((at u1))) (tag Reload_messages))
    "why? [Pasted text #1 +41 lines]"
    (Rpc
      (method_ prompt)
      (params (
        (text "why? [Pasted text #1]")
        (pasted (
          "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n0"))))
      (tag Show_error))
    (Append_history "why? [Pasted text #1 +41 lines]")
    (Rpc
      (method_ push_pref)
      (params (
        (key   history)
        (value "why? [Pasted text #1 +41 lines]")
        (max   500)))
      (tag Ignore))
    |}];
  print_endline "--- the other: its texts after what was typed";
  H.keys h "/fork";
  H.enter h;
  H.reply ~quiet:true h Entries_for_fork entries;
  H.step ~quiet:true h (Key (Key.plain Enter));
  editor h;
  [%expect
    {|
    --- the other: its texts after what was typed
    (Rpc (method_ get_entries) (params ()) (tag Entries_for_fork))
    "and these? [Pasted text #2 +42 lines] [Pasted text #3 +41 lines]"
    |}]
;;
