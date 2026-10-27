open! Core
open Prigh_web
module H = Harness

(* Trimming the context before one of the user's messages: its action on the
   message, the jump list's and [/trim]'s, the confirmation, the divider the
   transcript then starts with (Show, Undo), the rail's mark. *)

let base_ms = 1791194400000.

let user ?(at = 0) text =
  sprintf
    {|{"role":"user","text":"%s","at":%.0f}|}
    text
    (base_ms +. Float.of_int at)
;;

let assistant text =
  sprintf
    {|{"role":"assistant","content":[{"type":"text","text":"%s"}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
    text
;;

(* Exchange [n] is messages [2(n-1)] and [2n-1]. *)
let exchange n =
  [ user ~at:(n * 60_000) (sprintf "question %d" n)
  ; assistant (sprintf "answer %d" n)
  ]
;;

let exchanges ns =
  "[" ^ String.concat ~sep:"," (List.concat_map ns ~f:exchange) ^ "]"
;;

(* What [get_trim_points] says of exchanges [first..last] in the context: a
   point at each message but the first. *)
let trim_points ?(first = 1) last =
  sprintf
    {|{"points":[%s]}|}
    (String.concat
       ~sep:","
       (List.concat_map
          (List.range first (last + 1))
          ~f:(fun n ->
            let index = 2 * (n - first) in
            (if index = 0
             then []
             else
               [ sprintf
                   {|{"before":"u%d","index":%d,"role":"user","text":"question %d","at":%.0f,"messages":%d,"tokens":%d}|}
                   n
                   index
                   n
                   (base_ms +. Float.of_int (n * 60_000))
                   index
                   (index * 9_500)
               ])
            @ [ sprintf
                  {|{"before":"a%d","index":%d,"role":"assistant","text":"answer %d","messages":%d,"tokens":%d}|}
                  n
                  (index + 1)
                  n
                  (index + 1)
                  ((index + 1) * 9_500)
              ])))
;;

(* What [get_message_points] says of exchanges [first..last] in the
   context: a trim before each message but the first. *)
let message_points ?(first = 1) last =
  sprintf
    {|{"points":[%s]}|}
    (String.concat
       ~sep:","
       (List.concat_map
          (List.range first (last + 1))
          ~f:(fun n ->
            let index = 2 * (n - first) in
            let trim_before index =
              if index = 0
              then ""
              else
                sprintf
                  {|,"trim_before":{"messages":%d,"tokens":%d}|}
                  index
                  (index * 9_500)
            in
            [ sprintf
                {|{"message":"u%d","index":%d,"role":"user","text":"question %d","at":%.0f%s}|}
                n
                index
                n
                (base_ms +. Float.of_int (n * 60_000))
                (trim_before index)
            ; sprintf
                {|{"message":"a%d","index":%d,"role":"assistant","text":"answer %d"%s}|}
                n
                (index + 1)
                n
                (trim_before (index + 1))
            ])))
;;

let trim_json ~before ~messages ~tokens =
  sprintf
    {|{"before":"%s","messages":%d,"tokens":%d,"at":%.0f}|}
    before
    messages
    tokens
    (base_ms +. 600_000.)
;;

let state ?trim ?(running = false) () =
  H.state_json
    ~fields:
      ([ ("running", if running then `True else `False) ]
       @ Option.value_map trim ~default:[] ~f:(fun trim ->
         [ "trim", Jsonaf.of_string trim ]))
    ()
;;

let state_event ?trim ?running h =
  H.event h (sprintf {|{"event":"state","state":%s}|} (state ?trim ?running ()))
;;

(* The newest page of session s1, as when it opens. *)
let newest ?older ?before h messages =
  H.quiet h (fun () ->
    H.act
      h
      (Reply
         (H.newest "s1", Ok (Jsonaf.of_string (H.page ?older ?before messages)))))
;;

(* Session s1 with exchanges 1 to 3, all loaded. *)
let session () =
  let h = H.create () in
  newest h (exchanges [ 1; 2; 3 ]);
  h
;;

let dialog h = H.text h ~selector:".modal"
let toasts h = H.text h ~selector:".toast"
let divider h = H.text h ~selector:".trim-divider"

let%expect_test "every message but the first has the row of actions" =
  let h = session () in
  H.show h ~selector:".msg.user";
  [%expect
    {|
    <div data-entry="0" data-message-entry="0" tabindex="-1" class="msg user">
      <div dir="auto" class="bubble"> question 1 </div>
      <time title="Monday 5 October 2026, 10:01:00" datetime="2026-10-05T10:01:00Z" class="time"> 10:01 </time>
      <div class="msg-actions">
        <button type="button"
                title="Trim everything after this message from the model's context"
                aria-label="Trim everything after this"
                data-trim-after-entry="0"
                class="trim-after">
          <span aria-hidden="true" class="glyph"> ✂ </span>
          Trim after
        </button>
        <button type="button"
                title="Delete this message from the transcript and the model's context"
                aria-label="Delete"
                data-delete-entry="0"
                class="msg-delete">
          <span aria-hidden="true" class="glyph"> ✕ </span>
          Delete
        </button>
      </div>
    </div>
    <div data-entry="2" data-message-entry="2" tabindex="-1" class="msg user">
      <div dir="auto" class="bubble"> question 2 </div>
      <time title="Monday 5 October 2026, 10:02:00" datetime="2026-10-05T10:02:00Z" class="time"> 10:02 </time>
      <div class="msg-actions">
        <button type="button"
                title="Trim everything before this message from the model's context"
                aria-label="Trim everything before this"
                data-trim-entry="2"
                class="trim-before">
          <span aria-hidden="true" class="glyph"> ✂ </span>
          Trim before
        </button>
        <button type="button"
                title="Trim everything after this message from the model's context"
                aria-label="Trim everything after this"
                data-trim-after-entry="2"
                class="trim-after">
          <span aria-hidden="true" class="glyph"> ✂ </span>
          Trim after
        </button>
        <button type="button"
                title="Delete this message from the transcript and the model's context"
                aria-label="Delete"
                data-delete-entry="2"
                class="msg-delete">
          <span aria-hidden="true" class="glyph"> ✕ </span>
          Delete
        </button>
      </div>
    </div>
    <div data-entry="4" data-message-entry="4" tabindex="-1" class="msg user">
      <div dir="auto" class="bubble"> question 3 </div>
      <time title="Monday 5 October 2026, 10:03:00" datetime="2026-10-05T10:03:00Z" class="time"> 10:03 </time>
      <div class="msg-actions">
        <button type="button"
                title="Trim everything before this message from the model's context"
                aria-label="Trim everything before this"
                data-trim-entry="4"
                class="trim-before">
          <span aria-hidden="true" class="glyph"> ✂ </span>
          Trim before
        </button>
        <button type="button"
                title="Trim everything after this message from the model's context"
                aria-label="Trim everything after this"
                data-trim-after-entry="4"
                class="trim-after">
          <span aria-hidden="true" class="glyph"> ✂ </span>
          Trim after
        </button>
        <button type="button"
                title="Delete this message from the transcript and the model's context"
                aria-label="Delete"
                data-delete-entry="4"
                class="msg-delete">
          <span aria-hidden="true" class="glyph"> ✕ </span>
          Delete
        </button>
      </div>
    </div>
    |}]
;;

let%expect_test
    "trim before a message: confirmed, then the transcript starts there"
  =
  let h = session () in
  H.act h (Ask_trim (Entry 2));
  dialog h;
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag (Message_points (Trim_before (index 2) (at ("2026-10-05 10:02:00Z"))))))
    Trim the context
    (Close (Esc))
    Loading…
    Esc closes
    |}];
  H.reply h "get_message_points" (message_points 3);
  dialog h;
  [%expect
    {|
    (Focus dialog)
    Trim the context here?
    (Close (Esc))
    Trim 2 messages (~19k tokens) before this message?
    question 2 10:02
    They leave the model's context; the transcript keeps them folded, and Undo brings them back.
    Enter trims, Esc cancels (Cancel) (Trim)
    |}];
  (* Esc: nothing is sent. *)
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  H.quiet h (fun () ->
    H.act h (Ask_trim (Entry 2));
    H.reply h "get_message_points" (message_points 3));
  (* Enter trims; the dialog says so until the backend answers. *)
  H.key h "Enter" ~target:Page;
  dialog h;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ trim) (params ((before u2))) (tag Trimmed))
    Trim the context here?
    (Close (Esc))
    Trim 2 messages (~19k tokens) before this message?
    question 2 10:02
    They leave the model's context; the transcript keeps them folded, and Undo brings them back.
    Enter trims, Esc cancels (Cancel) (Trimming…)
    |}];
  (* The backend's state says what is trimmed: the transcript, which is the
     context, again, showing where it now starts. *)
  state_event h ~trim:(trim_json ~before:"u2" ~messages:2 ~tokens:19_000);
  H.reply h "trim" {|{"messages":2,"tokens":19000}|};
  toasts h;
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 3740))
    Trimmed 2 messages (~19k tokens): Undo is on the divider
    |}];
  H.reply h "get_messages_page" (H.page (exchanges [ 2; 3 ]));
  H.text h ~selector:"#chat";
  [%expect
    {|
    Scroll_to_bottom
    Scroll_to_trimmed
    ✂ 2 messages trimmed · ~19k tokens (Show) · (Undo)
    question 2
    10:02
    (✂ Trim after) (✕ Delete)
    answer 2
    (✂ Trim before) (✂ Trim after) (✕ Delete)
    question 3
    10:03
    (✂ Trim before) (✂ Trim after) (✕ Delete)
    answer 3
    (✂ Trim before) (✕ Delete)
    |}];
  (* The new first message has nothing before it to trim. *)
  H.act h (Ask_trim (Entry 0));
  [%expect {| (Expire_toast (id 1) (after_ms 4900)) |}];
  toasts h;
  [%expect
    {|
    Trimmed 2 messages (~19k tokens): Undo is on the divider
    That is the first message in the model's context: there is nothing before it to trim.
    |}]
;;

(* Session s1, trimmed before exchange 2: exchanges 2 and 3 loaded. *)
let trimmed () =
  let h =
    H.create
      ~state:
        (state ~trim:(trim_json ~before:"u2" ~messages:2 ~tokens:19_000) ())
      ()
  in
  newest h (exchanges [ 2; 3 ]);
  h
;;

let%expect_test
    "the divider: Show the trimmed messages, read-only, and Hide them"
  =
  let h = trimmed () in
  H.names h ~selector:".trim-divider";
  H.names h ~selector:".trim-divider button";
  [%expect
    {|
    [group] div "2 messages trimmed \194\183 ~19k tokens"
    button "Show" -- Show the trimmed messages above, read-only
    button "Undo" -- Bring them back into the model's context (/trim undo)
    |}];
  H.act h (Show_trimmed true);
  divider h;
  [%expect
    {|
    (Rpc (method_ get_trimmed) (params ())
     (tag (Trimmed_messages /sessions/s1.jsonl)))
    ✂ 2 messages trimmed · ~19k tokens Loading… · (Undo)
    |}];
  H.reply h "get_trimmed" (sprintf {|{"messages":%s}|} (exchanges [ 1 ]));
  H.text h ~selector:"#chat";
  [%expect
    {|
    Keep_scroll_position
    Not in the model's context Kept in the transcript only; the model no longer sees them.
    question 1
    10:01
    answer 1
    ✂ 2 messages trimmed · ~19k tokens (Hide) · (Undo)
    question 2
    10:02
    (✂ Trim after) (✕ Delete)
    answer 2
    (✂ Trim before) (✂ Trim after) (✕ Delete)
    question 3
    10:03
    (✂ Trim before) (✂ Trim after) (✕ Delete)
    answer 3
    (✂ Trim before) (✕ Delete)
    |}];
  (* Read-only: no actions, no landmarks of the transcript. *)
  H.show h ~selector:".trimmed-messages .msg.user";
  [%expect
    {|
    <div data-entry="0" class="msg user">
      <div dir="auto" class="bubble"> question 1 </div>
      <time title="Monday 5 October 2026, 10:01:00" datetime="2026-10-05T10:01:00Z" class="time"> 10:01 </time>
    </div>
    |}];
  H.act h (Show_trimmed false);
  H.text h ~selector:".trimmed-messages";
  divider h;
  [%expect {| ✂ 2 messages trimmed · ~19k tokens (Show) · (Undo) |}];
  (* Another session: folded there. *)
  H.quiet h (fun () ->
    H.act h (Show_trimmed true);
    H.reply h "get_trimmed" (sprintf {|{"messages":%s}|} (exchanges [ 1 ]));
    H.act h (Switch_session "/sessions/s2.jsonl");
    H.reply h "switch_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s2" ] ()));
  print_s [%sexp ((H.model h).trimmed : Trimmed.t)];
  [%expect
    {|
    ((shown Folded) (restoring false) (reveal ()) (ranges ())
     (restoring_ranges ()) (deleting ()) (reveal_hidden ()))
    |}]
;;

let%expect_test
    "Undo: restoring until the backend answers, then the messages are back"
  =
  let h = trimmed () in
  H.act h Untrim;
  divider h;
  [%expect
    {|
    (Rpc (method_ untrim) (params ()) (tag Untrimmed))
    ✂ 2 messages trimmed · ~19k tokens (Show) · Restoring…
    |}];
  (* Again while it is on its way: nothing more. *)
  H.act h Untrim;
  [%expect {| |}];
  state_event h;
  H.reply h "untrim" {|{"messages":2,"tokens":19000}|};
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    (Expire_toast (id 0) (after_ms 3300))
    |}];
  H.reply h "get_messages_page" (H.page (exchanges [ 1; 2; 3 ]));
  H.text h ~selector:"#chat";
  toasts h;
  [%expect
    {|
    Scroll_to_bottom
    question 1
    10:01
    (✂ Trim after) (✕ Delete)
    answer 1
    (✂ Trim before) (✂ Trim after) (✕ Delete)
    question 2
    10:02
    (✂ Trim before) (✂ Trim after) (✕ Delete)
    answer 2
    (✂ Trim before) (✂ Trim after) (✕ Delete)
    question 3
    10:03
    (✂ Trim before) (✂ Trim after) (✕ Delete)
    answer 3
    (✂ Trim before) (✕ Delete)
    Back in the context: 2 messages (~19k tokens)
    |}];
  (* Nothing trimmed: Undo says so. *)
  H.act h Untrim;
  toasts h;
  [%expect
    {|
    Back in the context: 2 messages (~19k tokens)
    Nothing is trimmed from the context: /trim picks a message to trim before. (Dismiss)
    |}]
;;

let%expect_test
    "while a run is in progress the backend refuses: said where asked"
  =
  let h = session () in
  H.quiet h (fun () ->
    H.act h (Ask_trim (Entry 4));
    H.reply h "get_message_points" (message_points 3);
    H.act h Dialog_accept);
  H.fail
    h
    "trim"
    "cannot trim while a run is in progress: wait for the run to end or abort \
     it (Esc)";
  dialog h;
  print_s [%sexp ((H.model h).trimmed.reveal : string option)];
  [%expect
    {|
    Trim the context here?
    (Close (Esc))
    Trim 4 messages (~38k tokens) before this message?
    question 3 10:03
    They leave the model's context; the transcript keeps them folded, and Undo brings them back.
    Cannot trim while a run is in progress: wait for the run to end or abort it (Esc)
    Enter trims, Esc cancels (Cancel) (Trim)
    ()
    |}];
  let h = trimmed () in
  H.act h Untrim;
  H.fail
    h
    "untrim"
    "cannot undo a trim while a run is in progress: wait for the run to end or \
     abort it (Esc)";
  divider h;
  toasts h;
  [%expect
    {|
    (Rpc (method_ untrim) (params ()) (tag Untrimmed))
    ✂ 2 messages trimmed · ~19k tokens (Show) · (Undo)
    Cannot undo a trim while a run is in progress: wait for the run to end or abort it (Esc) (Dismiss)
    |}]
;;

let%expect_test "/trim: a picker of where to trim, newest first; /trim undo" =
  let h = session () in
  H.type_ h "/trim";
  H.act h Send;
  H.reply h "get_trim_points" (trim_points 3);
  H.text h ~selector:".modal";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /trim) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_trim_points) (params ()) (tag (Trim_points Picker)))
    (Focus picker-input)
    Trim the context before
    (Close (Esc))
    []
    answer 3 reply  5 messages trimmed · ~48k tokens
    question 3 you  10:03  4 messages trimmed · ~38k tokens
    answer 2 reply  3 messages trimmed · ~29k tokens
    question 2 you  10:02  2 messages trimmed · ~19k tokens
    answer 1 reply  1 message trimmed · ~9.5k tokens
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.key h "Enter" ~target:Field;
  dialog h;
  [%expect
    {|
    Dialog_accept
    (Focus dialog)
    Trim the context here?
    (Close (Esc))
    Trim 5 messages (~48k tokens) before this reply?
    answer 3
    They leave the model's context; the transcript keeps them folded, and Undo brings them back.
    Enter trims, Esc cancels (Cancel) (Trim)
    |}];
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ trim) (params ((before a3))) (tag Trimmed))
    |}];
  (* With one message, nothing to trim before. *)
  let h = H.create () in
  newest h (exchanges [ 1 ]);
  H.quiet h (fun () ->
    H.type_ h "/trim";
    H.act h Send);
  H.reply h "get_trim_points" {|{"points":[]}|};
  toasts h;
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 5620))
    (Focus editor)
    Nothing to trim: the context has no message after its first. /trim drops what comes before a later one.
    |}];
  let h = trimmed () in
  H.type_ h "/trim undo";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/trim undo") (max 500))) (tag Ignore))
    (Rpc (method_ untrim) (params ()) (tag Untrimmed))
    |}];
  (* Its completion. *)
  H.type_ h "/trim ";
  H.text h ~selector:".popup";
  [%expect
    {|
    Trim ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    after pick a message to trim everything after
    undo bring back what the last trim took out of the context
    |}]
;;

let%expect_test "a message no longer in the context is not trimmed before" =
  let h = session () in
  H.quiet h (fun () -> H.act h (Ask_trim (Entry 2)));
  (* The backend has another message there (the transcript changed). *)
  H.reply h "get_message_points" (message_points ~first:2 4);
  toasts h;
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect
    {|
    (Focus editor)
    That message is not in the model's context any more: /trim lists those you can trim before. (Dismiss)
    false
    |}]
;;

let rail h =
  H.names h ~selector:".rail .tick";
  H.elements h ~selector:".rail .tick"
;;

let jump_list h =
  H.quiet h (fun () -> H.act h Open_jump_list);
  H.text h ~selector:".picker-items";
  H.quiet h (fun () -> H.act h Close_dialog)
;;

let%expect_test "the rail and the jump list: one mark for what is trimmed" =
  let h = session () in
  rail h;
  jump_list h;
  [%expect
    {|
    button "Your message 1, question 1, 10:01"
    button "Your message 2, question 2, 10:02"
    button "Your message 3, question 3, 10:03"
    <button class="tick user">
    <button class="tick user">
    <button class="tick user">
    question 3 3 of 3  10:03 (Trim before)
    question 2 2 of 3  10:02 (Trim before)
    question 1 1 of 3  10:01
    |}];
  H.quiet h (fun () ->
    state_event h ~trim:(trim_json ~before:"u2" ~messages:2 ~tokens:19_000);
    H.reply h "get_messages_page" (H.page (exchanges [ 2; 3 ])));
  rail h;
  jump_list h;
  [%expect
    {|
    button "Not in the model's context, 2 messages trimmed \194\183 ~19k tokens, 10:10"
    button "Your message 1, question 2, 10:02"
    button "Your message 2, question 3, 10:03"
    <button class="tick trimmed">
    <button class="tick user">
    <button class="tick user">
    question 3 2 of 2  10:03 (Trim before)
    question 2 1 of 2  10:02
    Trimmed: 2 messages · ~19k tokens out of the model's context
    |}];
  (* The mark and the list's row go to the divider. *)
  H.click h ~selector:".rail .tick.trimmed";
  [%expect
    {|
    (Jump_to Trimmed)
    Scroll_to_trimmed
    |}];
  H.quiet h (fun () -> H.act h Open_jump_list);
  H.key h "ArrowDown" ~target:Field;
  H.key h "ArrowDown" ~target:Field;
  H.key h "Enter" ~target:Field;
  [%expect
    {|
    (Dialog_move 1)
    (Dialog_move 1)
    Dialog_accept
    (Focus editor)
    Scroll_to_trimmed
    |}];
  (* Delete on one of the user's messages: trim before it. *)
  H.quiet h (fun () -> H.act h Open_jump_list);
  H.key h "Delete" ~target:Field;
  [%expect
    {|
    (Ask_trim (Entry 2))
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag (Message_points (Trim_before (index 2) (at ("2026-10-05 10:03:00Z"))))))
    |}];
  H.quiet h (fun () ->
    H.act h Close_dialog;
    state_event h;
    H.reply h "get_messages_page" (H.page (exchanges [ 1; 2; 3 ])));
  rail h;
  [%expect
    {|
    button "Your message 1, question 1, 10:01"
    button "Your message 2, question 2, 10:02"
    button "Your message 3, question 3, 10:03"
    <button class="tick user">
    <button class="tick user">
    <button class="tick user">
    |}]
;;

let%expect_test "jumping to the divider loads the pages down to it" =
  let h =
    H.create
      ~state:
        (state ~trim:(trim_json ~before:"u2" ~messages:2 ~tokens:19_000) ())
      ()
  in
  newest h ~older:4 ~before:"e5" (exchanges [ 4; 5 ]);
  (* Not loaded down to the start: no divider yet. *)
  H.text h ~selector:".trim-divider";
  H.act h (Jump_to Trimmed);
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (before e5) (limit 4)))
     (tag (Page (path /sessions/s1.jsonl) (before (e5)))))
    |}];
  H.reply h "get_messages_page" (H.page (exchanges [ 2; 3 ]));
  divider h;
  [%expect
    {|
    Keep_scroll_position
    Scroll_to_trimmed
    ✂ 2 messages trimmed · ~19k tokens (Show) · (Undo)
    |}]
;;

let%expect_test
    "trim before a reply: the transcript starts at it; Undo's toast replaces \
     the trim's"
  =
  let h = session () in
  H.act h (Ask_trim (Entry 3));
  H.reply h "get_message_points" (message_points 3);
  dialog h;
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag (Message_points (Trim_before (index 3) (at ())))))
    (Focus dialog)
    Trim the context here?
    (Close (Esc))
    Trim 3 messages (~29k tokens) before this reply?
    answer 2
    They leave the model's context; the transcript keeps them folded, and Undo brings them back.
    Enter trims, Esc cancels (Cancel) (Trim)
    |}];
  H.key h "Enter" ~target:Page;
  state_event h ~trim:(trim_json ~before:"a2" ~messages:3 ~tokens:28_500);
  H.reply h "trim" {|{"messages":3,"tokens":28500}|};
  H.reply
    h
    "get_messages_page"
    (H.page
       ("["
        ^ String.concat
            ~sep:","
            (assistant "answer 2" :: List.concat_map [ 3 ] ~f:exchange)
        ^ "]"));
  H.text h ~selector:"#chat";
  toasts h;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ trim) (params ((before a2))) (tag Trimmed))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    (Focus editor)
    (Expire_toast (id 0) (after_ms 3740))
    Scroll_to_bottom
    Scroll_to_trimmed
    ✂ 3 messages trimmed · ~29k tokens (Show) · (Undo)
    answer 2
    (✂ Trim after) (✕ Delete)
    question 3
    10:03
    (✂ Trim before) (✂ Trim after) (✕ Delete)
    answer 3
    (✂ Trim before) (✕ Delete)
    Trimmed 3 messages (~29k tokens): Undo is on the divider
    |}];
  jump_list h;
  [%expect
    {|
    question 3 1 of 1  10:03 (Trim before)
    Trimmed: 3 messages · ~29k tokens out of the model's context
    |}];
  H.act h Untrim;
  state_event h;
  H.reply h "untrim" {|{"messages":3,"tokens":28500}|};
  toasts h;
  [%expect
    {|
    (Rpc (method_ untrim) (params ()) (tag Untrimmed))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    (Expire_toast (id 1) (after_ms 3300))
    Back in the context: 3 messages (~29k tokens)
    |}]
;;
