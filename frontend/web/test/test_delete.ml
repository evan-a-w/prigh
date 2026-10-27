open! Core
open Prigh_web
module H = Harness

(* Deleting a message and trimming the context after one: their actions on
   the messages, the acknowledgement and its Undo, the confirmation, the
   divider (Show, Undo), [/delete] and [/trim after], the jump list. *)

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

(* Exchange [n]: question [n] and answer [n]. *)
let exchanges ns =
  messages
    (List.concat_map ns ~f:(fun n ->
       [ user n; assistant (sprintf "answer %d" n) ]))
;;

(* A [get_message_points] point. *)
let point ?at ?delete ?trim_after ~message ~index ~role text =
  let freed name =
    Option.value_map ~default:"" ~f:(fun (messages, tokens) ->
      sprintf {|,"%s":{"messages":%d,"tokens":%d}|} name messages tokens)
  in
  sprintf
    {|{"message":"%s","index":%d,"role":"%s","text":"%s"%s%s%s}|}
    message
    index
    role
    text
    (Option.value_map at ~default:"" ~f:(fun n ->
       sprintf {|,"at":%.0f|} (at_ms n)))
    (freed "delete" delete)
    (freed "trim_after" trim_after)
;;

let points l = sprintf {|{"points":[%s]}|} (String.concat ~sep:"," l)

(* Exchanges [ns], from the context's start: messages [u<n>] and [a<n>]. *)
let exchange_points ns =
  let total = 2 * List.length ns in
  points
    (List.concat_mapi ns ~f:(fun i n ->
       let after index =
         Option.some_if
           (index < total - 1)
           (total - 1 - index, (total - 1 - index) * 1000)
       in
       [ point
           ~at:n
           ~message:(sprintf "u%d" n)
           ~index:(2 * i)
           ~role:"user"
           ~delete:(1, 500)
           ?trim_after:(after (2 * i))
           (sprintf "question %d" n)
       ; point
           ~message:(sprintf "a%d" n)
           ~index:((2 * i) + 1)
           ~role:"assistant"
           ~delete:(1, 500)
           ?trim_after:(after ((2 * i) + 1))
           (sprintf "answer %d" n)
       ]))
;;

let hidden ?divider ?through ~entry ~kind ~message ~messages ~tokens () =
  sprintf
    {|{"entry":"%s","kind":"%s","message":"%s","messages":%d,"tokens":%d,"at":%.0f%s%s}|}
    entry
    kind
    message
    messages
    tokens
    (base_ms +. 600_000.)
    (Option.value_map through ~default:"" ~f:(sprintf {|,"through":"%s"|}))
    (Option.value_map divider ~default:"" ~f:(sprintf {|,"divider":%d|}))
;;

let deleted_u2 =
  hidden ~entry:"d1" ~kind:"deleted" ~message:"u2" ~messages:1 ~tokens:500 ()
;;

(* Trimmed after question 2: answer 2 and exchange 3. *)
let after_u2 =
  hidden
    ~entry:"t1"
    ~kind:"trim_after"
    ~through:"a3"
    ~message:"u2"
    ~messages:3
    ~tokens:3_000
    ~divider:3
    ()
;;

let trim_json =
  sprintf
    {|{"before":"u2","messages":2,"tokens":19000,"at":%.0f}|}
    (base_ms +. 600_000.)
;;

let state ?trim ?(hidden = []) ?(running = false) () =
  H.state_json
    ~fields:
      ([ ("running", if running then `True else `False)
       ; "hidden", Jsonaf.of_string ("[" ^ String.concat ~sep:"," hidden ^ "]")
       ]
       @ Option.value_map trim ~default:[] ~f:(fun trim ->
         [ "trim", Jsonaf.of_string trim ]))
    ()
;;

let state_event ?trim ?hidden ?running h =
  H.event
    h
    (sprintf {|{"event":"state","state":%s}|} (state ?trim ?hidden ?running ()))
;;

let newest ?older h messages =
  H.quiet h (fun () ->
    H.act
      h
      (Reply (H.newest "s1", Ok (Jsonaf.of_string (H.page ?older messages)))))
;;

(* Session s1 with exchanges 1 to 3, all loaded. *)
let session ?hidden ?trim ?(ns = [ 1; 2; 3 ]) () =
  let h = H.create ~state:(state ?hidden ?trim ()) () in
  newest h (exchanges ns);
  h
;;

let chat h = H.text h ~selector:"#chat"
let dialog h = H.text h ~selector:".modal"
let toasts h = H.text h ~selector:".toast"

let jump_list h =
  H.quiet h (fun () -> H.act h Open_jump_list);
  H.text h ~selector:".picker-items";
  H.quiet h (fun () -> H.act h Close_dialog)
;;

(* A reply that ran a tool, then said it was done: entries 0 (the question),
   1 (the call, its result joined to it) and 2 (the answer). *)
let tool_session () =
  let h = H.create ~state:(state ()) () in
  newest
    h
    (messages
       [ user 1
       ; {|{"role":"assistant","content":[{"type":"text","text":"Listing."},{"type":"tool_call","id":"c1","name":"bash","arguments":"{\"command\":\"ls\"}"}],"stop_reason":{"type":"tool_use"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}|}
       ; {|{"role":"tool_result","tool_call_id":"c1","tool_name":"bash","text":"a b","is_error":false}|}
       ; assistant "done"
       ]);
  h
;;

let%expect_test
    "every message of the context has Delete; Trim after but the newest"
  =
  let h = session ~ns:[ 1 ] () in
  H.names h ~selector:".msg-actions button";
  [%expect
    {|
    button "\226\156\149 Delete" -- Delete this message from the transcript and the model's context
    button "\226\156\130 Trim everything after this" -- Trim everything after this from the model's context
    button "\226\156\149 Delete" -- Delete this reply from the transcript and the model's context
    |}];
  (* A tool call's Delete is its reply's, with all its calls and results;
     trimming after it keeps its results. *)
  let h = tool_session () in
  H.show h ~selector:".msg.assistant > .msg-actions";
  [%expect
    {|
    <div class="msg-actions">
      <button type="button"
              title="Delete this reply with its tool call and their results from the transcript and the model's context"
              data-delete-entry="1"
              class="msg-delete">
        <span aria-hidden="true" class="glyph"> ✕ </span>
        Delete
      </button>
      <button type="button"
              title="Trim everything after this (and its tool results) from the model's context"
              data-trim-after-entry="1"
              class="trim-after">
        <span aria-hidden="true" class="glyph"> ✂ </span>
        Trim everything after this
      </button>
    </div>
    <div class="msg-actions">
      <button type="button"
              title="Delete this reply from the transcript and the model's context"
              data-delete-entry="2"
              class="msg-delete">
        <span aria-hidden="true" class="glyph"> ✕ </span>
        Delete
      </button>
    </div>
    |}];
  H.text h ~selector:"#chat";
  [%expect
    {|
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    Listing.
    ✓ bash ls
    a b
    (✕ Delete) (✂ Trim everything after this)
    done
    (✕ Delete)
    |}]
;;

let%expect_test "Delete: gone at once, acknowledged, out of the jump list; Undo"
  =
  let h = session () in
  H.act h (Ask_delete_message 2);
  chat h;
  jump_list h;
  [%expect
    {|
    (Rpc (method_ get_message_points) (params ())
     (tag
      (Message_points (Delete (entry 2) (index 2) (at ("2026-10-05 10:02:00Z"))))))
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    answer 2
    (✕ Delete) (✂ Trim everything after this)
    question 3
    10:03 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 3
    (✕ Delete)
    question 3 2 of 2  10:03 (Trim before)
    question 1 1 of 2  10:01
    |}];
  H.reply h "get_message_points" (exchange_points [ 1; 2; 3 ]);
  [%expect
    {| (Rpc (method_ delete) (params ((message u2))) (tag (Deleted (entry (2))))) |}];
  H.reply h "delete" deleted_u2;
  toasts h;
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 8000))
    Message deleted (Undo)
    |}];
  (* The backend's state says what is deleted: the transcript again. *)
  state_event h ~hidden:[ deleted_u2 ];
  H.reply
    h
    "get_messages_page"
    (H.page
       (messages
          [ user 1
          ; assistant "answer 1"
          ; assistant "answer 2"
          ; user 3
          ; assistant "answer 3"
          ]));
  chat h;
  jump_list h;
  [%expect
    {|
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    Scroll_to_bottom
    (Scroll_to_message 1)
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    answer 2
    (✕ Delete) (✂ Trim everything after this)
    question 3
    10:03 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 3
    (✕ Delete)
    question 3 2 of 2  10:03 (Trim before)
    question 1 1 of 2  10:01
    |}];
  H.click h ~selector:".toast-undo";
  toasts h;
  [%expect
    {|
    (Restore_hidden
     ((entry d1) (kind Deleted) (message u2) (messages 1) (tokens 500)
      (at "2026-10-05 10:10:00Z")))
    (Rpc (method_ undelete) (params ((entry d1))) (tag (Range_restored d1)))
    |}];
  H.reply h "undelete" deleted_u2;
  state_event h;
  H.reply h "get_messages_page" (H.page (exchanges [ 1; 2; 3 ]));
  chat h;
  toasts h;
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 3260))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    Scroll_to_bottom
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    question 2
    10:02 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 2
    (✕ Delete) (✂ Trim everything after this)
    question 3
    10:03 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 3
    (✕ Delete)
    Back in the context: 1 message (~500 tokens)
    |}]
;;

let%expect_test "Delete on a tool call deletes its reply" =
  let h = tool_session () in
  H.act h (Ask_delete_message 1);
  chat h;
  [%expect
    {|
    (Rpc (method_ get_message_points) (params ())
     (tag (Message_points (Delete (entry 1) (index 1) (at ())))))
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    done
    (✕ Delete)
    |}];
  H.reply
    h
    "get_message_points"
    (points
       [ point
           ~message:"u1"
           ~index:0
           ~role:"user"
           ~delete:(1, 10)
           ~trim_after:(3, 900)
           "question 1"
       ; point
           ~message:"a1"
           ~index:1
           ~role:"assistant"
           ~delete:(2, 600)
           ~trim_after:(2, 800)
           "Listing."
       ; point
           ~message:"r1"
           ~index:2
           ~role:"tool_result"
           ~trim_after:(1, 100)
           "a b"
       ; point ~message:"a2" ~index:3 ~role:"assistant" ~delete:(1, 100) "done"
       ]);
  [%expect
    {| (Rpc (method_ delete) (params ((message a1))) (tag (Deleted (entry (1))))) |}];
  H.reply
    h
    "delete"
    (hidden
       ~entry:"d1"
       ~kind:"deleted"
       ~message:"a1"
       ~messages:2
       ~tokens:600
       ());
  toasts h;
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 8000))
    2 messages deleted (Undo)
    |}]
;;

let%expect_test "two deletions: a toast each, each Undo its own" =
  let h = session () in
  H.quiet h (fun () ->
    H.act h (Ask_delete_message 2);
    H.reply h "get_message_points" (exchange_points [ 1; 2; 3 ]);
    H.reply h "delete" deleted_u2;
    H.act h (Ask_delete_message 0);
    H.reply h "get_message_points" (exchange_points [ 1; 2; 3 ]);
    H.reply
      h
      "delete"
      (hidden ~entry:"d2" ~kind:"deleted" ~message:"u1" ~messages:1 ~tokens:500 ()));
  toasts h;
  [%expect {|
    Message deleted (Undo)
    Message deleted (Undo)
    |}];
  H.click h ~selector:".toast:first-child .toast-undo";
  toasts h;
  [%expect {|
    (Restore_hidden
     ((entry d1) (kind Deleted) (message u2) (messages 1) (tokens 500)
      (at "2026-10-05 10:10:00Z")))
    (Rpc (method_ undelete) (params ((entry d1))) (tag (Range_restored d1)))
    Message deleted (Undo)
    |}]
;;

let%expect_test "a failed or stale delete shows the message again" =
  let h = session () in
  H.quiet h (fun () ->
    H.act h (Ask_delete_message 2);
    H.reply h "get_message_points" (exchange_points [ 1; 2; 3 ]));
  H.fail
    h
    "delete"
    "cannot delete a message while a run is in progress: wait for the run to \
     end or abort it (Esc)";
  chat h;
  toasts h;
  [%expect
    {|
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    question 2
    10:02 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 2
    (✕ Delete) (✂ Trim everything after this)
    question 3
    10:03 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 3
    (✕ Delete)
    Cannot delete a message while a run is in progress: wait for the run to end or abort it (Esc) (Dismiss)
    |}];
  (* The backend has other messages there. *)
  let h = session () in
  H.quiet h (fun () -> H.act h (Ask_delete_message 2));
  H.reply h "get_message_points" (exchange_points [ 2; 3 ]);
  chat h;
  toasts h;
  [%expect
    {|
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    question 2
    10:02 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 2
    (✕ Delete) (✂ Trim everything after this)
    question 3
    10:03 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 3
    (✕ Delete)
    That message is not in the model's context any more: /delete lists those you can delete. (Dismiss)
    |}]
;;

let%expect_test "while a run is in progress, nothing is asked" =
  let h = session () in
  H.quiet h (fun () ->
    state_event h ~running:true ~hidden:[ deleted_u2; after_u2 ]);
  let refused f =
    H.quiet h f;
    H.text h ~selector:".toast:last-child"
  in
  refused (fun () -> H.act h (Ask_delete_message 2));
  refused (fun () -> H.act h (Ask_trim_after 2));
  refused (fun () ->
    H.type_ h "/delete undo";
    H.act h Send);
  refused (fun () -> H.act h (Restore_range "t1"));
  refused (fun () ->
    H.type_ h "/trim after";
    H.act h Send);
  (* Nothing hidden meanwhile, nothing asked. *)
  print_s [%sexp ((H.model h).trimmed.deleting : Int.Set.t)];
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect
    {|
    Cannot delete a message while a run is in progress: wait for the run to end or abort it (Esc). (Dismiss)
    Cannot trim while a run is in progress: wait for the run to end or abort it (Esc). (Dismiss)
    Cannot undo a deletion while a run is in progress: wait for the run to end or abort it (Esc). (Dismiss)
    Cannot undo a trim while a run is in progress: wait for the run to end or abort it (Esc). (Dismiss)
    Cannot trim while a run is in progress: wait for the run to end or abort it (Esc). (Dismiss)
    ()
    false
    |}]
;;

let%expect_test
    "Trim after: confirmed (Esc cancels, Enter trims), then a divider"
  =
  let h = session () in
  H.act h (Ask_trim_after 2);
  dialog h;
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag
      (Message_points
       (Trim_after (index 2) (last 2) (at ("2026-10-05 10:02:00Z"))))))
    Trim the context after
    (Close (Esc))
    Loading…
    Esc closes
    |}];
  H.reply h "get_message_points" (exchange_points [ 1; 2; 3 ]);
  dialog h;
  [%expect
    {|
    (Focus dialog)
    Trim the context after this?
    (Close (Esc))
    Trim 3 messages (~3.0k tokens) after this message?
    question 2 10:02
    They leave the model's context; the transcript keeps them folded where they were, what you send next goes after them, and Undo brings them back.
    Enter trims, Esc cancels (Cancel) (Trim)
    |}];
  H.key h "Escape" ~target:Page;
  [%expect
    {|
    Close_dialog
    (Focus editor)
    |}];
  H.quiet h (fun () ->
    H.act h (Ask_trim_after 2);
    H.reply h "get_message_points" (exchange_points [ 1; 2; 3 ]));
  H.key h "Enter" ~target:Page;
  dialog h;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ trim_after) (params ((after u2))) (tag Trimmed_after))
    Trim the context after this?
    (Close (Esc))
    Trim 3 messages (~3.0k tokens) after this message?
    question 2 10:02
    They leave the model's context; the transcript keeps them folded where they were, what you send next goes after them, and Undo brings them back.
    Enter trims, Esc cancels (Cancel) (Trimming…)
    |}];
  H.reply h "trim_after" after_u2;
  state_event h ~hidden:[ after_u2 ];
  toasts h;
  [%expect
    {|
    (Focus editor)
    (Expire_toast (id 0) (after_ms 5020))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    Trimmed 3 messages (~3.0k tokens) from the context: Undo on the divider brings them back
    |}];
  H.reply
    h
    "get_messages_page"
    (H.page (messages [ user 1; assistant "answer 1"; user 2 ]));
  chat h;
  jump_list h;
  [%expect
    {|
    Scroll_to_bottom
    (Scroll_to_hidden t1)
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    question 2
    10:02 (✂ Trim everything before this)
    (✕ Delete)
    ✂ 3 messages trimmed · ~3.0k tokens (Show) · (Undo)
    question 2 2 of 2  10:02 (Trim before)
    question 1 1 of 2  10:01
    |}]
;;

let trimmed_after () =
  let h = H.create ~state:(state ~hidden:[ after_u2 ] ()) () in
  newest
    h
    (messages
       [ user 1; assistant "answer 1"; user 2; user 4; assistant "answer 4" ]);
  h
;;

let%expect_test "the divider: Show the messages, read-only, Hide; Undo" =
  let h = trimmed_after () in
  (* What was sent after the trim is below the divider. *)
  chat h;
  jump_list h;
  H.names h ~selector:".trim-divider.after";
  H.names h ~selector:".trim-divider.after button";
  [%expect
    {|
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    question 2
    10:02 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    ✂ 3 messages trimmed · ~3.0k tokens (Show) · (Undo)
    question 4
    10:04 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 4
    (✕ Delete)
    question 4 3 of 3  10:04 (Trim before)
    question 2 2 of 3  10:02 (Trim before)
    question 1 1 of 3  10:01
    [group] div "3 messages trimmed \194\183 ~3.0k tokens"
    button "Show" -- Show the trimmed messages below, read-only
    button "Undo" -- Bring them back into the model's context
    |}];
  H.act h (Show_hidden { entry = "t1"; show = true });
  H.text h ~selector:".trim-divider.after";
  [%expect
    {|
    (Rpc (method_ get_trimmed) (params ((entry t1)))
     (tag (Hidden_messages (path /sessions/s1.jsonl) (entry t1))))
    ✂ 3 messages trimmed · ~3.0k tokens Loading… · (Undo)
    |}];
  H.reply
    h
    "get_trimmed"
    (sprintf
       {|{"messages":%s}|}
       (messages [ assistant "answer 2"; user 3; assistant "answer 3" ]));
  chat h;
  H.elements h ~selector:".trimmed-messages .msg-actions";
  [%expect
    {|
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    question 2
    10:02 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    ✂ 3 messages trimmed · ~3.0k tokens (Hide) · (Undo)
    Not in the model's context Kept in the transcript only; the model no longer sees them.
    answer 2
    question 3
    10:03
    answer 3
    question 4
    10:04 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 4
    (✕ Delete)
    |}];
  H.act h (Show_hidden { entry = "t1"; show = false });
  H.text h ~selector:".hidden-range";
  [%expect {| ✂ 3 messages trimmed · ~3.0k tokens (Show) · (Undo) |}];
  H.act h (Restore_range "t1");
  H.text h ~selector:".trim-divider.after";
  [%expect
    {|
    (Rpc (method_ untrim) (params ((entry t1))) (tag (Range_restored t1)))
    ✂ 3 messages trimmed · ~3.0k tokens (Show) · Restoring…
    |}];
  H.act h (Restore_range "t1");
  [%expect {| |}];
  H.reply h "untrim" after_u2;
  state_event h;
  H.reply h "get_messages_page" (H.page (exchanges [ 1; 2; 3 ]));
  chat h;
  toasts h;
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 3340))
    (Rpc (method_ get_messages_page)
     (params ((session /sessions/s1.jsonl) (limit 60)))
     (tag (Page (path /sessions/s1.jsonl) (before ()))))
    Scroll_to_bottom
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    question 2
    10:02 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 2
    (✕ Delete) (✂ Trim everything after this)
    question 3
    10:03 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 3
    (✕ Delete)
    Back in the context: 3 messages (~3.0k tokens)
    |}]
;;

let%expect_test "with a trim before: two dividers, each Undo its own" =
  let before =
    hidden
      ~entry:"t0"
      ~kind:"trim_before"
      ~message:"u2"
      ~messages:2
      ~tokens:19_000
      ()
  in
  let after_u3 =
    hidden
      ~entry:"t1"
      ~kind:"trim_after"
      ~through:"a4"
      ~message:"u3"
      ~messages:3
      ~tokens:3_000
      ~divider:3
      ()
  in
  let h =
    H.create ~state:(state ~trim:trim_json ~hidden:[ before; after_u3 ] ()) ()
  in
  newest h (messages [ user 2; assistant "answer 2"; user 3; user 5 ]);
  chat h;
  [%expect
    {|
    ✂ 2 messages trimmed · ~19k tokens (Show) · (Undo)
    question 2
    10:02
    (✕ Delete) (✂ Trim everything after this)
    answer 2
    (✕ Delete) (✂ Trim everything after this)
    question 3
    10:03 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    ✂ 3 messages trimmed · ~3.0k tokens (Show) · (Undo)
    question 5
    10:05 (✂ Trim everything before this)
    (✕ Delete)
    |}];
  (* The first divider's Undo: the trim before, though the trim after is
     newer. *)
  H.act h Untrim;
  [%expect {| (Rpc (method_ untrim) (params ((entry t0))) (tag Untrimmed)) |}];
  (* [/trim undo]: the newest. *)
  H.type_ h "/trim undo";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/trim undo") (max 500))) (tag Ignore))
    (Rpc (method_ untrim) (params ((entry t1))) (tag (Range_restored t1)))
    |}]
;;

let%expect_test "/delete: a picker, newest first; /delete undo" =
  let h = session () in
  H.type_ h "/delete";
  H.act h Send;
  H.reply h "get_message_points" (exchange_points [ 1; 2; 3 ]);
  dialog h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /delete) (max 500)))
     (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag (Message_points Delete_picker)))
    (Focus picker-input)
    Delete a message
    (Close (Esc))
    []
    answer 3 reply  1 message · ~500 tokens
    question 3 you  10:03  1 message · ~500 tokens
    answer 2 reply  1 message · ~500 tokens
    question 2 you  10:02  1 message · ~500 tokens
    answer 1 reply  1 message · ~500 tokens
    question 1 you  10:01  1 message · ~500 tokens
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.key h "ArrowDown" ~target:Field;
  H.key h "Enter" ~target:Field;
  chat h;
  [%expect
    {|
    (Dialog_move 1)
    Dialog_accept
    (Focus editor)
    (Rpc (method_ delete) (params ((message u3))) (tag (Deleted (entry (4)))))
    question 1
    10:01
    (✕ Delete) (✂ Trim everything after this)
    answer 1
    (✕ Delete) (✂ Trim everything after this)
    question 2
    10:02 (✂ Trim everything before this)
    (✕ Delete) (✂ Trim everything after this)
    answer 2
    (✕ Delete) (✂ Trim everything after this)
    answer 3
    (✕ Delete)
    |}];
  (* Nothing deleted: undo says what to do. *)
  H.type_ h "/delete undo";
  H.act h Send;
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/delete undo") (max 500))) (tag Ignore))
    Nothing is deleted: /delete picks a message to delete. (Dismiss)
    |}];
  let h = session ~hidden:[ deleted_u2 ] () in
  H.type_ h "/delete undo";
  H.act h Send;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/delete undo") (max 500))) (tag Ignore))
    (Rpc (method_ undelete) (params ((entry d1))) (tag (Range_restored d1)))
    |}];
  H.type_ h "/delete ";
  H.text h ~selector:".popup";
  [%expect
    {|
    Delete ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    undo bring back the latest deleted message
    |}]
;;

let%expect_test "/trim after: a picker, then the confirmation" =
  let h = session () in
  H.type_ h "/trim after";
  H.act h Send;
  H.reply h "get_message_points" (exchange_points [ 1; 2; 3 ]);
  dialog h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/trim after") (max 500))) (tag Ignore))
    (Focus dialog)
    (Rpc (method_ get_message_points) (params ())
     (tag (Message_points Trim_after_picker)))
    (Focus picker-input)
    Trim the context after
    (Close (Esc))
    []
    question 3 you  10:03  1 message · ~1.0k tokens
    answer 2 reply  2 messages · ~2.0k tokens
    question 2 you  10:02  3 messages · ~3.0k tokens
    answer 1 reply  4 messages · ~4.0k tokens
    question 1 you  10:01  5 messages · ~5.0k tokens
    ↑↓ to move, Enter to choose, Esc to close
    |}];
  H.key h "ArrowDown" ~target:Field;
  H.key h "Enter" ~target:Field;
  dialog h;
  [%expect
    {|
    (Dialog_move 1)
    Dialog_accept
    (Focus dialog)
    Trim the context after this?
    (Close (Esc))
    Trim 2 messages (~2.0k tokens) after this message?
    answer 2
    They leave the model's context; the transcript keeps them folded where they were, what you send next goes after them, and Undo brings them back.
    Enter trims, Esc cancels (Cancel) (Trim)
    |}];
  H.key h "Enter" ~target:Page;
  [%expect
    {|
    Dialog_accept
    (Rpc (method_ trim_after) (params ((after a2))) (tag Trimmed_after))
    |}];
  H.type_ h "/trim sideways";
  H.act h Send;
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref)
     (params ((key history) (value "/trim sideways") (max 500))) (tag Ignore))
    Usage: /trim (pick the message to trim before), /trim after (pick the message to trim after) or /trim undo (Dismiss)
    |}]
;;

let%expect_test
    "switching sessions drops the deletion's toast and what is hidden"
  =
  let h = session () in
  H.quiet h (fun () ->
    H.act h (Ask_delete_message 2);
    H.reply h "get_message_points" (exchange_points [ 1; 2; 3 ]);
    H.reply h "delete" deleted_u2;
    H.act h (Ask_delete_message 4));
  toasts h;
  print_s [%sexp ((H.model h).trimmed.deleting : Int.Set.t)];
  [%expect
    {|
    Message deleted (Undo)
    (2 4)
    |}];
  H.quiet h (fun () ->
    H.act h (Switch_session "/sessions/s2.jsonl");
    H.reply h "switch_session" "{}";
    H.reply
      h
      "get_state"
      (H.state_json ~fields:[ "session_id", `String "s2" ] ()));
  toasts h;
  print_s [%sexp ((H.model h).trimmed : Trimmed.t)];
  [%expect
    {|
    ((shown Folded) (restoring false) (reveal ()) (ranges ())
     (restoring_ranges ()) (deleting ()) (reveal_hidden ()))
    |}]
;;
