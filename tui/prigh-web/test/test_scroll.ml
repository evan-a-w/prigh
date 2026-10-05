open! Core

(* The page tells the app when the user scrolls the chat away from its end;
   the app shows a button back. *)
let%expect_test "scrolled up: a jump button; it, or sending, follows again" =
  let h = Harness.create () in
  let button () = Harness.show h ~selector:".jump-to-bottom" in
  button ();
  [%expect {| |}];
  Harness.act h (Chat_scrolled { at_bottom = false });
  button ();
  [%expect
    {|
    <button title="Jump to the latest" class="jump-to-bottom" @on_click>
      <icon class="arrow_down"> </icon>
    </button>
    |}];
  Harness.event
    h
    {|{"event":"message_start","message":{"role":"user","text":"hello"}}|};
  button ();
  [%expect
    {|
    <button title="Jump to the latest" class="jump-to-bottom" @on_click>
      <icon class="arrow_down"> </icon>
    </button>
    |}];
  Harness.act h Jump_to_bottom;
  button ();
  [%expect {| Scroll_to_bottom |}];
  (* Scrolled back to the end by hand. *)
  Harness.act h (Chat_scrolled { at_bottom = false });
  Harness.act h (Chat_scrolled { at_bottom = true });
  button ();
  [%expect {| |}];
  (* Sending something goes back to the end. *)
  Harness.act h (Chat_scrolled { at_bottom = false });
  Harness.act h (Set_draft "next");
  Harness.act h Send;
  button ();
  [%expect
    {|
    (Save_history (next))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text next)))
     (tag (Sent (text next) (images ()))))
    |}]
;;
