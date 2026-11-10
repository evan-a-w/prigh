open! Core
module H = Harness

(* Settings' About you: the text the model is given in every session. *)

let section h = H.text h ~selector:".settings-panel"

let info text =
  sprintf {|{"text":%s,"max":4000}|} (Jsonaf.to_string (`String text))
;;

let opened ?(text = "I'm Lloyd. Short answers, please.") () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Settings_section About_you);
    H.reply h "get_user_info" (info text));
  h
;;

let changed h text =
  H.event
    h
    (sprintf
       {|{"type":"event","event":"user_info_changed","text":%s,"max":4000}|}
       (Jsonaf.to_string (`String text)))
;;

let%expect_test "opening reads it; the section says what it is for" =
  let h = H.create () in
  H.act h (Settings_section About_you);
  section h;
  H.reply h "get_user_info" (info "");
  section h;
  H.show h ~selector:"#about-you-text";
  H.text h ~selector:".settings-nav";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_user_info) (params ()) (tag About_you_loaded))
    About you
    What the model should know about you, given to it in every session. Kept on the backend, for your account.
    What the model should know about you Who you are, what you work on, how you like answers. This text is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    [] Reading what you wrote…
    Ctrl+Enter saves (Save)
    About you
    What the model should know about you, given to it in every session. Kept on the backend, for your account.
    What the model should know about you Who you are, what you work on, how you like answers. This text is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    []
    0 of 4000 bytes Ctrl+Enter saves (Save)
    <textarea id="about-you-text"
              aria-label="About you"
              placeholder="I'm a backend engineer working mostly in OCaml. Keep answers short, and ask before adding a dependency."
              rows="8"
              maxlength="4000"
              class="about-you-input text-input"
              #value=""
              @on_input> </textarea>
    (Appearance) (Sessions) (Models) (Memory) (About you) (Git hosting) (Account)
    |}]
;;

let%expect_test "typing makes it dirty: Save and Revert; Ctrl+Enter saves" =
  let h = opened () in
  H.text h ~selector:".about-you-buttons";
  H.input
    h
    ~selector:"#about-you-text"
    "I'm Lloyd. I work on prigh, in OCaml.\n";
  H.text h ~selector:".about-you-buttons";
  H.key h ~ctrl:true ~target:Multiline "Enter";
  H.text h ~selector:".about-you-buttons";
  H.show h ~selector:"#about-you-text";
  (* The broadcast of our own save arrives before its reply. *)
  changed h "I'm Lloyd. I work on prigh, in OCaml.";
  H.reply h "set_user_info" (info "I'm Lloyd. I work on prigh, in OCaml.");
  section h;
  [%expect
    {|
    33 of 4000 bytes Ctrl+Enter saves (Save)
    (About_you_text "I'm Lloyd. I work on prigh, in OCaml.\n")
    37 of 4000 bytes Ctrl+Enter saves (Revert) (Save)
    Dialog_accept
    (Rpc (method_ set_user_info)
     (params ((text "I'm Lloyd. I work on prigh, in OCaml.")))
     (tag About_you_saved))
    37 of 4000 bytes Ctrl+Enter saves (Revert) (Saving…)
    <textarea id="about-you-text"
              aria-label="About you"
              placeholder="I'm a backend engineer working mostly in OCaml. Keep answers short, and ask before adding a dependency."
              rows="8"
              maxlength="4000"
              disabled=""
              class="about-you-input text-input"
              #value="I'm Lloyd. I work on prigh, in OCaml.
    "
              @on_input> </textarea>
    About you
    What the model should know about you, given to it in every session. Kept on the backend, for your account.
    What the model should know about you Who you are, what you work on, how you like answers. This text is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    [I'm Lloyd. I work on prigh, in OCaml.] Saved. New sessions start with it; open ones get it with your next message in them.
    37 of 4000 bytes Ctrl+Enter saves (Save)
    |}];
  H.input h ~selector:"#about-you-text" "something else";
  H.click h ~selector:".about-you-buttons button.ghost";
  H.show h ~selector:"#about-you-text";
  H.text h ~selector:".about-you-buttons";
  [%expect
    {|
    (About_you_text "something else")
    About_you_revert
    (Focus about-you-text)
    <textarea id="about-you-text"
              aria-label="About you"
              placeholder="I'm a backend engineer working mostly in OCaml. Keep answers short, and ask before adding a dependency."
              rows="8"
              maxlength="4000"
              class="about-you-input text-input"
              #value="I'm Lloyd. I work on prigh, in OCaml."
              @on_input> </textarea>
    37 of 4000 bytes Ctrl+Enter saves (Save)
    |}]
;;

let%expect_test "emptying it removes it" =
  let h = opened () in
  H.input h ~selector:"#about-you-text" "  ";
  H.click h ~selector:".about-you-buttons button.primary";
  H.reply h "set_user_info" (info "");
  H.text h ~selector:".about-you-saved";
  [%expect
    {|
    (About_you_text "  ")
    About_you_save
    (Rpc (method_ set_user_info) (params ((text ""))) (tag About_you_saved))
    Removed. Open sessions are told with your next message in them.
    |}]
;;

let%expect_test "refused: the reason stays under the text, which is kept" =
  let h = opened () in
  H.input h ~selector:"#about-you-text" "new text";
  H.click h ~selector:".about-you-buttons button.primary";
  H.fail
    h
    "set_user_info"
    "cannot write /home/me/.prigh/user_info.md: Permission denied";
  H.text h ~selector:".about-you";
  [%expect
    {|
    (About_you_text "new text")
    About_you_save
    (Rpc (method_ set_user_info) (params ((text "new text")))
     (tag About_you_saved))
    What the model should know about you Who you are, what you work on, how you like answers. This text is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    [new text] cannot write /home/me/.prigh/user_info.md: Permission denied
    8 of 4000 bytes Ctrl+Enter saves (Revert) (Save)
    |}];
  (* Over the limit is said here, with nothing sent. *)
  H.quiet h (fun () ->
    H.input h ~selector:"#about-you-text" (String.make 4001 'x'));
  H.click h ~selector:".about-you-buttons button.primary";
  H.text h ~selector:".about-you-error";
  H.text h ~selector:".about-you-count";
  [%expect
    {|
    (About_you_text
     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
    About_you_save
    This is 4001 bytes and the limit is 4000: shorten it, and keep what is only sometimes needed in memory notes.
    4001 of 4000 bytes
    |}]
;;

let%expect_test "changed elsewhere: shown at once, unless something is typed" =
  let h = opened () in
  changed h "Written in another browser.";
  H.show h ~selector:"#about-you-text";
  H.input h ~selector:"#about-you-text" "Typed here.";
  changed h "Written in a third browser.";
  H.text h ~selector:".about-you";
  H.click h ~selector:".about-you-elsewhere button";
  H.text h ~selector:".about-you";
  [%expect
    {|
    <textarea id="about-you-text"
              aria-label="About you"
              placeholder="I'm a backend engineer working mostly in OCaml. Keep answers short, and ask before adding a dependency."
              rows="8"
              maxlength="4000"
              class="about-you-input text-input"
              #value="Written in another browser."
              @on_input> </textarea>
    (About_you_text "Typed here.")
    What the model should know about you Who you are, what you work on, how you like answers. This text is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    [Typed here.]
    It was changed somewhere else while you were typing. Saving replaces that with this. (Load that one)
    11 of 4000 bytes Ctrl+Enter saves (Revert) (Save)
    About_you_revert
    (Focus about-you-text)
    What the model should know about you Who you are, what you work on, how you like answers. This text is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    [Written in a third browser.]
    27 of 4000 bytes Ctrl+Enter saves (Save)
    |}]
;;

let%expect_test "a failed read says so" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Settings_section About_you));
  H.fail h "get_user_info" "cannot read /home/me/.prigh/user_info.md";
  H.text h ~selector:".about-you";
  [%expect
    {|
    What the model should know about you Who you are, what you work on, how you like answers. This text is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    [] Could not read it: cannot read /home/me/.prigh/user_info.md. Open the section again to retry.
    Ctrl+Enter saves (Save)
    |}]
;;

let%expect_test "/user-info opens the section; it takes no argument" =
  let h = H.create () in
  H.act h (Run "/user-info");
  H.text h ~selector:".settings-panel-head h3";
  H.act h (Run "/user-info I'm Lloyd");
  H.text h ~selector:".toast";
  H.act h (Run "/settings about");
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_user_info) (params ()) (tag About_you_loaded))
    About you
    /user-info takes no argument here: it opens Settings, About you, where you write the text. (Dismiss)
    (Focus dialog)
    (Rpc (method_ get_user_info) (params ()) (tag About_you_loaded))
    |}]
;;

let%expect_test "the note a session is told with is a margin note" =
  let chat =
    Chat_harness.chat
      [ {|{"event":"message_start","message":{"role":"user","text":"[User info: what the user wrote about themselves, for every session, is now:\nI'm Lloyd.]\n\nhello","origin":{"body":"hello","notes":[{"kind":"user_info","title":"About you: the model was given what you wrote","text":"[User info: what the user wrote about themselves, for every session, is now:\nI'm Lloyd.]"}]}}}|}
      ]
  in
  Chat_harness.text chat;
  [%expect
    {|
    ☺ About you: the model was given what you wrote [User info: what the user wrote about themselves, for every session, is now:
    I'm Lloyd.] hello
    |}]
;;
