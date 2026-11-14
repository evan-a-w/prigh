open! Core
module H = Harness

(* Settings' About you: the text the model is given in every session. *)

let section h = H.text h ~selector:".settings-panel"

let limits ?(short = 100) ?(total = 5000) () =
  sprintf
    {|"limits":{"name":%d,"background":1000,"anything_else":4000,"total":%d}|}
    short
    total
;;

(* As the backend says it: the form, and the block it renders. *)
let info ?short ?total ?(form = {|{}|}) ?(text = "") () =
  sprintf
    {|{"form":%s,"text":%s,%s}|}
    form
    (Jsonaf.to_string (`String text))
    (limits ?short ?total ())
;;

let lloyd =
  info
    ~form:{|{"name":"Lloyd","units":"metric"}|}
    ~text:"- Name: Lloyd\n- Units: metric (°C, km, kg, L)"
    ()
;;

let opened ?(info = lloyd) () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Settings_section About_you);
    H.reply h "get_user_info" info);
  h
;;

let changed h info =
  H.event
    h
    (String.substr_replace_first
       info
       ~pattern:"{"
       ~with_:{|{"type":"event","event":"user_info_changed",|})
;;

let%expect_test "opening reads it; the form's fields; what it is for" =
  let h = H.create () in
  H.act h (Settings_section About_you);
  section h;
  H.reply h "get_user_info" (info ());
  section h;
  H.show h ~selector:".about-you-form";
  H.text h ~selector:".settings-nav";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_user_info) (params ()) (tag About_you_loaded))
    About you
    What the model should know about you, given to it in every session. Kept on the backend, for your account.
    Tell the model about you Every field is optional. What you fill in is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    Name
    []
    Country or region
    []
    Time zone
    []
    Language and spelling
    []
    Units
    (Not set) (Metric) (Imperial) (US)
    Date format
    (Not set) (D/M/Y) (M/D/Y) (ISO)
    Occupation or role
    []
    Background and expertise
    []
    Anything else
    []
    Reading what you set…
    Ctrl+Enter saves (Save)
    About you
    What the model should know about you, given to it in every session. Kept on the backend, for your account.
    Tell the model about you Every field is optional. What you fill in is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    Name
    []
    Country or region
    []
    Time zone
    []
    Language and spelling
    []
    Units
    (Not set) (Metric) (Imperial) (US)
    Date format
    (Not set) (D/M/Y) (M/D/Y) (ISO)
    Occupation or role
    []
    Background and expertise
    []
    Anything else
    []
    0 of 5000 bytes Ctrl+Enter saves (Save)
    <div class="about-you-form">
      <div id="about-you-name-field" class="about-you-field">
        <label for="about-you-name" class="about-you-label"> Name </label>
        <input id="about-you-name"
               maxlength="100"
               type="text"
               placeholder="What to call you"
               autocomplete="off"
               class="about-you-line text-input"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-country-field" class="about-you-field">
        <label for="about-you-country" class="about-you-label"> Country or region </label>
        <input id="about-you-country"
               maxlength="100"
               type="text"
               placeholder="Australia"
               autocomplete="off"
               class="about-you-line text-input"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-time-zone-field" class="about-you-field">
        <label for="about-you-time-zone" class="about-you-label"> Time zone </label>
        <input id="about-you-time-zone"
               maxlength="100"
               type="text"
               placeholder="Australia/Sydney"
               autocomplete="off"
               class="about-you-line text-input"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-language-field" class="about-you-field">
        <label for="about-you-language" class="about-you-label"> Language and spelling </label>
        <input id="about-you-language"
               maxlength="100"
               type="text"
               placeholder="English, Australian spelling"
               autocomplete="off"
               class="about-you-line text-input"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-units-field" class="about-you-field">
        <label class="about-you-label"> Units </label>
        <div id="about-you-units" class="about-you-choice">
          <div role="group" aria-label="Units" class="segmented">
            <button type="button"
                    aria-pressed="true"
                    title="Leave it out"
                    class="on segment"
                    @on_click> Not set </button>
            <button type="button"
                    aria-pressed="false"
                    title="Metric (°C, km, kg)"
                    class="segment"
                    @on_click> Metric </button>
            <button type="button"
                    aria-pressed="false"
                    title="Imperial (miles, stone; °C)"
                    class="segment"
                    @on_click> Imperial </button>
            <button type="button"
                    aria-pressed="false"
                    title="US customary (°F, miles, lb)"
                    class="segment"
                    @on_click> US </button>
          </div>
        </div>
      </div>
      <div id="about-you-date-format-field" class="about-you-field">
        <label class="about-you-label"> Date format </label>
        <div id="about-you-date-format" class="about-you-choice">
          <div role="group" aria-label="Date format" class="segmented">
            <button type="button"
                    aria-pressed="true"
                    title="Leave it out"
                    class="on segment"
                    @on_click> Not set </button>
            <button type="button"
                    aria-pressed="false"
                    title="D/M/Y (25/12/2026)"
                    class="segment"
                    @on_click> D/M/Y </button>
            <button type="button"
                    aria-pressed="false"
                    title="M/D/Y (12/25/2026)"
                    class="segment"
                    @on_click> M/D/Y </button>
            <button type="button"
                    aria-pressed="false"
                    title="ISO (2026-12-25)"
                    class="segment"
                    @on_click> ISO </button>
          </div>
        </div>
      </div>
      <div id="about-you-occupation-field" class="about-you-field">
        <label for="about-you-occupation" class="about-you-label"> Occupation or role </label>
        <input id="about-you-occupation"
               maxlength="100"
               type="text"
               placeholder="Backend engineer"
               autocomplete="off"
               class="about-you-line text-input"
               #value=""
               @on_input/>
      </div>
      <div id="about-you-background-field" class="about-you-field wide">
        <label for="about-you-background" class="about-you-label"> Background and expertise </label>
        <textarea id="about-you-background"
                  maxlength="1000"
                  placeholder="Undergrad pure maths and CS; years of OCaml"
                  rows="3"
                  class="about-you-input text-input"
                  #value=""
                  @on_input> </textarea>
      </div>
      <div id="about-you-anything-else-field" class="about-you-field wide">
        <label for="about-you-anything-else" class="about-you-label"> Anything else </label>
        <textarea id="about-you-anything-else"
                  maxlength="4000"
                  placeholder="Keep answers short, and ask before adding a dependency."
                  rows="5"
                  class="about-you-input text-input"
                  #value=""
                  @on_input> </textarea>
      </div>
    </div>
    (Appearance) (Sessions) (New sessions) (Models) (Memory) (About you) (Custom instructions) (MCP servers) (Git hosting) (Account)
    |}]
;;

let%expect_test "the browser's time zone is a tap away" =
  let h = opened () in
  H.act h (Set_browser_time_zone "Australia/Sydney");
  H.text h ~selector:"#about-you-time-zone-field";
  H.click h ~selector:".about-you-detect";
  H.show h ~selector:"#about-you-time-zone";
  H.text h ~selector:"#about-you-time-zone-field";
  [%expect
    {|
    Time zone
    [] (Use Australia/Sydney)
    (About_you_field Time_zone Australia/Sydney)
    <input id="about-you-time-zone"
           maxlength="100"
           type="text"
           placeholder="Australia/Sydney"
           autocomplete="off"
           class="about-you-line text-input"
           #value="Australia/Sydney"
           @on_input/>
    Time zone
    [Australia/Sydney]
    |}]
;;

let%expect_test "editing makes it dirty: Save and Revert; Ctrl+Enter saves" =
  let h = opened () in
  H.text h ~selector:".about-you-buttons";
  H.input h ~selector:"#about-you-country" "Australia ";
  H.click h ~selector:"#about-you-date-format .segment:nth-child(2)";
  H.input h ~selector:"#about-you-background" "Pure maths and CS.\n";
  H.text h ~selector:".about-you-buttons";
  H.key h ~ctrl:true ~target:Multiline "Enter";
  H.text h ~selector:".about-you-buttons";
  (* The broadcast of our own save arrives before its reply. *)
  let saved =
    info
      ~form:
        {|{"name":"Lloyd","country":"Australia","units":"metric","date_format":"dmy","background":"Pure maths and CS."}|}
      ~text:"- Name: Lloyd\n- Country/region: Australia\n…"
      ()
  in
  changed h saved;
  H.reply h "set_user_info" saved;
  H.text h ~selector:".about-you";
  [%expect
    {|
    5 of 5000 bytes Ctrl+Enter saves (Save)
    (About_you_field Country "Australia ")
    (About_you_field Date_format dmy)
    (About_you_field Background "Pure maths and CS.\n")
    32 of 5000 bytes Ctrl+Enter saves (Revert) (Save)
    Dialog_accept
    (Rpc (method_ set_user_info)
     (params
      ((form
        ((name Lloyd) (country Australia) (units metric) (date_format dmy)
         (background "Pure maths and CS.")))))
     (tag About_you_saved))
    32 of 5000 bytes Ctrl+Enter saves (Revert) (Saving…)
    Tell the model about you Every field is optional. What you fill in is given to the model in every session: new ones start with it, and open ones get it with your next message. Memory is for notes the model looks up only when it needs them.
    Name
    [Lloyd]
    Country or region
    [Australia]
    Time zone
    []
    Language and spelling
    []
    Units
    (Not set) (Metric) (Imperial) (US)
    Date format
    (Not set) (D/M/Y) (M/D/Y) (ISO)
    Occupation or role
    []
    Background and expertise
    [Pure maths and CS.]
    Anything else
    []
    Saved. New sessions start with it; open ones get it with your next message in them.
    32 of 5000 bytes Ctrl+Enter saves (Save)
    |}];
  H.input h ~selector:"#about-you-name" "someone else";
  H.click h ~selector:"#about-you-units .segment:first-child";
  H.click h ~selector:".about-you-buttons button.ghost";
  H.show h ~selector:"#about-you-name";
  H.show h ~selector:"#about-you-units";
  H.text h ~selector:".about-you-buttons";
  [%expect
    {|
    (About_you_field Name "someone else")
    (About_you_field Units "")
    About_you_revert
    (Focus about-you-name)
    <input id="about-you-name"
           maxlength="100"
           type="text"
           placeholder="What to call you"
           autocomplete="off"
           class="about-you-line text-input"
           #value="Lloyd"
           @on_input/>
    <div id="about-you-units" class="about-you-choice">
      <div role="group" aria-label="Units" class="segmented">
        <button type="button" aria-pressed="false" title="Leave it out" class="segment" @on_click> Not set </button>
        <button type="button"
                aria-pressed="true"
                title="Metric (°C, km, kg)"
                class="on segment"
                @on_click> Metric </button>
        <button type="button"
                aria-pressed="false"
                title="Imperial (miles, stone; °C)"
                class="segment"
                @on_click> Imperial </button>
        <button type="button"
                aria-pressed="false"
                title="US customary (°F, miles, lb)"
                class="segment"
                @on_click> US </button>
      </div>
    </div>
    32 of 5000 bytes Ctrl+Enter saves (Save)
    |}]
;;

let%expect_test "emptying every field clears it" =
  let h = opened () in
  H.input h ~selector:"#about-you-name" "  ";
  H.click h ~selector:"#about-you-units .segment:first-child";
  H.click h ~selector:".about-you-buttons button.primary";
  H.reply h "set_user_info" (info ());
  H.text h ~selector:".about-you-saved";
  [%expect
    {|
    (About_you_field Name "  ")
    (About_you_field Units "")
    About_you_save
    (Rpc (method_ set_user_info) (params ((form ()))) (tag About_you_saved))
    Cleared. Open sessions are told with your next message in them.
    |}]
;;

let%expect_test "refused: the reason stays under the form, which is kept" =
  let h = opened () in
  H.input h ~selector:"#about-you-occupation" "student";
  H.click h ~selector:".about-you-buttons button.primary";
  H.fail
    h
    "set_user_info"
    "cannot write /home/me/.prigh/about_you.json: Permission denied";
  H.text h ~selector:".about-you-error";
  H.text h ~selector:".about-you-buttons";
  [%expect
    {|
    (About_you_field Occupation student)
    About_you_save
    (Rpc (method_ set_user_info)
     (params ((form ((name Lloyd) (units metric) (occupation student)))))
     (tag About_you_saved))
    cannot write /home/me/.prigh/about_you.json: Permission denied
    12 of 5000 bytes Ctrl+Enter saves (Revert) (Save)
    |}];
  (* Over a limit is said here, with nothing sent. *)
  let h =
    opened ~info:(info ~short:10 ~total:30 ~form:{|{"name":"Lloyd"}|} ()) ()
  in
  H.input h ~selector:"#about-you-name" "Lloyd Smith";
  H.click h ~selector:".about-you-buttons button.primary";
  H.text h ~selector:".about-you-error";
  H.input h ~selector:"#about-you-name" "Lloyd";
  H.input h ~selector:"#about-you-background" "Pure maths and CS, and more.";
  H.click h ~selector:".about-you-buttons button.primary";
  H.text h ~selector:".about-you-error";
  H.text h ~selector:".about-you-count";
  [%expect
    {|
    (About_you_field Name "Lloyd Smith")
    About_you_save
    Name is 11 bytes and the limit is 10: shorten it, and keep what is only sometimes needed in memory notes.
    (About_you_field Name Lloyd)
    (About_you_field Background "Pure maths and CS, and more.")
    About_you_save
    All together this is 33 bytes and the limit is 30: shorten it, and keep what is only sometimes needed in memory notes.
    33 of 30 bytes
    |}]
;;

let%expect_test "changed elsewhere: shown at once, unless something is typed" =
  let h = opened () in
  changed h (info ~form:{|{"name":"Elsewhere"}|} ~text:"- Name: Elsewhere" ());
  H.show h ~selector:"#about-you-name";
  H.input h ~selector:"#about-you-name" "Typed here";
  changed h (info ~form:{|{"name":"Third"}|} ~text:"- Name: Third" ());
  H.text h ~selector:".about-you-elsewhere";
  H.click h ~selector:".about-you-elsewhere button";
  H.show h ~selector:"#about-you-name";
  [%expect
    {|
    <input id="about-you-name"
           maxlength="100"
           type="text"
           placeholder="What to call you"
           autocomplete="off"
           class="about-you-line text-input"
           #value="Elsewhere"
           @on_input/>
    (About_you_field Name "Typed here")
    It was changed somewhere else while you were typing. Saving replaces that with this. (Load that one)
    About_you_revert
    (Focus about-you-name)
    <input id="about-you-name"
           maxlength="100"
           type="text"
           placeholder="What to call you"
           autocomplete="off"
           class="about-you-line text-input"
           #value="Third"
           @on_input/>
    |}]
;;

let%expect_test "a failed read says so" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Settings_section About_you));
  H.fail h "get_user_info" "cannot read /home/me/.prigh/about_you.json";
  H.text h ~selector:".about-you > .setting-note";
  [%expect
    {| Could not read it: cannot read /home/me/.prigh/about_you.json. Open the section again to retry. |}]
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
    /user-info takes no argument here: it opens Settings, About you, where you fill in the form. (Dismiss)
    (Focus dialog)
    (Rpc (method_ get_user_info) (params ()) (tag About_you_loaded))
    |}]
;;

let%expect_test "the note a session is told with is a margin note" =
  let chat =
    Chat_harness.chat
      [ {|{"event":"message_start","message":{"role":"user","text":"[User info: what the user wrote about themselves, for every session, is now:\nI'm Lloyd.]\n\nhello","origin":{"body":"hello","notes":[{"kind":"user_info","title":"About you: the model was given your details","text":"[User info: what the user wrote about themselves, for every session, is now:\nI'm Lloyd.]"}]}}}|}
      ]
  in
  Chat_harness.text chat;
  [%expect
    {|
    ☺ About you: the model was given your details [User info: what the user wrote about themselves, for every session, is now:
    I'm Lloyd.] hello
    |}]
;;
