open! Core
module H = Harness

(* Settings' Custom instructions: the user's standing instructions, in every
   session, whether the model may change them, and what they cost. *)

let note ?title id text =
  sprintf
    {|{"id":%S,%s"text":%S,"saved":"2026-11-0%c 09:30:00.000000Z"}|}
    id
    (Option.value_map title ~default:"" ~f:(sprintf {|"title":%S,|}))
    text
    id.[2]
;;

let json ?(enabled = true) notes =
  sprintf
    {|{"enabled":%b,"notes":[%s],"limits":{"text":1000,"title":80,"count":50}}|}
    enabled
    (String.concat ~sep:"," notes)
;;

let two =
  json
    [ note "ci1" "Use Australian spelling."
    ; note ~title:"Maths" "ci2" "Keep one memory note on my maths up to date."
    ]
;;

let section h = H.text h ~selector:".settings-panel"

let opened ?(json = two) () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Settings_section Custom_instructions);
    H.reply h "get_custom_instructions" json);
  h
;;

let%expect_test "opening reads them; empty, it says what they are" =
  let h = H.create () in
  H.act h (Run "/instructions");
  section h;
  H.reply h "get_custom_instructions" (json []);
  section h;
  H.text h ~selector:".settings-nav";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_custom_instructions) (params ()) (tag Instructions_loaded))
    Custom instructions
    Standing instructions for the model, given to it in every session; it may also add or edit them when you state a lasting preference. Kept on the backend, for your account.
    Let the model change them
    When you state a lasting preference or standing instruction, the model adds it here or edits the one that covers it, and tells you. Off, only you change them; they stay in every session either way. Reading your instructions…
    (Only you change them (/instructions off))
    Custom instructions
    Standing instructions for the model, given to it in every session; it may also add or edit them when you state a lasting preference. Kept on the backend, for your account.
    Let the model change them
    When you state a lasting preference or standing instruction, the model adds it here or edits the one that covers it, and tells you. Off, only you change them; they stay in every session either way.
    (Only you change them (/instructions off))
    Instructions
    No instructions yet
    Write what the model should always do (“Use Australian spelling”, “Prefer OCaml examples”), or tell it a lasting preference and it adds one here. Every session gets them.
    (Add instruction)
    (Appearance) (Sessions) (New sessions) (Models) (Memory) (About you) (Custom instructions) (MCP servers) (Git hosting) (Account)
    |}]
;;

let%expect_test "the list, in the model's order, with ids and their size" =
  let h = opened () in
  section h;
  [%expect
    {|
    Custom instructions
    Standing instructions for the model, given to it in every session; it may also add or edit them when you state a lasting preference. Kept on the backend, for your account.
    Let the model change them
    When you state a lasting preference or standing instruction, the model adds it here or edits the one that covers it, and tells you. Off, only you change them; they stay in every session either way.
    (Only you change them (/instructions off))
    Instructions 2 instructions
    2 of 50 · about 40 tokens in every session
    Use Australian spelling. ci1 · saved 2026-11-01
    (Edit) (Delete)
    Maths Keep one memory note on my maths up to date. ci2 · saved 2026-11-02
    (Edit) (Delete)
    (Add instruction)
    |}]
;;

let%expect_test "edit one: Enter in the title saves, the reply closes it" =
  let h = opened () in
  H.click h ~selector:"[data-note=ci1] button[aria-label='Edit ci1']";
  H.show h ~selector:".instruction-draft";
  H.input h ~selector:"#instruction-text" "Use British spelling.";
  H.key h ~target:Field "Enter";
  H.text h ~selector:".instruction-draft";
  H.reply
    h
    "save_custom_instruction"
    {|{"note":{"id":"ci1","text":"Use British spelling.","saved":"2026-11-12 10:00:00.000000Z"}}|};
  H.elements h ~selector:".instruction-draft";
  [%expect
    {|
    (Instruction_edit (ci1))
    (Focus instruction-text)
    <div class="instruction-draft memory-draft setting">
      <input id="instruction-title"
             type="text"
             aria-label="Title"
             placeholder="Title (optional): a few words"
             maxlength="80"
             autocomplete="off"
             class="memory-title-input text-input"
             #value=""
             @on_input/>
      <textarea id="instruction-text"
                aria-label="Instruction"
                placeholder="What the model should always do, e.g. “Use Australian spelling.”"
                rows="3"
                maxlength="1000"
                class="memory-text-input text-input"
                #value="Use Australian spelling."
                @on_input> </textarea>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div class="memory-draft-buttons">
        <span class="memory-draft-keys"> Ctrl+Enter saves, Esc cancels </span>
        <button type="button" title="Esc" class="btn ghost small" @on_click> Cancel </button>
        <button type="button"
                title="Ctrl+Enter (Enter in the title)"
                class="btn primary small"
                @on_click> Save </button>
      </div>
    </div>
    (Instruction_text "Use British spelling.")
    Dialog_accept
    (Rpc (method_ save_custom_instruction)
     (params ((id ci1) (text "Use British spelling."))) (tag Instruction_saved))
    [] [Use British spelling.]
    Ctrl+Enter saves, Esc cancels (Cancel) (Saving…)
    |}]
;;

let%expect_test "add one; too long or refused is said under it; Esc cancels" =
  let h = opened () in
  H.click h ~selector:".instruction-add";
  H.input h ~selector:"#instruction-text" (String.make 1001 'x');
  H.act h Instruction_save;
  H.text h ~selector:".instruction-draft .setting-note";
  H.input h ~selector:"#instruction-title" "Code";
  H.input h ~selector:"#instruction-text" "Prefer OCaml examples.";
  H.key h ~ctrl:true ~target:Multiline "Enter";
  H.fail h "save_custom_instruction" "there are 50 custom instructions";
  H.text h ~selector:".instruction-draft .setting-note";
  H.key h "Escape";
  H.elements h ~selector:".instruction-draft";
  H.elements h ~selector:".settings-dialog";
  [%expect
    {|
    (Instruction_edit ())
    (Focus instruction-text)
    (Instruction_text
     xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
    It is 1001 bytes and one can be at most 1000: they are in every session's context, so keep each to the point.
    (Instruction_title Code)
    (Instruction_text "Prefer OCaml examples.")
    Dialog_accept
    (Rpc (method_ save_custom_instruction)
     (params ((title Code) (text "Prefer OCaml examples.")))
     (tag Instruction_saved))
    There are 50 custom instructions
    Close_dialog
    <div class="modal settings-dialog">
    |}]
;;

let%expect_test "delete asks first; Keep and Esc keep it" =
  let h = opened () in
  H.click h ~selector:"[data-note=ci2] .memory-delete";
  H.text h ~selector:"[data-note=ci2]";
  H.key h "Escape";
  H.text h ~selector:"[data-note=ci2]";
  H.click h ~selector:"[data-note=ci2] .memory-delete";
  H.click h ~selector:"#instruction-remove-yes";
  H.reply h "remove_custom_instruction" {|{"removed":"ci2"}|};
  [%expect
    {|
    (Instruction_remove ci2)
    (Focus instruction-remove-yes)
    Maths Keep one memory note on my maths up to date. Delete this instruction? No session gets it any more.
    (Keep) (Delete)
    Close_dialog
    Maths Keep one memory note on my maths up to date. ci2 · saved 2026-11-02
    (Edit) (Delete)
    (Instruction_remove ci2)
    (Focus instruction-remove-yes)
    Instruction_remove_confirm
    (Rpc (method_ remove_custom_instruction) (params ((id ci2)))
     (tag Instruction_removed))
    |}]
;;

let%expect_test "the switch, /instructions on|off, and changes from elsewhere" =
  let h = opened () in
  H.click h ~selector:".instructions-enabled button";
  H.text h ~selector:".instructions-enabled";
  H.reply h "set_custom_instructions_enabled" {|{"enabled":false}|};
  H.event
    h
    (String.drop_suffix
       (sprintf
          {|{"type":"event","event":"custom_instructions_changed",%s|}
          (String.drop_prefix
             (json ~enabled:false [ note "ci3" "Answer briefly." ])
             1))
       0);
  section h;
  H.act h (Run "/instructions bogus");
  H.act h (Run "/instructions on");
  [%expect
    {|
    (Run "/instructions off")
    (Expire_toast (id 0) (after_ms 3700))
    (Rpc (method_ set_custom_instructions_enabled) (params ((enabled false)))
     (tag Instructions_enabled))
    Let the model change them
    When you state a lasting preference or standing instruction, the model adds it here or edits the one that covers it, and tells you. Off, only you change them; they stay in every session either way.
    (Let the model add and edit them (/instructions on))
    Custom instructions
    Standing instructions for the model, given to it in every session; it may also add or edit them when you state a lasting preference. Kept on the backend, for your account.
    Let the model change them
    When you state a lasting preference or standing instruction, the model adds it here or edits the one that covers it, and tells you. Off, only you change them; they stay in every session either way.
    (Let the model add and edit them (/instructions on))
    Instructions 1 instruction
    1 of 50 · about 23 tokens in every session
    Answer briefly. ci3 · saved 2026-11-03
    (Edit) (Delete)
    (Add instruction)
    (Expire_toast (id 2) (after_ms 4020))
    (Rpc (method_ set_custom_instructions_enabled) (params ((enabled true)))
     (tag Instructions_enabled))
    |}]
;;

let%expect_test "past the threshold, a warning says what they cost" =
  let long i = note (sprintf "ci%d" i) (String.make 900 'a') in
  let h = opened ~json:(json (List.init 7 ~f:(fun i -> long (i + 1)))) () in
  H.text h ~selector:".instructions-warning";
  H.elements h ~selector:".instructions-size";
  [%expect
    {| About 1,605 tokens in every session They are added to the context of every session, before you say anything: that is cost on every message and room the work no longer has. Trim them: merge near-duplicates, drop what no longer applies, and keep what is only sometimes needed in memory. |}]
;;

let%expect_test "a failed read says so, with a retry" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Settings_section Custom_instructions));
  H.fail
    h
    "get_custom_instructions"
    "custom_instructions.json is not valid JSON";
  H.text h ~selector:".memory-failed";
  H.click h ~selector:".memory-failed button";
  [%expect
    {|
    Your instructions
    The backend could not read them. custom_instructions.json is not valid JSON
    (Retry)
    (Run /instructions)
    (Focus dialog)
    (Rpc (method_ get_custom_instructions) (params ()) (tag Instructions_loaded))
    |}]
;;
