open! Core
module H = Harness

(* Settings' Memory: whether the model has it, and the notes it saved. *)

let notes_json =
  {|{"enabled":true,"notes":[
     {"id":"m1","title":"editor","text":"helix, with the default keymap",
      "saved":"2025-10-01 09:30:00.000000Z"},
     {"id":"m2","text":"Deploys go through the staging cluster first",
      "saved":"2025-10-02 14:05:00.000000Z"},
     {"id":"m3","title":"shell","text":"fish",
      "saved":"2025-10-03 08:00:00.000000Z"}]}|}
;;

let section h = H.text h ~selector:".settings-panel"

let opened ?(json = notes_json) () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Settings_section Memory);
    H.reply h "get_memory" json);
  h
;;

let%expect_test "opening reads the memory; empty, it says what memory is" =
  let h = H.create () in
  H.act h (Settings_section Memory);
  section h;
  H.reply h "get_memory" {|{"enabled":true,"notes":[]}|};
  section h;
  H.text h ~selector:".settings-nav";
  [%expect
    {|
    (Focus dialog)
    (Rpc (method_ get_memory) (params ()) (tag Memory_loaded))
    Memory
    What you asked the model to remember, for later sessions. Kept on the backend, for your account.
    Use memory
    The model saves a note only when you ask it to remember something, and looks one up when you refer back to it. Off, it is not given the memory tool, and your notes stay here. Reading your memory…
    (Stop giving the model your memory (/memory off))
    Memory
    What you asked the model to remember, for later sessions. Kept on the backend, for your account.
    Use memory
    The model saves a note only when you ask it to remember something, and looks one up when you refer back to it. Off, it is not given the memory tool, and your notes stay here.
    (Stop giving the model your memory (/memory off))
    Notes
    No notes yet
    Tell the model “remember that…” and it saves a note here. In a later session, ask about it (“what did I say about deploys?”) and it looks it up. It saves nothing you did not ask it to.
    (Add note)
    General
    (Appearance) (Sessions) (New sessions)
    Models
    (Models) (Usage)
    You
    (Memory) (About you) (Custom instructions)
    Integrations
    (MCP servers) (Git hosting)
    (Account)
    |}]
;;

let%expect_test "the notes, newest first; the filter narrows them" =
  let h = opened () in
  section h;
  H.input h ~selector:"#memory-filter" "deploy";
  H.text h ~selector:".memory-note";
  H.input h ~selector:"#memory-filter" "zzz";
  section h;
  [%expect
    {|
    Memory
    What you asked the model to remember, for later sessions. Kept on the backend, for your account.
    Use memory
    The model saves a note only when you ask it to remember something, and looks one up when you refer back to it. Off, it is not given the memory tool, and your notes stay here.
    (Stop giving the model your memory (/memory off))
    Notes 3 notes
    [] (Add note)
    shell fish Saved 2025-10-03
    (Edit) (Delete)
    Deploys go through the staging cluster first Saved 2025-10-02
    (Edit) (Delete)
    editor helix, with the default keymap Saved 2025-10-01
    (Edit) (Delete)
    (Memory_filter deploy)
    Deploys go through the staging cluster first Saved 2025-10-02
    (Edit) (Delete)
    (Memory_filter zzz)
    Memory
    What you asked the model to remember, for later sessions. Kept on the backend, for your account.
    Use memory
    The model saves a note only when you ask it to remember something, and looks one up when you refer back to it. Off, it is not given the memory tool, and your notes stay here.
    (Stop giving the model your memory (/memory off))
    Notes 3 notes
    [zzz] (Add note)
    No match
    No note matches “zzz”: change or clear the filter.
    |}]
;;

let%expect_test "edit a note: Enter in the title saves, the reply replaces it" =
  let h = opened () in
  H.click h ~selector:"[data-note=m1] button[aria-label='Edit editor']";
  H.show h ~selector:".memory-draft";
  H.input h ~selector:"#memory-text" "neovim";
  H.key h ~target:Field "Enter";
  H.text h ~selector:".memory-draft";
  H.reply
    h
    "save_memory"
    {|{"note":{"id":"m1","title":"editor","text":"neovim",
       "saved":"2025-10-04 10:00:00.000000Z"}}|};
  H.text h ~selector:"[data-note=m1]";
  [%expect
    {|
    (Memory_edit (m1))
    (Focus memory-text)
    <div class="border-border border-t first:border-t-0 flex flex-col flex-nowrap gap-x-4 gap-y-2.5 items-stretch memory-draft narrow:[&_.btn.icon]:size-11 narrow:[&_:is(.btn,select,.chip-button,.memory-filter,.memory-title-input)]:min-h-11 narrow:px-3.5 narrow:py-3.5 px-4 py-3 setting">
      <input id="memory-title"
             type="text"
             aria-label="Title"
             placeholder="Title (optional): a few words"
             maxlength="80"
             autocomplete="off"
             class="aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 disabled:pointer-events-none file:bg-transparent file:border-0 file:font-medium file:h-6 file:inline-flex file:text-foreground file:text-sm focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 h-8 md:text-sm memory-title-input min-w-0 narrow:h-11 narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-1 rounded-lg text-base transition-colors w-full"
             #value="editor"
             @on_input/>
      <textarea id="memory-text"
                aria-label="Note"
                placeholder="What the model should remember"
                rows="3"
                maxlength="2000"
                class="aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-transparent border border-input dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:disabled:bg-input/80 disabled:bg-input/50 disabled:cursor-not-allowed disabled:opacity-50 field-sizing-content flex focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 md:text-sm memory-text-input min-h-[4.5em] narrow:text-base outline-none placeholder:text-muted-foreground px-2.5 py-2 resize-y rounded-lg text-base transition-colors w-full"
                #value="helix, with the default keymap"
                @on_input> </textarea>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>
      <div class="flex flex-wrap gap-2 items-center justify-end memory-draft-buttons">
        <span class="keyboard memory-draft-keys mr-auto text-muted-foreground/80 text-xs"> Ctrl+Enter saves, Esc cancels </span>
        <button type="button" title="Esc" class="btn ghost small" @on_click> Cancel </button>
        <button type="button"
                title="Ctrl+Enter (Enter in the title)"
                class="btn primary small"
                @on_click> Save </button>
      </div>
    </div>
    (Memory_text neovim)
    Dialog_accept
    (Rpc (method_ save_memory) (params ((id m1) (title editor) (text neovim)))
     (tag Memory_saved))
    [editor] [neovim]
    Ctrl+Enter saves, Esc cancels (Cancel) (Saving…)
    editor helix, with the default keymap Saved 2025-10-01
    (Edit) (Delete)
    |}]
;;

let%expect_test "add a note; Esc cancels one, leaving Settings open" =
  let h = opened () in
  H.click h ~selector:".memory-add";
  H.input h ~selector:"#memory-text" "half written";
  H.key h "Escape";
  H.elements h ~selector:".memory-draft";
  H.elements h ~selector:".settings-dialog";
  H.click h ~selector:".memory-add";
  H.input h ~selector:"#memory-title" "pager";
  H.input h ~selector:"#memory-text" "less -R";
  H.key h ~ctrl:true ~target:Multiline "Enter";
  H.fail h "save_memory" "memory is full (200 notes): forget some first";
  H.text h ~selector:".memory-draft";
  [%expect
    {|
    (Memory_edit ())
    (Focus memory-text)
    (Memory_text "half written")
    Close_dialog
    <div class="[&>.modal-body]:flex-auto [&>.modal-body]:grid [&>.modal-body]:grid-cols-[208px_minmax(0,1fr)] [&>.modal-body]:overflow-hidden [&>.modal-body]:p-0 [&>.modal-buttons]:border-t [&>.modal-head]:border-b h-[min(720px,88vh)] max-h-none modal settings-dialog w-[min(940px,100%)]">
    (Memory_edit ())
    (Focus memory-text)
    (Memory_title pager)
    (Memory_text "less -R")
    Dialog_accept
    (Rpc (method_ save_memory) (params ((title pager) (text "less -R")))
     (tag Memory_saved))
    [pager] [less -R] Memory is full (200 notes): forget some first
    Ctrl+Enter saves, Esc cancels (Cancel) (Save)
    |}]
;;

let%expect_test "delete asks first; Keep and Esc keep it" =
  let h = opened () in
  H.click h ~selector:"[data-note=m3] .memory-delete";
  H.text h ~selector:"[data-note=m3]";
  H.key h "Escape";
  H.text h ~selector:"[data-note=m3]";
  H.click h ~selector:"[data-note=m3] .memory-delete";
  H.click h ~selector:"#memory-forget-yes";
  H.reply
    h
    "forget_memory"
    {|{"note":{"id":"m3","title":"shell","text":"fish",
       "saved":"2025-10-03 08:00:00.000000Z"}}|};
  H.names h ~selector:".memory-note .setting-name";
  [%expect
    {|
    (Memory_forget m3)
    (Focus memory-forget-yes)
    shell fish Delete this note? The model can no longer recall it.
    (Keep) (Delete)
    Close_dialog
    shell fish Saved 2025-10-03
    (Edit) (Delete)
    (Memory_forget m3)
    (Focus memory-forget-yes)
    Memory_forget_confirm
    (Rpc (method_ forget_memory) (params ((id m3))) (tag Memory_forgot))
    span "shell"
    span "editor"
    |}]
;;

let%expect_test "the switch, /memory on|off, and memory_changed from elsewhere" =
  let h = opened () in
  H.click h ~selector:".memory-enabled button";
  H.text h ~selector:".memory-enabled";
  H.reply h "set_memory_enabled" {|{"enabled":false}|};
  H.event
    h
    {|{"type":"event","event":"memory_changed","enabled":true,"notes":[
       {"id":"m4","title":"tabs","text":"two spaces",
        "saved":"2025-10-05 08:00:00.000000Z"}]}|};
  section h;
  H.act h (Run "/memory bogus");
  H.act h (Run "/memory off");
  [%expect
    {|
    (Run "/memory off")
    (Expire_toast (id 0) (after_ms 3380))
    (Rpc (method_ set_memory_enabled) (params ((enabled false)))
     (tag Memory_enabled))
    Use memory
    The model saves a note only when you ask it to remember something, and looks one up when you refer back to it. Off, it is not given the memory tool, and your notes stay here.
    (Give the model your memory (/memory on))
    Memory
    What you asked the model to remember, for later sessions. Kept on the backend, for your account.
    Use memory
    The model saves a note only when you ask it to remember something, and looks one up when you refer back to it. Off, it is not given the memory tool, and your notes stay here.
    (Stop giving the model your memory (/memory off))
    Notes 1 note
    [] (Add note)
    tabs two spaces Saved 2025-10-05
    (Edit) (Delete)
    (Expire_toast (id 2) (after_ms 3380))
    (Rpc (method_ set_memory_enabled) (params ((enabled false)))
     (tag Memory_enabled))
    |}]
;;

let%expect_test "a failed read says so, with a retry" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Settings_section Memory));
  H.fail h "get_memory" "memory.json is not valid JSON: fix or remove it";
  section h;
  H.click h ~selector:".memory-failed button";
  [%expect
    {|
    Memory
    What you asked the model to remember, for later sessions. Kept on the backend, for your account.
    Use memory
    The model saves a note only when you ask it to remember something, and looks one up when you refer back to it. Off, it is not given the memory tool, and your notes stay here.
    (Stop giving the model your memory (/memory off))
    Notes
    Your notes
    The backend could not read them. memory.json is not valid JSON: fix or remove it
    (Retry)
    (Run /memory)
    (Focus dialog)
    (Rpc (method_ get_memory) (params ()) (tag Memory_loaded))
    |}]
;;
