open! Core
module H = Harness

let running_state = H.state_json ~fields:[ "running", `True ] ()

let%expect_test
    "Enter sends, Shift+Enter is a newline, Alt+Enter queues a follow-up"
  =
  let h = H.create () in
  H.text h ~selector:".composer-row";
  [%expect
    {|
    []
    (Send (Enter))
    |}];
  H.show h ~selector:".send";
  [%expect
    {|
    <button type="button"
            title="Send (Enter)"
            aria-label="Send (Enter)"
            disabled=""
            class="btn primary send"
            @on_click>
      <icon class="send"> </icon>
    </button>
    <icon class="send"> </icon>
    |}];
  (* An empty editor sends nothing. *)
  H.key h "Enter";
  [%expect {| Send |}];
  H.type_ h "line one";
  H.key h "Enter" ~shift:true;
  [%expect {| (browser default) |}];
  H.type_ h "line one\nline two";
  H.key h "Enter";
  [%expect
    {|
    Send
    (Save_history ( "line one\
                   \nline two"))
    (Rpc (method_ prompt) (params ((text  "line one\
                                         \nline two")))
     (tag Show_error))
    |}];
  print_s [%sexp ((H.model h).draft : string)];
  [%expect {| "" |}];
  H.event h (sprintf {|{"event":"state","state":%s}|} running_state);
  H.show h ~selector:"#editor";
  [%expect
    {|
    <textarea id="editor"
              placeholder="Steer the agent (Enter) or queue a follow-up (Alt+Enter)…"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    |}];
  H.type_ h "use the other file";
  H.key h "Enter";
  H.type_ h "then run the tests";
  H.key h "Enter" ~alt:true;
  [%expect
    {|
    Send
    (Save_history ("use the other file"  "line one\
                                        \nline two"))
    (Rpc (method_ steer) (params ((text "use the other file"))) (tag Show_error))
    Send_follow_up
    (Save_history
     ("then run the tests" "use the other file"  "line one\
                                                \nline two"))
    (Rpc (method_ follow_up) (params ((text "then run the tests")))
     (tag Show_error))
    |}];
  (* Esc stops the run; when idle it does nothing. *)
  H.key h "Escape";
  [%expect
    {|
    Abort
    (Rpc (method_ abort) (params ()) (tag Restored))
    |}];
  H.event h (sprintf {|{"event":"state","state":%s}|} (H.state_json ()));
  H.key h "Escape";
  [%expect {| (browser default) |}]
;;

let image =
  { Prigh_protocol.Image.mime_type = "image/png"
  ; data = "iVBORw0KGgo="
  ; bytes = 8
  }
;;

let%expect_test "pending images: thumbnails, remove, sent with the prompt" =
  let h = H.create () in
  H.act h (Add_image image);
  H.act h (Add_image { image with mime_type = "image/jpeg" });
  H.show h ~selector:".pending-images";
  [%expect
    {|
    <div class="pending-images">
      <div class="pending-image">
        <img src="data:image/png;base64,iVBORw0KGgo="
             alt="[image: image/png, 8 B]"
             title="[image: image/png, 8 B]"/>
        <button type="button"
                title="Remove image"
                aria-label="Remove image"
                class="btn remove-image"
                @on_click>
          <icon class="close"> </icon>
        </button>
      </div>
      <div class="pending-image">
        <img src="data:image/jpeg;base64,iVBORw0KGgo="
             alt="[image: image/jpeg, 8 B]"
             title="[image: image/jpeg, 8 B]"/>
        <button type="button"
                title="Remove image"
                aria-label="Remove image"
                class="btn remove-image"
                @on_click>
          <icon class="close"> </icon>
        </button>
      </div>
    </div>
    |}];
  H.act h (Remove_image 1);
  (* An image alone can be sent. *)
  H.key h "Enter";
  [%expect
    {|
    Send
    (Rpc (method_ prompt)
     (params ((text "") (images (((mime_type image/png) (data iVBORw0KGgo=))))))
     (tag Show_error))
    |}];
  H.show h ~selector:".pending-images";
  [%expect {| |}];
  (* A slash command with an image attached is a prompt. *)
  H.act h (Add_image image);
  H.type_ h "/help";
  H.act h Send;
  [%expect
    {|
    (Save_history (/help))
    (Rpc (method_ prompt)
     (params
      ((text /help) (images (((mime_type image/png) (data iVBORw0KGgo=))))))
     (tag Show_error))
    |}]
;;

let%expect_test "prompt history: Up and Down at the first and last line" =
  let h = H.create () in
  H.act h (Load_history [ "newest"; "older\ntwo lines"; "oldest" ]);
  (* Down does nothing until browsing. *)
  H.key h "ArrowDown";
  H.type_ h "my draft";
  H.key h "ArrowUp" ~target:(Editor { cursor = 3 });
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (browser default)
    History_older
    newest
    |}];
  H.key h "ArrowUp" ~target:(Editor { cursor = 0 });
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    History_older
     "older\
    \ntwo lines"
    |}];
  (* Inside a multi-line entry the arrows move the caret. *)
  H.key h "ArrowDown" ~target:(Editor { cursor = 2 });
  H.key h "ArrowUp" ~target:(Editor { cursor = 8 });
  [%expect
    {|
    (browser default)
    (browser default)
    |}];
  H.key h "ArrowUp" ~target:(Editor { cursor = 0 });
  H.key h "ArrowUp" ~target:(Editor { cursor = 0 });
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    History_older
    (browser default)
    oldest
    |}];
  H.key h "ArrowDown";
  H.key h "ArrowDown";
  H.key h "ArrowDown";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    History_newer
    History_newer
    History_newer
    "my draft"
    |}];
  H.key h "ArrowDown";
  [%expect {| (browser default) |}];
  (* Sending saves it, newest first, without repeating the last entry. *)
  H.act h Send;
  H.type_ h "my draft";
  H.act h Send;
  [%expect
    {|
    (Save_history ("my draft" newest  "older\
                                     \ntwo lines" oldest))
    (Rpc (method_ prompt) (params ((text "my draft"))) (tag Show_error))
    (Save_history ("my draft" newest  "older\
                                     \ntwo lines" oldest))
    (Rpc (method_ prompt) (params ((text "my draft"))) (tag Show_error))
    |}]
;;

let%expect_test "@ completes paths from the backend" =
  let h = H.create () in
  H.type_ h "look at @src/ma";
  [%expect
    {| (Rpc (method_ list_paths) (params ((prefix src/ma))) (tag (Paths src/ma))) |}];
  (* Nothing to show until the backend answers. *)
  H.text h ~selector:".popup";
  [%expect {| |}];
  H.reply h "list_paths" {|["src/main.ml","src/main.mli","src/macros/"]|};
  H.text h ~selector:".popup";
  [%expect
    {|
    Files ↑↓ Tab Enter Esc
    src/main.ml
    src/macros/
    src/main.mli
    |}];
  H.key h "ArrowDown";
  H.key h "ArrowDown";
  H.key h "Tab";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Complete_move 1)
    (Complete_move 1)
    (Complete_accept (run false))
    "look at @src/main.mli "
    |}];
  (* Directories keep completing; a stale answer is ignored. *)
  H.type_ h "look at @src/m";
  H.type_ h "look at @src/macros/";
  H.reply h "list_paths" {|["src/main.ml"]|};
  H.text h ~selector:".popup";
  [%expect
    {|
    (Rpc (method_ list_paths) (params ((prefix src/m))) (tag (Paths src/m)))
    (Rpc (method_ list_paths) (params ((prefix src/macros/)))
     (tag (Paths src/macros/)))
    |}];
  H.reply h "list_paths" {|["src/macros/x.ml","src/macros/y/"]|};
  H.text h ~selector:".popup-items";
  H.key h "Enter";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    src/macros/y/
    src/macros/x.ml
    (Complete_accept (run true))
    (Rpc (method_ list_paths) (params ((prefix src/macros/y/)))
     (tag (Paths src/macros/y/)))
    "look at @src/macros/y/"
    |}];
  (* Esc closes the popup; it does not stop anything. *)
  H.reply h "list_paths" {|["src/macros/y/z.ml"]|};
  H.key h "Escape";
  H.text h ~selector:".popup";
  [%expect {| Complete_close |}];
  (* A failed listing shows nothing. *)
  H.type_ h "see @zz";
  H.fail h "list_paths" "no such directory";
  H.text h ~selector:".toasts";
  [%expect
    {| (Rpc (method_ list_paths) (params ((prefix zz))) (tag (Paths zz))) |}]
;;

let%expect_test "slash commands: the popup, arguments, running, unknown ones" =
  let h = H.create () in
  H.type_ h "/";
  H.text h ~selector:".popup";
  [%expect
    {|
    Commands ↑↓ Tab Enter Esc
    /help [command] show commands and keys, or a command's usage
    /hotkeys show the keyboard shortcuts
    /new start a new session
    /model [name] pick or switch the model
    /scoped-models pick the models Ctrl+P and Alt+P cycle through
    /thinking [off|low|on|high|max] pick or set the thinking level
    /change_default save the model and thinking level as the default for new sessions
    /verbosity [quiet|normal|verbose] how much of tool calls and thinking the transcript shows
    /confirm [on|off] ask before bash, write and edit run
    /compact [instructions] summarise older messages to free context
    /name [name] rename the session
    /session show the session's details and statistics
    /sessions search the saved sessions
    /switch [path] switch to a saved session
    /clone copy this session into a new one
    /fork start a new session from an earlier message
    /rewind go back to an earlier message in this session
    /tree show the session tree and move to any message in it
    /cd [path] change the working directory
    /host [name|backend] pick where tools run, and the directory there
    /export [path] export the transcript on the backend (markdown, or .jsonl)
    /import [path] import a session from a JSONL file on the backend
    /copy copy the last reply to the clipboard
    /btw <question> ask a side question without interrupting the run (not added to the conversation)
    /abort stop the current run
    /agents [cancel <n>] show background subagents and jobs, or cancel subagent n
    /jobs [id|kill <id>] list background jobs (output, kill), or show or kill one
    /login [provider] log in to a model provider (or /login custom)
    /logout [provider] remove a provider's login
    /auth show which providers are logged in
    /setusr [user] act as another user (superusers); without a user, pick one
    /signout sign out of this account
    /retry-backend-connection reconnect to the backend now
    /state show the session state as the backend reports it
    /clear clear the transcript view (the conversation is kept)
    /quit how to leave (close the tab; /signout signs out)
    |}];
  H.type_ h "/mod";
  H.key h "Tab";
  print_s [%sexp ((H.model h).draft : string)];
  H.text h ~selector:".popup";
  [%expect
    {|
    (Complete_accept (run false))
    "/model "
    Models ↑↓ Tab Enter Esc
    Claude Opus 5.5 anthropic
    Claude Sonnet 5 anthropic
    GPT-6 openai
    DeepSeek Chat deepseek
    |}];
  H.type_ h "/model gpt";
  H.key h "Enter";
  [%expect
    {|
    (Complete_accept (run true))
    (Save_history ("/model openai/gpt-6"))
    (Rpc (method_ set_model) (params ((model openai/gpt-6))) (tag Show_error))
    |}];
  H.type_ h "/comp";
  H.key h "Enter";
  H.text h ~selector:".toast";
  [%expect
    {|
    (Complete_accept (run true))
    (Save_history (/compact "/model openai/gpt-6"))
    (Expire_toast (id 0) (after_ms 4000))
    (Rpc (method_ compact) (params ())
     (tag (Notice "Compacted the conversation")))
    Compacting the conversation…
    |}];
  H.reply h "compact" {|{"summary":"..."}|};
  H.text h ~selector:".toast";
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 4000))
    Compacting the conversation…
    Compacted the conversation
    |}];
  H.act h (Dismiss_toast 0);
  H.act h (Dismiss_toast 1);
  (* Unknown commands name the closest one. *)
  H.type_ h "/compcat";
  H.act h Send;
  H.type_ h "/frobnicate";
  H.act h Send;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Save_history (/compcat /compact "/model openai/gpt-6"))
    (Save_history (/frobnicate /compcat /compact "/model openai/gpt-6"))
    Unknown command /compcat. Did you mean /compact? (/help lists them)
    Unknown command /frobnicate: /help lists the commands.
    |}];
  (* A path with a question is a prompt, not a command. *)
  H.type_ h "/etc/hosts has a typo?";
  H.act h Send;
  [%expect
    {|
    (Save_history
     ("/etc/hosts has a typo?" /frobnicate /compcat /compact
      "/model openai/gpt-6"))
    (Rpc (method_ prompt) (params ((text "/etc/hosts has a typo?")))
     (tag Show_error))
    |}];
  H.type_ h "/help";
  H.key h "Enter";
  H.text h ~selector:".modal h2";
  [%expect
    {|
    (Complete_accept (run true))
    (Save_history
     (/help "/etc/hosts has a typo?" /frobnicate /compcat /compact
      "/model openai/gpt-6"))
    (Focus dialog)
    Commands and keys
    |}]
;;

let%expect_test "/cd completes directories" =
  let h = H.create () in
  H.type_ h "/cd ";
  [%expect
    {| (Rpc (method_ list_dirs) (params ((prefix ""))) (tag (Paths ""))) |}];
  H.reply h "list_dirs" {|["src/","test/"]|};
  H.key h "Enter";
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Complete_accept (run true))
    (Rpc (method_ list_dirs) (params ((prefix src/))) (tag (Paths src/)))
    "/cd src/"
    |}];
  H.reply h "list_dirs" "[]";
  H.key h "Enter";
  [%expect
    {|
    Send
    (Save_history ("/cd src/"))
    (Rpc (method_ set_cwd) (params ((path src/)))
     (tag (Notice "Working directory: src/")))
    |}];
  H.type_ h "/cd";
  H.act h Send;
  H.text h ~selector:".toast";
  [%expect
    {|
    (Save_history (/cd "/cd src/"))
    (Focus dialog-input)
    (Rpc (method_ list_dirs) (params ((prefix /work) (host backend)))
     (tag (Prompt_paths /work)))
    |}]
;;
