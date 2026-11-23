open! Core
module H = Harness

(* Chat mode: a session without the tool host. The chip and its menu, /chat,
   Alt+M, the composer, and the way to chat when no host can run a message. *)

let run h command =
  H.type_ h command;
  H.act h Send
;;

let toasts h = H.text h ~selector:".toast"

let no_host =
  "no tool host connected: connect one with `prigh tool-host -connect ...` or \
   a TUI, then pick it with /host, or talk without one in chat mode (/mode \
   chat)"
;;

let state ?(mode = "auto") ?(plan_mode = false) ?(chat_mode = false) ?no_host ()
  =
  H.state_json
    ~fields:
      ([ "approval_mode", `String mode
       ; ("plan_mode", if plan_mode then `True else `False)
       ; ("chat_mode", if chat_mode then `True else `False)
       ]
       @ Option.value_map no_host ~default:[] ~f:(fun r ->
         [ "no_host", `String r ]))
    ()
;;

let placeholder h = H.show h ~selector:"#editor"

(* The mode, as the context card names it (the pending one at once). *)
let mode_line h = H.text h ~selector:".context-card .mode-line"

let chrome h =
  mode_line h;
  H.text h ~selector:".host-chip";
  placeholder h
;;

let%expect_test "in chat mode: the chip, the host chip and the placeholder" =
  let h = H.create ~state:(state ~chat_mode:true ()) () in
  chrome h;
  [%expect
    {|
    Mode Chat: no tool host
    (no host)
    <textarea id="editor"
              placeholder="Chat, without a tool host: no files or commands. Ask anything…"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    |}];
  H.quiet h (fun () -> H.click h ~selector:".model-chip");
  H.click h ~selector:".model-access-button";
  H.text h ~selector:".model-access-menu";
  [%expect
    {|
    (Open_chip Model)
    Model_menu_access
    (Ask first Asks: edits & commands) (Auto-edit Asks: commands) (Smart Smart: a model decides) (Full access Auto: never asks) (Plan Plan mode) (Chat Chat: no tool host ✓)
    |}];
  H.key h "Escape" ~target:Field;
  H.quiet h (fun () -> H.act h (Set_narrow true));
  placeholder h;
  [%expect
    {|
    Close_dialog
    <textarea id="editor"
              placeholder="Chat: no files or commands…"
              aria-label="Message"
              dir="auto"
              rows="1"
              autocomplete="off"
              enterkeyhint="send"
              class="editor"
              #value=""
              @on_input> </textarea>
    |}]
;;

let%expect_test "/chat toggles; leaving returns to the mode before; Alt+M" =
  let h = H.create ~state:(state ~mode:"auto-edit" ()) () in
  run h "/chat";
  mode_line h;
  H.reply h "set_mode" (state ~mode:"auto-edit" ~chat_mode:true ());
  run h "/chat";
  mode_line h;
  H.reply h "set_mode" (state ~mode:"auto-edit" ());
  run h "/chat maybe";
  toasts h;
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /chat) (max 500)))
     (tag Ignore))
    (Rpc (method_ set_mode)
     (params ((approval_mode auto-edit) (plan_mode false) (chat_mode true)))
     (tag
      (Mode_set
       "Mode: chat (a chat without the tool host: no files, commands or project, only the web and other host-free tools, until you leave it (/chat off or Alt+M))")))
    Mode Chat: no tool host
    (Expire_toast (id 0) (after_ms 6000))
    (Rpc (method_ push_pref) (params ((key history) (value /chat) (max 500)))
     (tag Ignore))
    (Rpc (method_ set_mode)
     (params ((approval_mode auto-edit) (plan_mode false) (chat_mode false)))
     (tag
      (Mode_set
       "Mode: auto-edit (file edits run; bash, job_input and MCP tools ask first)")))
    Mode Asks: commands
    (Expire_toast (id 1) (after_ms 4420))
    (Rpc (method_ push_pref)
     (params ((key history) (value "/chat maybe") (max 500))) (tag Ignore))
    Mode: auto-edit (file edits run; bash, job_input and MCP tools ask first)
    Unknown argument “maybe”: use /chat on, /chat off, or /chat to toggle. (Dismiss)
    |}];
  (* Alt+M: plan, then chat, then round to ask. *)
  let h = H.create ~state:(state ~mode:"auto" ()) () in
  H.key h "m" ~alt:true;
  H.reply h "set_mode" (state ~mode:"auto" ~plan_mode:true ());
  H.key h "m" ~alt:true;
  mode_line h;
  [%expect
    {|
    Cycle_mode
    (Rpc (method_ set_mode)
     (params ((approval_mode auto) (plan_mode true) (chat_mode false)))
     (tag
      (Mode_set
       "Mode: plan (read-only planning, until you leave it (/plan off or Alt+M): no file edits; bash, job_input and MCP tools ask first)")))
    (Expire_toast (id 0) (after_ms 6000))
    Cycle_mode
    (Rpc (method_ set_mode)
     (params ((approval_mode auto) (plan_mode true) (chat_mode true)))
     (tag
      (Mode_set
       "Mode: chat (a chat without the tool host: no files, commands or project, only the web and other host-free tools, until you leave it (/chat off or Alt+M))")))
    Mode Chat: no tool host
    |}]
;;

let%expect_test "no tool host: the banner, the message kept, one click to chat" =
  let h = H.create ~state:(state ~no_host ()) () in
  H.show h ~selector:".no-host-banner";
  [%expect
    {|
    <div data-slot="alert"
         role="status"
         aria-live="polite"
         title="No tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host, or talk without one in chat mode (/mode chat)"
         class="*:[svg:not([class*='size-'])]:size-4 *:[svg]:row-span-2 *:[svg]:text-current *:[svg]:translate-y-0.5 [&>.icon]:shrink-0 [&>.icon]:text-primary bg-primary/10 border border-primary/40 flex gap-2 group/alert has-[>svg]:gap-x-2 has-[>svg]:grid-cols-[auto_1fr] has-data-[slot=alert-action]:pr-18 has-data-[slot=alert-action]:relative items-center loon no-host-banner pl-2.5 pr-1 px-2.5 py-1 relative rounded-lg text-[13.5px] text-card-foreground text-left w-full">
      <icon class="server_off"> </icon>
      <span class="flex-1 leading-tight line-clamp-2 min-w-0 no-host-banner-text"> No tool host connected </span>
      <button type="button"
              title="Switch this session to chat mode (no files or commands) and send your message"
              class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 active:not-aria-[haspopup]:translate-y-px aria-expanded:bg-muted aria-expanded:text-foreground aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-background bg-clip-padding border border-border dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:bg-input/30 dark:border-input dark:hover:bg-input/50 disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 font-medium gap-1.5 group/button h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground in-data-[slot=button-group]:rounded-lg inline-flex items-center justify-center narrow:h-11 no-host-banner-chat outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg select-none shrink-0 text-[0.8rem] text-primary transition-all whitespace-nowrap"
              @on_click>
        <icon class="message"> </icon>
        <span class="long max-[359px]:hidden"> Chat without a host </span>
        <span class="hidden max-[359px]:inline short"> Chat anyway </span>
      </button>
    </div>
    |}];
  (* Sent anyway: refused here, the message stays. *)
  run h "what is a monad?";
  toasts h;
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    No tool host connected: your message stays here. Chat without a host (over it) sends it as a chat, or connect a tool host and pick it (/host). (Dismiss)
    "what is a monad?"
    |}];
  (* The button: chat mode first, then the message once it is taken. *)
  H.click h ~selector:".no-host-banner-chat";
  [%expect
    {|
    Chat_without_host
    (Rpc (method_ set_mode)
     (params ((approval_mode auto) (plan_mode false) (chat_mode true)))
     (tag
      (Chat_then_send
       "Mode: chat (a chat without the tool host: no files, commands or project, only the web and other host-free tools, until you leave it (/chat off or Alt+M))")))
    |}];
  H.reply h "set_mode" (state ~chat_mode:true ~no_host ());
  print_s [%sexp ((H.model h).draft : string)];
  H.text h ~selector:".no-host-banner";
  [%expect
    {|
    (Expire_toast (id 1) (after_ms 6000))
    (Rpc (method_ push_pref)
     (params ((key history) (value "what is a monad?") (max 500))) (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text "what is a monad?")))
     (tag (Sent (text "what is a monad?") (images ()))))
    ""
    |}]
;;

let%expect_test "no tool host: the backend refusing the prompt keeps the draft" =
  let h = H.create ~state:(state ()) () in
  run h "hello";
  H.fail h "prompt" no_host;
  toasts h;
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value hello) (max 500)))
     (tag Ignore))
    Scroll_to_bottom
    (Rpc (method_ prompt) (params ((text hello)))
     (tag (Sent (text hello) (images ()))))
    Couldn't send: no tool host connected: connect one with `prigh tool-host -connect ...` or a TUI, then pick it with /host, or talk without one in chat mode (/mode chat). Your message is back in the editor. (Dismiss)
    hello
    |}]
;;

let%expect_test "the chat button failing leaves the message and says why" =
  let h = H.create ~state:(state ~no_host ()) () in
  H.type_ h "hi";
  H.click h ~selector:".no-host-banner-chat";
  H.fail h "set_mode" "no session";
  toasts h;
  print_s [%sexp ((H.model h).draft : string)];
  [%expect
    {|
    Chat_without_host
    (Rpc (method_ set_mode)
     (params ((approval_mode auto) (plan_mode false) (chat_mode true)))
     (tag
      (Chat_then_send
       "Mode: chat (a chat without the tool host: no files, commands or project, only the web and other host-free tools, until you leave it (/chat off or Alt+M))")))
    (Rpc (method_ get_state) (params ()) (tag State))
    Couldn't set the mode: no session (Dismiss)
    hi
    |}]
;;
