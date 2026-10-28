open! Core
module H = Harness

(* The settings page: opened from the sidebar's gear, /settings or Ctrl+,;
   Esc or Done closes it. Its controls run the commands that change them and
   leave it open. *)

let settings h = H.text h ~selector:".settings-dialog"

let%expect_test "the gear, /settings and Ctrl+, open it; Esc closes it" =
  let h = H.create () in
  H.click h ~selector:".sidebar-footer .settings-toggle";
  settings h;
  [%expect
    {|
    Open_settings
    (Focus dialog)
    Settings
    (Close (Esc))
    Appearance
    Theme Follows you to every browser you sign in on.
    (Original) (Dark) (Light)
    Accent Your turns, the selection, focus and work in progress.
    (Orange (/accent orange)) (Blue (/accent blue)) (Green (/accent green)) (Purple (/accent purple)) (Pink (/accent pink)) (Teal (/accent teal)) (Yellow (/accent yellow))
    [#f0b44c] Custom
    Preferences
    Notifications Tell you when a turn ends or something waits for you while this tab is hidden. The browser has not been asked yet: Allow asks it.
    (Allow) (Turn notifications off (/notify off))
    Tabs Show the open sessions as tabs over the chat (not on a phone). Saved for your account.
    (Turn session tabs on (/tabs on))
    Transcript detail How much of tool calls and thinking the transcript shows (Ctrl+O cycles).
    (Quiet) (Normal) (Verbose)
    On the backend
    Tool confirmation Ask before bash, edits and MCP tools run, in this session and new ones.
    (Ask before tools run (/confirm on))
    Model providers Logged in to Anthropic.
    (Manage providers)
    Git hosting
    GitHub github.com
    Not signed in.
    (Use a token) (Connect)
    Forgejo codeberg.org
    Not signed in.
    (Use a token) (Connect)
    Another Forgejo host A self-hosted Forgejo or Gitea, by its host name.
    (Add host…)
    Compaction
    Compact at Also summarise once the context passes this many tokens (default 200k), not only at 80% of the model's window.
    [200k] tokens
    (Off)
    Summary model The model that writes the summaries: a cheaper one costs less, but cannot reuse the conversation's prompt cache.
    (The conversation's model)
    Mask old tool outputs Replace tool outputs older than the last 10 turns with placeholders first, which often saves summarising.
    (Keep old tool outputs (/compaction mask off))
    Account
    No sign-in This backend lets anyone in: there is no account to switch or sign out of.
    (Keyboard shortcuts) (Done)
    |}];
  (* Esc: closed, nothing changed. *)
  let before = H.model h in
  H.key h "Escape" ~target:Page;
  H.elements h ~selector:".settings-dialog";
  print_s
    [%sexp
      (Poly.equal before.notifications (H.model h).notifications
       && Prigh_ui.Verbosity.equal before.verbosity (H.model h).verbosity
       : bool)];
  [%expect
    {|
    Close_dialog
    (Focus editor)
    true
    |}];
  H.type_ h "/settings";
  H.act h Send;
  H.elements h ~selector:".settings-dialog";
  H.key h "Escape" ~target:Page;
  H.key h "," ~ctrl:true;
  H.elements h ~selector:".settings-dialog";
  (* Enter (not on one of its controls) closes it too. *)
  H.key h "Enter" ~target:Page;
  H.elements h ~selector:".settings-dialog";
  [%expect
    {|
    (Rpc (method_ push_pref) (params ((key history) (value /settings) (max 500)))
     (tag Ignore))
    (Focus dialog)
    <div class="modal settings-dialog">
    Close_dialog
    (Focus editor)
    Open_settings
    (Focus dialog)
    <div class="modal settings-dialog">
    Dialog_accept
    (Focus editor)
    |}];
  (* It owns the keyboard: the page under it is inert. *)
  H.quiet h (fun () -> H.act h Open_settings);
  H.elements h ~selector:".sidebar[inert]";
  H.elements h ~selector:".composer[inert]";
  [%expect
    {|
    <aside class="sidebar">
    <footer class="composer">
    |}]
;;

let%expect_test "notifications: the switch, and what the browser allows" =
  let h = H.create () in
  let notify () = H.text h ~selector:".setting.notify" in
  H.quiet h (fun () -> H.act h Open_settings);
  (* Wanted, but the browser has not been asked: Allow asks it. *)
  notify ();
  [%expect
    {|
    Notifications Tell you when a turn ends or something waits for you while this tab is hidden. The browser has not been asked yet: Allow asks it.
    (Allow) (Turn notifications off (/notify off))
    |}];
  H.click h ~selector:".setting.notify .primary";
  [%expect
    {|
    (Run "/notify on")
    (Cache_prefs (((key web.notify) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.notify true)))))
     (tag Show_error))
    Request_notification_permission
    |}];
  H.act h (Set_notification_permission "granted");
  notify ();
  H.show h ~selector:".setting.notify .switch";
  [%expect
    {|
    Notifications Tell you when a turn ends or something waits for you while this tab is hidden.
    (Turn notifications off (/notify off))
    <button type="button"
            role="switch"
            aria-checked="true"
            aria-label="Notifications"
            title="Turn notifications off (/notify off)"
            class="on switch"
            @on_click>
      <span class="switch-knob"> </span>
    </button>
    |}];
  (* Off, and the page stays open. *)
  H.click h ~selector:".setting.notify .switch";
  notify ();
  H.elements h ~selector:".settings-dialog";
  [%expect
    {|
    (Run "/notify off")
    (Cache_prefs (((key web.notify) (value (false)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.notify false)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 3220))
    Notifications Tell you when a turn ends or something waits for you while this tab is hidden.
    (Turn notifications on (/notify on))
    <div class="modal settings-dialog">
    |}];
  H.click h ~selector:".setting.notify .switch";
  notify ();
  [%expect
    {|
    (Run "/notify on")
    (Cache_prefs (((key web.notify) (value (true)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.notify true)))))
     (tag Show_error))
    (Expire_toast (id 1) (after_ms 6000))
    Notifications Tell you when a turn ends or something waits for you while this tab is hidden.
    (Turn notifications off (/notify off))
    |}];
  H.act h (Set_notification_permission "denied");
  notify ();
  H.act h (Set_notification_permission "unsupported");
  notify ();
  [%expect
    {|
    Notifications Tell you when a turn ends or something waits for you while this tab is hidden. The browser blocks them: allow notifications in its site settings (the icon left of the address), then turn them off and on.
    (Turn notifications off (/notify off))
    Notifications Tell you when a turn ends or something waits for you while this tab is hidden. This page cannot show them: they need https or localhost. The tab's title still counts what happened.
    (Turn notifications off (/notify off))
    |}]
;;

let%expect_test "transcript detail, tool confirmation, providers, shortcuts" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  H.names h ~selector:".segment";
  H.elements h ~selector:".segment";
  H.click h ~selector:".segment";
  H.elements h ~selector:".segment";
  print_s [%sexp ((H.model h).verbosity : Prigh_ui.Verbosity.t)];
  [%expect
    {|
    button "Quiet" -- /verbosity quiet
    button "Normal" -- /verbosity normal
    button "Verbose" -- /verbosity verbose
    label "tokens"
    button "Off" -- Only at 80% of the model's window (/compaction at off)
    <button class="segment">
    <button class="on segment">
    <button class="segment">
    <label class="compact-at-field on segment">
    <button class="segment">
    (Run "/verbosity quiet")
    (Cache_prefs (((key web.verbosity) (value (quiet)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.verbosity quiet)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 4660))
    <button class="on segment">
    <button class="segment">
    <button class="segment">
    <label class="compact-at-field on segment">
    <button class="segment">
    Quiet
    |}];
  (* Tool confirmation is the config's: off (mode auto) by default. *)
  H.text h ~selector:".setting.confirm";
  H.elements h ~selector:".setting.confirm .switch";
  H.click h ~selector:".setting.confirm .switch";
  H.elements h ~selector:".setting.confirm .switch";
  H.elements h ~selector:".settings-dialog";
  [%expect
    {|
    Tool confirmation Ask before bash, edits and MCP tools run, in this session and new ones.
    (Ask before tools run (/confirm on))
    <button class="switch">
    (Run "/confirm on")
    (Rpc (method_ set_config) (params ((config ((approval_mode ask)))))
     (tag
      (Config_saved
       "Tool confirmation on (mode ask, also for new sessions): bash, job_input, edits and MCP tools ask first")))
    <button class="on switch">
    <div class="modal settings-dialog">
    |}];
  H.text h ~selector:".setting.providers";
  [%expect
    {|
    Model providers Logged in to Anthropic.
    (Manage providers)
    |}];
  H.click h ~selector:".setting.providers .btn";
  H.text h ~selector:".modal h2";
  [%expect
    {|
    (Run /auth)
    (Focus dialog)
    (Rpc (method_ auth_status) (params ()) (tag (Auth_status Show)))
    (Rpc (method_ forge_status) (params ()) (tag (Forge Status)))
    Providers
    |}];
  H.key h "Escape" ~target:Page;
  H.quiet h (fun () -> H.act h Open_settings);
  H.click h ~selector:".settings-dialog .shortcuts";
  H.text h ~selector:".modal h2";
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Run /hotkeys)
    (Focus dialog)
    Keyboard shortcuts
    |}]
;;

(* Every control is named by what it changes; the tooltip has its command. *)
let%expect_test "the names of the settings' controls" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  List.iter [ "button"; "[role]" ] ~f:(fun selector ->
    H.names h ~selector:(".settings-dialog " ^ selector));
  [%expect
    {|
    button "Close" -- Close (Esc)
    [radio] button "Original" -- graphite, warm greys (/theme original)
    [radio] button "Dark" -- neutral greys, only the accent in colour (/theme dark)
    [radio] button "Light" -- paper white (/theme light)
    [radio] button "Orange" -- Orange (/accent orange)
    [radio] button "Blue" -- Blue (/accent blue)
    [radio] button "Green" -- Green (/accent green)
    [radio] button "Purple" -- Purple (/accent purple)
    [radio] button "Pink" -- Pink (/accent pink)
    [radio] button "Teal" -- Teal (/accent teal)
    [radio] button "Yellow" -- Yellow (/accent yellow)
    button "Allow" -- Ask the browser to allow notifications (/notify on)
    [switch] button "Notifications" -- Turn notifications off (/notify off)
    [switch] button "Tabs" -- Turn session tabs on (/tabs on)
    button "Quiet" -- /verbosity quiet
    button "Normal" -- /verbosity normal
    button "Verbose" -- /verbosity verbose
    [switch] button "Tool confirmation" -- Ask before tools run (/confirm on)
    button "Manage providers" -- /auth (/login, /logout)
    button "Connect GitHub (github.com) with a token" -- /login github token
    button "Connect GitHub (github.com)" -- /login github
    button "Connect Forgejo (codeberg.org) with a token" -- /login forgejo codeberg.org token
    button "Connect Forgejo (codeberg.org)" -- /login forgejo codeberg.org
    button "Add host\226\128\166" -- /login forgejo HOST
    button "Off" -- Only at 80% of the model's window (/compaction at off)
    button "Summary model: The conversation's model" -- Choose the model (/compaction model)
    [switch] button "Mask old tool outputs" -- Keep old tool outputs (/compaction mask off)
    button "Keyboard shortcuts" -- /hotkeys
    button "Done"
    [radiogroup] div "Theme"
    [radio] button "Original" -- graphite, warm greys (/theme original)
    [radio] button "Dark" -- neutral greys, only the accent in colour (/theme dark)
    [radio] button "Light" -- paper white (/theme light)
    [radiogroup] div "Accent colour"
    [radio] button "Orange" -- Orange (/accent orange)
    [radio] button "Blue" -- Blue (/accent blue)
    [radio] button "Green" -- Green (/accent green)
    [radio] button "Purple" -- Purple (/accent purple)
    [radio] button "Pink" -- Pink (/accent pink)
    [radio] button "Teal" -- Teal (/accent teal)
    [radio] button "Yellow" -- Yellow (/accent yellow)
    [switch] button "Notifications" -- Turn notifications off (/notify off)
    [switch] button "Tabs" -- Turn session tabs on (/tabs on)
    [group] div "Transcript detail"
    [switch] button "Tool confirmation" -- Ask before tools run (/confirm on)
    [group] div "Compact at"
    [switch] button "Mask old tool outputs" -- Keep old tool outputs (/compaction mask off)
    |}]
;;

let%expect_test "an open backend has no account to show" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  H.text h ~selector:".settings-section:last-child";
  [%expect
    {|
    Account
    No sign-in This backend lets anyone in: there is no account to switch or sign out of.
    |}]
;;
