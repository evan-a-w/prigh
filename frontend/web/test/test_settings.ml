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
    General
    (Appearance) (Sessions) (New sessions)
    Models
    (Models) (Usage)
    You
    (Memory) (About you) (Custom instructions)
    Integrations
    (MCP servers) (Git hosting)
    (Account)
    Appearance
    How this page looks. Saved for your account, so it follows you to every browser you sign in on.
    Look
    Theme
    The page's colours.
    (Original) (Dark) (Light)
    Accent
    Your turns, the selection, focus and work in progress.
    (Orange (/accent orange)) (Blue (/accent blue)) (Green (/accent green)) (Purple (/accent purple)) (Pink (/accent pink)) (Teal (/accent teal)) (Yellow (/accent yellow))
    [#f0b44c] Custom
    Corners
    How round buttons, cards and dialogs are.
    (Sharp) (Small) (Default) (Round)
    Font
    Prose and the interface.
    (Recursive) (Inter) (System)
    Code font
    Code, commands and their output.
    (Recursive) (System)
    Layout
    Transcript detail This browser
    How much of tool calls and thinking the transcript shows (Ctrl+O) .
    (Quiet) (Normal) (Verbose)
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
    <div class="[&>.modal-body]:flex-auto [&>.modal-body]:grid [&>.modal-body]:grid-cols-[208px_minmax(0,1fr)] [&>.modal-body]:overflow-hidden [&>.modal-body]:p-0 [&>.modal-buttons]:border-border [&>.modal-buttons]:border-t [&>.modal-head]:border-b [&>.modal-head]:border-border h-[min(720px,88vh)] max-h-none modal settings-dialog w-[min(940px,100%)]">
    Close_dialog
    (Focus editor)
    Open_settings
    (Focus dialog)
    <div class="[&>.modal-body]:flex-auto [&>.modal-body]:grid [&>.modal-body]:grid-cols-[208px_minmax(0,1fr)] [&>.modal-body]:overflow-hidden [&>.modal-body]:p-0 [&>.modal-buttons]:border-border [&>.modal-buttons]:border-t [&>.modal-head]:border-b [&>.modal-head]:border-border h-[min(720px,88vh)] max-h-none modal settings-dialog w-[min(940px,100%)]">
    Dialog_accept
    (Focus editor)
    |}];
  (* It owns the keyboard: the page under it is inert. *)
  H.quiet h (fun () -> H.act h Open_settings);
  H.elements h ~selector:".sidebar[inert]";
  H.elements h ~selector:".composer[inert]";
  [%expect
    {|
    <aside class="bg-sidebar loon sidebar text-sidebar-foreground text-sm">
    <footer class="composer">
    |}]
;;

let%expect_test "notifications: the switch, and what the browser allows" =
  let h = H.create () in
  let notify () = H.text h ~selector:".setting.notify" in
  H.quiet h (fun () -> H.act h (Settings_section Sessions));
  (* Wanted, but the browser has not been asked: Allow asks it. *)
  notify ();
  [%expect
    {|
    Notifications This browser
    Tell you when a turn ends or something waits for you while this tab is hidden. The browser has not been asked yet: Allow asks it.
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
    Notifications This browser
    Tell you when a turn ends or something waits for you while this tab is hidden.
    (Turn notifications off (/notify off))
    <button type="button"
            role="switch"
            aria-checked="true"
            data-state="checked"
            data-slot="switch"
            data-size="default"
            aria-label="Notifications"
            title="Turn notifications off (/notify off)"
            class="after:-inset-x-3 after:-inset-y-2 after:absolute aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 border border-transparent dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:data-unchecked:bg-input/80 data-[size=default]:h-[18.4px] data-[size=default]:w-[32px] data-[size=sm]:h-[14px] data-[size=sm]:w-[24px] data-checked:bg-primary data-disabled:cursor-not-allowed data-disabled:opacity-50 data-unchecked:bg-input focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 group-has-[:focus-visible]/field-label:border-transparent group-has-[:focus-visible]/field-label:ring-0 group/switch inline-flex items-center narrow:-my-3 narrow:[&>span]:relative narrow:before:absolute narrow:before:bg-input narrow:before:inset-x-[6px] narrow:before:inset-y-[13px] narrow:before:rounded-full narrow:before:transition-colors narrow:bg-transparent! narrow:border-0 narrow:data-checked:before:bg-primary narrow:h-11! narrow:px-[7px] narrow:w-11! on outline-none peer relative rounded-full shrink-0 switch transition-all"
            #value="on"
            @on_click>
      <span data-state="checked"
            data-slot="switch-thumb"
            class="bg-background block dark:data-checked:bg-primary-foreground dark:data-unchecked:bg-foreground group-data-[size=default]/switch:data-checked:translate-x-[calc(100%-2px)] group-data-[size=default]/switch:data-unchecked:translate-x-0 group-data-[size=default]/switch:size-4 group-data-[size=sm]/switch:data-checked:translate-x-[calc(100%-2px)] group-data-[size=sm]/switch:data-unchecked:translate-x-0 group-data-[size=sm]/switch:size-3 pointer-events-none ring-0 rounded-full transition-transform"> </span>
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
    Notifications This browser
    Tell you when a turn ends or something waits for you while this tab is hidden.
    (Turn notifications on (/notify on))
    <div class="[&>.modal-body]:flex-auto [&>.modal-body]:grid [&>.modal-body]:grid-cols-[208px_minmax(0,1fr)] [&>.modal-body]:overflow-hidden [&>.modal-body]:p-0 [&>.modal-buttons]:border-border [&>.modal-buttons]:border-t [&>.modal-head]:border-b [&>.modal-head]:border-border h-[min(720px,88vh)] max-h-none modal settings-dialog w-[min(940px,100%)]">
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
    Notifications This browser
    Tell you when a turn ends or something waits for you while this tab is hidden.
    (Turn notifications off (/notify off))
    |}];
  H.act h (Set_notification_permission "denied");
  notify ();
  H.act h (Set_notification_permission "unsupported");
  notify ();
  [%expect
    {|
    Notifications This browser
    Tell you when a turn ends or something waits for you while this tab is hidden. The browser blocks them: allow notifications in its site settings (the icon left of the address), then turn them off and on.
    (Turn notifications off (/notify off))
    Notifications This browser
    Tell you when a turn ends or something waits for you while this tab is hidden. This page cannot show them: they need https or localhost. The tab's title still counts what happened.
    (Turn notifications off (/notify off))
    |}]
;;

let%expect_test "transcript detail, default mode, providers, shortcuts" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h Open_settings);
  H.names h ~selector:".setting.verbosity .segment";
  H.elements h ~selector:".setting.verbosity .segment";
  H.click h ~selector:".setting.verbosity .segment";
  H.elements h ~selector:".setting.verbosity .segment";
  print_s [%sexp ((H.model h).verbosity : Prigh_ui.Verbosity.t)];
  [%expect
    {|
    [radio] button "Quiet" -- /verbosity quiet
    [radio] button "Normal" -- /verbosity normal
    [radio] button "Verbose" -- /verbosity verbose
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 on outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    (Run "/verbosity quiet")
    (Cache_prefs (((key web.verbosity) (value (quiet)))))
    (Rpc (method_ set_prefs) (params ((prefs ((web.verbosity quiet)))))
     (tag Show_error))
    (Expire_toast (id 0) (after_ms 4420))
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 on outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    Quiet
    |}];
  (* The default mode is the config's: auto by default. Ask, auto-edit or
     auto, for new sessions only: this one keeps its mode. *)
  H.click h ~selector:".settings-tab-new-sessions";
  H.text h ~selector:".setting.default-mode";
  H.elements h ~selector:".setting.default-mode .segment";
  H.click h ~selector:".setting.default-mode .segment:nth-child(2)";
  H.elements h ~selector:".setting.default-mode .segment";
  H.elements h ~selector:".settings-dialog";
  [%expect
    {|
    (Settings_section New_sessions)
    Mode
    What asks before it runs: everything (Ask), all but file edits (Auto-edit), what the approval model leaves to you (Smart) or nothing (Auto); or a chat without the tool host (Chat).
    (Ask) (Auto-edit) (Smart) (Auto) (Chat)
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 on outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    (Set_default (Mode (Approval Auto_edit)))
    (Rpc (method_ set_config)
     (params
      ((config ((approval_mode auto-edit) (default_chat_mode false)))
       (defaults_only true)))
     (tag (Default_set (Mode (Approval Auto_edit)))))
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 on outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <button class="[&_svg:not([class*='size-'])]:size-3.5 [&_svg]:pointer-events-none [&_svg]:shrink-0 aria-invalid:border-destructive aria-invalid:ring-destructive/20 aria-pressed:bg-muted bg-transparent border border-input dark:aria-invalid:ring-destructive/40 data-[state=on]:bg-muted disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-[3px] focus-visible:ring-ring/50 focus-visible:z-10 focus:z-10 font-medium gap-1 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-end]:pr-1.5 group-data-[spacing=0]/toggle-group:has-data-[icon=inline-start]:pl-1.5 group-data-[spacing=0]/toggle-group:px-2 group-data-[spacing=0]/toggle-group:rounded-none group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:border-l-0 group-data-horizontal/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-l group-data-horizontal/toggle-group:data-[spacing=0]:first:rounded-l-lg group-data-horizontal/toggle-group:data-[spacing=0]:last:rounded-r-lg group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:border-t-0 group-data-vertical/toggle-group:data-[spacing=0]:data-[variant=outline]:first:border-t group-data-vertical/toggle-group:data-[spacing=0]:first:rounded-t-lg group-data-vertical/toggle-group:data-[spacing=0]:last:rounded-b-lg group/toggle h-7 has-data-[icon=inline-end]:pr-1.5 has-data-[icon=inline-start]:pl-1.5 hover:bg-muted hover:text-foreground inline-flex items-center justify-center min-w-7 narrow:flex-1 narrow:h-11 narrow:px-3 outline-none px-2.5 rounded-[min(var(--radius-md),12px)] rounded-lg segment shrink-0 text-[0.8rem] transition-all whitespace-nowrap">
    <div class="[&>.modal-body]:flex-auto [&>.modal-body]:grid [&>.modal-body]:grid-cols-[208px_minmax(0,1fr)] [&>.modal-body]:overflow-hidden [&>.modal-body]:p-0 [&>.modal-buttons]:border-border [&>.modal-buttons]:border-t [&>.modal-head]:border-b [&>.modal-head]:border-border h-[min(720px,88vh)] max-h-none modal settings-dialog w-[min(940px,100%)]">
    |}];
  H.click h ~selector:".settings-tab-models";
  H.text h ~selector:".setting.providers";
  [%expect
    {|
    (Settings_section Models)
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    Model providers
    Logged in to Anthropic.
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
  List.iter
    (Prigh_web.Settings_section.available ~superuser:false)
    ~f:(fun section ->
      H.quiet h (fun () -> H.act h (Settings_section section));
      print_endline ("-- " ^ Prigh_web.Settings_section.name section);
      List.iter [ "button"; "[role]" ] ~f:(fun selector ->
        H.names h ~selector:(".settings-panel " ^ selector)));
  print_endline "-- around them";
  List.iter
    [ ".modal-head button"
    ; ".settings-nav"
    ; ".settings-tab"
    ; ".modal-buttons button"
    ]
    ~f:(fun selector -> H.names h ~selector:(".settings-dialog " ^ selector));
  [%expect
    {|
    -- Appearance
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
    [radio] button "Sharp" -- square corners (/corners sharp)
    [radio] button "Small" -- slightly rounded (/corners small)
    [radio] button "Default" -- rounded, as prigh-web has been (/corners default)
    [radio] button "Round" -- softer, rounder (/corners round)
    [radio] button "Recursive" -- Recursive Sans, prigh-web's own (/font recursive)
    [radio] button "Inter" -- Inter, neutral and compact (/font inter)
    [radio] button "System" -- this device's interface font (/font system)
    [radio] button "Recursive" -- Recursive Mono, prigh-web's own (/code-font recursive)
    [radio] button "System" -- this device's monospace font (/code-font system)
    [radio] button "Quiet" -- /verbosity quiet
    [radio] button "Normal" -- /verbosity normal
    [radio] button "Verbose" -- /verbosity verbose
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
    [radiogroup] div "Corners"
    [radio] button "Sharp" -- square corners (/corners sharp)
    [radio] button "Small" -- slightly rounded (/corners small)
    [radio] button "Default" -- rounded, as prigh-web has been (/corners default)
    [radio] button "Round" -- softer, rounder (/corners round)
    [radiogroup] div "Font"
    [radio] button "Recursive" -- Recursive Sans, prigh-web's own (/font recursive)
    [radio] button "Inter" -- Inter, neutral and compact (/font inter)
    [radio] button "System" -- this device's interface font (/font system)
    [radiogroup] div "Code font"
    [radio] button "Recursive" -- Recursive Mono, prigh-web's own (/code-font recursive)
    [radio] button "System" -- this device's monospace font (/code-font system)
    [radiogroup] div "Transcript detail"
    [radio] button "Quiet" -- /verbosity quiet
    [radio] button "Normal" -- /verbosity normal
    [radio] button "Verbose" -- /verbosity verbose
    -- Sessions
    [switch] button "Archive sessions after" -- Archive only by hand (/auto-archive off)
    button "Allow" -- Ask the browser to allow notifications (/notify on)
    [switch] button "Notifications" -- Turn notifications off (/notify off)
    [group] div "Archive sessions after"
    [switch] button "Archive sessions after" -- Archive only by hand (/auto-archive off)
    [switch] button "Notifications" -- Turn notifications off (/notify off)
    -- New sessions
    button "New sessions' model: The backend's choice" -- Choose the model new sessions start with
    button "This session\226\128\153s" -- Start new sessions with Claude Opus 5.5 · thinking provider's default, its thinking level too (/change_default)
    [radio] button "Off" -- New sessions think at off
    [radio] button "Provider's default" -- New sessions think at provider's default
    [radio] button "Minimal" -- New sessions think at minimal
    [radio] button "Low" -- New sessions think at low
    [radio] button "Medium" -- New sessions think at medium
    [radio] button "High" -- New sessions think at high
    [radio] button "Xhigh" -- New sessions think at xhigh
    [radio] button "Max" -- New sessions think at max
    [radio] button "Ask" -- New sessions start in Ask mode
    [radio] button "Auto-edit" -- New sessions start in Auto-edit mode
    [radio] button "Smart" -- New sessions start in Smart mode
    [radio] button "Auto" -- New sessions start in Auto mode
    [radio] button "Chat" -- New sessions start in Chat mode
    button "New sessions' tool host: Where you are" -- Choose the tool host new sessions start on (/default-host)
    button "Add" -- Add a model to the end of the chain (/fallback MODEL…)
    [switch] button "Budget" -- Stop sessions starting turns past a cost (/default-budget)
    [radiogroup] div "New sessions' thinking"
    [radio] button "Off" -- New sessions think at off
    [radio] button "Provider's default" -- New sessions think at provider's default
    [radio] button "Minimal" -- New sessions think at minimal
    [radio] button "Low" -- New sessions think at low
    [radio] button "Medium" -- New sessions think at medium
    [radio] button "High" -- New sessions think at high
    [radio] button "Xhigh" -- New sessions think at xhigh
    [radio] button "Max" -- New sessions think at max
    [radiogroup] div "New sessions' mode"
    [radio] button "Ask" -- New sessions start in Ask mode
    [radio] button "Auto-edit" -- New sessions start in Auto-edit mode
    [radio] button "Smart" -- New sessions start in Smart mode
    [radio] button "Auto" -- New sessions start in Auto mode
    [radio] button "Chat" -- New sessions start in Chat mode
    [group] div "Budget"
    [switch] button "Budget" -- Stop sessions starting turns past a cost (/default-budget)
    -- Models
    button "Manage providers" -- /auth (/login, /logout)
    button "Drafting model: Default" -- Choose the model (/generate-model)
    button "Approval model: Default" -- Choose the model (/approval-model)
    button "Naming model: Default" -- Choose the model (/naming model)
    button "Summary model: The conversation's model" -- Choose the model (/compaction model)
    [switch] button "Compact at" -- Only at 80% of the model's window (/compaction at off)
    [switch] button "Mask old tool outputs" -- Keep old tool outputs (/compaction mask off)
    [group] div "Compact at"
    [switch] button "Compact at" -- Only at 80% of the model's window (/compaction at off)
    [switch] button "Mask old tool outputs" -- Keep old tool outputs (/compaction mask off)
    -- Usage
    [switch] button "Resume after a usage limit" -- Stay stopped at a usage limit (/resume off)
    button "Reading\226\128\166"
    [switch] button "Resume after a usage limit" -- Stay stopped at a usage limit (/resume off)
    -- Memory
    [switch] button "Use memory" -- Stop giving the model your memory (/memory off)
    [switch] button "Use memory" -- Stop giving the model your memory (/memory off)
    -- About you
    [radio] button "Not set" -- Leave it out
    [radio] button "Metric" -- Metric (°C, km, kg)
    [radio] button "Imperial" -- Imperial (miles, stone; °C)
    [radio] button "US" -- US customary (°F, miles, lb)
    [radio] button "Not set" -- Leave it out
    [radio] button "D/M/Y" -- D/M/Y (25/12/2026)
    [radio] button "M/D/Y" -- M/D/Y (12/25/2026)
    [radio] button "ISO" -- ISO (2026-12-25)
    button "Save" -- Ctrl+Enter
    [radiogroup] div "Units"
    [radio] button "Not set" -- Leave it out
    [radio] button "Metric" -- Metric (°C, km, kg)
    [radio] button "Imperial" -- Imperial (miles, stone; °C)
    [radio] button "US" -- US customary (°F, miles, lb)
    [radiogroup] div "Date format"
    [radio] button "Not set" -- Leave it out
    [radio] button "D/M/Y" -- D/M/Y (25/12/2026)
    [radio] button "M/D/Y" -- M/D/Y (12/25/2026)
    [radio] button "ISO" -- ISO (2026-12-25)
    -- Custom instructions
    [switch] button "Let the model change them" -- Only you change them (/instructions off)
    [switch] button "Let the model change them" -- Only you change them (/instructions off)
    -- MCP servers
    -- Git hosting
    button "Connect GitHub (github.com) with a token" -- /login github token
    button "Connect GitHub (github.com)" -- /login github
    button "Connect Forgejo (codeberg.org) with a token" -- /login forgejo codeberg.org token
    button "Connect Forgejo (codeberg.org)" -- /login forgejo codeberg.org
    button "Add host\226\128\166" -- /login forgejo HOST
    -- Account
    -- around them
    button "Close" -- Close (Esc)
    [tablist] div "Settings sections"
    [tab] button "Appearance" -- /settings appearance
    [tab] button "Sessions" -- /settings sessions
    [tab] button "New sessions" -- /settings new-sessions
    [tab] button "Models" -- /settings models
    [tab] button "Usage" -- /settings usage
    [tab] button "Memory" -- /settings memory
    [tab] button "About you" -- /settings user-info
    [tab] button "Custom instructions" -- /settings instructions
    [tab] button "MCP servers" -- /settings mcp
    [tab] button "Git hosting" -- /settings git
    [tab] button "Account" -- /settings account
    button "Keyboard shortcuts" -- /hotkeys
    button "Done"
    |}]
;;

let%expect_test "an open backend has no account to show" =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Settings_section Account));
  H.text h ~selector:".settings-panel";
  [%expect
    {|
    Account
    Who you are on this backend.
    No sign-in
    This backend lets anyone in: there is no account to switch or sign out of.
    |}]
;;

(* The sections down the side are tabs: a click or the arrows show one, and
   the arrows move the focus with it; /settings SECTION opens at one. *)
let%expect_test "sections: clicks, arrows, Home and End, /settings SECTION" =
  let h = H.create () in
  let shown () =
    H.text h ~selector:".settings-tab[aria-selected=true]";
    H.text h ~selector:".settings-panel-head h3"
  in
  H.quiet h (fun () -> H.act h Open_settings);
  shown ();
  H.show h ~selector:".settings-tab-appearance";
  [%expect
    {|
    (Appearance)
    Appearance
    <button id="settings-tab-appearance"
            type="button"
            role="tab"
            data-settings-tab="appearance"
            aria-selected="true"
            aria-controls="settings-panel"
            aria-label="Appearance"
            title="/settings appearance"
            tabindex="0"
            class="aria-selected:[&>svg]:text-primary aria-selected:bg-sidebar-accent aria-selected:font-medium aria-selected:text-sidebar-accent-foreground flex focus-visible:ring-2 focus-visible:ring-ring gap-2 h-8 hover:bg-sidebar-accent hover:text-sidebar-accent-foreground items-center min-w-0 on outline-none px-2 rounded-md settings-tab settings-tab-appearance text-[13px] text-left text-sidebar-foreground/80 w-full"
            @on_click>
      <icon class="palette"> </icon>
      <span class="min-w-0 settings-tab-name truncate"> Appearance </span>
    </button>
    |}];
  H.click h ~selector:".settings-tab-git";
  shown ();
  [%expect
    {|
    (Settings_section Git_hosting)
    (Git hosting)
    Git hosting
    |}];
  H.key h "ArrowDown" ~target:Settings_tab;
  shown ();
  H.key h "ArrowDown" ~target:Settings_tab;
  shown ();
  H.key h "ArrowUp" ~target:Settings_tab;
  H.key h "ArrowLeft" ~target:Settings_tab;
  shown ();
  H.key h "End" ~target:Settings_tab;
  shown ();
  H.key h "Home" ~target:Settings_tab;
  H.key h "ArrowUp" ~target:Settings_tab;
  shown ();
  [%expect
    {|
    (Settings_move Next)
    (Focus settings-tab-account)
    (Account)
    Account
    (Settings_move Next)
    (Focus settings-tab-appearance)
    (Appearance)
    Appearance
    (Settings_move Prev)
    (Focus settings-tab-account)
    (Settings_move Prev)
    (Focus settings-tab-git)
    (Git hosting)
    Git hosting
    (Settings_move Last)
    (Focus settings-tab-account)
    (Account)
    Account
    (Settings_move First)
    (Focus settings-tab-appearance)
    (Settings_move Prev)
    (Focus settings-tab-account)
    (Account)
    Account
    |}];
  (* The arrows elsewhere on the page are the page's. *)
  H.key h "ArrowDown" ~target:Control;
  shown ();
  [%expect
    {|
    (browser default)
    (Account)
    Account
    |}];
  (* Esc closes; the page opens again where it was. *)
  H.key h "Escape" ~target:Settings_tab;
  H.quiet h (fun () -> H.act h Open_settings);
  shown ();
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Account)
    Account
    |}];
  H.key h "Escape" ~target:Page;
  H.quiet h (fun () ->
    H.type_ h "/settings models";
    H.key h "Enter");
  shown ();
  [%expect
    {|
    Close_dialog
    (Focus editor)
    (Complete_accept (run true))
    (Models)
    Models
    |}];
  (* Superusers' only section, and a section there is not, say so. *)
  H.key h "Escape" ~target:Page;
  List.iter [ "/settings nix"; "/settings colours" ] ~f:(fun command ->
    H.quiet h (fun () ->
      H.type_ h command;
      H.key h "Enter" ~ctrl:true);
    H.text h ~selector:".toast:last-child");
  [%expect
    {|
    Close_dialog
    (Focus editor)
    Send
    Nix store is only for superusers: try /settings appearance, /settings sessions, /settings new-sessions, /settings models, /settings usage, /settings memory, /settings user-info, /settings instructions, /settings mcp, /settings git, /settings account. (Dismiss)
    Send
    No settings section "colours": try /settings appearance, /settings sessions, /settings new-sessions, /settings models, /settings usage, /settings memory, /settings user-info, /settings instructions, /settings mcp, /settings git, /settings account. (Dismiss)
    |}]
;;

let%expect_test "superusers have the Nix store's section" =
  let h = H.create () in
  H.act
    h
    (Hello
       { client_id = "c1"
       ; namespace = None
       ; user = Some "root"
       ; superuser = true
       ; features = []
       });
  H.quiet h (fun () -> H.act h Open_settings);
  H.text h ~selector:".settings-nav";
  [%expect
    {|
    General
    (Appearance) (Sessions) (New sessions)
    Models
    (Models) (Usage)
    You
    (Memory) (About you) (Custom instructions)
    Integrations
    (MCP servers) (Git hosting) (Nix store)
    (R root)
    |}]
;;

let%expect_test "/settings completes the sections" =
  let h = H.create () in
  H.type_ h "/settings ";
  H.text h ~selector:".popup";
  [%expect
    {|
    Settings sections ↑↓ Tab Enter Esc   Ctrl+Enter as typed
    appearance How this page looks. Saved for your account, so it follows you to every browser you sign in on.
    sessions How the session list shows sessions, and how this browser tells you that something happened. Kept on the backend.
    new-sessions What a new session starts with. A change applies from the next new session: open ones keep theirs. Kept on the backend.
    models The providers whose models you use, the models prigh uses for its own work, and when long conversations are summarised. Kept on the backend.
    usage How much of your Claude and ChatGPT subscriptions' usage limits you have used, as the providers report it (read when you open this page), and whether a run stopped by one carries on when it resets.
    memory What you asked the model to remember, for later sessions. Kept on the backend, for your account.
    user-info What the model should know about you, given to it in every session. Kept on the backend, for your account.
    instructions Standing instructions for the model, given to it in every session; it may also add or edit them when you state a lasting preference. Kept on the backend, for your account.
    mcp Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.
    git The hosts agents push branches to and open pull requests on.
    account Who you are on this backend.
    |}]
;;

let%expect_test
    "a phone: the list of sections, one a level down, back to the list"
  =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Set_narrow true));
  H.act h Open_settings;
  settings h;
  (* A section, by tap: its page, with the way back. *)
  H.click h ~selector:".settings-home-row[data-settings-tab=models]";
  H.elements h ~selector:".settings-dialog .modal-head > *";
  H.elements h ~selector:".settings-home";
  H.click h ~selector:".modal-back";
  H.elements h ~selector:".settings-home";
  (* /settings SECTION goes straight to it. *)
  H.key h "Escape" ~target:Page;
  H.type_ h "/settings account";
  H.act h Send;
  H.text h ~selector:".settings-dialog .modal-head h2";
  [%expect
    {|
    (Focus dialog)
    Settings
    (Close (Esc))
    General
    (Appearance How this page looks. Saved for your account, so it follows you to every browser you sign in on.) (Sessions How the session list shows sessions, and how this browser tells you that something happened. Kept on the backend.) (New sessions What a new session starts with. A change applies from the next new session: open ones keep theirs. Kept on the backend.)
    Models
    (Models The providers whose models you use, the models prigh uses for its own work, and when long conversations are summarised. Kept on the backend.) (Usage How much of your Claude and ChatGPT subscriptions' usage limits you have used, as the providers report it (read when you open this page), and whether a run stopped by one carries on when it resets.)
    You
    (Memory What you asked the model to remember, for later sessions. Kept on the backend, for your account.) (About you What the model should know about you, given to it in every session. Kept on the backend, for your account.) (Custom instructions Standing instructions for the model, given to it in every session; it may also add or edit them when you state a lasting preference. Kept on the backend, for your account.)
    Integrations
    (MCP servers Tools the model can use from MCP servers. The servers here are kept on the backend for your account and run on every host your sessions use; a host's ~/.prigh/mcp.json and a project's .mcp.json add their own.) (Git hosting The hosts agents push branches to and open pull requests on.)
    (Account Who you are on this backend.)
    (Settings_section Models)
    (Rpc (method_ usage_limits) (params ()) (tag Usage_loaded))
    <button class="btn ghost modal-back">
    <h2 class="">
    <button class="btn ghost icon">
    Settings_home
    <nav class="flex flex-col loon pb-[max(16px,var(--safe-b))] pl-[max(12px,var(--safe-l))] px-[max(12px,var(--safe-r))] settings-home">
    Close_dialog
    (Focus editor)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/settings account") (max 500))) (tag Ignore))
    (Focus dialog)
    Account
    |}]
;;
