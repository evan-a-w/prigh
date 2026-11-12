open! Core
module H = Harness

(* Settings' "Archive after": the config's [archive_after_days], what
   [/auto-archive] sets; every client lists the sessions again after it. *)

let config days =
  Jsonaf.to_string
    (`Object [ "scoped_models", `Array []; "archive_after_days", `Number days ])
;;

let settings () =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h Open_settings;
    H.act h (Settings_section Sessions));
  h
;;

let row h = H.text h ~selector:".setting.auto-archive"

let%expect_test "archive after: shown, typed, off, refused" =
  let h = settings () in
  row h;
  H.names h ~selector:".setting.auto-archive input";
  H.names h ~selector:".setting.auto-archive button";
  [%expect
    {|
    Archive after
    Sessions without activity for 7 days move to Archived (pinned and running ones never do).
    [7] days
    (Archive only by hand (/auto-archive off))
    input "Archive sessions after, in days" -- /auto-archive DAYS: up to 365 days
    [switch] button "Archive sessions after" -- Archive only by hand (/auto-archive off)
    |}];
  H.input h ~selector:"#auto-archive" "14";
  H.reply h "set_config" (config "14");
  row h;
  [%expect
    {|
    (Sessions_field_input Archive_days 14)
    (Sessions_field_commit Archive_days)
    (Rpc (method_ set_config) (params ((config ((archive_after_days 14)))))
     (tag Auto_archive_saved))
    (Expire_toast (id 0) (after_ms 4580))
    Archive after
    Sessions without activity for 14 days move to Archived (pinned and running ones never do).
    [14] days
    (Archive only by hand (/auto-archive off))
    |}];
  H.click h ~selector:".setting.auto-archive .switch";
  H.reply h "set_config" (config "0");
  row h;
  [%expect
    {|
    (Switch_number Archive_days false)
    (Rpc (method_ set_config) (params ((config ((archive_after_days 0)))))
     (tag Auto_archive_saved))
    (Expire_toast (id 1) (after_ms 5620))
    Archive after
    Sessions without activity this many days move to Archived (pinned and running ones never do). Off: sessions are archived only by hand.
    (Archive sessions without activity (/auto-archive DAYS))
    |}];
  (* On again: the 14 days it was. *)
  H.click h ~selector:".setting.auto-archive .switch";
  H.reply h "set_config" (config "14");
  row h;
  [%expect
    {|
    (Switch_number Archive_days true)
    (Rpc (method_ set_config) (params ((config ((archive_after_days 14)))))
     (tag Auto_archive_saved))
    (Expire_toast (id 2) (after_ms 4580))
    Archive after
    Sessions without activity for 14 days move to Archived (pinned and running ones never do).
    [14] days
    (Archive only by hand (/auto-archive off))
    |}];
  H.input h ~selector:"#auto-archive" "a fortnight";
  row h;
  H.input h ~selector:"#auto-archive" "400";
  H.key h "Enter" ~target:Field;
  row h;
  [%expect
    {|
    (Sessions_field_input Archive_days "a fortnight")
    (Sessions_field_commit Archive_days)
    Archive after
    Sessions without activity for 14 days move to Archived (pinned and running ones never do). “a fortnight” isn't a number of days from 0 to 365: type one like 7, or off to archive only by hand.
    [a fortnight] days
    (Archive only by hand (/auto-archive off))
    (Sessions_field_input Archive_days 400)
    (Sessions_field_commit Archive_days)
    Dialog_accept
    Archive after
    Sessions without activity for 14 days move to Archived (pinned and running ones never do). “400” isn't a number of days from 0 to 365: type one like 7, or off to archive only by hand.
    [400] days
    (Archive only by hand (/auto-archive off))
    |}]
;;

let run h command =
  H.type_ h command;
  H.act h Send
;;

let%expect_test "/auto-archive: shows, sets, refuses; the list follows" =
  let h = H.create () in
  let toast () = H.text h ~selector:".toast:last-child" in
  H.quiet h (fun () -> run h "/auto-archive");
  toast ();
  run h "/auto-archive 2.5d";
  H.reply h "set_config" (config "2.5");
  toast ();
  [%expect
    {|
    Sessions are archived after 7 days without activity (/auto-archive DAYS|off)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/auto-archive 2.5d") (max 500)))
     (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((archive_after_days 2.5)))))
     (tag Auto_archive_saved))
    (Expire_toast (id 1) (after_ms 4620))
    Sessions are archived after 2.5 days without activity (/auto-archive DAYS|off)
    |}];
  H.quiet h (fun () -> run h "/auto-archive soon");
  toast ();
  run h "/auto-archive off";
  H.fail h "set_config" "nope";
  toast ();
  [%expect
    {|
    "soon" is not a number of days from 0 to 365 (off: never archive by age); usage: /auto-archive or /auto-archive DAYS|off (Dismiss)
    (Rpc (method_ push_pref)
     (params ((key history) (value "/auto-archive off") (max 500))) (tag Ignore))
    (Rpc (method_ set_config) (params ((config ((archive_after_days 0)))))
     (tag Auto_archive_saved))
    (Rpc (method_ get_config) (params ()) (tag Config))
    Couldn't set when sessions are archived: nope (Dismiss)
    |}];
  (* The backend tells every client after a change. *)
  H.event h {|{"event":"sessions_changed"}|};
  [%expect {| (Rpc (method_ list_sessions) (params ()) (tag Sessions)) |}]
;;

let%expect_test "switched on with nothing to restore: the default, 7 days" =
  let h = settings () in
  H.quiet h (fun () ->
    H.event
      h
      (sprintf
         {|{"type":"event","event":"config_changed","config":%s}|}
         (config "0")));
  row h;
  H.click h ~selector:".setting.auto-archive .switch";
  [%expect
    {|
    Archive after
    Sessions without activity this many days move to Archived (pinned and running ones never do). Off: sessions are archived only by hand.
    (Archive sessions without activity (/auto-archive DAYS))
    (Switch_number Archive_days true)
    (Rpc (method_ set_config) (params ((config ((archive_after_days 7)))))
     (tag Auto_archive_saved))
    |}]
;;
