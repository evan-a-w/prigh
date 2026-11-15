open! Core
open Prigh_web
module H = Harness

let sessions =
  sprintf
    "[%s]"
    (String.concat
       ~sep:","
       [ H.session_json
           ~name:"Make the port configurable"
           ~first_prompt:"prigh serve should take its port from the config"
           ~updated_at:"2026-10-05 09:00:00Z"
           "s1"
       ; H.session_json
           ~first_prompt:"fix the parser bug"
           ~updated_at:"2026-10-05 08:00:00Z"
           "s2"
       ; H.session_json
           ~name:"Release notes"
           ~first_prompt:"write the release notes"
           ~updated_at:"2026-10-04 08:00:00Z"
           "s3"
       ])
;;

let palette h = H.text h ~selector:"#control-centre"

let%expect_test "Ctrl+K opens the control centre: the session's settings, \
                 recent sessions, a few commands"
  =
  let h = H.create ~sessions () in
  H.key h "k" ~ctrl:true;
  palette h;
  [%expect
    {|
    Open_control
    (Focus control-input)
    [] (Close)
    This session
    Set model Claude Fable 5.1
    Ctrl+L
    Set thinking medium
    Alt+T
    Set verbosity normal
    Ctrl+O
    Switch host
    Set mode ask
    Alt+M
    Context and cost 0% · $0.00
    Recent sessions
    fix the parser bug /work · 2h ago
    Release notes /work · 1d ago
    Commands
    /new start a new session
    /settings show the settings: notifications, transcript detail, your account
    /review review the session's changes file by file, beside the chat
    /files open or close the file browser beside the chat
    /help show commands and keys, or a command's usage
    ↑↓ move ↵ open ⌫ back esc close
    |}];
  H.show h ~selector:".control-input-row";
  [%expect {| |}];
  (* Again: closed. *)
  H.key h "k" ~ctrl:true ~target:Field;
  [%expect {| |}];
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  [%expect {| |}]
;;

let%expect_test "the sidebar's Search opens it; Esc closes it, nothing changed" =
  let h = H.create ~sessions () in
  H.text h ~selector:".sidebar-actions";
  [%expect {| |}];
  H.click h ~selector:"#control-open";
  H.input h ~selector:"#control-input" "verbos";
  H.key h "Escape" ~target:Field;
  print_s
    [%message
      ""
        ~dialog:(Option.is_some (H.model h).dialog : bool)
        ~verbosity:(Prigh_ui.Verbosity.name (H.model h).verbosity)];
  [%expect {| |}]
;;

let%expect_test "typing ranks the settings, their values, sessions and commands" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "verbos";
  palette h;
  H.show h ~selector:".control-row.selected";
  [%expect {| |}];
  H.input h ~selector:"#control-input" "default mode";
  palette h;
  [%expect {| |}]
;;

let%expect_test "a value applies in one step" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "verbosity verbose";
  H.key h "Enter" ~target:Field;
  print_s
    [%message
      ""
        ~dialog:(Option.is_some (H.model h).dialog : bool)
        ~verbosity:(Prigh_ui.Verbosity.name (H.model h).verbosity)];
  [%expect {| |}]
;;

let%expect_test "Enter on a setting opens its values; ⌫ in the empty filter \
                 goes back; Enter sets one and closes"
  =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "verbos";
  H.key h "Enter" ~target:Field;
  palette h;
  H.show h ~selector:".control-input-row";
  [%expect {| |}];
  H.key h "Backspace" ~target:Field;
  palette h;
  [%expect {| |}];
  H.key h "Enter" ~target:Field;
  H.key h "ArrowDown" ~target:Field;
  H.key h "ArrowDown" ~target:Field;
  H.show h ~selector:".control-row.selected";
  H.key h "Enter" ~target:Field;
  print_s
    [%message
      ""
        ~dialog:(Option.is_some (H.model h).dialog : bool)
        ~verbosity:(Prigh_ui.Verbosity.name (H.model h).verbosity)];
  [%expect {| |}]
;;

let%expect_test "a setting's values filter; a click sets one" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "set thinking";
  H.key h "Enter" ~target:Field;
  H.input h ~selector:"#control-input" "hi";
  palette h;
  H.click h ~selector:".control-row";
  [%expect {| |}]
;;

let%expect_test "the session's mode and a default through it" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "set mode";
  H.key h "Enter" ~target:Field;
  palette h;
  [%expect {| |}];
  H.key h "Escape" ~target:Field;
  H.act h Open_control;
  H.input h ~selector:"#control-input" "new sessions' mode: auto";
  palette h;
  H.key h "Enter" ~target:Field;
  [%expect {| |}]
;;

let%expect_test "a setting kept on the settings page opens it there" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "archive after";
  H.key h "Enter" ~target:Field;
  print_s
    [%message
      ""
        ~dialog:(Option.map (H.model h).dialog ~f:(fun d -> Sexp.to_string_hum ([%sexp_of: Dialog.t] d) |> String.prefix ~len:40) : string option)
        ~section:((H.model h).settings_section : Settings_section.t)];
  [%expect {| |}]
;;

let%expect_test "sessions: titles at once, the messages once the typing \
                 pauses; Enter switches"
  =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "port";
  H.input h ~selector:"#control-input" "port config";
  H.text h ~selector:".control-list";
  [%expect {| |}];
  (* The first pause came too soon: the query changed since. *)
  H.act h (Search_sessions 1);
  H.act h (Search_sessions 2);
  H.reply
    h
    "search_sessions"
    {|{"query":"port config","hits":[
      {"id":"s2","path":"/sessions/s2.jsonl","score":44,
       "snippet":{"text":"…then make the port configurable via the config file","ranges":[[17,4],[22,6],[43,6]]}},
      {"id":"s1","path":"/sessions/s1.jsonl","score":30,"snippet":null}]}|};
  H.text h ~selector:".control-list";
  H.show h ~selector:".control-snippet";
  [%expect {| |}];
  H.key h "ArrowDown" ~target:Field;
  H.key h "Enter" ~target:Field;
  [%expect {| |}]
;;

let%expect_test "nothing found says what to do; a command runs" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "zzzz";
  palette h;
  [%expect {| |}];
  H.input h ~selector:"#control-input" "/hotkeys";
  H.key h "Enter" ~target:Field;
  [%expect {| |}];
  H.act h Close_dialog;
  H.act h Open_control;
  H.input h ~selector:"#control-input" "rename";
  H.key h "Enter" ~target:Field;
  print_s [%sexp ((H.model h).draft : string)];
  [%expect {| |}]
;;

let%expect_test "switching sessions resets it" =
  let h = H.create ~sessions () in
  H.act h Open_control;
  H.input h ~selector:"#control-input" "release";
  H.key h "Enter" ~target:Field;
  print_s [%sexp (Option.is_some (H.model h).dialog : bool)];
  H.act h Open_control;
  palette h;
  [%expect {| |}]
;;

(* Every row of the settings page is in the control centre's table. *)
let%expect_test "each setting has an entry" =
  let h = H.create ~sessions () in
  H.act h (Hello { client_id = "c1"; namespace = None; user = None; superuser = true; features = [] });
  let covered =
    List.filter_map Control_setting.all ~f:Control_setting.page
    |> List.map ~f:(fun (section, name) -> Settings_section.name section, name)
  in
  let missing = ref [] in
  List.iter (Settings_section.available ~superuser:true) ~f:(fun section ->
    H.quiet h (fun () -> H.act h (Settings_section section));
    let names = H.texts h ~selector:".setting-name" in
    List.iter names ~f:(fun name ->
      if not
           (List.mem
              covered
              (Settings_section.name section, name)
              ~equal:[%equal: string * string])
      then missing := (Settings_section.name section, name) :: !missing));
  print_s [%sexp (List.rev !missing : (string * string) list)];
  [%expect {| |}]
;;
