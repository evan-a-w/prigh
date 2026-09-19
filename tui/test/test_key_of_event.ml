open! Core
open! Expect_test_helpers_core
module Event = Bonsai_term.Event
module Ekey = Bonsai_term.Event.Key
module Mod = Bonsai_term.Event.Modifier
module Position = Bonsai_term.Position
module Key = Prigh_ui.Key
module Keymap = Prigh_ui.Keymap
module Key_of_event = Prigh_ui_term.Key_of_event

let key_press key mods = Event.Key_press { key; mods }

let show label (event : Event.t) =
  printf
    "%-28s %-48s -> %s\n"
    label
    (Sexp.to_string_hum (Event.sexp_of_t event))
    (Sexp.to_string_hum
       (Option.sexp_of_t Key.sexp_of_t (Key_of_event.key event)))
;;

let%expect_test "key_of_event: terminal event -> key" =
  let cases : (string * Event.t) list =
    [ "ASCII '\\015' (^O raw)", key_press (Ekey.ASCII '\015') []
    ; "ASCII 'o' + [Ctrl]", key_press (Ekey.ASCII 'o') [ Mod.Ctrl ]
    ; "ASCII '\\n'", key_press (Ekey.ASCII '\n') []
    ; "ASCII '\\r'", key_press (Ekey.ASCII '\r') []
    ; "Enter", key_press Ekey.Enter []
    ; "Enter + [Meta]", key_press Ekey.Enter [ Mod.Meta ]
    ; "ASCII '\\t'", key_press (Ekey.ASCII '\t') []
    ; "Tab", key_press Ekey.Tab []
    ; "Tab + [Shift]", key_press Ekey.Tab [ Mod.Shift ]
    ; "ASCII '\\t' + [Shift]", key_press (Ekey.ASCII '\t') [ Mod.Shift ]
    ; "ASCII '\\127'", key_press (Ekey.ASCII '\127') []
    ; "ASCII '\\008'", key_press (Ekey.ASCII '\008') []
    ; "Backspace", key_press Ekey.Backspace []
    ; "ASCII '\\027'", key_press (Ekey.ASCII '\027') []
    ; "Escape", key_press Ekey.Escape []
    ; "Arrow Up", key_press (Ekey.Arrow `Up) []
    ; "Arrow Down", key_press (Ekey.Arrow `Down) []
    ; "Arrow Left", key_press (Ekey.Arrow `Left) []
    ; "Arrow Right", key_press (Ekey.Arrow `Right) []
    ; "Home", key_press Ekey.Home []
    ; "End", key_press Ekey.End []
    ; "Page Up", key_press (Ekey.Page `Up) []
    ; "Page Down", key_press (Ekey.Page `Down) []
    ; "Delete", key_press Ekey.Delete []
    ; "ASCII 'a'", key_press (Ekey.ASCII 'a') []
    ; "Uchar U+00E9", key_press (Ekey.Uchar (Stdlib.Uchar.of_int 0xE9)) []
    ; "Uchar U+1F600", key_press (Ekey.Uchar (Stdlib.Uchar.of_int 0x1F600)) []
    ; "ASCII 'c' + [Ctrl]", key_press (Ekey.ASCII 'c') [ Mod.Ctrl ]
    ; "ASCII '\\003' (^C raw)", key_press (Ekey.ASCII '\003') []
    ; "ASCII '\\031' (^_ raw)", key_press (Ekey.ASCII '\031') []
    ; "ASCII 'j' + [Ctrl]", key_press (Ekey.ASCII 'j') [ Mod.Ctrl ]
    ; "ASCII 'p' + [Ctrl]", key_press (Ekey.ASCII 'p') [ Mod.Ctrl ]
    ; "ASCII 'p' + [Meta]", key_press (Ekey.ASCII 'p') [ Mod.Meta ]
    ; "ASCII 't' + [Ctrl]", key_press (Ekey.ASCII 't') [ Mod.Ctrl ]
    ; "Paste Start", Event.Paste `Start
    ; "Paste End", Event.Paste `End
    ; ( "Mouse Left"
      , Event.Mouse
          { kind = Left; position = { Position.x = 3; y = 4 }; mods = [] } )
    ]
  in
  List.iter cases ~f:(fun (label, event) -> show label event);
  [%expect
    {|
    ASCII '\015' (^O raw)        (Key_press (key (ASCII "\015")))                 -> (((code (Char o)) (ctrl true) (alt false) (shift false)))
    ASCII 'o' + [Ctrl]           (Key_press (key (ASCII o)) (mods (Ctrl)))        -> (((code (Char o)) (ctrl true) (alt false) (shift false)))
    ASCII '\n'                   (Key_press (key (ASCII "\n")))                   -> (((code Enter) (ctrl false) (alt false) (shift false)))
    ASCII '\r'                   (Key_press (key (ASCII "\r")))                   -> (((code Enter) (ctrl false) (alt false) (shift false)))
    Enter                        (Key_press (key Enter))                          -> (((code Enter) (ctrl false) (alt false) (shift false)))
    Enter + [Meta]               (Key_press (key Enter) (mods (Meta)))            -> (((code Enter) (ctrl false) (alt true) (shift false)))
    ASCII '\t'                   (Key_press (key (ASCII "\t")))                   -> (((code Tab) (ctrl false) (alt false) (shift false)))
    Tab                          (Key_press (key Tab))                            -> (((code Tab) (ctrl false) (alt false) (shift false)))
    Tab + [Shift]                (Key_press (key Tab) (mods (Shift)))             -> (((code Tab) (ctrl false) (alt false) (shift true)))
    ASCII '\t' + [Shift]         (Key_press (key (ASCII "\t")) (mods (Shift)))    -> (((code Tab) (ctrl false) (alt false) (shift true)))
    ASCII '\127'                 (Key_press (key (ASCII "\127")))                 -> (((code Backspace) (ctrl false) (alt false) (shift false)))
    ASCII '\008'                 (Key_press (key (ASCII "\b")))                   -> (((code Backspace) (ctrl false) (alt false) (shift false)))
    Backspace                    (Key_press (key Backspace))                      -> (((code Backspace) (ctrl false) (alt false) (shift false)))
    ASCII '\027'                 (Key_press (key (ASCII "\027")))                 -> (((code Escape) (ctrl false) (alt false) (shift false)))
    Escape                       (Key_press (key Escape))                         -> (((code Escape) (ctrl false) (alt false) (shift false)))
    Arrow Up                     (Key_press (key (Arrow Up)))                     -> (((code Up) (ctrl false) (alt false) (shift false)))
    Arrow Down                   (Key_press (key (Arrow Down)))                   -> (((code Down) (ctrl false) (alt false) (shift false)))
    Arrow Left                   (Key_press (key (Arrow Left)))                   -> (((code Left) (ctrl false) (alt false) (shift false)))
    Arrow Right                  (Key_press (key (Arrow Right)))                  -> (((code Right) (ctrl false) (alt false) (shift false)))
    Home                         (Key_press (key Home))                           -> (((code Home) (ctrl false) (alt false) (shift false)))
    End                          (Key_press (key End))                            -> (((code End) (ctrl false) (alt false) (shift false)))
    Page Up                      (Key_press (key (Page Up)))                      -> (((code Page_up) (ctrl false) (alt false) (shift false)))
    Page Down                    (Key_press (key (Page Down)))                    -> (((code Page_down) (ctrl false) (alt false) (shift false)))
    Delete                       (Key_press (key Delete))                         -> (((code Delete) (ctrl false) (alt false) (shift false)))
    ASCII 'a'                    (Key_press (key (ASCII a)))                      -> (((code (Char a)) (ctrl false) (alt false) (shift false)))
    Uchar U+00E9                 (Key_press (key (Uchar U+00E9)))                 -> (((code (Char "\195\169")) (ctrl false) (alt false) (shift false)))
    Uchar U+1F600                (Key_press (key (Uchar U+1F600)))                -> (((code (Char "\240\159\152\128")) (ctrl false) (alt false) (shift false)))
    ASCII 'c' + [Ctrl]           (Key_press (key (ASCII c)) (mods (Ctrl)))        -> (((code (Char c)) (ctrl true) (alt false) (shift false)))
    ASCII '\003' (^C raw)        (Key_press (key (ASCII "\003")))                 -> (((code (Char c)) (ctrl true) (alt false) (shift false)))
    ASCII '\031' (^_ raw)        (Key_press (key (ASCII "\031")))                 -> (((code (Char _)) (ctrl true) (alt false) (shift false)))
    ASCII 'j' + [Ctrl]           (Key_press (key (ASCII j)) (mods (Ctrl)))        -> (((code (Char j)) (ctrl true) (alt false) (shift false)))
    ASCII 'p' + [Ctrl]           (Key_press (key (ASCII p)) (mods (Ctrl)))        -> (((code (Char p)) (ctrl true) (alt false) (shift false)))
    ASCII 'p' + [Meta]           (Key_press (key (ASCII p)) (mods (Meta)))        -> (((code (Char p)) (ctrl false) (alt true) (shift false)))
    ASCII 't' + [Ctrl]           (Key_press (key (ASCII t)) (mods (Ctrl)))        -> (((code (Char t)) (ctrl true) (alt false) (shift false)))
    Paste Start                  (Paste Start)                                    -> ()
    Paste End                    (Paste End)                                      -> ()
    Mouse Left                   (Mouse (kind Left) (position ((x 3) (y 4))))     -> ()
    |}]
;;

let mods_of_key (key : Key.t) =
  List.filter_opt
    [ Option.some_if key.ctrl Mod.Ctrl
    ; Option.some_if key.alt Mod.Meta
    ; Option.some_if key.shift Mod.Shift
    ]
;;

let key_press_of_key (key : Key.t) : Event.t =
  let mods = mods_of_key key in
  let key =
    match key.code with
    | Escape -> Ekey.Escape
    | Enter -> Ekey.Enter
    | Tab -> Ekey.Tab
    | Backspace -> Ekey.Backspace
    | Delete -> Ekey.Delete
    | Insert -> Ekey.Insert
    | Home -> Ekey.Home
    | End -> Ekey.End
    | Up -> Ekey.Arrow `Up
    | Down -> Ekey.Arrow `Down
    | Left -> Ekey.Arrow `Left
    | Right -> Ekey.Arrow `Right
    | Page_up -> Ekey.Page `Up
    | Page_down -> Ekey.Page `Down
    | Function n -> Ekey.Function n
    | Char s -> Ekey.ASCII (String.get s 0)
  in
  key_press key mods
;;

let%expect_test "keymap: every bound key is reachable from a terminal event" =
  List.iter Keymap.bindings ~f:(fun b ->
    List.iter b.keys ~f:(fun key ->
      let event = key_press_of_key key in
      let status =
        match Key_of_event.key event with
        | Some actual when Key.equal actual key -> "ok"
        | Some actual ->
          sprintf
            "MISSING (mapped to %s)"
            (Sexp.to_string_hum (Key.sexp_of_t actual))
        | None -> "MISSING (None)"
      in
      printf
        "%-16s %-32s %s\n"
        (Key.to_string key)
        (Sexp.to_string_hum (Event.sexp_of_t event))
        status));
  [%expect
    {|
    Enter            (Key_press (key Enter))          ok
    Alt+Enter        (Key_press (key Enter) (mods (Meta))) ok
    Ctrl+J           (Key_press (key (ASCII j)) (mods (Ctrl))) ok
    Alt+J            (Key_press (key (ASCII j)) (mods (Meta))) ok
    Esc              (Key_press (key Escape))         ok
    Tab              (Key_press (key Tab))            ok
    Up               (Key_press (key (Arrow Up)))     ok
    Down             (Key_press (key (Arrow Down)))   ok
    Alt+Up           (Key_press (key (Arrow Up)) (mods (Meta))) ok
    Left             (Key_press (key (Arrow Left)))   ok
    Right            (Key_press (key (Arrow Right)))  ok
    Alt+B            (Key_press (key (ASCII b)) (mods (Meta))) ok
    Ctrl+Left        (Key_press (key (Arrow Left)) (mods (Ctrl))) ok
    Alt+F            (Key_press (key (ASCII f)) (mods (Meta))) ok
    Ctrl+Right       (Key_press (key (Arrow Right)) (mods (Ctrl))) ok
    Alt+D            (Key_press (key (ASCII d)) (mods (Meta))) ok
    Home             (Key_press (key Home))           ok
    Ctrl+A           (Key_press (key (ASCII a)) (mods (Ctrl))) ok
    End              (Key_press (key End))            ok
    Ctrl+E           (Key_press (key (ASCII e)) (mods (Ctrl))) ok
    PageUp           (Key_press (key (Page Up)))      ok
    PageDown         (Key_press (key (Page Down)))    ok
    Backspace        (Key_press (key Backspace))      ok
    Ctrl+H           (Key_press (key (ASCII h)) (mods (Ctrl))) ok
    Delete           (Key_press (key Delete))         ok
    Ctrl+K           (Key_press (key (ASCII k)) (mods (Ctrl))) ok
    Ctrl+U           (Key_press (key (ASCII u)) (mods (Ctrl))) ok
    Ctrl+W           (Key_press (key (ASCII w)) (mods (Ctrl))) ok
    Alt+Backspace    (Key_press (key Backspace) (mods (Meta))) ok
    Ctrl+Y           (Key_press (key (ASCII y)) (mods (Ctrl))) ok
    Alt+Y            (Key_press (key (ASCII y)) (mods (Meta))) ok
    Ctrl+_           (Key_press (key (ASCII _)) (mods (Ctrl))) ok
    Ctrl+O           (Key_press (key (ASCII o)) (mods (Ctrl))) ok
    Ctrl+R           (Key_press (key (ASCII r)) (mods (Ctrl))) ok
    Ctrl+G           (Key_press (key (ASCII g)) (mods (Ctrl))) ok
    Ctrl+L           (Key_press (key (ASCII l)) (mods (Ctrl))) ok
    Ctrl+P           (Key_press (key (ASCII p)) (mods (Ctrl))) ok
    Alt+P            (Key_press (key (ASCII p)) (mods (Meta))) ok
    Ctrl+T           (Key_press (key (ASCII t)) (mods (Ctrl))) ok
    Ctrl+N           (Key_press (key (ASCII n)) (mods (Ctrl))) ok
    Ctrl+X           (Key_press (key (ASCII x)) (mods (Ctrl))) ok
    Ctrl+Z           (Key_press (key (ASCII z)) (mods (Ctrl))) ok
    Shift+Tab        (Key_press (key Tab) (mods (Shift))) ok
    Alt+1            (Key_press (key (ASCII 1)) (mods (Meta))) ok
    Ctrl+C           (Key_press (key (ASCII c)) (mods (Ctrl))) ok
    Ctrl+D           (Key_press (key (ASCII d)) (mods (Ctrl))) ok
    |}]
;;
