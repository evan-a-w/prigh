open! Core
open Prigh_web
module H = Harness

(* A text field's menu in the desktop app's browser window: Cut, Copy,
   Paste and Select all, where the browser's own menu is hidden; disabled
   items; the keyboard; and nothing of it outside the browser shell. *)

let field ?(selection = false) ?(editable = true) ?(secret = false) () =
  { Field_menu.Field.selection; editable; secret }
;;

(* What the page's listener does ({!Field_menu_listeners}) on a
   right-click in the composer. *)
let right_click ?selection ?editable ?secret h =
  H.act
    h
    (Open_field_menu
       { field = field ?selection ?editable ?secret ()
       ; anchor = Point { x = 200; y = 500 }
       })
;;

let shell () =
  let h = H.create () in
  H.quiet h (fun () -> H.act h (Set_browser_shell true));
  h
;;

let items h = H.names h ~selector:".field-menu [role=menuitem]"
let menu h = H.text h ~selector:".field-menu"

let%expect_test "what a right-click does where" =
  List.iter
    [ false, None
    ; false, Some (field ~selection:true ())
    ; true, None
    ; true, Some (field ())
    ]
    ~f:(fun (browser_shell, field) ->
      print_s
        [%message
          (browser_shell : bool)
            (field : Field_menu.Field.t option)
            ~_:(Field_menu.decide ~browser_shell field : Field_menu.Decision.t)]);
  [%expect
    {|
    ((browser_shell false) (field ()) Native)
    ((browser_shell false)
     (field (((selection true) (editable true) (secret false)))) Native)
    ((browser_shell true) (field ()) Suppress)
    ((browser_shell true)
     (field (((selection false) (editable true) (secret false))))
     (Open ((selection false) (editable true) (secret false))))
    |}]
;;

let%expect_test "a right-click on the composer in the browser shell" =
  let h = shell () in
  H.type_ h "hello world";
  right_click h ~selection:true;
  H.show h ~selector:".field-menu-backdrop";
  [%expect
    {|
    (Place_field_menu (Point (x 200) (y 500)))
    <div class="context-menu-backdrop field-menu-backdrop menu-backdrop" @on_click @on_contextmenu>
      <div id="field-menu"
           role="menu"
           aria-label="Edit"
           tabindex="-1"
           class="context-menu field-menu menu-surface">
        <div class="menu-section">
          <div role="menuitem"
               data-item="cut"
               class="menu-item selected"
               @on_click
               @on_mousedown
               @on_mouseenter>
            <icon class="scissors"> </icon>
            <span class="menu-label"> Cut </span>
            <kbd class="menu-command"> Ctrl+X </kbd>
          </div>
          <div role="menuitem"
               data-item="copy"
               class="menu-item"
               @on_click
               @on_mousedown
               @on_mouseenter>
            <icon class="copy"> </icon>
            <span class="menu-label"> Copy </span>
            <kbd class="menu-command"> Ctrl+C </kbd>
          </div>
          <div role="menuitem"
               data-item="paste"
               class="menu-item"
               @on_click
               @on_mousedown
               @on_mouseenter>
            <icon class="clipboard"> </icon>
            <span class="menu-label"> Paste </span>
            <kbd class="menu-command"> Ctrl+V </kbd>
          </div>
        </div>
        <div class="menu-section">
          <div role="menuitem"
               data-item="select_all"
               class="menu-item"
               @on_click
               @on_mousedown
               @on_mouseenter>
            <icon class="lines"> </icon>
            <span class="menu-label"> Select all </span>
            <kbd class="menu-command"> Ctrl+A </kbd>
          </div>
        </div>
      </div>
    </div>
    |}];
  (* Each item does its thing to the field. *)
  List.iter [ "cut"; "copy"; "paste"; "select_all" ] ~f:(fun item ->
    H.click h ~selector:(sprintf ".field-menu [data-item=%s]" item);
    menu h;
    right_click h ~selection:true);
  [%expect
    {|
    (Field_menu_choose (Cut))
    (Edit_field (Cut))
    (Place_field_menu (Point (x 200) (y 500)))
    (Field_menu_choose (Copy))
    (Edit_field (Copy))
    (Place_field_menu (Point (x 200) (y 500)))
    (Field_menu_choose (Paste))
    (Edit_field (Paste))
    (Place_field_menu (Point (x 200) (y 500)))
    (Field_menu_choose (Select_all))
    (Edit_field (Select_all))
    (Place_field_menu (Point (x 200) (y 500)))
    |}]
;;

let%expect_test "nothing selected: Cut and Copy are disabled" =
  let h = shell () in
  right_click h;
  items h;
  H.elements h ~selector:".field-menu .menu-item";
  [%expect
    {|
    (Place_field_menu (Point (x 200) (y 500)))
    [menuitem] div "Cut Ctrl+X"
    [menuitem] div "Copy Ctrl+C"
    [menuitem] div "Paste Ctrl+V"
    [menuitem] div "Select all Ctrl+A"
    <div class="disabled menu-item">
    <div class="disabled menu-item">
    <div class="menu-item selected">
    <div class="menu-item">
    |}];
  (* A click on one does nothing, and leaves the menu open. *)
  H.click h ~selector:".field-menu [data-item=cut]";
  H.click h ~selector:".field-menu [data-item=copy]";
  H.elements h ~selector:".field-menu";
  [%expect {| <div class="context-menu field-menu menu-surface"> |}];
  (* The keyboard skips them. *)
  H.key h "ArrowDown";
  H.key h "ArrowDown";
  H.elements h ~selector:".field-menu .menu-item.selected";
  [%expect
    {|
    (Field_menu_move 1)
    (Field_menu_move 1)
    <div class="menu-item selected">
    |}];
  H.key h "Enter";
  [%expect
    {|
    (Field_menu_choose ())
    (Edit_field (Paste))
    |}]
;;

let%expect_test "read-only and password fields" =
  let h = shell () in
  right_click h ~selection:true ~editable:false;
  H.elements h ~selector:".field-menu .menu-item";
  H.key h "Escape";
  right_click h ~selection:true ~secret:true;
  H.elements h ~selector:".field-menu .menu-item";
  [%expect
    {|
    (Place_field_menu (Point (x 200) (y 500)))
    <div class="disabled menu-item">
    <div class="menu-item selected">
    <div class="disabled menu-item">
    <div class="menu-item">
    Close_field_menu
    (Edit_field ())
    (Place_field_menu (Point (x 200) (y 500)))
    <div class="disabled menu-item">
    <div class="disabled menu-item">
    <div class="menu-item selected">
    <div class="menu-item">
    |}]
;;

let%expect_test "the keyboard: it owns it until Esc, which does nothing else" =
  let h = shell () in
  H.type_ h "draft";
  right_click h ~selection:true;
  H.key h "ArrowUp";
  H.text h ~selector:".field-menu .menu-item.selected";
  H.key h "ArrowDown";
  (* Not the editor's: no send, no history. *)
  H.key h "Escape";
  H.text h ~selector:".field-menu";
  H.text h ~selector:"#editor";
  [%expect
    {|
    (Place_field_menu (Point (x 200) (y 500)))
    (Field_menu_move -1)
    Select all Ctrl+A
    (Field_menu_move 1)
    Close_field_menu
    (Edit_field ())
    [draft]
    |}];
  (* A click outside closes it the same way (the e2e clicks it). *)
  right_click h;
  H.act h Close_field_menu;
  menu h;
  [%expect
    {|
    (Place_field_menu (Point (x 200) (y 500)))
    (Edit_field ())
    |}]
;;

let%expect_test "over a dialog, which stays open under it" =
  let h = shell () in
  H.quiet h (fun () -> H.act h (Run "/name"));
  right_click h ~selection:true;
  H.elements h ~selector:".modal";
  H.elements h ~selector:".field-menu";
  H.key h "Escape";
  H.elements h ~selector:".modal";
  H.elements h ~selector:".field-menu";
  [%expect
    {|
    (Place_field_menu (Point (x 200) (y 500)))
    <div class="modal">
    <div class="context-menu field-menu menu-surface">
    Close_field_menu
    (Edit_field ())
    <div class="modal">
    |}]
;;

let%expect_test "not outside the browser shell" =
  let h = H.create () in
  right_click h ~selection:true;
  H.elements h ~selector:".field-menu";
  H.key h "Escape";
  [%expect {| (browser default) |}]
;;
