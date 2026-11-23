open! Core
open Prigh_web
module H = Harness

(* A text field's menu in the desktop app's browser window: Cut, Copy,
   Paste and Select all, where the browser's own menu is hidden; disabled
   items; the keyboard; and nothing of it outside the browser shell. *)

let field
      ?(kind = Field_menu.Kind.Text_field)
      ?(selection = false)
      ?(editable = true)
      ?(secret = false)
      ()
  =
  { Field_menu.Field.kind; selection; editable; secret }
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
     (field
      (((kind Text_field) (selection true) (editable true) (secret false))))
     Native)
    ((browser_shell true) (field ()) Suppress)
    ((browser_shell true)
     (field
      (((kind Text_field) (selection false) (editable true) (secret false))))
     (Open ((kind Text_field) (selection false) (editable true) (secret false))))
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
    <div class="context-menu-backdrop field-menu-backdrop menu-backdrop z-(--z-field-menu)"
         @on_click
         @on_contextmenu>
      <div data-slot="dropdown-menu-content"
           role="menu"
           aria-label="Edit"
           tabindex="-1"
           id="field-menu"
           class="[&:not(.placed)]:opacity-0 [&:not(.placed)]:pointer-events-none [&_.menu-label]:shrink-0 animate-in bg-popover cn-menu-target context-menu duration-100 fade-in-0 field-menu fixed loon max-[760px]:[&_.menu-item]:min-h-10 max-[760px]:[&_.menu-item]:text-[15px] max-[760px]:bottom-2 max-[760px]:left-2! max-[760px]:max-w-none max-[760px]:right-2 max-[760px]:top-auto! max-h-[calc(100vh-16px)] max-w-[min(360px,calc(100vw-16px))] menu-surface min-w-32 min-w-[200px] min-w-[248px] motion-reduce:animate-none narrow:animate-[sheet-up_0.18s_ease-out] narrow:border-border narrow:border-t narrow:overscroll-contain narrow:pb-[max(8px,var(--safe-b))] narrow:pl-[max(8px,var(--safe-l))] narrow:pr-[max(8px,var(--safe-r))] narrow:pt-0 narrow:ring-0 narrow:rounded-b-none narrow:rounded-t-xl narrow:shadow-[0_-12px_40px_-12px_var(--edge-shadow)] narrow:z-(--z-menu-surface) overflow-x-hidden overflow-y-auto p-1 ring-1 ring-foreground/10 rounded-lg shadow-md text-popover-foreground zoom-in-95">
        <div data-slot="dropdown-menu-group"
             role="group"
             class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
          <div data-slot="dropdown-menu-item"
               data-highlighted=""
               role="menuitem"
               data-item="cut"
               class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm"
               @on_click
               @on_mousedown
               @on_mouseenter>
            <icon class="scissors"> </icon>
            <span class="menu-label truncate"> Cut </span>
            <kbd data-slot="dropdown-menu-shortcut"
                 class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> Ctrl+X </kbd>
          </div>
          <div data-slot="dropdown-menu-item"
               role="menuitem"
               data-item="copy"
               class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
               @on_click
               @on_mousedown
               @on_mouseenter>
            <icon class="copy"> </icon>
            <span class="menu-label truncate"> Copy </span>
            <kbd data-slot="dropdown-menu-shortcut"
                 class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> Ctrl+C </kbd>
          </div>
          <div data-slot="dropdown-menu-item"
               role="menuitem"
               data-item="paste"
               class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
               @on_click
               @on_mousedown
               @on_mouseenter>
            <icon class="clipboard"> </icon>
            <span class="menu-label truncate"> Paste </span>
            <kbd data-slot="dropdown-menu-shortcut"
                 class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> Ctrl+V </kbd>
          </div>
        </div>
        <div data-slot="dropdown-menu-group"
             role="group"
             class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
          <div data-slot="dropdown-menu-item"
               role="menuitem"
               data-item="select_all"
               class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm"
               @on_click
               @on_mousedown
               @on_mouseenter>
            <icon class="lines"> </icon>
            <span class="menu-label truncate"> Select all </span>
            <kbd data-slot="dropdown-menu-shortcut"
                 class="font-mono group-data-highlighted/dropdown-menu-item:text-accent-foreground group-hover/dropdown-menu-item:text-accent-foreground menu-command min-w-0 ml-auto pl-3 text-muted-foreground text-xs truncate"> Ctrl+A </kbd>
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
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground disabled flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground disabled flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    |}];
  (* A click on one does nothing, and leaves the menu open. *)
  H.click h ~selector:".field-menu [data-item=cut]";
  H.click h ~selector:".field-menu [data-item=copy]";
  H.elements h ~selector:".field-menu";
  [%expect {| <div class="[&:not(.placed)]:opacity-0 [&:not(.placed)]:pointer-events-none [&_.menu-label]:shrink-0 animate-in bg-popover cn-menu-target context-menu duration-100 fade-in-0 field-menu fixed loon max-[760px]:[&_.menu-item]:min-h-10 max-[760px]:[&_.menu-item]:text-[15px] max-[760px]:bottom-2 max-[760px]:left-2! max-[760px]:max-w-none max-[760px]:right-2 max-[760px]:top-auto! max-h-[calc(100vh-16px)] max-w-[min(360px,calc(100vw-16px))] menu-surface min-w-32 min-w-[200px] min-w-[248px] motion-reduce:animate-none narrow:animate-[sheet-up_0.18s_ease-out] narrow:border-border narrow:border-t narrow:overscroll-contain narrow:pb-[max(8px,var(--safe-b))] narrow:pl-[max(8px,var(--safe-l))] narrow:pr-[max(8px,var(--safe-r))] narrow:pt-0 narrow:ring-0 narrow:rounded-b-none narrow:rounded-t-xl narrow:shadow-[0_-12px_40px_-12px_var(--edge-shadow)] narrow:z-(--z-menu-surface) overflow-x-hidden overflow-y-auto p-1 ring-1 ring-foreground/10 rounded-lg shadow-md text-popover-foreground zoom-in-95"> |}];
  (* The keyboard skips them. *)
  H.key h "ArrowDown";
  H.key h "ArrowDown";
  H.elements h ~selector:".field-menu .menu-item.selected";
  [%expect
    {|
    (Field_menu_move 1)
    (Field_menu_move 1)
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
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
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground disabled flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground disabled flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    Close_field_menu
    (Edit_field ())
    (Place_field_menu (Point (x 200) (y 500)))
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground disabled flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground disabled flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
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
    <div class="[&:not(.placed)]:opacity-0 [&:not(.placed)]:pointer-events-none [&_.menu-label]:shrink-0 animate-in bg-popover cn-menu-target context-menu duration-100 fade-in-0 field-menu fixed loon max-[760px]:[&_.menu-item]:min-h-10 max-[760px]:[&_.menu-item]:text-[15px] max-[760px]:bottom-2 max-[760px]:left-2! max-[760px]:max-w-none max-[760px]:right-2 max-[760px]:top-auto! max-h-[calc(100vh-16px)] max-w-[min(360px,calc(100vw-16px))] menu-surface min-w-32 min-w-[200px] min-w-[248px] motion-reduce:animate-none narrow:animate-[sheet-up_0.18s_ease-out] narrow:border-border narrow:border-t narrow:overscroll-contain narrow:pb-[max(8px,var(--safe-b))] narrow:pl-[max(8px,var(--safe-l))] narrow:pr-[max(8px,var(--safe-r))] narrow:pt-0 narrow:ring-0 narrow:rounded-b-none narrow:rounded-t-xl narrow:shadow-[0_-12px_40px_-12px_var(--edge-shadow)] narrow:z-(--z-menu-surface) overflow-x-hidden overflow-y-auto p-1 ring-1 ring-foreground/10 rounded-lg shadow-md text-popover-foreground zoom-in-95">
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

(* The terminal's: Copy, Paste and Select all, with the terminal's keys; its
   keys are the menu's while it is open (the shell's again after). *)
let%expect_test "in the terminal" =
  let h = shell () in
  let right_click ~selection =
    H.act
      h
      (Open_field_menu
         { field = field ~kind:Terminal ~selection ()
         ; anchor = Point { x = 300; y = 200 }
         })
  in
  right_click ~selection:true;
  items h;
  H.elements h ~selector:".field-menu .menu-section";
  [%expect
    {|
    (Place_field_menu (Point (x 300) (y 200)))
    [menuitem] div "Copy Ctrl+Shift+C"
    [menuitem] div "Paste Ctrl+Shift+V"
    [menuitem] div "Select all"
    <div class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
    <div class=" [&+&]:-mx-1 [&+&]:border-border [&+&]:border-t [&+&]:mt-1 [&+&]:pt-1 [&+&]:px-1 flex flex-col menu-section">
    |}];
  H.key h ~target:Terminal "ArrowDown";
  H.text h ~selector:".field-menu .menu-item.selected";
  H.key h ~target:Terminal "Enter";
  [%expect
    {|
    (Field_menu_move 1)
    Paste Ctrl+Shift+V
    (Field_menu_choose ())
    (Edit_field (Paste))
    |}];
  (* Closed, Esc and Enter are the shell's again. *)
  H.key h ~target:Terminal "Escape";
  [%expect {| (browser default) |}];
  (* Nothing selected: Copy is disabled and Paste comes first. *)
  right_click ~selection:false;
  H.elements h ~selector:".field-menu .menu-item";
  H.key h ~target:Terminal "Escape";
  [%expect
    {|
    (Place_field_menu (Point (x 300) (y 200)))
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground disabled flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none selected text-sm">
    <div class="[&>.icon]:shrink-0 [&>.icon]:size-4 [&>.icon]:text-muted-foreground aria-disabled:*:opacity-50 aria-disabled:hover:bg-transparent aria-disabled:hover:text-inherit cursor-default data-[variant=destructive]:[&>.icon]:text-destructive data-[variant=destructive]:data-highlighted:bg-destructive/10 data-[variant=destructive]:data-highlighted:text-destructive data-[variant=destructive]:hover:bg-destructive/10 data-[variant=destructive]:hover:text-destructive data-[variant=destructive]:text-destructive data-highlighted:[&>.icon]:text-accent-foreground data-highlighted:bg-accent data-highlighted:text-accent-foreground flex gap-1.5 group/dropdown-menu-item hover:[&>.icon]:text-accent-foreground hover:bg-accent hover:text-accent-foreground items-center menu-item narrow:min-h-11 outline-hidden px-1.5 py-1 relative rounded-md select-none text-sm">
    Close_field_menu
    (Edit_field ())
    |}]
;;
