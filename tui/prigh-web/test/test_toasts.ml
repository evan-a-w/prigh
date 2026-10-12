open! Core
open Prigh_web
module H = Harness

(* Toasts: two at most, repeats collapsed, short ones gone sooner, long paths
   elided, over the bottom of the chat column. *)

let show_toasts h = H.show h ~selector:".toast-anchor"

let%expect_test "lifetimes grow with the text; errors stay" =
  List.iter
    [ "Archived", false
    ; "Plan approved", false
    ; {|Archived "hello again" (a new message unarchives it)|}, false
    ; String.make 200 'x', false
    ; "Upload failed: disk full", true
    ]
    ~f:(fun (text, error) ->
      let _, t = Toast.push [] ~id:0 ~error text in
      print_s
        [%message
          (String.prefix text 30 : string)
            ~lifetime_ms:(Toast.lifetime_ms t : int option)]);
  [%expect
    {|
    (("String.prefix text 30" Archived) (lifetime_ms (2500)))
    (("String.prefix text 30" "Plan approved") (lifetime_ms (2500)))
    (("String.prefix text 30" "Archived \"hello again\" (a new ")
     (lifetime_ms (3580)))
    (("String.prefix text 30" xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx)
     (lifetime_ms (6000)))
    (("String.prefix text 30" "Upload failed: disk full") (lifetime_ms ()))
    |}]
;;

let%expect_test "long paths are elided in the middle" =
  List.iter
    [ "Working directory: \
       /tmp/nix-shell.MOI5Ul/nix-shell.cHNpd9/tmp.bHrKSDsr6i/chromium/cwd"
    ; "Copied the path ~/projects/prigh/tui/prigh-web/lib/deeper/files_view.ml"
    ; "Exported to \
       /home/me/a-very-long-directory-name-without-slashes-after.html"
    ; "Short paths stay: ~/proj/src/main.ml"
    ; "Not a path: https://example.com/a/b/c/d/e/f/g/h/i/j/k/l/m/n/o"
    ; "Unicode: /home/me/ééééééééééééééééééééééééééééééé/ünïcödé-fïlé-nämé.txt"
    ]
    ~f:(fun text -> print_endline (Toast.short text));
  [%expect
    {|
    Working directory: /tmp/…/chromium/cwd
    Copied the path ~/projects/…/deeper/files_view.ml
    Exported to /home/me/…thout-slashes-after.html
    Short paths stay: ~/proj/src/main.ml
    Not a path: https://example.com/a/b/c/d/e/f/g/h/i/j/k/l/m/n/o
    Unicode: /home/me/…cödé-fïlé-nämé.txt
    |}]
;;

let%expect_test "two at most, the newest; a repeat is counted, not stacked" =
  let h = H.create () in
  List.iter
    [ "Model: a"; "Copied the last reply to the clipboard"; "Answered" ]
    ~f:(fun text -> H.act h (Show_toast { text; error = false }));
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    (Expire_toast (id 1) (after_ms 3020))
    (Expire_toast (id 2) (after_ms 2500))
    |}];
  H.text h ~selector:".toasts";
  [%expect
    {|
    Copied the last reply to the clipboard
    Answered
    |}];
  (* Said again: one toast, counted, its timer started again (the old one's
     expiry finds nothing). *)
  H.act
    h
    (Show_toast
       { text = "Copied the last reply to the clipboard"; error = false });
  H.act
    h
    (Show_toast
       { text = "Copied the last reply to the clipboard"; error = false });
  H.act h (Dismiss_toast 1);
  show_toasts h;
  [%expect
    {|
    (Expire_toast (id 3) (after_ms 3020))
    (Expire_toast (id 4) (after_ms 3020))
    <div class="toast-anchor">
      <div aria-live="polite" class="toasts">
        <div role="status" title="Dismiss" class="toast" @on_click>
          <span> Answered </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
        <div role="status" title="Dismiss" class="toast" @on_click>
          <span> Copied the last reply to the clipboard </span>
          <span class="toast-repeats"> ×3 </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
      </div>
    </div>
    |}];
  (* A long path is shortened; the whole text is in the title. Errors stay,
     with a close button. *)
  H.act
    h
    (Show_toast
       { text =
           "Working directory: \
            /tmp/nix-shell.MOI5Ul/nix-shell.cHNpd9/tmp.bHrKSDsr6i/chromium/cwd"
       ; error = true
       });
  show_toasts h;
  [%expect
    {|
    <div class="toast-anchor">
      <div aria-live="polite" class="toasts">
        <div role="status" title="Dismiss" class="toast" @on_click>
          <span> Copied the last reply to the clipboard </span>
          <span class="toast-repeats"> ×3 </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
        </div>
        <div role="alert"
             title="Working directory: /tmp/nix-shell.MOI5Ul/nix-shell.cHNpd9/tmp.bHrKSDsr6i/chromium/cwd (click to dismiss)"
             class="error toast"
             @on_click>
          <span> Working directory: /tmp/…/chromium/cwd </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <button type="button"
                  title="Dismiss"
                  aria-label="Dismiss"
                  class="btn ghost icon toast-close"
                  @on_click>
            <icon class="close"> </icon>
          </button>
        </div>
      </div>
    </div>
    |}];
  H.click h ~selector:".toast.error";
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Dismiss_toast 5)
    Copied the last reply to the clipboard ×3
    |}]
;;

let%expect_test "toasts are not under a dialog's inert page" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Show_toast { text = "Answered"; error = false }));
  H.key h "l" ~ctrl:true;
  H.elements h ~selector:"[inert]";
  H.elements h ~selector:".toast";
  [%expect
    {|
    Open_model_picker
    (Focus picker-input)
    <aside class="sidebar">
    <header class="topbar">
    <div class="chat verbosity-normal">
    <footer class="composer">
    <div class="toast">
    |}]
;;
