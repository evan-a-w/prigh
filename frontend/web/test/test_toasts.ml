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
        <div role="status"
             data-sonner-type="success"
             title="Dismiss"
             class="bg-popover border border-border flex gap-2 items-start leading-normal px-3.5 py-2.5 rounded-lg shadow-lg text-[13px] text-popover-foreground toast"
             @on_click>
          <span dir="auto" class="flex-1 font-medium min-w-0"> Answered </span>
        </div>
        <div role="status"
             data-sonner-type="success"
             title="Dismiss"
             class="bg-popover border border-border flex gap-2 items-start leading-normal px-3.5 py-2.5 rounded-lg shadow-lg text-[13px] text-popover-foreground toast"
             @on_click>
          <span dir="auto" class="flex-1 font-medium min-w-0"> Copied the last reply to the clipboard </span>
          <span class="font-mono leading-[1.7] text-[11.5px] text-muted-foreground toast-repeats"> ×3 </span>
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
        <div role="status"
             data-sonner-type="success"
             title="Dismiss"
             class="bg-popover border border-border flex gap-2 items-start leading-normal px-3.5 py-2.5 rounded-lg shadow-lg text-[13px] text-popover-foreground toast"
             @on_click>
          <span dir="auto" class="flex-1 font-medium min-w-0"> Copied the last reply to the clipboard </span>
          <span class="font-mono leading-[1.7] text-[11.5px] text-muted-foreground toast-repeats"> ×3 </span>
        </div>
        <div role="alert"
             data-sonner-type="error"
             title="Working directory: /tmp/nix-shell.MOI5Ul/nix-shell.cHNpd9/tmp.bHrKSDsr6i/chromium/cwd (click to dismiss)"
             class="bg-popover border border-border error flex gap-2 items-start leading-normal px-3.5 py-2.5 rounded-lg shadow-lg text-[13px] text-popover-foreground toast"
             @on_click>
          <svg xmlns="http://www.w3.org/2000/svg"
               width="24"
               height="24"
               viewBox="0 0 24 24"
               fill="none"
               stroke="currentColor"
               stroke-width="2"
               stroke-linecap="round"
               stroke-linejoin="round"
               class="lucide lucide-circle-x mt-0.5 shrink-0 size-4 text-destructive">
            <circle cx="12" cy="12" r="10"> </circle>
            <path d="m15 9-6 6"> </path>
            <path d="m9 9 6 6"> </path>
          </svg>
          <span dir="auto" class="flex-1 font-medium min-w-0"> Working directory: /tmp/…/chromium/cwd </span>
          <button type="button"
                  title="Dismiss"
                  aria-label="Dismiss"
                  data-close-button=""
                  class="-mr-1.5 -my-0.5 [&_svg:not([class*='size-'])]:size-3 [&_svg]:pointer-events-none [&_svg]:shrink-0 active:not-aria-[haspopup]:translate-y-px aria-expanded:bg-muted aria-expanded:text-foreground aria-invalid:border-destructive aria-invalid:ring-3 aria-invalid:ring-destructive/20 bg-clip-padding border border-transparent dark:aria-invalid:border-destructive/50 dark:aria-invalid:ring-destructive/40 dark:hover:bg-muted/50 disabled:opacity-50 disabled:pointer-events-none focus-visible:border-ring focus-visible:ring-3 focus-visible:ring-ring/50 font-medium group/button hover:bg-muted hover:text-foreground in-data-[slot=button-group]:rounded-lg inline-flex items-center justify-center outline-none rounded-[min(var(--radius-md),10px)] rounded-lg select-none shrink-0 size-6 text-muted-foreground text-sm toast-close transition-all whitespace-nowrap"
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
    (Open_chip Model)
    (Focus popover-filter)
    Place_popover
    <aside class="bg-sidebar loon sidebar text-sidebar-foreground text-sm">
    <div class="chat-frame">
    <div class="editor-wrap">
    <div class="context-ring-anchor">
    <div class="composer-buttons">
    <div class="tray">
    <div class="bg-popover border border-border flex gap-2 items-start leading-normal px-3.5 py-2.5 rounded-lg shadow-lg text-[13px] text-popover-foreground toast">
    |}]
;;

(* What the user typed comes back as they typed it: no OCaml escapes for
   accents, scripts or emoji. *)
let%expect_test "toasts quote non-ASCII text as it is" =
  let h = H.create () in
  H.quiet h (fun () ->
    H.act h (Run "/model 日本語 é 😀");
    H.act h (Run "/thinking שלום"));
  H.text h ~selector:".toasts";
  [%expect
    {|
    No model matches “日本語 é 😀”. Ctrl+L lists them all. (Dismiss)
    Claude Opus 5.5 has no thinking “שלום”: use default, low, medium, high, xhigh, max. (Dismiss)
    |}]
;;
