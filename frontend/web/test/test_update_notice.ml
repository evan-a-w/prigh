open! Core
open Prigh_web
module H = Harness

(* A new build's service worker waiting: one notice over the chat, beside
   the toasts, that stays until answered. *)

let show h = H.show h ~selector:".toast-anchor"

let%expect_test "a waiting build: the notice, and Reload hands over to it" =
  let h = H.create () in
  show h;
  [%expect
    {|
    <div class="toast-anchor">
      <div aria-live="polite" class="toasts"> </div>
    </div>
    |}];
  H.act h App.Action.Update_ready;
  show h;
  [%expect
    {|
    <div class="toast-anchor">
      <div aria-live="polite" class="toasts">
        <div role="status"
             data-sonner-type="info"
             class="bg-popover border border-border cursor-default flex gap-2 items-start leading-normal px-3.5 py-2.5 rounded-lg shadow-lg text-[13px] text-popover-foreground toast update-notice">
          <svg xmlns="http://www.w3.org/2000/svg"
               width="24"
               height="24"
               viewBox="0 0 24 24"
               fill="none"
               stroke="currentColor"
               stroke-width="2"
               stroke-linecap="round"
               stroke-linejoin="round"
               class="lucide lucide-circle-fading-arrow-up mt-0.5 shrink-0 size-4 text-primary">
            <path d="M12 2a10 10 0 0 1 7.38 16.75"> </path>
            <path d="m16 12-4-4-4 4"> </path>
            <path d="M12 16V8"> </path>
            <path d="M2.5 8.875a10 10 0 0 0-.5 3"> </path>
            <path d="M2.83 16a10 10 0 0 0 2.43 3.4"> </path>
            <path d="M4.636 5.235a10 10 0 0 1 .891-.857"> </path>
            <path d="M8.644 21.42a10 10 0 0 0 7.631-.38"> </path>
          </svg>
          <span class="flex-1 font-medium min-w-0"> A new version of prigh-web is ready. </span>
          <button type="button"
                  title="Reload the page into it, in this session"
                  data-button=""
                  class="bg-foreground cursor-pointer font-medium h-6 hover:bg-foreground/85 inline-flex items-center ml-1 narrow:min-h-11 px-2 rounded-[4px] shrink-0 text-background text-xs toast-undo"
                  @on_click> Reload </button>
          <button type="button"
                  title="Later (the next page load uses it)"
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
  H.act h (App.Action.Show_toast { text = "Archived"; error = false });
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Expire_toast (id 0) (after_ms 2500))
    A new version of prigh-web is ready. (Reload) (Later (the next page load uses it))
    Archived
    |}];
  H.click h ~selector:".update-notice .toast-undo";
  [%expect
    {|
    Apply_update
    Apply_update
    |}];
  show h;
  [%expect
    {|
    <div class="toast-anchor">
      <div aria-live="polite" class="toasts">
        <div role="status"
             data-sonner-type="success"
             title="Dismiss"
             class="bg-popover border border-border flex gap-2 items-start leading-normal px-3.5 py-2.5 rounded-lg shadow-lg text-[13px] text-popover-foreground toast"
             @on_click>
          <span dir="auto" class="flex-1 font-medium min-w-0"> Archived </span>
        </div>
      </div>
    </div>
    |}]
;;

let%expect_test "dismissed: gone, with nothing done, until the next page load" =
  let h = H.create () in
  H.act h App.Action.Update_ready;
  H.click h ~selector:".update-notice .toast-close";
  [%expect {| Dismiss_update |}];
  show h;
  [%expect
    {|
    <div class="toast-anchor">
      <div aria-live="polite" class="toasts"> </div>
    </div>
    |}];
  H.act h App.Action.Backend_closed;
  H.act h App.Action.Update_ready;
  H.text h ~selector:".toasts";
  [%expect
    {|
    (Reconnect (generation 1) (delay_ms 0) (session (s1)))
    A new version of prigh-web is ready. (Reload) (Later (the next page load uses it))
    |}]
;;
