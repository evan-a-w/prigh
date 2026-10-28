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
        <div role="status" class="toast update-notice">
          <span> A new version of prigh-web is ready. </span>
          <button type="button"
                  title="Reload the page into it, in this session"
                  class="btn ghost small toast-undo"
                  @on_click> Reload </button>
          <button type="button"
                  title="Later (the next page load uses it)"
                  aria-label="Dismiss"
                  class="btn ghost icon toast-close"
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
        <div role="status" title="Dismiss" class="toast" @on_click>
          <span dir="auto"> Archived </span>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
          <Vdom.Node.none-widget> </Vdom.Node.none-widget>
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
