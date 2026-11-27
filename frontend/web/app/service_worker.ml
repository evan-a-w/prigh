open! Core
open Js_of_ocaml

(* The browser glue, in JavaScript: it is all promises and events. *)
let glue : Js.Unsafe.any Lazy.t =
  lazy
    (Js.Unsafe.js_expr
       {|(function () {
  const sw = window.isSecureContext && navigator.serviceWorker;
  let registration = null;
  let hadController = !!(sw && sw.controller);
  let updating = false;
  let reloading = false;
  let onUpdateReady = null;
  let toldWorker = null;
  const reloadOnce = () => {
    if (reloading) return;
    reloading = true;
    window.location.reload();
  };
  const tell = () => {
    const waiting = registration && registration.waiting;
    if (onUpdateReady && waiting && waiting !== toldWorker && sw && sw.controller) {
      toldWorker = waiting;
      onUpdateReady();
    }
  };
  return {
    register: function () {
      if (!sw) return;
      sw.addEventListener("controllerchange", () => {
        toldWorker = null;
        if (updating || hadController) {
          reloadOnce();
        } else {
          hadController = true;
        }
      });
      // The worker showed the page it keeps, and a proxy in front now wants
      // its sign-in (sw.template.js): the next load goes to the network.
      sw.addEventListener("message", event => {
        if (event.data === "prigh-sw: reload") reloadOnce();
      });
      sw.register("sw.js").then(r => {
        if (!r) return;
        registration = r;
        tell();
        r.addEventListener("updatefound", () => {
          const worker = r.installing;
          if (worker)
            worker.addEventListener("statechange", () => {
              if (worker.state === "installed") tell();
            });
        });
        const check = () => r.update().catch(() => {});
        setInterval(check, 60 * 60 * 1000);
        document.addEventListener("visibilitychange", () => {
          if (!document.hidden) check();
        });
      }).catch(e => console.warn("prigh-web: no service worker:", e));
    },
    onUpdateReady: function (f) {
      onUpdateReady = f;
      if (registration) tell();
    },
    applyUpdate: function () {
      toldWorker = null;
      const waiting = registration && registration.waiting;
      if (!waiting || waiting.state === "redundant" || waiting.state === "activated") {
        reloadOnce();
        return;
      }
      updating = true;
      waiting.addEventListener("statechange", () => {
        if (waiting.state === "redundant") reloadOnce();
      });
      setTimeout(reloadOnce, 3000);
      try {
        waiting.postMessage("skip-waiting");
      } catch (e) {
        reloadOnce();
      }
    },
    showNotification: function (title, options) {
      if (!(sw && sw.controller && registration)) return false;
      registration.showNotification(title, options)
        .catch(e => console.warn("prigh-web: notification:", e));
      return true;
    },
  };
})()|})
;;

let register () =
  ignore (Js.Unsafe.meth_call (Lazy.force glue) "register" [||] : Js.Unsafe.any)
;;

let on_update_ready f =
  ignore
    (Js.Unsafe.meth_call
       (Lazy.force glue)
       "onUpdateReady"
       [| Js.Unsafe.inject (Js.wrap_callback f) |]
     : Js.Unsafe.any)
;;

let apply_update () =
  ignore
    (Js.Unsafe.meth_call (Lazy.force glue) "applyUpdate" [||] : Js.Unsafe.any)
;;

let show_notification ~title options =
  Js.to_bool
    (Js.Unsafe.meth_call
       (Lazy.force glue)
       "showNotification"
       [| Js.Unsafe.inject (Js.string title); Js.Unsafe.inject options |])
;;
