open! Core
open Js_of_ocaml

(* The browser glue, in JavaScript: it is all promises and events. *)
let glue : Js.Unsafe.any Lazy.t =
  lazy
    (Js.Unsafe.js_expr
       {|(function () {
  const sw = window.isSecureContext && navigator.serviceWorker;
  let registration = null;
  let updating = false;
  let onUpdateReady = null;
  let told = false;
  const tell = () => {
    if (onUpdateReady && !told && registration.waiting && sw.controller) {
      told = true;
      onUpdateReady();
    }
  };
  return {
    register: function () {
      if (!sw) return;
      sw.addEventListener("controllerchange", () => {
        if (updating) window.location.reload();
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
      const waiting = registration && registration.waiting;
      if (!waiting) { window.location.reload(); return; }
      updating = true;
      waiting.postMessage("skip-waiting");
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
