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
  return {
    register: function (onUpdateReady) {
      if (!sw) return;
      let told = false;
      const tell = () => {
        if (!told && registration.waiting && sw.controller) {
          told = true;
          onUpdateReady();
        }
      };
      sw.addEventListener("controllerchange", () => {
        if (updating) window.location.reload();
      });
      sw.register("sw.js").then(r => {
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

let register ~on_update_ready =
  ignore
    (Js.Unsafe.meth_call
       (Lazy.force glue)
       "register"
       [| Js.Unsafe.inject (Js.wrap_callback on_update_ready) |]
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
