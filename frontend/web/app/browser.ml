open! Core
open Js_of_ocaml

let storage () = Js.Optdef.to_option Dom_html.window##.localStorage

let get_item key =
  Option.bind (storage ()) ~f:(fun s ->
    Js.Opt.to_option (s##getItem (Js.string key)) |> Option.map ~f:Js.to_string)
;;

let set_item key value =
  Option.iter (storage ()) ~f:(fun s ->
    s##setItem (Js.string key) (Js.string value))
;;

let remove_item key =
  Option.iter (storage ()) ~f:(fun s -> s##removeItem (Js.string key))
;;

let query_param name =
  let search = Js.to_string Dom_html.window##.location##.search in
  let search =
    String.chop_prefix search ~prefix:"?" |> Option.value ~default:search
  in
  List.find_map (String.split search ~on:'&') ~f:(fun pair ->
    match String.lsplit2 pair ~on:'=' with
    | Some (k, v) when String.equal k name ->
      Some (Js.to_string (Js.decodeURIComponent (Js.string v)))
    | _ -> None)
;;

let same_origin_ws_url () =
  let location = Dom_html.window##.location in
  let scheme =
    if String.equal (Js.to_string location##.protocol) "https:"
    then "wss"
    else "ws"
  in
  sprintf "%s://%s/ws" scheme (Js.to_string location##.host)
;;

(* Without the Clipboard API (only in secure contexts: not on a plain http
   address other than localhost) or when it refuses: the old way, selecting
   the text in a textarea out of sight and asking for a copy, then putting
   back the focus and the selection. [readonly] keeps a phone's keyboard
   shut; iOS selects only with [setSelectionRange]. *)
let exec_copy : Js.js_string Js.t -> bool Js.t =
  Js.Unsafe.js_expr
    {js|(function (text) {
  var doc = document, active = doc.activeElement, sel = doc.getSelection();
  var ranges = [];
  if (sel) for (var i = 0; i < sel.rangeCount; i++) ranges.push(sel.getRangeAt(i));
  var ta = doc.createElement("textarea");
  ta.value = text;
  ta.setAttribute("readonly", "");
  ta.setAttribute("aria-hidden", "true");
  ta.tabIndex = -1;
  ta.style.cssText =
    "position:fixed;top:0;left:0;width:1px;height:1px;padding:0;border:0;" +
    "margin:0;opacity:0;pointer-events:none;font-size:16px;contain:strict";
  doc.body.appendChild(ta);
  var ok = false;
  try {
    ta.focus({ preventScroll: true });
    ta.select();
    ta.setSelectionRange(0, text.length);
    ok = doc.execCommand("copy");
  } catch (e) { ok = false; }
  ta.remove();
  try {
    if (active && active !== doc.body && active.focus) active.focus({ preventScroll: true });
    if (sel) { sel.removeAllRanges(); ranges.forEach(function (r) { sel.addRange(r); }); }
  } catch (e) {}
  return !!ok;
})|js}
;;

let fallback_copy text = Js.to_bool (exec_copy (Js.string text))

let copy_to_clipboard text =
  let navigator = Js.Unsafe.get Dom_html.window (Js.string "navigator") in
  let clipboard = Js.Unsafe.get navigator (Js.string "clipboard") in
  if
    Js.Optdef.test clipboard
    && Js.Opt.test clipboard
    && Js.Optdef.test (Js.Unsafe.get clipboard (Js.string "writeText"))
  then (
    let result = Async_kernel.Ivar.create () in
    let answer ok = Async_kernel.Ivar.fill_if_empty result ok in
    (try
       let promise =
         Js.Unsafe.meth_call
           clipboard
           "writeText"
           [| Js.Unsafe.inject (Js.string text) |]
       in
       ignore
         (Js.Unsafe.meth_call
            promise
            "then"
            [| Js.Unsafe.inject (Js.wrap_callback (fun _ -> answer true))
             ; Js.Unsafe.inject
                 (Js.wrap_callback (fun _ -> answer (fallback_copy text)))
            |]
          : Js.Unsafe.any)
     with
     | _ -> answer (fallback_copy text));
    Async_kernel.Ivar.read result)
  else Async_kernel.Deferred.return (fallback_copy text)
;;

let secure_context () =
  Js.to_bool (Js.Unsafe.get Dom_html.window (Js.string "isSecureContext"))
;;

(* The visual viewport is what the user actually sees: unlike the layout
   viewport (window.innerHeight) it shrinks when the on-screen keyboard opens
   and moves when the browser scrolls a focused field into view. *)
module Viewport = struct
  let visual () =
    let vv = Js.Unsafe.get Dom_html.window (Js.string "visualViewport") in
    if Js.Optdef.test vv && Js.Opt.test vv then Some vv else None
  ;;

  let listen target event f =
    ignore
      (Dom_html.addEventListener
         target
         (Dom_html.Event.make event)
         (Dom.handler (fun _ ->
            f ();
            Js._true))
         Js._false
       : Dom_html.event_listener_id)
  ;;

  let on_change f =
    listen Dom_html.window "resize" f;
    Option.iter (visual ()) ~f:(fun vv ->
      let vv : Dom_html.eventTarget Js.t = Js.Unsafe.coerce vv in
      listen vv "resize" f;
      listen vv "scroll" f)
  ;;
end

let waiting_for_render = Queue.create ()
let after_render f = Queue.enqueue waiting_for_render f

(* In a microtask: after the mutation observers' (queued as Bonsai patched
   the DOM), which keep the chat's scroll position, and still before the
   browser paints. *)
let rendered () =
  let run () =
    let callbacks = Queue.to_list waiting_for_render in
    Queue.clear waiting_for_render;
    List.iter callbacks ~f:(fun f -> f ())
  in
  if not (Queue.is_empty waiting_for_render)
  then Js.Unsafe.global##queueMicrotask (Js.wrap_callback run)
;;

let on_viewport_change = Viewport.on_change

let href_with_backend ~pathname ~search ~backend =
  let search =
    String.chop_prefix search ~prefix:"?" |> Option.value ~default:search
  in
  let search =
    String.split search ~on:'&'
    |> List.filter ~f:(fun pair ->
      let key =
        String.lsplit2 pair ~on:'=' |> Option.value_map ~default:pair ~f:fst
      in
      not (String.equal key "backend" || String.equal key "token"))
    |> String.concat ~sep:"&"
  in
  let suffix = if String.is_empty search then "" else "&" ^ search in
  pathname
  ^ "?backend="
  ^ Js.to_string (Js.encodeURIComponent (Js.string backend))
  ^ suffix
;;

let pairs_without ~search name =
  let search =
    String.chop_prefix search ~prefix:"?" |> Option.value ~default:search
  in
  String.split search ~on:'&'
  |> List.filter ~f:(fun pair ->
    (not (String.is_empty pair))
    && not
         (String.equal
            (String.lsplit2 pair ~on:'='
             |> Option.value_map ~default:pair ~f:fst)
            name))
;;

let with_query_param ~search name value =
  "?"
  ^ String.concat
      ~sep:"&"
      (pairs_without ~search name
       @ [ name ^ "=" ^ Js.to_string (Js.encodeURIComponent (Js.string value)) ]
      )
;;

let without_query_param ~search name =
  let pairs = pairs_without ~search name in
  if List.is_empty pairs then "" else "?" ^ String.concat ~sep:"&" pairs
;;

let reload_without_query_param name =
  let location = Dom_html.window##.location in
  location##.href
  := Js.string
       (Js.to_string location##.pathname
        ^ without_query_param ~search:(Js.to_string location##.search) name)
;;

let replace_search f =
  let location = Dom_html.window##.location in
  let search = f ~search:(Js.to_string location##.search) in
  if not (String.equal search (Js.to_string location##.search))
  then
    Dom_html.window##.history##replaceState
      Js.null
      (Js.string "")
      (Js.some (Js.string (Js.to_string location##.pathname ^ search)))
;;

let replace_query_param name value =
  replace_search (fun ~search -> with_query_param ~search name value)
;;

let hash () = Js.to_string Dom_html.window##.location##.hash

let clear_hash () =
  let location = Dom_html.window##.location in
  Dom_html.window##.history##replaceState
    Js.null
    (Js.string "")
    (Js.some
       (Js.string
          (Js.to_string location##.pathname ^ Js.to_string location##.search)))
;;

let navigate url = Dom_html.window##.location##replace (Js.string url)

let post ~content_type ~url ~body ~f =
  let xhr = XmlHttpRequest.create () in
  xhr##_open (Js.string "POST") (Js.string url) Js._true;
  xhr##setRequestHeader (Js.string "Content-Type") (Js.string content_type);
  xhr##.onreadystatechange
  := Js.wrap_callback (fun () ->
       match xhr##.readyState with
       | XmlHttpRequest.DONE ->
         f
           (if xhr##.status = 200
            then
              Option.map (Js.Opt.to_option xhr##.responseText) ~f:Js.to_string
            else None)
       | _ -> ());
  xhr##send (Js.some (Js.string body))
;;

let set_app_html html =
  match Dom_html.getElementById_opt "app" with
  | Some el -> el##.innerHTML := Js.string html
  | None -> ()
;;

let input_value id =
  match Dom_html.getElementById_coerce id Dom_html.CoerceTo.input with
  | Some input -> Js.to_string input##.value
  | None -> ""
;;

let visual_height () =
  Option.map (Viewport.visual ()) ~f:(fun vv ->
    Js.float_of_number (Js.Unsafe.get vv (Js.string "height")))
;;

let post_status ~content_type ~url ~body ~f =
  let xhr = XmlHttpRequest.create () in
  xhr##_open (Js.string "POST") (Js.string url) Js._true;
  xhr##setRequestHeader (Js.string "Content-Type") (Js.string content_type);
  xhr##.onreadystatechange
  := Js.wrap_callback (fun () ->
       match xhr##.readyState with
       | XmlHttpRequest.DONE ->
         f
           ~status:xhr##.status
           ~body:
             (Option.value_map
                (Js.Opt.to_option xhr##.responseText)
                ~default:""
                ~f:Js.to_string)
       | _ -> ());
  xhr##send (Js.some (Js.string body))
;;

let submit_form ~url ~fields =
  let document = Dom_html.document in
  let form = Dom_html.createForm document in
  form##.action := Js.string url;
  form##._method := Js.string "POST";
  form##.style##.display := Js.string "none";
  List.iter fields ~f:(fun (name, value) ->
    let input =
      Dom_html.createInput
        ~_type:(Js.string "hidden")
        ~name:(Js.string name)
        document
    in
    input##.value := Js.string value;
    Dom.appendChild form input);
  Dom.appendChild document##.body form;
  form##submit
;;

let host () = Js.to_string Dom_html.window##.location##.host
