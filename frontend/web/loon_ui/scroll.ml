open! Import
open Js_of_ocaml

let scroll selector =
  match
    Js.Opt.to_option (Dom_html.document##querySelector (Js.string selector))
  with
  | None -> ()
  | Some el ->
    (Js.Unsafe.coerce el)##scrollIntoView
      object%js
        val block = Js.string "nearest"
      end
;;

let into_view selector =
  Effect.of_thunk (fun () ->
    if Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)
    then (
      let (_ : Dom_html.animation_frame_request_id) =
        Dom_html.window##requestAnimationFrame
          (Js.wrap_callback (fun (_ : Js.number_t) -> scroll selector))
      in
      ()))
;;

let locks = ref 0

(* As react-remove-scroll-bar (Radix's RemoveScroll): the body hides its
   overflow and takes the page scrollbar's width as a right margin, so that
   nothing moves when the scrollbar goes. Its rules apply only while
   [data-scroll-locked] is on the body. Without react-remove-scroll-bar's
   [--removed-body-scroll-bar-size] (which nothing reads): an inherited
   custom property changed on the body has the browser compute every
   element's style again. *)
let lock_style_id = "loon-scroll-lock"

let lock_css ~left ~top ~right ~gap =
  sprintf
    {|body[data-scroll-locked]{overflow:hidden !important;overscroll-behavior:contain;position:relative !important;padding-left:%dpx;padding-top:%dpx;padding-right:%dpx;margin-left:0;margin-top:0;margin-right:%dpx !important}body[data-scroll-locked] .right-scroll-bar-position{right:%dpx !important}body[data-scroll-locked] .width-before-scroll-bar{margin-right:%dpx !important}body[data-scroll-locked] .right-scroll-bar-position .right-scroll-bar-position{right:0 !important}body[data-scroll-locked] .width-before-scroll-bar .width-before-scroll-bar{margin-right:0 !important}|}
    left
    top
    right
    gap
    gap
    gap
;;

let px_of s =
  match Float.of_string_opt (String.chop_suffix_if_exists s ~suffix:"px") with
  | Some f -> Float.to_int f
  | None -> 0
;;

(* The stylesheet stays in the document between locks, its text changed only
   when the measurements do: adding or changing a stylesheet makes the
   browser match every element's style again. *)
let lock_style : (Dom_html.styleElement Js.t * string) option ref = ref None

let set_lock_css doc css =
  match !lock_style with
  | Some (style, old) when Js.to_bool (Js.Unsafe.coerce style)##.isConnected ->
    if not (String.equal old css)
    then (
      style##.textContent := Js.some (Js.string css);
      lock_style := Some (style, css))
  | _ ->
    let style = Dom_html.createStyle doc in
    style##.id := Js.string lock_style_id;
    Dom.appendChild style (doc##createTextNode (Js.string css));
    Dom.appendChild doc##.head style;
    lock_style := Some (style, css)
;;

let add_lock () =
  let doc = Dom_html.document in
  let body = doc##.body in
  if !locks = 0
  then (
    let cs = Dom_html.window##getComputedStyle body in
    let left = px_of (Js.to_string cs##.marginLeft) in
    let top = px_of (Js.to_string cs##.marginTop) in
    let right = px_of (Js.to_string cs##.marginRight) in
    let window_width = Dom_html.window##.innerWidth in
    let gap = Int.max 0 (window_width - doc##.documentElement##.clientWidth + right - left) in
    set_lock_css doc (lock_css ~left ~top ~right ~gap));
  incr locks;
  body##setAttribute (Js.string "data-scroll-locked") (Js.string (Int.to_string !locks))
;;

let remove_lock () =
  let doc = Dom_html.document in
  decr locks;
  if !locks <= 0
  then (
    locks := 0;
    doc##.body##removeAttribute (Js.string "data-scroll-locked"))
  else
    doc##.body##setAttribute
      (Js.string "data-scroll-locked")
      (Js.string (Int.to_string !locks))
;;

module Lock_hook = Vdom.Attr.Hooks.Make (struct
    module State = Unit

    module Input = struct
      type t = unit [@@deriving sexp_of]

      let combine () () = ()
    end

    let init () _ = add_lock ()
    let on_mount = `Do_nothing
    let update ~old_input:() ~new_input:() () _ = ()
    let destroy () () _ = remove_lock ()
  end)

let lock = Vdom.Attr.create_hook "loon-scroll-lock" (Lock_hook.create ())
