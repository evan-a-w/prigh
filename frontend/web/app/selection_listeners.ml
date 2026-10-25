open! Core
open Js_of_ocaml
module Point = Prigh_web.Code_selection.Point
module Side = Prigh_protocol.Review_comment.Side

let call obj name args : 'a = Js.Unsafe.meth_call obj name args
let inject = Js.Unsafe.inject

let element_of (node : Dom.node Js.t) : Dom_html.element Js.t option =
  match node##.nodeType with
  | Dom.ELEMENT -> Some (Js.Unsafe.coerce node)
  | _ -> Js.Opt.to_option node##.parentNode |> Option.map ~f:Js.Unsafe.coerce
;;

let closest (node : Dom.node Js.t) selector : Dom_html.element Js.t option =
  Option.bind (element_of node) ~f:(fun el ->
    Js.Opt.to_option (call el "closest" [| inject (Js.string selector) |]))
;;

let attribute (el : Dom_html.element Js.t) name =
  Js.Opt.to_option (el##getAttribute (Js.string name))
  |> Option.map ~f:Js.to_string
;;

(* -1, 0 or 1: the point before, in or after [el]'s contents. *)
let contents (el : Dom_html.element Js.t) =
  let range = call Dom_html.document "createRange" [||] in
  ignore (call range "selectNodeContents" [| inject el |] : Js.Unsafe.any);
  range
;;

let compare_point el node offset : int =
  call (contents el) "comparePoint" [| inject node; inject offset |]
;;

(* How far into [row]'s text the point is, in UTF-16 code units. *)
let offset_in (row : Dom_html.element Js.t) node offset =
  match Js.Opt.to_option (row##querySelector (Js.string "td.text")) with
  | None -> 0
  | Some td ->
    (match compare_point td node offset with
     | c when c < 0 -> 0
     | c when c > 0 -> Int.max_value
     | _ ->
       let range = contents td in
       ignore
         (call range "setEnd" [| inject node; inject offset |] : Js.Unsafe.any);
       (call range "toString" [||] : Js.js_string Js.t)##.length)
;;

let point_of row ~offset =
  let open Option.Let_syntax in
  let%bind side =
    match attribute row "data-side" with
    | Some "old" -> Some Side.Old
    | Some "new" -> Some Side.New
    | _ -> None
  in
  let%map line = Option.bind (attribute row "data-line") ~f:Int.of_string_opt in
  { Point.side; line; offset }
;;

let rows (code : Dom_html.element Js.t) =
  Dom.list_of_nodeList (code##querySelectorAll (Js.string "tr[data-line]"))
;;

(* An end of the selection: its row, or (outside the rows: in a thread, past
   the table) the first row after it for the start and the last before it
   for the end. *)
let endpoint code node offset ~start =
  match closest node "tr[data-line]" with
  | Some row -> point_of row ~offset:(offset_in row node offset)
  | None ->
    let rows = rows code in
    let rows = if start then rows else List.rev rows in
    List.find_map rows ~f:(fun row ->
      let c = compare_point row node offset in
      if start && c <= 0
      then point_of row ~offset:0
      else if (not start) && c >= 0
      then point_of row ~offset:Int.max_value
      else None)
;;

(* Below the selection's end, inside the code view. *)
let pop_at (code : Dom_html.element Js.t) range =
  let rects = call range "getClientRects" [||] in
  let n = (Js.Unsafe.get rects "length" : int) in
  let rect i = Js.Unsafe.get rects i in
  let width r = Js.to_float (Js.Unsafe.get r "width") in
  let last =
    List.find
      (List.rev (List.range 0 n))
      ~f:(fun i -> Float.(width (rect i) > 0.))
  in
  Option.map last ~f:(fun i ->
    let r = rect i in
    let box = code##getBoundingClientRect in
    let get name = Js.to_float (Js.Unsafe.get r name) in
    let left = get "right" -. Js.to_float box##.left in
    let top = get "bottom" -. Js.to_float box##.top +. 4. in
    let max_left = Js.to_float box##.right -. Js.to_float box##.left -. 132. in
    { Prigh_web.Code_review.Pop.left =
        Float.iround_nearest_exn
          (Float.max 0. (Float.min (left -. 24.) max_left))
    ; top = Float.iround_nearest_exn top
    })
;;

let in_field () =
  match Js.Opt.to_option Dom_html.document##.activeElement with
  | Some el ->
    (match String.lowercase (Js.to_string el##.tagName) with
     | "textarea" | "input" -> true
     | _ -> false)
  | None -> false
;;

(* What the selection is now, as the review takes it. *)
let current () =
  let open Option.Let_syntax in
  let selection = Dom_html.window##getSelection in
  let%bind () =
    Option.some_if
      (selection##.rangeCount > 0
       && (not (Js.to_bool selection##.isCollapsed))
       && not (in_field ()))
      ()
  in
  let range = selection##getRangeAt 0 in
  let get name = Js.Unsafe.get range name in
  let start_node : Dom.node Js.t = get "startContainer" in
  let end_node : Dom.node Js.t = get "endContainer" in
  let%bind code =
    Option.first_some
      (closest start_node ".review-code")
      (closest end_node ".review-code")
  in
  let%bind path = attribute code "data-path" in
  let in_row node = Option.is_some (closest node "tr[data-line]") in
  let%bind () = Option.some_if (in_row start_node || in_row end_node) () in
  let%bind start = endpoint code start_node (get "startOffset") ~start:true in
  let%map end_ = endpoint code end_node (get "endOffset") ~start:false in
  Prigh_web.App.Action.Review
    (Select_text { path; start; end_; pop = pop_at code range })
;;

let reported = ref None
let pressed = ref false
let scheduled = ref false

let report ~schedule =
  let now = current () in
  match now, !reported with
  | Some action, Some last when Poly.equal action last -> ()
  | Some action, _ ->
    reported := Some action;
    schedule action
  | None, Some _ ->
    reported := None;
    schedule (Prigh_web.App.Action.Review Clear_selection)
  | None, None -> ()
;;

let report_soon ~schedule =
  if not !scheduled
  then (
    scheduled := true;
    ignore
      (Dom_html.window##requestAnimationFrame
         (Js.wrap_callback (fun (_ : Js.number_t) ->
            scheduled := false;
            report ~schedule))
       : Dom_html.animation_frame_request_id))
;;

let clear () =
  reported := None;
  let selection = Dom_html.window##getSelection in
  if not (in_field ()) then selection##removeAllRanges
;;

let install ~schedule =
  let listen event f =
    ignore
      (Dom_html.addEventListener
         Dom_html.document
         event
         (Dom.handler (fun ev ->
            f ev;
            Js._true))
         Js._true
       : Dom_html.event_listener_id)
  in
  (* While the mouse drags, the selection is not done yet. *)
  listen Dom_html.Event.mousedown (fun (ev : Dom_html.mouseEvent Js.t) ->
    if ev##.button = 0 then pressed := true);
  listen Dom_html.Event.mouseup (fun (_ : Dom_html.mouseEvent Js.t) ->
    pressed := false;
    report_soon ~schedule);
  listen
    (Dom_html.Event.make "selectionchange")
    (fun (_ : Dom_html.event Js.t) ->
       if not !pressed then report_soon ~schedule)
;;
