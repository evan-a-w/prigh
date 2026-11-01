open! Core
open Js_of_ocaml
module Zoom = Prigh_web.Zoom
module Icon = Prigh_web.Html.Icon
module Action = Prigh_web.App.Action

let get o name = Js.Unsafe.get o (Js.string name)
let meth o name args = Js.Unsafe.meth_call o name args
let float_of o name : float = Js.float_of_number (get o name)
let document = Dom_html.document

let query (el : #Dom_html.element Js.t) selector =
  Js.Opt.to_option (el##querySelector (Js.string selector))
;;

let attribute (el : #Dom_html.element Js.t) name =
  Js.Opt.to_option (el##getAttribute (Js.string name))
  |> Option.map ~f:Js.to_string
;;

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (meth el "closest" [| Js.Unsafe.inject (Js.string selector) |])
;;

(* One image the viewer can show: its thumbnail's button (focus goes back to
   it), the source, a caption and whether Draw is offered. *)
type item =
  { opener : Dom_html.element Js.t
  ; src : string
  ; name : string
  ; draw : bool
  }

type pointer =
  { mutable px : float
  ; mutable py : float
  }

type t =
  { dialog : Dom_html.element Js.t
  ; stage : Dom_html.element Js.t
  ; img : Dom_html.imageElement Js.t
  ; items : item array
  ; mutable index : int
  ; mutable zoom : Zoom.t option
  ; pointers : (int, pointer) Hashtbl.t
  ; mutable moved : float (* how far the pointer went since it went down *)
  ; mutable pinch : float option (* the two pointers' distance *)
  ; mutable last_tap : (float * float * float) option (* time, x, y *)
  ; mutable swipe : (float * float * float) option
  (* A finger down on the image at fit: where and when (a swipe between
     images, or down to close) *)
  ; restore : Dom_html.element Js.t option
  }

let current : t option ref = ref None
let schedule = ref (fun (_ : Action.t) -> ())

let item_of (el : Dom_html.element Js.t) =
  let img =
    match Js.Opt.to_option (Dom_html.CoerceTo.img el) with
    | Some img -> Some img
    | None ->
      Option.bind (query el "img") ~f:(fun i ->
        Js.Opt.to_option (Dom_html.CoerceTo.img i))
  in
  Option.map img ~f:(fun img ->
    let alt = Js.to_string img##.alt in
    let name =
      match attribute el Prigh_web.Image_view.viewer_attribute with
      | Some n when not (String.is_empty n) -> n
      | _ -> alt
    in
    { opener = el
    ; src = Js.to_string img##.src
    ; name
    ; draw = Option.is_some (attribute el "data-viewer-draw")
    })
;;

let button ?(cls = "") ~action ~label ?(key = "") body =
  sprintf
    {|<button type="button" class="viewer-btn %s" data-v="%s" aria-label="%s" title="%s%s">%s</button>|}
    cls
    action
    label
    label
    (if String.is_empty key then "" else sprintf " (%s)" key)
    body
;;

let markup ~many =
  String.concat
    [ {|<div class="viewer-bar">|}
    ; {|<div class="viewer-caption"><span class="viewer-count"></span><span class="viewer-name"></span></div>|}
    ; {|<div class="viewer-tools" role="toolbar" aria-label="Image">|}
    ; {|<div class="viewer-group viewer-zoom">|}
    ; button ~action:"out" ~label:"Zoom out" ~key:"−" (Icon.svg Zoom_out)
    ; button
        ~cls:"viewer-pct"
        ~action:"toggle"
        ~label:"Zoom level: fit or 100%"
        ~key:"0, 1"
        ""
    ; button ~action:"in" ~label:"Zoom in" ~key:"+" (Icon.svg Zoom_in)
    ; {|</div>|}
    ; {|<div class="viewer-group viewer-sizes">|}
    ; button ~action:"fit" ~label:"Fit to screen" ~key:"0" (Icon.svg Fit)
    ; button
        ~cls:"viewer-actual"
        ~action:"actual"
        ~label:"Actual size, 100%"
        ~key:"1"
        "1:1"
    ; {|</div>|}
    ; {|<div class="viewer-group">|}
    ; button
        ~cls:"viewer-draw"
        ~action:"draw"
        ~label:"Draw on this image"
        (Icon.svg Pencil ^ "<span>Draw</span>")
    ; {|<a class="viewer-btn" data-v="download" aria-label="Download" title="Download">|}
    ; Icon.svg Download
    ; {|</a>|}
    ; {|</div>|}
    ; button
        ~cls:"viewer-close"
        ~action:"close"
        ~label:"Close"
        ~key:"Esc"
        (Icon.svg Close)
    ; {|</div></div>|}
    ; {|<div class="viewer-stage"><img class="viewer-img" draggable="false" alt=""/>|}
    ; (if many
       then
         button
           ~cls:"viewer-nav viewer-prev"
           ~action:"prev"
           ~label:"Previous image"
           ~key:"←"
           (Icon.svg Prev)
         ^ button
             ~cls:"viewer-nav viewer-next"
             ~action:"next"
             ~label:"Next image"
             ~key:"→"
             (Icon.svg Next)
       else "")
    ; {|</div>|}
    ]
;;

(* The stage's room for the image: its box less a margin, so the image
   never touches the bar or the screen's edge. *)
let stage_size v : Zoom.size =
  let margin = if v.stage##.clientWidth < 600 then 8. else 24. in
  { w = Float.max 1. (Float.of_int v.stage##.clientWidth -. (2. *. margin))
  ; h = Float.max 1. (Float.of_int v.stage##.clientHeight -. (2. *. margin))
  }
;;

let set_text v selector text =
  Option.iter (query v.dialog selector) ~f:(fun el ->
    el##.textContent := Js.some (Js.string text))
;;

let set_disabled v action disabled =
  Option.iter
    (query v.dialog (sprintf "[data-v=%s]" action))
    ~f:(fun el ->
      if disabled
      then el##setAttribute (Js.string "disabled") (Js.string "")
      else el##removeAttribute (Js.string "disabled"))
;;

let render ?(animate = false) v =
  Option.iter v.zoom ~f:(fun (z : Zoom.t) ->
    let cl = v.dialog##.classList in
    if animate
    then cl##add (Js.string "animate")
    else cl##remove (Js.string "animate");
    v.img##.style##.transform := Js.string (Zoom.transform z);
    let fit = Zoom.at_fit z in
    if fit
    then cl##remove (Js.string "zoomed")
    else cl##add (Js.string "zoomed");
    set_text v ".viewer-pct" (Zoom.percent z);
    set_disabled v "out" Float.(z.scale <= Zoom.min_scale z +. 1e-3);
    set_disabled v "in" Float.(z.scale >= Zoom.max_scale z -. 1e-3);
    set_disabled v "fit" fit;
    set_disabled v "actual" Float.(abs (z.scale -. 1.) < 1e-3))
;;

let update ?animate v f =
  Option.iter v.zoom ~f:(fun z ->
    v.zoom <- Some (f z);
    render ?animate v)
;;

(* From the page's coordinates to the stage centre's. *)
let at v ~x ~y =
  let r = v.stage##getBoundingClientRect in
  let left = Js.to_float r##.left
  and top = Js.to_float r##.top in
  ( x -. left -. (Float.of_int v.stage##.clientWidth /. 2.)
  , y -. top -. (Float.of_int v.stage##.clientHeight /. 2.) )
;;

let sized v =
  let w = v.img##.naturalWidth
  and h = v.img##.naturalHeight in
  if w > 0 && h > 0
  then (
    let s = v.img##.style in
    s##.width := Js.string (sprintf "%dpx" w);
    s##.height := Js.string (sprintf "%dpx" h);
    s##.marginLeft := Js.string (sprintf "%gpx" (-.Float.of_int w /. 2.));
    s##.marginTop := Js.string (sprintf "%gpx" (-.Float.of_int h /. 2.));
    v.zoom
    <- Some
         (Zoom.create
            ~stage:(stage_size v)
            ~image:{ w = Float.of_int w; h = Float.of_int h });
    v.dialog##.classList##add (Js.string "ready");
    render v)
;;

let extension src =
  match String.lsplit2 src ~on:';' with
  | Some (header, _) when String.is_prefix header ~prefix:"data:image/" ->
    (match String.chop_prefix_exn header ~prefix:"data:image/" with
     | "jpeg" -> ".jpg"
     | "svg+xml" -> ".svg"
     | e -> "." ^ e)
  | _ -> ""
;;

let show v index =
  let n = Array.length v.items in
  v.index <- ((index % n) + n) % n;
  let item = v.items.(v.index) in
  v.zoom <- None;
  v.dialog##.classList##remove (Js.string "ready");
  v.img##.alt := Js.string item.name;
  set_text v ".viewer-name" item.name;
  set_text
    v
    ".viewer-count"
    (if n > 1 then sprintf "%d / %d" (v.index + 1) n else "");
  Option.iter (query v.dialog ".viewer-draw") ~f:(fun el ->
    el##.hidden := Js.bool (not item.draw));
  Option.iter (query v.dialog "[data-v=download]") ~f:(fun el ->
    el##setAttribute (Js.string "href") (Js.string item.src);
    let file =
      if String.is_prefix item.src ~prefix:"data:"
      then "image" ^ extension item.src
      else item.name
    in
    el##setAttribute (Js.string "download") (Js.string file));
  v.img##.onload
  := Dom.handler (fun _ ->
       sized v;
       Js._true);
  v.img##.src := Js.string item.src;
  if Js.to_bool v.img##.complete then sized v
;;

let close () =
  Option.iter !current ~f:(fun v ->
    current := None;
    if Js.to_bool (get v.dialog "open")
    then ignore (meth v.dialog "close" [||] : Js.Unsafe.any);
    Js.Opt.iter v.dialog##.parentNode (fun p -> Dom.removeChild p v.dialog);
    Option.iter v.restore ~f:(fun el -> el##focus))
;;

let draw v =
  let item = v.items.(v.index) in
  match String.lsplit2 item.src ~on:',' with
  | Some (header, data) ->
    (match
       String.chop_prefix header ~prefix:"data:"
       |> Option.bind ~f:(String.chop_suffix ~suffix:";base64")
     with
     | Some mime_type ->
       close ();
       !schedule
         (Action.Annotate
            { image =
                { mime_type
                ; data
                ; bytes = Prigh_protocol.Image.decoded_size data
                }
            ; target = New
            })
     | None -> ())
  | None -> ()
;;

let act v action =
  let animate = true in
  match action with
  | "in" -> update ~animate v (fun z -> Zoom.zoom z ~factor:Zoom.step)
  | "out" -> update ~animate v (fun z -> Zoom.zoom z ~factor:(1. /. Zoom.step))
  | "fit" -> update ~animate v Zoom.fit
  | "actual" -> update ~animate v (fun z -> Zoom.actual z)
  | "toggle" ->
    update ~animate v (fun z ->
      if Zoom.at_fit z then Zoom.actual z else Zoom.fit z)
  | "prev" -> show v (v.index - 1)
  | "next" -> show v (v.index + 1)
  | "close" -> close ()
  | "draw" -> draw v
  | _ -> ()
;;

let key v (ev : Dom_html.keyboardEvent Js.t) =
  let k = Js.Optdef.case ev##.key (fun () -> "") Js.to_string in
  let ctrl = Js.to_bool ev##.ctrlKey || Js.to_bool ev##.metaKey in
  let pan dx dy =
    let step = if Js.to_bool ev##.shiftKey then 0.5 else 0.15 in
    let s = stage_size v in
    update v (fun z ->
      Zoom.pan z ~dx:(dx *. s.w *. step) ~dy:(dy *. s.h *. step))
  in
  let zoomed () =
    Option.value_map v.zoom ~default:false ~f:(fun z -> not (Zoom.at_fit z))
  in
  let many = Array.length v.items > 1 in
  let handled =
    match k with
    | ("+" | "=") when not ctrl ->
      act v "in";
      true
    | ("-" | "_") when not ctrl ->
      act v "out";
      true
    | "0" when not ctrl ->
      act v "fit";
      true
    | "1" when not ctrl ->
      act v "actual";
      true
    | "ArrowLeft" when many && not (zoomed ()) ->
      act v "prev";
      true
    | "ArrowRight" when many && not (zoomed ()) ->
      act v "next";
      true
    | "ArrowLeft" ->
      pan 1. 0.;
      true
    | "ArrowRight" ->
      pan (-1.) 0.;
      true
    | "ArrowUp" ->
      pan 0. 1.;
      true
    | "ArrowDown" ->
      pan 0. (-1.);
      true
    | "Escape" ->
      close ();
      true
    | _ -> false
  in
  if handled then Dom.preventDefault ev;
  (* The page's shortcuts (the composer, Ctrl+K…) are not for the viewer. *)
  Dom_html.stopPropagation ev
;;

let wheel v ev =
  ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
  let dy = float_of ev "deltaY" in
  let dy =
    match (get ev "deltaMode" : int) with
    | 1 -> dy *. 16.
    | 2 -> dy *. 400.
    | _ -> dy
  in
  (* A pinch on a trackpad comes as Ctrl+wheel with small steps. *)
  let rate = if Js.to_bool (get ev "ctrlKey") then 0.01 else 0.002 in
  let factor = Float.exp (-.dy *. rate) in
  let x = float_of ev "clientX"
  and y = float_of ev "clientY" in
  update v (fun z -> Zoom.zoom ~at:(at v ~x ~y) z ~factor)
;;

let distance v =
  match Hashtbl.data v.pointers with
  | [ a; b ] ->
    Some
      ( Float.hypot (a.px -. b.px) (a.py -. b.py)
      , ((a.px +. b.px) /. 2., (a.py +. b.py) /. 2.) )
  | _ -> None
;;

let now () = Js.to_float (new%js Js.date_now)##getTime

(* The image follows the finger; the page behind fades as it goes down. *)
let swiped v ~dx ~dy =
  let style = v.img##.style in
  if Float.(dx = 0. && dy = 0.)
  then (
    ignore (meth style "removeProperty" [| Js.Unsafe.inject (Js.string "translate") |] : Js.Unsafe.any);
    ignore (meth v.dialog##.style "removeProperty" [| Js.Unsafe.inject (Js.string "--swipe") |] : Js.Unsafe.any))
  else (
    ignore
      (meth style "setProperty"
         [| Js.Unsafe.inject (Js.string "translate")
          ; Js.Unsafe.inject (Js.string (sprintf "%.0fpx %.0fpx" dx (Float.max 0. dy)))
         |]
       : Js.Unsafe.any);
    ignore
      (meth v.dialog##.style "setProperty"
         [| Js.Unsafe.inject (Js.string "--swipe")
          ; Js.Unsafe.inject (Js.string (sprintf "%.3f" (Float.min 1. (Float.max 0. dy /. 400.))))
         |]
       : Js.Unsafe.any))
;;

let pointer_down v ev =
  let target : Dom_html.element Js.t = get ev "target" in
  if Option.is_none (closest target ".viewer-bar, .viewer-nav")
  then (
    let x = float_of ev "clientX"
    and y = float_of ev "clientY" in
    (* A first finger or the mouse: forget any pointer whose up was lost
       (outside the window). *)
    if Js.to_bool (get ev "isPrimary") then Hashtbl.clear v.pointers;
    (try
       ignore
         (meth v.stage "setPointerCapture" [| get ev "pointerId" |]
          : Js.Unsafe.any)
     with
     | _ -> ());
    Hashtbl.set v.pointers ~key:(get ev "pointerId") ~data:{ px = x; py = y };
    if Hashtbl.length v.pointers = 1 then v.moved <- 0.;
    v.pinch <- Option.map (distance v) ~f:fst;
    (* One finger at fit drags the image itself: aside to the next, down
       to close. *)
    v.swipe
    <- (if
          Hashtbl.length v.pointers = 1
          && String.equal (Js.to_string (get ev "pointerType")) "touch"
          && Option.value_map v.zoom ~default:false ~f:Zoom.at_fit
        then Some (x, y, now ())
        else (
          swiped v ~dx:0. ~dy:0.;
          None));
    v.dialog##.classList##add (Js.string "dragging"))
;;

let pointer_move v ev =
  match Hashtbl.find v.pointers (get ev "pointerId") with
  | None -> ()
  | Some p ->
    let x = float_of ev "clientX"
    and y = float_of ev "clientY" in
    let before = distance v in
    let dx = x -. p.px
    and dy = y -. p.py in
    p.px <- x;
    p.py <- y;
    v.moved <- v.moved +. Float.abs dx +. Float.abs dy;
    match v.swipe with
    | Some (x0, y0, _) when Hashtbl.length v.pointers = 1 ->
      let dx = x -. x0
      and dy = y -. y0 in
      (* Locked to the way it started: aside, or down. *)
      if Float.(abs dx > abs dy) then swiped v ~dx ~dy:0. else swiped v ~dx:0. ~dy
    | _ ->
    (match before, distance v with
     | Some (d0, (mx0, my0)), Some (d1, (mx1, my1)) ->
       let d0 = Option.value v.pinch ~default:d0 in
       v.pinch <- Some d1;
       update v (fun z ->
         Zoom.pan
           (Zoom.zoom
              ~at:(at v ~x:mx1 ~y:my1)
              z
              ~factor:(d1 /. Float.max 1. d0))
           ~dx:(mx1 -. mx0)
           ~dy:(my1 -. my0))
     | _ -> update v (fun z -> Zoom.pan z ~dx ~dy))
;;

let pointer_up v ev =
  match Hashtbl.find v.pointers (get ev "pointerId") with
  | None -> ()
  | Some _ ->
    Hashtbl.remove v.pointers (get ev "pointerId");
    v.pinch <- None;
    if Hashtbl.is_empty v.pointers
    then (
      v.dialog##.classList##remove (Js.string "dragging");
      let x = float_of ev "clientX"
      and y = float_of ev "clientY" in
      let swipe = v.swipe in
      v.swipe <- None;
      swiped v ~dx:0. ~dy:0.;
      let flung =
        match swipe with
        | Some (x0, y0, t0)
          when not (String.equal (get ev "type" |> Js.to_string) "pointercancel") ->
          let dx = x -. x0
          and dy = y -. y0 in
          let speed d = Float.abs d /. Float.max 1. (now () -. t0) in
          if Float.(abs dy > abs dx && dy > 0. && (dy > 120. || (dy > 30. && speed dy > 0.5)))
          then (
            close ();
            true)
          else if Float.(abs dx > abs dy && (abs dx > 70. || (abs dx > 30. && speed dx > 0.4)))
                  && Array.length v.items > 1
          then (
            act v (if Float.(dx < 0.) then "next" else "prev");
            true)
          else false
        | _ -> false
      in
      if flung
      then v.last_tap <- None
      else if
        Float.(v.moved < 6.)
        && not (String.equal (get ev "type" |> Js.to_string) "pointercancel")
      then (
        let target : Dom_html.element Js.t = get ev "target" in
        let on_image =
          (* The capture sends everything to the stage: is it over the image? *)
          let r = v.img##getBoundingClientRect in
          Float.(
            x >= Js.to_float r##.left
            && x <= Js.to_float r##.right
            && y >= Js.to_float r##.top
            && y <= Js.to_float r##.bottom)
        in
        ignore target;
        let t = now () in
        match v.last_tap with
        | Some (t0, x0, y0)
          when Float.(
                 t -. t0 < 350. && abs (x -. x0) < 30. && abs (y -. y0) < 30.)
          ->
          v.last_tap <- None;
          update ~animate:true v (fun z -> Zoom.toggle ~at:(at v ~x ~y) z)
        | _ ->
          if on_image
          then v.last_tap <- Some (t, x, y)
          else (
            (* A tap beside the image closes, as before. *)
            v.last_tap <- None;
            close ())))
;;

let resize v =
  Option.iter v.zoom ~f:(fun z ->
    v.zoom <- Some (Zoom.resize z ~stage:(stage_size v));
    render v)
;;

let listen (el : Dom_html.element Js.t) event handler =
  ignore
    (Dom_html.addEventListener
       el
       (Dom.Event.make event)
       (Dom.handler (fun ev ->
          handler (Js.Unsafe.coerce ev);
          Js._true))
       Js._false
     : Dom_html.event_listener_id)
;;

let open_ (items : item array) index =
  close ();
  let restore = Js.Opt.to_option document##.activeElement in
  let dialog = Dom_html.createDialog document in
  let dialog = (dialog :> Dom_html.element Js.t) in
  dialog##.className := Js.string "viewer";
  dialog##setAttribute (Js.string "aria-label") (Js.string "Image viewer");
  dialog##.innerHTML := Js.string (markup ~many:(Array.length items > 1));
  Dom.appendChild document##.body dialog;
  match query dialog ".viewer-stage", query dialog ".viewer-img" with
  | Some stage, Some img ->
    let img : Dom_html.imageElement Js.t = Js.Unsafe.coerce img in
    let v =
      { dialog
      ; stage
      ; img
      ; items
      ; index
      ; zoom = None
      ; pointers = Hashtbl.create (module Int)
      ; moved = 0.
      ; pinch = None
      ; last_tap = None
      ; swipe = None
      ; restore
      }
    in
    current := Some v;
    listen dialog "keydown" (fun ev -> key v ev);
    (* Esc on a modal dialog: its cancel, which would close it without us. *)
    listen dialog "cancel" (fun ev ->
      ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
      close ());
    listen dialog "click" (fun ev ->
      let target : Dom_html.element Js.t = get ev "target" in
      Option.iter (closest target "[data-v]") ~f:(fun el ->
        match attribute el "data-v" with
        | Some "download" | None -> ()
        | Some a ->
          ignore (meth ev "preventDefault" [||] : Js.Unsafe.any);
          act v a));
    listen stage "wheel" (fun ev -> wheel v ev);
    listen stage "pointerdown" (fun ev -> pointer_down v ev);
    listen stage "pointermove" (fun ev -> pointer_move v ev);
    listen stage "pointerup" (fun ev -> pointer_up v ev);
    listen stage "pointercancel" (fun ev -> pointer_up v ev);
    ignore (meth dialog "showModal" [||] : Js.Unsafe.any);
    Option.iter (query dialog ".viewer-close") ~f:(fun el -> el##focus);
    show v index
  | _ -> Js.Opt.iter dialog##.parentNode (fun p -> Dom.removeChild p dialog)
;;

let viewer_selector = sprintf "[%s]" Prigh_web.Image_view.viewer_attribute

let on_click (ev : Dom_html.mouseEvent Js.t) =
  match Js.Opt.to_option ev##.target with
  | None -> ()
  | Some target ->
    (match closest target viewer_selector with
     | Some el when Option.is_none (closest el ".viewer") ->
       let group =
         match
           closest el (sprintf "[%s]" Prigh_web.Image_view.group_attribute)
         with
         | Some g ->
           Dom.list_of_nodeList
             (g##querySelectorAll (Js.string viewer_selector))
         | None -> [ el ]
       in
       let items = List.filter_map group ~f:item_of |> Array.of_list in
       (match Array.findi items ~f:(fun _ i -> phys_equal i.opener el) with
        | Some (index, _) ->
          Dom.preventDefault ev;
          Dom_html.stopPropagation ev;
          open_ items index
        | None -> ())
     | Some _ | None -> ())
;;

let is_open () = Option.is_some !current

let install ~schedule:f =
  schedule := f;
  ignore
    (Dom_html.addEventListener
       document
       Dom_html.Event.click
       (Dom.handler (fun ev ->
          on_click ev;
          Js._true))
       Js._false
     : Dom_html.event_listener_id);
  ignore
    (Dom_html.addEventListener
       Dom_html.window
       Dom_html.Event.resize
       (Dom.handler (fun _ ->
          Option.iter !current ~f:resize;
          Js._true))
       Js._false
     : Dom_html.event_listener_id)
;;
