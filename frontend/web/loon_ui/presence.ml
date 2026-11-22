open! Import
open Js_of_ocaml

let in_browser () = Js.Optdef.test (Js.Unsafe.global##.document : _ Js.Optdef.t)

let connected (node : Dom.node Js.t) =
  Js.to_bool (Js.Unsafe.coerce node)##.isConnected
;;

let elements (root : Dom_html.element Js.t) selector =
  Dom.list_of_nodeList (root##querySelectorAll (Js.string selector))
;;

let remove (el : Dom_html.element Js.t) =
  Js.Opt.iter el##.parentNode (fun parent -> Dom.removeChild parent el)
;;

let close_state (el : Dom_html.element Js.t) =
  el##setAttribute (Js.string "data-state") (Js.string "closed")
;;

let remove_ghosts (parent : Dom.node Js.t) =
  List.iter
    (Dom.list_of_nodeList parent##.childNodes)
    ~f:(fun (child : Dom.node Js.t) ->
      Js.Opt.iter (Dom_html.CoerceTo.element child) (fun el ->
        if Js.to_bool (el##hasAttribute (Js.string "data-presence-ghost"))
        then Dom.removeChild parent child))
;;

let ghost (el : Dom_html.element Js.t) (parent : Dom.node Js.t) =
  let copy : Dom_html.element Js.t = Js.Unsafe.coerce (el##cloneNode Js._true) in
  let selector =
    "[data-state=open], [data-state=delayed-open], [data-state=instant-open]"
  in
  let opened = elements copy selector in
  let opened =
    match Js.Opt.to_option (copy##getAttribute (Js.string "data-state")) with
    | Some s
      when List.mem
             [ "open"; "delayed-open"; "instant-open" ]
             (Js.to_string s)
             ~equal:String.equal -> copy :: opened
    | _ -> opened
  in
  List.iter (copy :: elements copy "[id]") ~f:(fun e -> e##removeAttribute (Js.string "id"));
  List.iter opened ~f:close_state;
  copy##setAttribute (Js.string "data-presence-ghost") (Js.string "");
  copy##setAttribute (Js.string "aria-hidden") (Js.string "true");
  copy##setAttribute (Js.string "inert") (Js.string "");
  copy##.style##.pointerEvents := Js.string "none";
  Dom.appendChild parent copy;
  Portal.show copy;
  let animating e =
    let style = Dom_html.window##getComputedStyle e in
    let name = Js.to_string ((Js.Unsafe.coerce style)##getPropertyValue (Js.string "animation-name")) in
    not (String.is_empty name || String.equal name "none")
  in
  let animated = List.filter opened ~f:animating in
  if List.is_empty animated
  then remove copy
  else (
    let pending = ref (List.length animated) in
    let finish () =
      decr pending;
      if !pending <= 0 then remove copy
    in
    List.iter animated ~f:(fun (e : Dom_html.element Js.t) ->
      let (_ : Dom_html.event_listener_id) =
        Dom_html.addEventListener
          e
          (Dom_html.Event.make "animationend")
          (Dom_html.handler (fun (ev : Dom_html.event Js.t) ->
             if Js.Opt.case ev##.target (fun () -> false) (fun t -> phys_equal t e) then finish ();
             Js._true))
          Js._false
      in
      ());
    (* In case an animation never ends (a tab in the background). *)
    let (_ : Dom_html.timeout_id_safe) =
      Dom_html.setTimeout (fun () -> remove copy) 1000.
    in
    ())
;;

module Hook = Vdom.Attr.Hooks.Make (struct
    module State = struct
      type t = { mutable parent : Dom.node Js.t option }
    end

    module Input = struct
      type t = unit [@@deriving sexp_of]

      let combine () () = ()
    end

    let init () _ = { State.parent = None }

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun () (state : State.t) (el : Dom_html.element Js.t) ->
          state.parent <- Js.Opt.to_option el##.parentNode;
          Option.iter state.parent ~f:remove_ghosts)
    ;;

    let update ~old_input:() ~new_input:() (state : State.t) (el : Dom_html.element Js.t) =
      Js.Opt.iter el##.parentNode (fun p -> state.parent <- Some p)
    ;;

    let destroy () (state : State.t) (el : Dom_html.element Js.t) =
      if in_browser ()
      then (
        let parent =
          match Js.Opt.to_option el##.parentNode with
          | Some p -> Some p
          | None -> state.parent
        in
        match parent with
        | Some parent when connected parent -> ghost el parent
        | _ -> ())
    ;;
  end)

let attr = Vdom.Attr.create_hook "loon-presence" (Hook.create ())
