open! Import
open Js_of_ocaml

module Reason = struct
  type t =
    | Escape
    | Pointer_outside
    | Focus_outside
  [@@deriving sexp_of, equal]
end

module Input = struct
  type t =
    { inside : string list
    ; focus_outside : bool
    ; modal : bool
    ; on_dismiss : Reason.t -> unit Effect.t
    }

  let sexp_of_t t = [%sexp_of: string list * bool * bool] (t.inside, t.focus_outside, t.modal)
  let combine _ second = second
end

module Layer = struct
  type t =
    { el : Dom_html.element Js.t
    ; mutable input : Input.t
    ; mutable dismissed : bool
    }
end

let layers : Layer.t list ref = ref []
let installed = ref false
let body_pointer_events = ref None

let dispatch (layer : Layer.t) reason =
  Effect.Expert.handle_non_dom_event_exn (layer.input.on_dismiss reason)
;;

let is_inside (layer : Layer.t) (target : Dom.node Js.t) =
  let contains (el : Dom_html.element Js.t) =
    Js.to_bool ((Js.Unsafe.coerce el)##contains target)
  in
  contains layer.el
  || List.exists layer.input.inside ~f:(fun id ->
    match Js.Opt.to_option (Dom_html.document##getElementById (Js.string id)) with
    | Some el -> contains el
    | None -> false)
;;

(* Whether [target] contains one of [inside] (the trigger). *)
let contains_inside (layer : Layer.t) (target : Dom.node Js.t) =
  List.exists layer.input.inside ~f:(fun id ->
    match Js.Opt.to_option (Dom_html.document##getElementById (Js.string id)) with
    | Some el -> Js.to_bool ((Js.Unsafe.coerce target)##contains el)
    | None -> false)
;;

let target (ev : #Dom_html.event Js.t) : Dom.node Js.t option =
  Js.Opt.to_option (Js.Unsafe.coerce ev)##.target
;;

let update_pointer_events () =
  let body = Dom_html.document##.body in
  let modal = List.exists !layers ~f:(fun l -> l.input.modal && not l.dismissed) in
  match modal, !body_pointer_events with
  | true, None ->
    body_pointer_events := Some (Js.to_string body##.style##.pointerEvents);
    body##.style##.pointerEvents := Js.string "none"
  | false, Some old ->
    body##.style##.pointerEvents := Js.string old;
    body_pointer_events := None
  | _ -> ()
;;

let listen event ~capture f =
  let (_ : Dom_html.event_listener_id) =
    Dom_html.addEventListener
      Dom_html.document
      (Dom_html.Event.make event)
      (Dom_html.handler (fun ev ->
         f ev;
         Js._true))
      (Js.bool capture)
  in
  ()
;;

let install () =
  if not !installed
  then (
    installed := true;
    listen "keydown" ~capture:false (fun (ev : Dom_html.keyboardEvent Js.t) ->
      match !layers, Dom_util.key ev with
      | top :: _, Escape when not (Js.to_bool (Js.Unsafe.coerce ev)##.defaultPrevented) ->
        ev##preventDefault;
        dispatch top Escape
      | _ -> ());
    listen "pointerdown" ~capture:true (fun (ev : Dom_html.event Js.t) ->
      match !layers, target ev with
      | top :: _, Some t when not (is_inside top t) ->
        (* React removes Radix's layer before the pointerup: the page takes
           pointer events again for the rest of this click (a right click
           elsewhere reopens a context menu there). Ours goes on the next
           render, so stop blocking the page now. *)
        top.dismissed <- true;
        update_pointer_events ();
        let (_ : Dom_html.timeout_id_safe) =
          Dom_html.setTimeout
            (fun () ->
              if top.dismissed && List.exists !layers ~f:(phys_equal top)
              then (
                top.dismissed <- false;
                update_pointer_events ()))
            500.
        in
        dispatch top Pointer_outside
      | _ -> ());
    listen "focusin" ~capture:true (fun (ev : Dom_html.event Js.t) ->
      match !layers, target ev with
      | top :: _, Some t
        when top.input.focus_outside
             && (not (is_inside top t))
             (* Not the focus Bonsai moves to the app's root (an ancestor of
                the trigger) when the focused element is removed (an item
                chosen): as Radix ignores focus on the body. The layer is
                closing already, and its focus goes back where it was. *)
             && not (contains_inside top t) ->
        dispatch top Focus_outside
      | _ -> ()))
;;

module Hook = Vdom.Attr.Hooks.Make (struct
    module State = struct
      type t = { mutable layer : Layer.t option }
    end

    module Input = Input

    let init _ _ = { State.layer = None }

    let on_mount =
      `Schedule_immediately_after_this_dom_patch_completes
        (fun input (state : State.t) el ->
          install ();
          let layer = { Layer.el; input; dismissed = false } in
          state.layer <- Some layer;
          layers := layer :: !layers;
          if input.modal then el##.style##.pointerEvents := Js.string "auto";
          update_pointer_events ())
    ;;

    (* Still here after a render: the dismissal was not taken. *)
    let update ~old_input:_ ~new_input (state : State.t) _ =
      Option.iter state.layer ~f:(fun l ->
        l.input <- new_input;
        if l.dismissed
        then (
          l.dismissed <- false;
          update_pointer_events ()))
    ;;

    let destroy _ (state : State.t) _ =
      Option.iter state.layer ~f:(fun l ->
        layers := List.filter !layers ~f:(fun x -> not (phys_equal x l));
        update_pointer_events ());
      state.layer <- None
    ;;
  end)

let attr ?(inside = []) ?(focus_outside = false) ?(modal = false) ~on_dismiss () =
  Vdom.Attr.create_hook
    "loon-dismiss"
    (Hook.create { Input.inside; focus_outside; modal; on_dismiss })
;;

let escape_keydown ~on_escape ev =
  match Dom_util.key ev with
  | Escape -> Some (Effect.Many [ Dom_util.stop ev; on_escape ])
  | _ -> None
;;
