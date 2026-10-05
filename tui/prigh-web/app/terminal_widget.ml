open! Core
open Js_of_ocaml
open Bonsai_web
module Browser = Prigh_ui_web_app.Browser
module Status = Prigh_web.Terminal.Status

let contains = Prigh_ui_web_app.Terminal_panel.contains

let only_inside
      (records : MutationObserver.mutationRecord Js.t Js.js_array Js.t)
  =
  records##.length > 0
  && List.for_all
       (List.init records##.length ~f:Fn.id)
       ~f:(fun i ->
         Js.Optdef.case
           (Js.array_get records i)
           (fun () -> true)
           (fun (record : MutationObserver.mutationRecord Js.t) ->
              Js.Opt.case
                (Dom_html.CoerceTo.element record##.target)
                (fun () ->
                   (* Text: its parent says where it is. *)
                   Js.Opt.case
                     record##.target##.parentNode
                     (fun () -> false)
                     (fun parent ->
                        Js.Opt.case
                          (Dom_html.CoerceTo.element parent)
                          (fun () -> false)
                          contains))
                contains))
;;

(* xterm.js and its glue, loaded the first time a terminal opens. *)
module Assets = struct
  type state =
    | Not_loaded
    | Loading of ((unit, string) Result.t -> unit) Queue.t
    | Loaded

  let state = ref Not_loaded

  let add_to_head (el : #Dom.node Js.t) =
    Dom.appendChild Dom_html.document##.head el
  ;;

  let script src ~k =
    let el = Dom_html.createScript Dom_html.document in
    let once = ref false in
    let finish result =
      if not !once
      then (
        once := true;
        k result)
    in
    let listen event result =
      ignore
        (Dom_html.addEventListener
           el
           event
           (Dom.handler (fun _ ->
              finish result;
              Js._true))
           Js._false
         : Dom_html.event_listener_id)
    in
    listen Dom_html.Event.load (Ok ());
    listen Dom_html.Event.error (Error src);
    el##.src := Js.string src;
    add_to_head el
  ;;

  let load k =
    match !state with
    | Loaded -> k (Ok ())
    | Loading waiting -> Queue.enqueue waiting k
    | Not_loaded ->
      let waiting = Queue.singleton k in
      state := Loading waiting;
      let css = Dom_html.createLink Dom_html.document in
      css##.rel := Js.string "stylesheet";
      css##.href := Js.string "xterm.css";
      add_to_head css;
      let rec next = function
        | [] -> finish (Ok ())
        | src :: rest ->
          script src ~k:(function
            | Ok () -> next rest
            | Error src -> finish (Error src))
      and finish result =
        (state
         := match result with
            | Ok () -> Loaded
            | Error _ -> Not_loaded);
        Queue.iter waiting ~f:(fun k -> k result)
      in
      next [ "xterm.js"; "addon-fit.js"; "terminal.js" ]
  ;;
end

module Handle = struct
  type t = < focus : unit Js.meth ; dispose : unit Js.meth > Js.t
end

(* prigh-web's palette (bin/style.css). *)
let theme () =
  Js.Unsafe.obj
    [| "background", Js.Unsafe.inject (Js.string "#0b0d11")
     ; "foreground", Js.Unsafe.inject (Js.string "#e6e8ee")
     ; "cursor", Js.Unsafe.inject (Js.string "#7c9cff")
     ; "cursorAccent", Js.Unsafe.inject (Js.string "#0b0d11")
     ; "selectionBackground", Js.Unsafe.inject (Js.string "#5b7cf066")
     ; "blue", Js.Unsafe.inject (Js.string "#7c9cff")
     ; "brightBlue", Js.Unsafe.inject (Js.string "#9db4ff")
     ; "green", Js.Unsafe.inject (Js.string "#3ecf8e")
     ; "red", Js.Unsafe.inject (Js.string "#ff6b6b")
     ; "yellow", Js.Unsafe.inject (Js.string "#f2c94c")
    |]
;;

let status_of_js state message : Status.t =
  match state with
  | "open" -> Connected
  | "reconnecting" -> Reconnecting
  | "exited" -> Exited
  | "failed" -> Failed (Option.value message ~default:"it could not start")
  | _ -> Connecting
;;

module Mounted = struct
  type t =
    { mutable handle : Handle.t option
    ; mutable disposed : bool
    }
end

(* The mounted terminal, for [focus], and whether a focus is owed to the next
   one. *)
let current : Mounted.t option ref = ref None
let focus_owed = ref false

let focus () =
  Browser.after_render (fun () ->
    match !current with
    | Some { handle = Some handle; _ } -> handle##focus
    | Some { handle = None; _ } | None -> focus_owed := true)
;;

module Input = struct
  type t =
    { url : string
    ; key : string
    ; on_status : Status.t -> unit Effect.t
    }

  let sexp_of_t t = [%sexp_of: string] t.key
end

let mount (input : Input.t) (mounted : Mounted.t) host =
  let report status =
    if not mounted.disposed
    then Effect.Expert.handle_non_dom_event_exn (input.on_status status)
  in
  Assets.load (function
    | Error src -> report (Failed (sprintf "could not load %s" src))
    | Ok () when mounted.disposed -> ()
    | Ok () ->
      let on_status =
        Js.wrap_callback (fun state message ->
          report
            (status_of_js
               (Js.to_string state)
               (Js.Optdef.to_option message |> Option.map ~f:Js.to_string)))
      in
      let options =
        Js.Unsafe.obj
          [| "onStatus", Js.Unsafe.inject on_status
           ; "focus", Js.Unsafe.inject (Js.bool !focus_owed)
           ; "theme", Js.Unsafe.inject (theme ())
          |]
      in
      focus_owed := false;
      let api = Js.Unsafe.get Dom_html.window (Js.string "prighTerminal") in
      mounted.handle
      <- Some
           (Js.Unsafe.meth_call
              api
              "mount"
              [| Js.Unsafe.inject host
               ; Js.Unsafe.inject (Js.string input.url)
               ; Js.Unsafe.inject options
              |]))
;;

module Widget = struct
  type dom = Dom_html.divElement

  module Input = Input

  module State = struct
    type t = Mounted.t

    let sexp_of_t _ = Sexp.Atom "<terminal>"
  end

  let name = "prigh-web-terminal"

  let create input =
    let host = Dom_html.createDiv Dom_html.document in
    host##.className := Js.string "terminal-host";
    let mounted = { Mounted.handle = None; disposed = false } in
    current := Some mounted;
    mount input mounted host;
    mounted, host
  ;;

  let destroy ~prev_input:_ ~(state : State.t) ~element:_ =
    state.disposed <- true;
    Option.iter state.handle ~f:(fun handle -> handle##dispose);
    match !current with
    | Some mounted when phys_equal mounted state -> current := None
    | _ -> ()
  ;;

  let update ~(prev_input : Input.t) ~(input : Input.t) ~state ~element =
    if String.equal prev_input.key input.key
    then state, element
    else (
      destroy ~prev_input ~state ~element;
      create input)
  ;;

  let to_vdom_for_testing = `Sexp_of_input
end

let widget = Staged.unstage (Vdom.Node.widget_of_module (module Widget))
let view ~url ~key ~on_status = widget { Input.url; key; on_status }

(* ---- height *)

let height_key = "prigh-web.terminal-height"

let clamp px =
  Int.max
    Prigh_web.Terminal.min_height
    (Int.min px (Dom_html.window##.innerHeight - 160))
;;

let panel () = Dom_html.getElementById_opt "terminal-panel"

let closest (el : Dom_html.element Js.t) selector =
  Js.Opt.test
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |]
     : Dom_html.element Js.t Js.opt)
;;

let open_key = "prigh-web.terminal-open"

let remember open_ =
  if open_ then Browser.set_item open_key "1" else Browser.remove_item open_key
;;

let install ~schedule =
  if Option.is_some (Browser.get_item open_key)
  then schedule Prigh_web.App.Action.Reopen_terminal;
  Option.iter
    (Option.bind (Browser.get_item height_key) ~f:Int.of_string_opt)
    ~f:(fun px ->
      schedule (Prigh_web.App.Action.Set_terminal_height (clamp px)));
  let listen event handler =
    ignore
      (Dom_html.addEventListener
         Dom_html.document
         event
         (Dom.handler (fun ev ->
            handler ev;
            Js._true))
         Js._false
       : Dom_html.event_listener_id)
  in
  let dragging = ref None in
  let body_class = Js.string "resizing-terminal" in
  listen Dom_html.Event.pointerdown (fun (ev : Dom_html.pointerEvent Js.t) ->
    match Js.Opt.to_option ev##.target with
    | Some el when closest el ".terminal-resize" ->
      Dom.preventDefault ev;
      dragging := Some None;
      Dom_html.document##.body##.classList##add body_class
    | _ -> ());
  listen Dom_html.Event.pointermove (fun (ev : Dom_html.pointerEvent Js.t) ->
    match !dragging, panel () with
    | Some _, Some panel ->
      let bottom = Js.to_float panel##getBoundingClientRect##.bottom in
      let px =
        clamp (Float.iround_nearest_exn (bottom -. Js.to_float ev##.clientY))
      in
      dragging := Some (Some px);
      ignore
        (Js.Unsafe.meth_call
           panel##.style
           "setProperty"
           [| Js.Unsafe.inject (Js.string "--terminal-height")
            ; Js.Unsafe.inject (Js.string (sprintf "%dpx" px))
           |]
         : Js.Unsafe.any)
    | _ -> ());
  let stop (_ : Dom_html.pointerEvent Js.t) =
    match !dragging with
    | None -> ()
    | Some px ->
      dragging := None;
      Dom_html.document##.body##.classList##remove body_class;
      Option.iter px ~f:(fun px ->
        Browser.set_item height_key (Int.to_string px);
        schedule (Prigh_web.App.Action.Set_terminal_height px))
  in
  listen Dom_html.Event.pointerup stop;
  listen (Dom_html.Event.make "pointercancel") stop
;;
