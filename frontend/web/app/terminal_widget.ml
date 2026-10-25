open! Core
open Js_of_ocaml
open Bonsai_web
module Status = Prigh_web.Terminal.Status

let contains = Terminal_panel.contains

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
  type t =
    < focus : unit Js.meth
    ; dispose : unit Js.meth
    ; setTheme : Js.Unsafe.any -> unit Js.meth >
      Js.t
end

(* xterm.js wants colours, not variables: the page's, as the theme on show
   computes them (bin/style.css), and ANSI colours that read on its ground. *)
let look = ref Prigh_web.Theme.default

let theme_js () =
  let style =
    Dom_html.window##getComputedStyle Dom_html.document##.documentElement
  in
  let var name =
    String.strip (Js.to_string (style##getPropertyValue (Js.string name)))
  in
  let accent = var "--sodium" in
  let ansi : (string * string) list =
    match !look.name with
    | Light ->
      [ "black", "#2b2f36"
      ; "red", "#b42318"
      ; "green", "#1d7a3e"
      ; "yellow", "#8a6100"
      ; "blue", "#1f5fbf"
      ; "magenta", "#8e3fb5"
      ; "cyan", "#0f7a80"
      ; "white", "#6b7280"
      ; "brightBlack", "#5c6370"
      ; "brightRed", "#d0362b"
      ; "brightGreen", "#23914a"
      ; "brightYellow", "#a67600"
      ; "brightBlue", "#2b73e0"
      ; "brightMagenta", "#a650cf"
      ; "brightCyan", "#138f96"
      ; "brightWhite", "#1b1e23"
      ]
    | Original | Dark ->
      [ "blue", "#7c9cff"
      ; "brightBlue", "#9db4ff"
      ; "green", var "--sage"
      ; "red", var "--brick"
      ; "yellow", "#f0b44c"
      ]
  in
  Js.Unsafe.obj
    (Array.of_list
       (List.map
          ([ "background", var "--well"
           ; "foreground", var "--chalk"
           ; "cursor", accent
           ; "cursorAccent", var "--well"
           ; ( "selectionBackground"
             , if String.length accent = 7 then accent ^ "55" else accent )
           ]
           @ ansi)
          ~f:(fun (k, v) -> k, Js.Unsafe.inject (Js.string v))))
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
           ; "theme", Js.Unsafe.inject (theme_js ())
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
let set_theme theme =
  if not (Prigh_web.Theme.equal theme !look)
  then (
    look := theme;
    match !current with
    | Some { handle = Some handle; _ } -> handle##setTheme (theme_js ())
    | Some { handle = None; _ } | None -> ())
;;
