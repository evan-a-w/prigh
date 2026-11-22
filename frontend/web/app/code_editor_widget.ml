open! Core
open Js_of_ocaml
open Bonsai_web
module Code_editor = Prigh_web.Code_editor
module Input = Prigh_web.Code_editor_view.Input

let assets =
  Prigh_web.Page_assets.create
    ~css:[]
    ~scripts:[ "codemirror.js"; "code_editor.js" ]
;;

module Handle = struct
  type t =
    < load : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
    ; setDoc : Js.js_string Js.t -> Js.js_string Js.t -> unit Js.meth
    ; setChange : Js.Unsafe.any -> Js.Unsafe.any -> unit Js.meth
    ; exec : Js.js_string Js.t -> Js.Unsafe.any -> unit Js.meth
    ; focus : unit Js.meth
    ; dispose : unit Js.meth >
      Js.t
end

module Mounted = struct
  type t =
    { mutable handle : Handle.t option
    ; mutable disposed : bool
    ; mutable input : Input.t
    ; mutable shown : (string * int) option (** the path and revision *)
    ; mutable change : (Code_editor.Hunk.t list * (int * int) list) option
    ; mutable owed : Code_editor.Command.t list (** before it loaded *)
    }
end

let current : Mounted.t option ref = ref None

let contains (el : Dom_html.element Js.t) =
  let rec up (el : Dom_html.element Js.t) =
    Js.to_bool (el##.classList##contains (Js.string "code-editor-host"))
    || Js.Opt.case el##.parentNode (fun () -> false) (fun p ->
      Js.Opt.case (Dom_html.CoerceTo.element p) (fun () -> false) up)
  in
  up el
;;

let fire effect = Effect.Expert.handle_non_dom_event_exn effect
let js_string s = Js.Unsafe.inject (Js.string s)

let hunks_js (hunks : Code_editor.Hunk.t list) =
  Js.Unsafe.inject
    (Js.array
       (Array.of_list_map hunks ~f:(fun (h : Code_editor.Hunk.t) ->
          Js.Unsafe.obj
            [| "line", Js.Unsafe.inject h.line
             ; ( "removed"
               , Js.Unsafe.inject
                   (Js.array (Array.of_list_map h.removed ~f:Js.string)) )
             ; "added", Js.Unsafe.inject h.added
            |])))
;;

let marks_js marks =
  Js.Unsafe.inject
    (Js.array
       (Array.of_list_map marks ~f:(fun (a, b) ->
          Js.array [| Js.Unsafe.inject a; Js.Unsafe.inject b |])))
;;

let exec (handle : Handle.t) (c : Code_editor.Command.t) =
  let none = Js.Unsafe.inject Js.null in
  match c with
  | Insert s -> handle##exec (Js.string "insert") (js_string s)
  | Indent -> handle##exec (Js.string "indent") none
  | Undo -> handle##exec (Js.string "undo") none
  | Redo -> handle##exec (Js.string "redo") none
  | Left -> handle##exec (Js.string "left") none
  | Right -> handle##exec (Js.string "right") none
  | Goto_line n -> handle##exec (Js.string "goto") (Js.Unsafe.inject n)
  | Find -> handle##exec (Js.string "find") none
  | Focus -> handle##focus
;;

(* What the input says, onto the editor. *)
let sync (m : Mounted.t) =
  match m.handle with
  | None -> ()
  | Some handle ->
    let i = m.input in
    let lang = Js.string (Code_editor.Lang.name i.lang) in
    (match m.shown with
     | Some (path, _) when not (String.equal path i.path) ->
       handle##load (Js.string i.text) lang;
       m.change <- None
     | None ->
       handle##load (Js.string i.text) lang;
       m.change <- None
     | Some (_, revision) when revision <> i.revision ->
       handle##setDoc (Js.string i.text) lang;
       m.change <- None
     | Some _ -> ());
    m.shown <- Some (i.path, i.revision);
    let change = i.hunks, i.marks in
    if
      not
        (Option.equal
           [%equal: Code_editor.Hunk.t list * (int * int) list]
           m.change
           (Some change))
    then (
      m.change <- Some change;
      handle##setChange (hunks_js i.hunks) (marks_js i.marks))
;;

let mount (m : Mounted.t) host =
  Prigh_web.Page_assets.load assets (function
    | Error src ->
      Console.console##error (Js.string (sprintf "could not load %s" src))
    | Ok () when m.disposed -> ()
    | Ok () ->
      let shown_path () =
        Option.value_map m.shown ~default:m.input.path ~f:fst
      in
      let options =
        Js.Unsafe.obj
          [| ( "onChange"
             , Js.Unsafe.inject
                 (Js.wrap_callback (fun text ->
                    if not m.disposed
                    then
                      fire
                        (m.input.on_change
                           ~path:(shown_path ())
                           (Js.to_string text)))) )
           ; ( "onSelect"
             , Js.Unsafe.inject
                 (Js.wrap_callback (fun first last text ->
                    if not m.disposed
                    then
                      fire
                        (m.input.on_select
                           { Code_editor.Selection.first
                           ; last
                           ; text = Js.to_string text
                           }))) )
           ; ( "onScroll"
             , Js.Unsafe.inject
                 (Js.wrap_callback (fun dir ->
                    if not m.disposed
                    then
                      fire
                        (m.input.on_scroll
                           (match Js.to_string dir with
                            | "down" -> `Down
                            | _ -> `Up)))) )
          |]
      in
      let api = Js.Unsafe.get Dom_html.window (Js.string "prighCodeEditor") in
      let handle : Handle.t =
        Js.Unsafe.meth_call
          api
          "mount"
          [| Js.Unsafe.inject host; Js.Unsafe.inject options |]
      in
      m.handle <- Some handle;
      sync m;
      List.iter (List.rev m.owed) ~f:(exec handle);
      m.owed <- [])
;;

let command c =
  match !current with
  | Some { handle = Some handle; _ } -> exec handle c
  | Some ({ handle = None; _ } as m) -> m.owed <- c :: m.owed
  | None -> ()
;;

module Widget = struct
  type dom = Dom_html.element

  module Input = Input

  module State = struct
    type t = Mounted.t

    let sexp_of_t _ = Sexp.Atom "<code editor>"
  end

  let name = "prigh-web-code-editor"

  let create input =
    let host = Dom_html.createDiv Dom_html.document in
    host##.className := Js.string "code-editor-host";
    let m =
      { Mounted.handle = None
      ; disposed = false
      ; input
      ; shown = None
      ; change = None
      ; owed = []
      }
    in
    current := Some m;
    mount m host;
    m, host
  ;;

  let destroy ~prev_input:_ ~(state : State.t) ~element:_ =
    state.disposed <- true;
    Option.iter state.handle ~f:(fun h -> h##dispose);
    match !current with
    | Some m when phys_equal m state -> current := None
    | _ -> ()
  ;;

  let update ~prev_input:_ ~input ~(state : State.t) ~element =
    state.input <- input;
    sync state;
    state, element
  ;;

  let to_vdom_for_testing = `Sexp_of_input
end

let view = Staged.unstage (Vdom.Node.widget_of_module (module Widget))
