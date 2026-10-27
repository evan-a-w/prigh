open! Core
open! Import
open Js_of_ocaml

module Input = struct
  type t =
    { tex : string
    ; display : bool
    }
  [@@deriving sexp_of, equal]
end

let classes ({ display; _ } : Input.t) =
  [ "math"; (if display then "math-display" else "math-inline") ]
;;

let set_classes (el : Dom_html.element Js.t) input extra =
  el##.className := Js.string (String.concat ~sep:" " (classes input @ extra))
;;

let draw (el : Dom_html.element Js.t) (input : Input.t) =
  el##removeAttribute (Js.string "title");
  match Katex.render ~display:input.display input.tex with
  | Some (Ok html) ->
    set_classes el input [];
    el##.innerHTML := Js.string html
  | Some (Error message) ->
    set_classes el input [ "math-error" ];
    el##.textContent := Js.some (Js.string input.tex);
    el##setAttribute (Js.string "title") (Js.string message)
  | None ->
    set_classes el input [ "math-pending" ];
    el##.textContent := Js.some (Js.string input.tex)
;;

module Widget = struct
  type dom = Dom_html.element

  module Input = Input

  module State = struct
    (* What the element shows, for a draw once KaTeX has loaded. *)
    type t = Input.t ref [@@deriving sexp_of]
  end

  let name = "math"

  let create input =
    let el = Dom_html.createSpan Dom_html.document in
    let state = ref input in
    draw el input;
    if not (Katex.loaded ())
    then Katex.when_loaded (fun () -> draw el !state);
    state, el
  ;;

  let update ~prev_input:_ ~input ~state ~element =
    if not (Input.equal input !state)
    then (
      state := input;
      draw element input);
    state, element
  ;;

  let destroy ~prev_input:_ ~state:_ ~element:_ = ()

  let to_vdom_for_testing =
    `Custom
      (fun (input : Input.t) ->
        Node.span ~attrs:[ Attr.classes (classes input) ] [ Node.text input.tex ])
  ;;
end

let widget = Staged.unstage (Node.widget_of_module (module Widget))
let render ~display tex = widget { tex; display }
