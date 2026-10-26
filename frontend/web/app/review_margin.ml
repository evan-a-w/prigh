open! Core
open Js_of_ocaml

let gap = 6.

let attribute (el : Dom_html.element Js.t) name =
  Js.Opt.to_option (el##getAttribute (Js.string name))
  |> Option.map ~f:Js.to_string
;;

let closest (el : Dom_html.element Js.t) selector : Dom_html.element Js.t option
  =
  Js.Opt.to_option
    (Js.Unsafe.meth_call
       el
       "closest"
       [| Js.Unsafe.inject (Js.string selector) |])
;;

let all (root : Dom_html.element Js.t) selector =
  Dom.list_of_nodeList (root##querySelectorAll (Js.string selector))
;;

let px (el : Dom_html.element Js.t) name value =
  el##.style##setProperty
    (Js.string name)
    (Js.string (sprintf "%.0fpx" value))
    Js.undefined
  |> ignore
;;

let observer
  : < observe : Dom_html.element Js.t -> unit Js.meth > Js.t option ref
  =
  ref None
;;

let observe el = Option.iter !observer ~f:(fun o -> o##observe el)

let layout_one (margin : Dom_html.element Js.t) =
  match closest margin ".review-code-grid" with
  | None -> ()
  | Some grid ->
    let base = Js.to_float margin##getBoundingClientRect##.top in
    let row side line =
      Js.Opt.to_option
        (grid##querySelector
           (Js.string
              (sprintf
                 "table.review-diff tr[data-side=%S][data-line=%S]"
                 side
                 line)))
    in
    Option.iter
      (Js.Opt.to_option (grid##querySelector (Js.string "table.review-diff")))
      ~f:observe;
    let items =
      List.map (all margin ".review-margin-item") ~f:(fun item ->
        observe item;
        let want =
          match attribute item "data-side", attribute item "data-line" with
          | Some side, Some line ->
            (match row side line with
             | Some r -> Js.to_float r##getBoundingClientRect##.top -. base
             | None -> 0.)
          | _ -> 0.
        in
        want, item)
      |> List.stable_sort ~compare:(fun (a, _) (b, _) -> Float.compare a b)
    in
    let bottom =
      List.fold items ~init:Float.neg_infinity ~f:(fun bottom (want, item) ->
        let top = Float.max want (bottom +. gap) in
        px item "top" top;
        top +. Float.of_int item##.offsetHeight)
    in
    px margin "min-height" (Float.max 0. bottom)
;;

let layout () =
  Dom.list_of_nodeList
    (Dom_html.document##querySelectorAll (Js.string ".review-margin"))
  |> List.iter ~f:layout_one
;;

let in_code (node : Dom.node Js.t) =
  let el =
    match Dom.nodeType node with
    | Element el -> Some (Js.Unsafe.coerce el : Dom_html.element Js.t)
    | _ ->
      Option.map
        (Js.Opt.to_option node##.parentNode)
        ~f:(fun p -> (Js.Unsafe.coerce p : Dom_html.element Js.t))
  in
  Option.exists el ~f:(fun el -> Option.is_some (closest el ".review-code"))
;;

let install () =
  let resize_observer = Js.Unsafe.global##._ResizeObserver in
  if Js.Optdef.test resize_observer
  then
    observer
    := Some (new%js resize_observer (Js.wrap_callback (fun _ _ -> layout ())));
  let mutations =
    new%js MutationObserver.mutationObserver
      (Js.wrap_callback (fun records _ ->
         let records = Js.to_array records in
         if
           Array.exists
             records
             ~f:(fun (r : MutationObserver.mutationRecord Js.t) ->
               in_code r##.target)
         then layout ()))
  in
  let options = MutationObserver.empty_mutation_observer_init () in
  options##.childList := true;
  options##.subtree := true;
  mutations##observe (Dom_html.document :> Dom.node Js.t) options;
  Browser.on_viewport_change layout
;;
