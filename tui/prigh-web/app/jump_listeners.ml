open! Core
open Js_of_ocaml

let chat () = Dom_html.getElementById_opt "chat"

let all (el : #Dom_html.element Js.t) selector =
  Dom.list_of_nodeList (el##querySelectorAll (Js.string selector))
;;

let entry (chat : Dom_html.element Js.t) n =
  Js.Opt.to_option
    (chat##querySelector
       (Js.string (sprintf ":scope > .entries > [data-entry=\"%d\"]" n)))
;;

let attribute (el : Dom_html.element Js.t) name =
  Js.Opt.to_option (el##getAttribute (Js.string name))
  |> Option.map ~f:Js.to_string
;;

(* The rail's mark for the entry numbered [n] is the current one. *)
let set_current n =
  let n = Option.map n ~f:Int.to_string in
  List.iter
    (all Dom_html.document##.documentElement ".rail .tick[aria-current]")
    ~f:(fun (tick : Dom_html.element Js.t) ->
      if not (Option.equal String.equal (attribute tick "data-jump-entry") n)
      then tick##removeAttribute (Js.string "aria-current"));
  Option.iter n ~f:(fun n ->
    Js.Opt.iter
      (Dom_html.document##querySelector
         (Js.string (sprintf ".rail .tick[data-jump-entry=\"%s\"]" n)))
      (fun tick ->
         tick##setAttribute (Js.string "aria-current") (Js.string "location")))
;;

(* The last landmark whose top is above the upper third of the view. *)
let in_view () =
  Option.bind (chat ()) ~f:(fun chat ->
    let rect = chat##getBoundingClientRect in
    let line =
      Js.to_float rect##.top
      +. ((Js.to_float rect##.bottom -. Js.to_float rect##.top) /. 3.)
    in
    List.fold_until
      (all chat ":scope > .entries > [data-entry]")
      ~init:None
      ~f:(fun current (el : Dom_html.element Js.t) ->
        if Float.(Js.to_float el##getBoundingClientRect##.top <= line)
        then
          Continue
            (Option.first_some
               (Option.bind (attribute el "data-entry") ~f:Int.of_string_opt)
               current)
        else Stop current)
      ~finish:Fn.id)
;;

let scheduled = ref false

let mark_soon () =
  if not !scheduled
  then (
    scheduled := true;
    ignore
      (Dom_html.window##requestAnimationFrame
         (Js.wrap_callback (fun (_ : Js.number_t) ->
            scheduled := false;
            set_current (in_view ())))
       : Dom_html.animation_frame_request_id))
;;

let arrived (el : Dom_html.element Js.t) =
  el##.classList##remove (Js.string "jumped");
  (* Again from the start when jumping to it twice. *)
  ignore (el##.offsetWidth : int);
  el##.classList##add (Js.string "jumped");
  ignore
    (Dom_html.setTimeout
       (fun () -> el##.classList##remove (Js.string "jumped"))
       1600.
     : Dom_html.timeout_id_safe);
  set_current (Option.bind (attribute el "data-entry") ~f:Int.of_string_opt)
;;

let install () =
  ignore
    (Dom_html.addEventListener
       Dom_html.document
       (Dom_html.Event.make "scroll")
       (Dom.handler (fun ev ->
          (match chat () with
           | Some chat
             when phys_equal
                    (Dom_html.eventTarget ev :> Dom.node Js.t)
                    (chat :> Dom.node Js.t) -> mark_soon ()
           | _ -> ());
          Js._true))
       Js._true
     : Dom_html.event_listener_id);
  (* A rail made again (new landmarks) has lost the mark. *)
  let observer =
    new%js MutationObserver.mutationObserver
      (Js.wrap_callback (fun records _ ->
         if
           Array.exists
             (Js.to_array records)
             ~f:(fun (r : MutationObserver.mutationRecord Js.t) ->
               Js.Opt.case
                 (Dom_html.CoerceTo.element r##.target)
                 (fun () -> false)
                 (fun el ->
                    List.exists
                      [ "jump-nav"; "rail"; "chat-frame" ]
                      ~f:(fun cls ->
                        Js.to_bool (el##.classList##contains (Js.string cls)))))
         then mark_soon ()))
  in
  let options = MutationObserver.empty_mutation_observer_init () in
  options##.childList := true;
  options##.subtree := true;
  observer##observe (Dom_html.document :> Dom.node Js.t) options
;;
