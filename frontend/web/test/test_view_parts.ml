open! Core
open Prigh_web

(* The page's parts are made again only when what they show may have
   changed ([View_memo], [View.Part]): a key typed remakes the composer
   alone, a streamed delta the parts that show the transcript. *)

let inject (_ : App.Action.t) = Virtual_dom.Vdom.Effect.Ignore

(* Renders the page (with one [inject] throughout, as the page has) and
   prints the parts made again. *)
let render h =
  ignore (Harness.node ~inject h () : _ list);
  match View_memo.made () with
  | [] -> print_endline "made again: nothing"
  | parts ->
    print_endline
      ("made again: "
       ^ String.concat ~sep:" " (List.sort parts ~compare:String.compare))
;;

(* The model's fields that are not the same value as before (by identity:
   what the parts compare). *)
let changed (before : App.Model.t) (after : App.Model.t) =
  match App.Model.sexp_of_t before with
  | List fields ->
    List.filter_mapi fields ~f:(fun i field ->
      match field with
      | List (Atom name :: _)
        when not
               (phys_equal
                  (Stdlib.Obj.field (Stdlib.Obj.repr before) i)
                  (Stdlib.Obj.field (Stdlib.Obj.repr after) i)) -> Some name
      | _ -> None)
  | _ -> []
;;

let start =
  {|{"event":"message_start","message":{"role":"assistant","content":[],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}}|}
;;

let delta ~seq text =
  sprintf
    {|{"event":"message_update","stream":3,"seq":%d,"delta":{"type":"text_delta","text":%S,"block":0}}|}
    seq
    text
;;

let%expect_test "a key typed remakes the composer only; a delta not the sidebar"
  =
  let h = Harness.create () in
  Harness.quiet h (fun () -> Harness.type_ h "hello");
  Harness.quiet h (fun () -> Harness.act h Send);
  render h;
  [%expect
    {| made again: agents btw chat chrome dialogs files review session_card sidebar terminal |}];
  render h;
  [%expect {| made again: nothing |}];
  let before = Harness.model h in
  Harness.quiet h (fun () -> Harness.type_ h "w");
  print_s [%sexp (changed before (Harness.model h) : string list)];
  render h;
  [%expect
    {|
    (draft cursor)
    made again: nothing
    |}];
  (* A completion shows: still the composer only. *)
  Harness.quiet h (fun () -> Harness.type_ h "/mo");
  render h;
  [%expect {| made again: nothing |}];
  Harness.quiet h (fun () -> Harness.type_ h "");
  render h;
  [%expect {| made again: nothing |}];
  (* A reply streams: what shows the transcript, not the sidebar, the review,
     the side answer, the terminal or the files. *)
  Harness.quiet h (fun () ->
    Harness.event h start;
    Harness.event h (delta ~seq:1 "Hi"));
  render h;
  [%expect {| made again: agents chat chrome dialogs session_card |}];
  let before = Harness.model h in
  Harness.quiet h (fun () -> Harness.event h (delta ~seq:2 " there"));
  print_s [%sexp (changed before (Harness.model h) : string list)];
  render h;
  [%expect
    {|
    (chat transcript_events)
    made again: agents chat chrome dialogs session_card
    |}]
;;

let%expect_test "the clock remakes only what shows the time, when it shows" =
  let h = Harness.create () in
  Harness.quiet h (fun () ->
    Harness.event h start;
    Harness.event h (delta ~seq:1 "Hi"));
  render h;
  [%expect
    {| made again: agents btw chat chrome dialogs files review session_card sidebar terminal |}];
  let now = Option.value_exn (Harness.model h).now in
  let tick after =
    Harness.quiet h (fun () ->
      Harness.act h (Tick (Time_ns.add now (Time_ns.Span.of_sec after))));
    render h
  in
  (* The page's 30 s tick, within the minute: nothing ("3m ago" reads the
     same). *)
  tick 0.;
  [%expect {| made again: nothing |}];
  (* The next minute: what shows ages, not the transcript (its times read
     by the day). *)
  tick 60.;
  [%expect
    {| made again: agents btw chrome dialogs files review session_card sidebar terminal |}];
  (* The next day: the transcript too ("Yesterday"). *)
  tick 86_400.;
  [%expect
    {| made again: agents btw chat chrome dialogs files review session_card sidebar terminal |}]
;;

let%expect_test "another inject remakes every part" =
  let h = Harness.create () in
  render h;
  [%expect
    {| made again: agents btw chat chrome dialogs files review session_card sidebar terminal |}];
  ignore (Harness.node h () : _ list);
  print_s
    [%sexp
      (List.sort (View_memo.made ()) ~compare:String.compare : string list)];
  [%expect {| (agents btw chat chrome dialogs files review session_card sidebar terminal) |}]
;;
