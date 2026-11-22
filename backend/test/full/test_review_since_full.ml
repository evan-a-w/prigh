open! Core
open! Prigh
open Tool_test_helpers
open! Test_review_since

let%expect_test "a session keeps at most max_kept_bytes of texts" =
  let mib = 1024 * 1024 in
  let snapshot i =
    let text =
      String.make (mib - 10) (Char.of_int_exn (Char.to_int 'a' + (i % 26)))
      ^ Int.to_string i
    in
    let content, text =
      Review_snapshot.content_of (Some text) ~size:(String.length text)
    in
    ( { Review_snapshot.root = "/r"
      ; path = "big.txt"
      ; action = Approved
      ; by = None
      ; at = Float.of_int i
      ; content
      }
    , text )
  in
  let review =
    List.fold (List.init 40 ~f:Fn.id) ~init:Review_state.empty ~f:(fun r i ->
      let s, text = snapshot i in
      List.fold (Review_state.snapshot r s ~text) ~init:r ~f:Review_state.apply)
  in
  (* The same content again is not kept twice. *)
  let review =
    let s, text = snapshot 0 in
    List.fold
      (Review_state.snapshot review s ~text)
      ~init:review
      ~f:Review_state.apply
  in
  print_s
    [%message
      ""
        ~snapshots:
          (List.length
             (Review_state.snapshots review ~root:"/r" ~path:"big.txt")
           : int)
        ~blobs:(Map.length review.blobs : int)
        ~kept_mib:(review.kept / mib : int)
        ~max_mib:(Review_state.max_kept_bytes / mib : int)];
  let since index =
    match
      Review_state.since
        review
        ~root:"/r"
        ~path:"big.txt"
        ~index
        ~current:(Review_snapshot.content_of (Some "now\n") ~size:4)
        ()
    with
    | Ok s ->
      print_s
        [%message
          ""
            ~index:(s.index : int)
            ~added:(s.diff.added : int)
            ~removed:(s.diff.removed : int)
            ~note:(s.note : string option)]
    | Error e -> print_s [%sexp (e : Error.t)]
  in
  since 0;
  since 39;
  [%expect
    {|
    ((snapshots 41) (blobs 32) (kept_mib 31) (max_mib 32))
    ((index 0) (added 1) (removed 1) (note ()))
    ((index 39) (added 0) (removed 0) (note ("its text then was not kept")))
    |}]
;;
