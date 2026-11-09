(* Running [prigh browser-signin -json] for the TUI, against a stand-in
   script: its lines, a pasted code written to its stdin, and cancelling. *)
open! Core
open! Async
open! Expect_test_helpers_core
open! Expect_test_helpers_async
module Browser_signin = Prigh_client_unix.Browser_signin

let%expect_test "the lines it prints" =
  List.iter
    [ {|{"url":"https://ai.example.com/#cli-signin=1.s&program=tui","port":1}|}
    ; {|{"url":"https://ai.example.com/#cli-signin=1.s&program=tui&paste=K","port":1,"paste":true}|}
    ; {|{"paste_error":"that code is not for this sign-in"}|}
    ; {|{"proxy_cookie":"c","token":null,"user":"alice"}|}
    ; {|{"error":"the sign-in was cancelled in the browser"}|}
    ; "not json"
    ]
    ~f:(fun line ->
      print_s [%sexp (Browser_signin.Line.parse line : Browser_signin.Line.t option)]);
  [%expect
    {|
    ((
      Url
      (url https://ai.example.com/#cli-signin=1.s&program=tui)
      (port  1)
      (paste false)))
    ((
      Url
      (url https://ai.example.com/#cli-signin=1.s&program=tui&paste=K)
      (port  1)
      (paste true)))
    ((Paste_refused "that code is not for this sign-in"))
    ((Granted ((proxy_cookie c) (token ()) (user (alice)))))
    ((Failed "the sign-in was cancelled in the browser"))
    ()
    |}];
  return ()
;;

let%expect_test "over SSH, unless PRIGH_SIGNIN_PASTE says" =
  List.iter
    [ []
    ; [ "SSH_CONNECTION", "10.0.0.1 5 10.0.0.2 22" ]
    ; [ "SSH_TTY", "/dev/pts/3" ]
    ; [ "SSH_TTY", "" ]
    ; [ "SSH_TTY", "/dev/pts/3"; "PRIGH_SIGNIN_PASTE", "0" ]
    ; [ "PRIGH_SIGNIN_PASTE", "1" ]
    ]
    ~f:(fun env ->
      let getenv name = List.Assoc.find env name ~equal:String.equal in
      print_s
        [%sexp
          (env : (string * string) list)
        , (Browser_signin.paste_wanted ~getenv : bool)]);
  [%expect
    {|
    (() false)
    (((SSH_CONNECTION "10.0.0.1 5 10.0.0.2 22")) true)
    (((SSH_TTY /dev/pts/3)) true)
    (((SSH_TTY "")) false)
    (((SSH_TTY            /dev/pts/3)
      (PRIGH_SIGNIN_PASTE 0))
     false)
    (((PRIGH_SIGNIN_PASTE 1)) true)
    |}];
  return ()
;;

(* Stands in for the backend: prints its arguments, the link, then reads
   codes from stdin until one is "prigh1:good". *)
let fake_backend () =
  let dir = Filename_unix.temp_dir "prigh-browser-signin" "" in
  let path = dir ^/ "prigh" in
  Out_channel.write_all
    path
    ~data:
      {|#!/bin/sh
echo "args: $*" >&2
echo '{"url":"https://ai.example.com/#cli-signin=1.s&program=tui&paste=K","port":1,"paste":true}'
while read -r code; do
  if [ "$code" = prigh1:good ]; then
    echo '{"proxy_cookie":"proxy_session=ok","token":"t","user":"alice"}'
    exit 0
  fi
  echo '{"paste_error":"that code is not for this sign-in"}'
done
echo '{"error":"the input ended"}'
|};
  Core_unix.chmod path ~perm:0o755;
  path
;;

let%expect_test "a pasted code goes to its stdin; a refusal is reported" =
  let backend = fake_backend () in
  let input = Ivar.create () in
  let%bind result =
    Browser_signin.run
      ~backend
      ~connect:"https://ai.example.com/"
      ~paste:true
      ~on_url:(fun ~url ~port ~paste ->
        printf "url %s port %d paste %b\n" url port (Option.is_some paste);
        Option.iter paste ~f:(fun paste ->
          Browser_signin.Code_input.send paste "  prigh1:stale ";
          Ivar.fill_if_empty input paste))
      ~on_paste_refused:(fun why ->
        printf "refused: %s\n" why;
        Ivar.read input
        >>> fun paste -> Browser_signin.Code_input.send paste "prigh1:good")
  in
  print_s [%sexp (result : (Browser_signin.Grant.t, string) Result.t)];
  [%expect
    {|
    url https://ai.example.com/#cli-signin=1.s&program=tui&paste=K port 1 paste true
    refused: that code is not for this sign-in
    (Ok (
      (proxy_cookie proxy_session=ok)
      (token (t))
      (user  (alice))))
    |}];
  return ()
;;

let%expect_test "cancelled while it waits" =
  let backend = fake_backend () in
  let%bind result =
    Browser_signin.run
      ~backend
      ~connect:"https://ai.example.com/"
      ~paste:true
      ~on_url:(fun ~url:_ ~port:_ ~paste ->
        Option.iter paste ~f:Browser_signin.Code_input.cancel)
      ~on_paste_refused:(fun why -> printf "refused: %s\n" why)
  in
  print_s [%sexp (result : (Browser_signin.Grant.t, string) Result.t)];
  [%expect {| (Error cancelled) |}];
  return ()
;;
