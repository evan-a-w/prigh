open! Core
open Prigh_web

let%expect_test "prigh web's sign-in link: the code, and what it is traded for" =
  List.iter
    [ "#signin=0123abcd"
    ; "signin=x"
    ; "#a=1&signin=y"
    ; "#signin="
    ; "#other"
    ; ""
    ]
    ~f:(fun hash ->
      print_s
        [%message hash ~code:(Signin_link.code_of_hash hash : string option)]);
  List.iter
    [ {|{"user":null,"token":"sekrit","prefer_host":"host-laptop"}|}
    ; {|{"user":"me","token":"t","prefer_host":null}|}
    ; {|{"user":"","token":""}|}
    ; "not json"
    ; "[]"
    ]
    ~f:(fun reply ->
      print_s
        [%sexp
          (Signin_link.Credentials.of_json reply
           : Signin_link.Credentials.t option)]);
  [%expect {|
    (#signin=0123abcd (code (0123abcd)))
    (signin=x (code (x)))
    (#a=1&signin=y (code (y)))
    (#signin= (code ()))
    (#other (code ()))
    ("" (code ()))
    (((user ()) (token (sekrit)) (prefer_host (host-laptop))))
    (((user (me)) (token (t)) (prefer_host ())))
    (((user ()) (token ()) (prefer_host ())))
    ()
    ()
    |}]
;;
