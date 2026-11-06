open! Core
module E = Prigh_web.Ui_expr

let env = function
  | "x" -> Some 3.
  | "rate" -> Some 0.05
  | "zero" -> Some 0.
  | _ -> None
;;

let show expr =
  match E.parse expr with
  | Error e -> printf "%-28s parse error: %s\n" expr (Error.to_string_hum e)
  | Ok t ->
    (match E.eval t ~env with
     | Ok v -> printf "%-28s = %s\n" expr (Float.to_string v)
     | Error e -> printf "%-28s error: %s\n" expr (Error.to_string_hum e))
;;

let%expect_test "precedence and associativity" =
  List.iter
    ~f:show
    [ "1 + 2 * 3"
    ; "(1 + 2) * 3"
    ; "-2^2"
    ; "(-2)^2"
    ; "2^3^2"
    ; "-x"
    ; "--x"
    ; "10 % 4"
    ; "7 / 2"
    ; "1 - 2 - 3"
    ; "2 * -3"
    ; "1.5e3 + .5"
    ];
  [%expect {|
    1 + 2 * 3                    = 7.
    (1 + 2) * 3                  = 9.
    -2^2                         = -4.
    (-2)^2                       = 4.
    2^3^2                        = 512.
    -x                           = -3.
    --x                          = 3.
    10 % 4                       = 2.
    7 / 2                        = 3.5
    1 - 2 - 3                    = -4.
    2 * -3                       = -6.
    1.5e3 + .5                   = 1500.5
    |}]
;;

let%expect_test "comparisons, conditionals, functions" =
  List.iter
    ~f:show
    [ "x > 2"
    ; "x <= 2"
    ; "x == 3"
    ; "x != 3"
    ; "x > 2 ? 10 : 20"
    ; "x > 5 ? 1 : x > 2 ? 2 : 3"
    ; "1 + 2 < 4"
    ; "3 > 2 > 1"
    ; "1 < 2 == 1"
    ; "min(4, x, 9)"
    ; "max(1, x)"
    ; "abs(-4)"
    ; "round(2.5)"
    ; "round(3.14159, 2)"
    ; "floor(-1.5)"
    ; "ceil(1.2)"
    ; "sqrt(16)"
    ; "pow(2, 10)"
    ; "log(1000)"
    ; "ln(exp(2))"
    ; "1000 * (1 + rate)^10"
    ];
  [%expect {|
    x > 2                        = 1.
    x <= 2                       = 0.
    x == 3                       = 1.
    x != 3                       = 0.
    x > 2 ? 10 : 20              = 10.
    x > 5 ? 1 : x > 2 ? 2 : 3    = 2.
    1 + 2 < 4                    = 1.
    3 > 2 > 1                    = 0.
    1 < 2 == 1                   = 1.
    min(4, x, 9)                 = 3.
    max(1, x)                    = 3.
    abs(-4)                      = 4.
    round(2.5)                   = 3.
    round(3.14159, 2)            = 3.14
    floor(-1.5)                  = -2.
    ceil(1.2)                    = 2.
    sqrt(16)                     = 4.
    pow(2, 10)                   = 1024.
    log(1000)                    = 3.
    ln(exp(2))                   = 2.
    1000 * (1 + rate)^10         = 1628.894626777442
    |}]
;;

let%expect_test "errors say what to do" =
  List.iter
    ~f:show
    [ ""
    ; "1 +"
    ; "(1 + 2"
    ; "1 + 2)"
    ; "y * 2"
    ; "foo(1)"
    ; "sqrt(1, 2)"
    ; "x / zero"
    ; "x % zero"
    ; "sqrt(-1)"
    ; "1 $ 2"
    ; "x ? 1"
    ];
  [%expect {|
                                 parse error: expected a number, a name or ( at 1, found the end
    1 +                          parse error: expected a number, a name or ( at 4, found the end
    (1 + 2                       parse error: expected ")" at 7, found the end
    1 + 2)                       parse error: unexpected ")" at 6
    y * 2                        error: unknown name "y"
    foo(1)                       parse error: unknown function "foo": use one of min max abs round floor ceil sqrt pow log ln exp
    sqrt(1, 2)                   error: sqrt takes 1 argument
    x / zero                     error: division by zero
    x % zero                     error: division by zero
    sqrt(-1)                     error: not a real number
    1 $ 2                        parse error: '$' at 3: use numbers, input names, + - * / ^ %, comparisons, ?: and functions
    x ? 1                        parse error: expected ":" at 6, found the end
    |}]
;;

let%expect_test "names" =
  print_s
    [%sexp
      (E.names (Or_error.ok_exn (E.parse "builds * minutes + max(builds, x)"))
       : string list)];
  [%expect {| (builds minutes x) |}]
;;
