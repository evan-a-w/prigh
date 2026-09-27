open! Core
open! Prigh

let hex s =
  String.to_list s
  |> List.map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
  |> String.concat ~sep:" "
;;

let show s =
  let out = Utf8.sanitize s in
  printf
    "%-30s valid=%-5b -> %s%s\n"
    (hex s)
    (Utf8.is_valid s)
    (hex out)
    (if phys_equal out s then "  (unchanged)" else "")
;;

let%expect_test "valid input is returned as-is" =
  List.iter
    ~f:show
    [ ""; "abc"; "caf\xC3\xA9"; "\xE2\x88\xA8"; "\xF0\x9F\x98\x8A" ];
  [%expect
    {|
                                   valid=true  ->   (unchanged)
    61 62 63                       valid=true  -> 61 62 63  (unchanged)
    63 61 66 c3 a9                 valid=true  -> 63 61 66 c3 a9  (unchanged)
    e2 88 a8                       valid=true  -> e2 88 a8  (unchanged)
    f0 9f 98 8a                    valid=true  -> f0 9f 98 8a  (unchanged)
    |}]
;;

let%expect_test "invalid bytes become U+FFFD" =
  List.iter
    ~f:show
    [ "(\xC2"
    ; "\xC2\n3"
    ; "\xE2\x88"
    ; "a\x80b"
    ; "\xFF\xFE"
    ; "\xC0\xAF"
    ; "\xED\xA0\x80"
    ; "\xF4\x90\x80\x80"
    ; "\xF5\x80"
    ];
  [%expect
    {|
    28 c2                          valid=false -> 28 ef bf bd
    c2 0a 33                       valid=false -> ef bf bd 0a 33
    e2 88                          valid=false -> ef bf bd ef bf bd
    61 80 62                       valid=false -> 61 ef bf bd 62
    ff fe                          valid=false -> ef bf bd ef bf bd
    c0 af                          valid=false -> ef bf bd ef bf bd
    ed a0 80                       valid=false -> ef bf bd ef bf bd ef bf bd
    f4 90 80 80                    valid=false -> ef bf bd ef bf bd ef bf bd ef bf bd
    f5 80                          valid=false -> ef bf bd ef bf bd
    |}]
;;

let%expect_test "split_incomplete_suffix holds back only a possible prefix" =
  let show s =
    let complete, pending = Utf8.split_incomplete_suffix s in
    printf "%-20s -> [%s] [%s]\n" (hex s) (hex complete) (hex pending)
  in
  List.iter
    ~f:show
    [ ""
    ; "abc"
    ; "ab\xC3"
    ; "ab\xE2\x88"
    ; "ab\xF0\x9F\x98"
    ; "ab\xC3\xA9"
    ; "ab\xE0\x80"
    ; "ab\x80\x80"
    ; "\xF0\x9F\x98\x8A\xF0"
    ];
  [%expect
    {|
                         -> [] []
    61 62 63             -> [61 62 63] []
    61 62 c3             -> [61 62] [c3]
    61 62 e2 88          -> [61 62] [e2 88]
    61 62 f0 9f 98       -> [61 62] [f0 9f 98]
    61 62 c3 a9          -> [61 62 c3 a9] []
    61 62 e0 80          -> [61 62 e0 80] []
    61 62 80 80          -> [61 62 80 80] []
    f0 9f 98 8a f0       -> [f0 9f 98 8a] [f0]
    |}]
;;
