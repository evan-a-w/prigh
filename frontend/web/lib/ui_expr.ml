open! Core

type t =
  | Num of float
  | Var of string
  | Neg of t
  | Bin of string * t * t
  | If of t * t * t
  | Call of string * t list
[@@deriving sexp_of]

module Token = struct
  type t =
    | Num of float
    | Name of string
    | Op of string
    | End
  [@@deriving sexp_of, equal]

  let describe = function
    | Num f -> Float.to_string f
    | Name n -> n
    | Op o -> sprintf "%S" o
    | End -> "the end"
  ;;
end

let is_name_char c = Char.is_alphanum c || Char.equal c '_'

let tokenize s =
  let n = String.length s in
  let rec go i acc =
    if i >= n
    then Ok (List.rev ((Token.End, n) :: acc))
    else (
      let c = s.[i] in
      if Char.is_whitespace c
      then go (i + 1) acc
      else if
        Char.is_digit c
        || (Char.equal c '.' && i + 1 < n && Char.is_digit s.[i + 1])
      then (
        let j = ref i in
        while !j < n && (Char.is_digit s.[!j] || Char.equal s.[!j] '.') do
          incr j
        done;
        (* An exponent: 1e6, 2.5E-3. *)
        if !j < n && Char.equal (Char.lowercase s.[!j]) 'e'
        then (
          let k = ref (!j + 1) in
          if !k < n && (Char.equal s.[!k] '+' || Char.equal s.[!k] '-')
          then incr k;
          if !k < n && Char.is_digit s.[!k]
          then (
            while !k < n && Char.is_digit s.[!k] do
              incr k
            done;
            j := !k));
        let text = String.sub s ~pos:i ~len:(!j - i) in
        match Float.of_string_opt text with
        | Some f -> go !j ((Token.Num f, i) :: acc)
        | None -> Or_error.errorf "%S at %d is not a number" text (i + 1))
      else if Char.is_alpha c || Char.equal c '_'
      then (
        let j = ref i in
        while !j < n && is_name_char s.[!j] do
          incr j
        done;
        go !j ((Token.Name (String.sub s ~pos:i ~len:(!j - i)), i) :: acc))
      else (
        let two = if i + 1 < n then String.sub s ~pos:i ~len:2 else "" in
        match two with
        | "<=" | ">=" | "==" | "!=" -> go (i + 2) ((Token.Op two, i) :: acc)
        | _ ->
          (match c with
           | '+'
           | '-'
           | '*'
           | '/'
           | '^'
           | '%'
           | '('
           | ')'
           | ','
           | '<'
           | '>'
           | '?'
           | ':' -> go (i + 1) ((Token.Op (String.of_char c), i) :: acc)
           | _ ->
             Or_error.errorf
               "%C at %d: use numbers, input names, + - * / ^ %%, comparisons, \
                ?: and functions"
               c
               (i + 1))))
  in
  go 0 []
;;

let functions =
  [ "min"
  ; "max"
  ; "abs"
  ; "round"
  ; "floor"
  ; "ceil"
  ; "sqrt"
  ; "pow"
  ; "log"
  ; "ln"
  ; "exp"
  ]
;;

exception Parse_error of string

let parse s =
  match tokenize s with
  | Error e -> Error e
  | Ok tokens ->
    let tokens = ref tokens in
    let peek () = fst (List.hd_exn !tokens) in
    let pos () = snd (List.hd_exn !tokens) + 1 in
    let advance () = tokens := List.tl_exn !tokens in
    let fail fmt = Printf.ksprintf (fun m -> raise (Parse_error m)) fmt in
    let expect op =
      if Token.equal (peek ()) (Op op)
      then advance ()
      else
        fail
          "expected %S at %d, found %s"
          op
          (pos ())
          (Token.describe (peek ()))
    in
    let rec ternary () =
      let c = comparison () in
      if Token.equal (peek ()) (Op "?")
      then (
        advance ();
        let a = ternary () in
        expect ":";
        let b = ternary () in
        If (c, a, b))
      else c
    and comparison () =
      let rec loop a =
        match peek () with
        | Op (("<" | "<=" | ">" | ">=" | "==" | "!=") as op) ->
          advance ();
          loop (Bin (op, a, additive ()))
        | _ -> a
      in
      loop (additive ())
    and additive () =
      let rec loop a =
        match peek () with
        | Op (("+" | "-") as op) ->
          advance ();
          loop (Bin (op, a, multiplicative ()))
        | _ -> a
      in
      loop (multiplicative ())
    and multiplicative () =
      let rec loop a =
        match peek () with
        | Op (("*" | "/" | "%") as op) ->
          advance ();
          loop (Bin (op, a, unary ()))
        | _ -> a
      in
      loop (unary ())
    and unary () =
      match peek () with
      | Op "-" ->
        advance ();
        Neg (unary ())
      | Op "+" ->
        advance ();
        unary ()
      | _ -> power ()
    and power () =
      let base = primary () in
      if Token.equal (peek ()) (Op "^")
      then (
        advance ();
        (* Right associative, and the exponent may be negative: 2^-1. *)
        Bin ("^", base, unary ()))
      else base
    and primary () =
      match peek () with
      | Num f ->
        advance ();
        Num f
      | Op "(" ->
        advance ();
        let e = ternary () in
        expect ")";
        e
      | Name name when Token.equal (peek_second ()) (Op "(") ->
        if not (List.mem functions name ~equal:String.equal)
        then
          fail
            "unknown function %S: use one of %s"
            name
            (String.concat ~sep:" " functions);
        advance ();
        advance ();
        let args =
          if Token.equal (peek ()) (Op ")")
          then []
          else (
            let rec loop acc =
              let acc = ternary () :: acc in
              if Token.equal (peek ()) (Op ",")
              then (
                advance ();
                loop acc)
              else List.rev acc
            in
            loop [])
        in
        expect ")";
        Call (name, args)
      | Name name ->
        advance ();
        Var name
      | tok ->
        fail
          "expected a number, a name or ( at %d, found %s"
          (pos ())
          (Token.describe tok)
    and peek_second () =
      match !tokens with
      | _ :: (t, _) :: _ -> t
      | _ -> End
    in
    (match
       let e = ternary () in
       if not (Token.equal (peek ()) End)
       then fail "unexpected %s at %d" (Token.describe (peek ())) (pos ());
       e
     with
     | e -> Ok e
     | exception Parse_error m -> Or_error.error_string m)
;;

let names t =
  let rec go acc = function
    | Num _ -> acc
    | Var n -> n :: acc
    | Neg a -> go acc a
    | Bin (_, a, b) -> go (go acc a) b
    | If (c, a, b) -> go (go (go acc c) a) b
    | Call (_, args) -> List.fold args ~init:acc ~f:go
  in
  go [] t |> List.rev |> List.stable_dedup ~compare:String.compare
;;

let bool b = if b then 1. else 0.

let eval t ~env =
  let open Or_error.Let_syntax in
  let rec go = function
    | Num f -> Ok f
    | Var n ->
      (match env n with
       | Some v -> Ok v
       | None -> Or_error.errorf "unknown name %S" n)
    | Neg a ->
      let%map a = go a in
      -.a
    | If (c, a, b) ->
      let%bind c = go c in
      if Float.(c <> 0.) then go a else go b
    | Bin (op, a, b) ->
      let%bind a = go a in
      let%bind b = go b in
      (match op with
       | "+" -> Ok (a +. b)
       | "-" -> Ok (a -. b)
       | "*" -> Ok (a *. b)
       | "/" ->
         if Float.(b = 0.)
         then Or_error.error_string "division by zero"
         else Ok (a /. b)
       | "%" ->
         if Float.(b = 0.)
         then Or_error.error_string "division by zero"
         else Ok (Float.mod_float a b)
       | "^" -> Ok (a **. b)
       | "<" -> Ok (bool Float.(a < b))
       | "<=" -> Ok (bool Float.(a <= b))
       | ">" -> Ok (bool Float.(a > b))
       | ">=" -> Ok (bool Float.(a >= b))
       | "==" -> Ok (bool Float.(a = b))
       | "!=" -> Ok (bool Float.(a <> b))
       | op -> Or_error.errorf "unknown operator %S" op)
    | Call (f, args) ->
      let%bind args = Or_error.all (List.map args ~f:go) in
      let arity n =
        if List.length args = n
        then Ok ()
        else
          Or_error.errorf
            "%s takes %d argument%s"
            f
            n
            (if n = 1 then "" else "s")
      in
      (match f, args with
       | ("min" | "max"), [] ->
         Or_error.errorf "%s takes at least one argument" f
       | "min", x :: xs -> Ok (List.fold xs ~init:x ~f:Float.min)
       | "max", x :: xs -> Ok (List.fold xs ~init:x ~f:Float.max)
       | "round", [ x ] -> Ok (Float.round_nearest x)
       | "round", [ x; d ] ->
         let p = 10. **. Float.round_nearest d in
         Ok (Float.round_nearest (x *. p) /. p)
       | "round", _ -> Or_error.error_string "round takes 1 or 2 arguments"
       | "pow", [ x; y ] -> Ok (x **. y)
       | "pow", _ -> arity 2 |> Or_error.map ~f:(fun () -> 0.)
       | ("abs" | "floor" | "ceil" | "sqrt" | "log" | "ln" | "exp"), [ x ] ->
         Ok
           (match f with
            | "abs" -> Float.abs x
            | "floor" -> Float.round_down x
            | "ceil" -> Float.round_up x
            | "sqrt" -> Float.sqrt x
            | "log" -> Float.log10 x
            | "ln" -> Float.log x
            | _ -> Float.exp x)
       | _ -> arity 1 |> Or_error.map ~f:(fun () -> 0.))
  in
  let%bind v = go t in
  if Float.is_finite v then Ok v else Or_error.error_string "not a real number"
;;
