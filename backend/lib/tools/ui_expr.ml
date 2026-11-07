open! Core
open! Import

type t =
  | Number of float
  | Name of string
  | Neg of t
  | Binary of string * t * t
  | Cond of t * t * t
  | Call of string * t list
[@@deriving sexp_of, equal]

let functions =
  [ "min", `At_least 1
  ; "max", `At_least 1
  ; "abs", `Exactly 1
  ; "round", `Exactly 1
  ; "floor", `Exactly 1
  ; "ceil", `Exactly 1
  ; "sqrt", `Exactly 1
  ; "pow", `Exactly 2
  ; "log", `Exactly 1
  ; "ln", `Exactly 1
  ; "exp", `Exactly 1
  ]
;;

module Token = struct
  type t =
    | Num of float
    | Ident of string
    | Op of string (** also parentheses, [,], [?] and [:] *)
    | End
  [@@deriving equal]

  let describe = function
    | Num _ -> "a number"
    | Ident name -> sprintf "%S" name
    | Op op -> sprintf "%S" op
    | End -> "the end"
  ;;
end

exception Parse_error of string

let fail fmt = ksprintf (fun s -> raise (Parse_error s)) fmt
let is_ident_start c = Char.is_alpha c || Char.equal c '_'
let is_ident_char c = Char.is_alphanum c || Char.equal c '_'

(* Tokens with their 1-based columns. *)
let tokenize s =
  let n = String.length s in
  let rec go i acc =
    if i >= n
    then List.rev ((Token.End, n + 1) :: acc)
    else (
      let c = s.[i] in
      if Char.is_whitespace c
      then go (i + 1) acc
      else if
        Char.is_digit c
        || (Char.equal c '.' && i + 1 < n && Char.is_digit s.[i + 1])
      then (
        let j = ref i in
        let digits () =
          while !j < n && Char.is_digit s.[!j] do
            incr j
          done
        in
        digits ();
        if !j < n && Char.equal s.[!j] '.'
        then (
          incr j;
          digits ());
        if !j < n && (Char.equal s.[!j] 'e' || Char.equal s.[!j] 'E')
        then (
          let k = !j + 1 in
          let k =
            if k < n && (Char.equal s.[k] '+' || Char.equal s.[k] '-')
            then k + 1
            else k
          in
          if k < n && Char.is_digit s.[k]
          then (
            j := k;
            digits ()));
        let text = String.sub s ~pos:i ~len:(!j - i) in
        match Float.of_string_opt text with
        | Some f -> go !j ((Token.Num f, i + 1) :: acc)
        | None -> fail "bad number %S at column %d" text (i + 1))
      else if is_ident_start c
      then (
        let j = ref i in
        while !j < n && is_ident_char s.[!j] do
          incr j
        done;
        go !j ((Token.Ident (String.sub s ~pos:i ~len:(!j - i)), i + 1) :: acc))
      else (
        let two = if i + 1 < n then String.sub s ~pos:i ~len:2 else "" in
        if List.mem [ "<="; ">="; "=="; "!=" ] two ~equal:String.equal
        then go (i + 2) ((Token.Op two, i + 1) :: acc)
        else if String.mem "+-*/%^<>()?:," c
        then go (i + 1) ((Token.Op (String.of_char c), i + 1) :: acc)
        else if Char.equal c '='
        then fail "\"=\" at column %d: compare with \"==\"" (i + 1)
        else fail "unexpected %S at column %d" (String.of_char c) (i + 1)))
  in
  go 0 []
;;

let parse s =
  match
    let tokens = ref (tokenize s) in
    let peek () = List.hd_exn !tokens in
    let advance () = tokens := List.tl_exn !tokens in
    let is_op op =
      match fst (peek ()) with
      | Op o -> String.equal o op
      | Num _ | Ident _ | End -> false
    in
    let expect op =
      if is_op op
      then advance ()
      else (
        let token, column = peek () in
        fail
          "expected %S at column %d, found %s"
          op
          column
          (Token.describe token))
    in
    let rec cond () =
      let c = comparison () in
      if is_op "?"
      then (
        advance ();
        let a = cond () in
        expect ":";
        let b = cond () in
        Cond (c, a, b))
      else c
    and left_assoc ops next () =
      let rec loop left =
        match fst (peek ()) with
        | Op op when List.mem ops op ~equal:String.equal ->
          advance ();
          loop (Binary (op, left, next ()))
        | _ -> left
      in
      loop (next ())
    and comparison () =
      left_assoc [ "<"; "<="; ">"; ">="; "=="; "!=" ] additive ()
    and additive () = left_assoc [ "+"; "-" ] multiplicative ()
    and multiplicative () = left_assoc [ "*"; "/"; "%" ] unary ()
    and unary () =
      if is_op "-"
      then (
        advance ();
        Neg (unary ()))
      else power ()
    and power () =
      let base = atom () in
      if is_op "^"
      then (
        advance ();
        Binary ("^", base, unary ()))
      else base
    and atom () =
      let token, column = peek () in
      match token with
      | Num f ->
        advance ();
        Number f
      | Ident name ->
        advance ();
        if is_op "("
        then (
          advance ();
          if is_op ")"
          then (
            advance ();
            Call (name, []))
          else (
            let rec args acc =
              let acc = cond () :: acc in
              if is_op ","
              then (
                advance ();
                args acc)
              else (
                expect ")";
                List.rev acc)
            in
            Call (name, args [])))
        else Name name
      | Op "(" ->
        advance ();
        let e = cond () in
        expect ")";
        e
      | End -> fail "expected a value at the end"
      | Op _ ->
        fail
          "expected a value at column %d, found %s"
          column
          (Token.describe token)
    in
    if Token.equal (fst (peek ())) End
    then fail "the expression is empty"
    else (
      let e = cond () in
      match peek () with
      | End, _ -> e
      | token, column ->
        fail "unexpected %s at column %d" (Token.describe token) column)
  with
  | e -> Ok e
  | exception Parse_error msg -> Error msg
;;

let check t ~inputs =
  let errors = Queue.create () in
  let add fmt = ksprintf (Queue.enqueue errors) fmt in
  let rec go = function
    | Number _ -> ()
    | Name name ->
      if not (List.mem inputs name ~equal:String.equal)
      then (
        match List.Assoc.find functions name ~equal:String.equal with
        | Some _ -> add "%s is a function: call it, e.g. %s(x)" name name
        | None ->
          add
            "unknown name %S; %s"
            name
            (match inputs with
             | [] -> "there are no inputs"
             | inputs -> "the inputs are " ^ String.concat ~sep:", " inputs))
    | Neg e -> go e
    | Binary (_, a, b) ->
      go a;
      go b
    | Cond (c, a, b) ->
      go c;
      go a;
      go b
    | Call (name, args) ->
      (match List.Assoc.find functions name ~equal:String.equal with
       | None ->
         add
           "unknown function %S; the functions are %s"
           name
           (String.concat ~sep:", " (List.map functions ~f:fst))
       | Some arity ->
         let n = List.length args in
         let plural k = if k = 1 then "" else "s" in
         (match arity with
          | `Exactly k when n <> k ->
            add "%s takes %d argument%s, not %d" name k (plural k) n
          | `At_least k when n < k ->
            add "%s takes at least %d argument%s" name k (plural k)
          | `Exactly _ | `At_least _ -> ()));
      List.iter args ~f:go
  in
  go t;
  Queue.to_list errors
;;

let eval t ~inputs =
  let open Option.Let_syntax in
  let truth b = if b then 1. else 0. in
  let rec go = function
    | Number f -> Some f
    | Name name -> inputs name
    | Neg e -> go e >>| Float.neg
    | Binary (op, a, b) ->
      let%bind a = go a in
      let%bind b = go b in
      (match op with
       | "+" -> Some (a +. b)
       | "-" -> Some (a -. b)
       | "*" -> Some (a *. b)
       | "/" -> Option.some_if Float.(b <> 0.) (a /. b)
       | "%" -> Option.some_if Float.(b <> 0.) (Float.mod_float a b)
       | "^" -> Some (a **. b)
       | "<" -> Some (truth Float.(a < b))
       | "<=" -> Some (truth Float.(a <= b))
       | ">" -> Some (truth Float.(a > b))
       | ">=" -> Some (truth Float.(a >= b))
       | "==" -> Some (truth Float.(a = b))
       | "!=" -> Some (truth Float.(a <> b))
       | _ -> None)
    | Cond (c, a, b) ->
      let%bind c = go c in
      if Float.(c <> 0.) then go a else go b
    | Call (name, args) ->
      let%bind args = Option.all (List.map args ~f:go) in
      (match name, args with
       | "min", x :: xs -> Some (List.fold xs ~init:x ~f:Float.min)
       | "max", x :: xs -> Some (List.fold xs ~init:x ~f:Float.max)
       | "abs", [ x ] -> Some (Float.abs x)
       | "round", [ x ] -> Some (Float.round_nearest x)
       | "floor", [ x ] -> Some (Float.round_down x)
       | "ceil", [ x ] -> Some (Float.round_up x)
       | "sqrt", [ x ] -> Some (Float.sqrt x)
       | "pow", [ x; y ] -> Some (x **. y)
       | "log", [ x ] -> Some (Float.log10 x)
       | "ln", [ x ] -> Some (Float.log x)
       | "exp", [ x ] -> Some (Float.exp x)
       | _ -> None)
  in
  go t |> Option.filter ~f:Float.is_finite
;;
