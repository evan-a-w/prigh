open! Core

module Kind = struct
  type t =
    | Plain
    | Keyword
    | Type
    | String
    | Number
    | Constant
    | Comment
    | Property
    | Variable
    | Meta
    | Tag
    | Heading
    | Emph
    | Strong
    | Link
    | Added
    | Removed
  [@@deriving sexp_of, equal, enumerate]

  let css = function
    | Plain -> ""
    | Keyword -> "hl-kw"
    | Type -> "hl-ty"
    | String -> "hl-str"
    | Number -> "hl-num"
    | Constant -> "hl-con"
    | Comment -> "hl-com"
    | Property -> "hl-prop"
    | Variable -> "hl-var"
    | Meta -> "hl-meta"
    | Tag -> "hl-tag"
    | Heading -> "hl-head"
    | Emph -> "hl-em"
    | Strong -> "hl-strong"
    | Link -> "hl-link"
    | Added -> "hl-add"
    | Removed -> "hl-del"
  ;;
end

module Token = struct
  type t =
    { kind : Kind.t
    ; text : string
    }
  [@@deriving sexp_of, equal]
end

module Lang = struct
  type t =
    | Ocaml
    | Javascript
    | Python
    | Rust
    | Go
    | C
    | Shell
    | Json
    | Yaml
    | Toml
    | Markdown
    | Html
    | Css
    | Nix
    | Sql
    | Sexp
    | Diff
  [@@deriving sexp_of, equal, enumerate]

  let of_word = function
    | "ocaml" | "ml" | "mli" | "mll" | "mly" -> Some Ocaml
    | "js"
    | "javascript"
    | "jsx"
    | "mjs"
    | "cjs"
    | "ts"
    | "typescript"
    | "tsx"
    | "mts"
    | "cts" -> Some Javascript
    | "py" | "python" | "python3" | "py3" | "pyi" -> Some Python
    | "rs" | "rust" -> Some Rust
    | "go" | "golang" -> Some Go
    | "c"
    | "h"
    | "cpp"
    | "c++"
    | "cc"
    | "cxx"
    | "hpp"
    | "hh"
    | "hxx"
    | "java"
    | "cs"
    | "csharp" -> Some C
    | "sh" | "bash" | "zsh" | "shell" | "console" | "shellsession" | "ksh" ->
      Some Shell
    | "json" | "jsonc" | "json5" | "jsonl" | "ndjson" -> Some Json
    | "yaml" | "yml" -> Some Yaml
    | "toml" -> Some Toml
    | "md" | "markdown" -> Some Markdown
    | "html" | "htm" | "xml" | "svg" | "xhtml" -> Some Html
    | "css" | "scss" | "less" -> Some Css
    | "nix" -> Some Nix
    | "sql" | "psql" | "postgres" | "postgresql" | "mysql" | "sqlite" ->
      Some Sql
    | "dune"
    | "sexp"
    | "lisp"
    | "scheme"
    | "scm"
    | "el"
    | "elisp"
    | "rkt"
    | "racket" -> Some Sexp
    | "diff" | "patch" -> Some Diff
    | _ -> None
  ;;

  let of_name name =
    let name = String.lowercase (String.strip name) in
    let name = String.lstrip name ~drop:(fun c -> Char.equal c '{') in
    let name = String.chop_prefix_if_exists name ~prefix:"." in
    match String.split_on_chars name ~on:[ ' '; ','; '{'; '}'; '\t' ] with
    | word :: _ -> of_word word
    | [] -> None
  ;;

  let of_path path =
    let base =
      match String.rsplit2 path ~on:'/' with
      | Some (_, base) -> base
      | None -> path
    in
    match base with
    | "dune" | "dune-project" | "dune-workspace" -> Some Sexp
    | "flake.lock" -> Some Json
    | "Cargo.lock" | "uv.lock" | "poetry.lock" -> Some Toml
    | ".bashrc"
    | ".zshrc"
    | ".profile"
    | ".bash_profile"
    | ".envrc"
    | "PKGBUILD" -> Some Shell
    | _ ->
      (match String.rsplit2 base ~on:'.' with
       | Some (stem, ext) when not (String.is_empty stem) ->
         of_word (String.lowercase ext)
       | _ -> None)
  ;;
end

module State = struct
  type t =
    { mode : mode
    ; depth : int (* CSS's braces *)
    }

  and mode =
    | Code
    | Comment of
        { open_ : string
        ; close : string
        ; nests : bool
        ; level : int
        }
    | Str of
        { close : string
        ; escapes : bool
        }
    | Text of { indent : int } (* a YAML block scalar, deeper than [indent] *)
    | Tag of string (* inside an HTML tag, after its name *)
    | Embed of
        { lang : Lang.t
        ; close : string
        ; inner : t
        }
      (* HTML's script or style, until [close] *)
    | Fence of
        { lang : Lang.t option
        ; fence : string
        ; inner : t
        }
      (* a Markdown code block *)
  [@@deriving sexp_of, equal]

  let initial = { mode = Code; depth = 0 }
  let code depth = { mode = Code; depth }
end

(* Tokens, adjacent ones of the same kind merged. *)
module Acc = struct
  type t =
    { mutable rev : Token.t list
    ; mutable kind : Kind.t
    ; buf : Buffer.t
    }

  let create () = { rev = []; kind = Plain; buf = Buffer.create 64 }

  let flush t =
    if Buffer.length t.buf > 0
    then (
      t.rev <- { Token.kind = t.kind; text = Buffer.contents t.buf } :: t.rev;
      Buffer.clear t.buf)
  ;;

  let add t (kind : Kind.t) text =
    if not (String.is_empty text)
    then (
      if not (Kind.equal kind t.kind)
      then (
        flush t;
        t.kind <- kind);
      Buffer.add_string t.buf text)
  ;;

  let sub t kind line ~pos ~stop =
    if stop > pos then add t kind (String.sub line ~pos ~len:(stop - pos))
  ;;

  let finish t =
    flush t;
    List.rev t.rev
  ;;
end

let at line i s = String.is_substring_at line ~pos:i ~substring:s
let is_space c = Char.equal c ' ' || Char.equal c '\t' || Char.equal c '\r'
let get line i = if i >= 0 && i < String.length line then line.[i] else '\000'

let skip_spaces line i =
  let n = String.length line in
  let rec go i = if i < n && is_space line.[i] then go (i + 1) else i in
  go i
;;

let leading_spaces line = skip_spaces line 0

(* From [i]: the body of a comment, to its close (or the line's end). *)
let comment_body acc line i ~open_ ~close ~nests ~level =
  let n = String.length line in
  let rec go j level =
    if j >= n
    then (
      Acc.sub acc Comment line ~pos:i ~stop:n;
      `Open level)
    else if at line j close
    then (
      let j = j + String.length close in
      if level = 0
      then (
        Acc.sub acc Comment line ~pos:i ~stop:j;
        `Closed j)
      else go j (level - 1))
    else if nests && at line j open_
    then go (j + String.length open_) (level + 1)
    else go (j + 1) level
  in
  go i level
;;

let string_body acc kind line i ~close ~escapes =
  let n = String.length line in
  let rec go j =
    if j >= n
    then (
      Acc.sub acc kind line ~pos:i ~stop:n;
      `Open)
    else if escapes && Char.equal line.[j] '\\'
    then go (j + 2)
    else if at line j close
    then (
      let j = j + String.length close in
      Acc.sub acc kind line ~pos:i ~stop:j;
      `Closed j)
    else go (j + 1)
  in
  go i
;;

(* ---- the generic tokenizer, configured per language *)

module Rule = struct
  type t =
    { open_ : string
    ; close : string
    ; escapes : bool
    ; multiline : bool
    }

  let v ?(escapes = true) ?(multiline = false) ?close open_ =
    { open_; close = Option.value close ~default:open_; escapes; multiline }
  ;;
end

module Hook = struct
  type t =
    | Token of Kind.t * int (* the token ends here *)
    | Open of
        { from : int (* the opening ends here *)
        ; close : string
        ; escapes : bool
        }
  (* a string, possibly over lines *)
end

module Key_rule = struct
  type t =
    | No_keys
    | String_colon (* JSON: a string before [:] *)
    | Ident_eq (* Nix: a name before [=] *)
    | Css
end

type spec =
  { line_comments : string list
  ; comment_after_space : bool (* shell's [#] *)
  ; blocks : (string * string * bool) list (* open, close, nests *)
  ; strings : Rule.t list (* longest openings first *)
  ; prefixes : string list (* Python's [r"..."] *)
  ; keywords : String.Set.t
  ; types : String.Set.t
  ; constants : String.Set.t
  ; caseless : bool
  ; capitals : bool (* [Foo] a type, [FOO] a constant *)
  ; caps_constants : bool (* [FOO] a constant *)
  ; macros : bool (* Rust's [name!] *)
  ; heads : bool (* a sexp's first atom *)
  ; keys : Key_rule.t
  ; ident_start : char list
  ; ident_char : char list
  ; hook : string -> int -> depth:int -> Hook.t option
  }

let words s = String.Set.of_list (String.split_on_chars s ~on:[ ' '; '\n' ])

let base =
  { line_comments = []
  ; comment_after_space = false
  ; blocks = []
  ; strings = []
  ; prefixes = []
  ; keywords = String.Set.empty
  ; types = String.Set.empty
  ; constants = String.Set.empty
  ; caseless = false
  ; capitals = false
  ; caps_constants = false
  ; macros = false
  ; heads = false
  ; keys = No_keys
  ; ident_start = []
  ; ident_char = []
  ; hook = (fun _ _ ~depth:_ -> None)
  }
;;

let word_char c =
  Char.is_alphanum c || Char.equal c '_' || Char.to_int c >= 0x80
;;

let ident_start spec c =
  Char.is_alpha c
  || Char.equal c '_'
  || Char.to_int c >= 0x80
  || List.mem spec.ident_start ~equal:Char.equal c
;;

let ident_char spec c =
  word_char c || List.mem spec.ident_char ~equal:Char.equal c
;;

let ident_end spec line i =
  let n = String.length line in
  let rec go j = if j < n && ident_char spec line.[j] then go (j + 1) else j in
  go i
;;

let number_end line i =
  let n = String.length line in
  let rec go j =
    if j >= n
    then j
    else (
      let c = line.[j] in
      if word_char c
      then go (j + 1)
      else if Char.equal c '.' && Char.is_digit (get line (j + 1))
      then go (j + 1)
      else if
        Char.equal c '.'
        && (not (Char.equal (get line (j + 1)) '.'))
        && (not (ident_start base (get line (j + 1))))
        && String.for_all (String.sub line ~pos:i ~len:(j - i)) ~f:Char.is_digit
      then (* [2.] *) j + 1
      else if
        (Char.equal c '-' || Char.equal c '+')
        && (Char.equal line.[j - 1] 'e' || Char.equal line.[j - 1] 'E')
        && Char.is_digit (get line (j + 1))
        && not (at line i "0x")
      then go (j + 1)
      else if Char.equal c '%'
      then j + 1
      else j)
  in
  go (i + 1)
;;

(* In CSS: whether [i] is in a selector, before a [{] (rather than a [;] or
   [}]). *)
let selector line i =
  let n = String.length line in
  let rec go j =
    if j >= n
    then false
    else (
      match line.[j] with
      | '{' -> true
      | ';' | '}' -> false
      | _ -> go (j + 1))
  in
  go i
;;

let is_upper_word w =
  String.length w >= 2
  && String.exists w ~f:Char.is_alpha
  && String.for_all w ~f:(fun c ->
    Char.is_uppercase c || Char.is_digit c || Char.equal c '_')
;;

let classify spec word =
  let key = if spec.caseless then String.lowercase word else word in
  if Set.mem spec.keywords key
  then Some Kind.Keyword
  else if Set.mem spec.constants key
  then Some Constant
  else if Set.mem spec.types key
  then Some Type
  else if (spec.capitals || spec.caps_constants) && is_upper_word word
  then Some Constant
  else if spec.capitals && Char.is_uppercase word.[0]
  then Some Type
  else None
;;

module Step = struct
  type t =
    | Next of
        { i : int
        ; depth : int
        ; prev : char (* the last character before [i] that is not a space *)
        }
    | Done of State.t
end

(* One token of [line] at [i], in code. *)
let rec step spec acc line i ~depth ~prev : Step.t =
  let n = String.length line in
  let c = line.[i] in
  let next j kind ~prev : Step.t =
    Acc.sub acc kind line ~pos:i ~stop:j;
    Next { i = j; depth; prev }
  in
  let open_string ~start ~from ~close ~escapes ~multiline : Step.t =
    Acc.sub acc String line ~pos:start ~stop:from;
    match string_body acc String line from ~close ~escapes with
    | `Closed j -> Next { i = j; depth; prev = '"' }
    | `Open when multiline -> Done { mode = Str { close; escapes }; depth }
    | `Open -> Done (State.code depth)
  in
  if is_space c
  then next (skip_spaces line i) Plain ~prev
  else if
    List.exists spec.line_comments ~f:(at line i)
    && ((not spec.comment_after_space) || i = 0 || is_space line.[i - 1])
  then (
    Acc.sub acc Comment line ~pos:i ~stop:n;
    Done (State.code depth))
  else (
    match List.find spec.blocks ~f:(fun (o, _, _) -> at line i o) with
    | Some (open_, close, nests) ->
      let from = i + String.length open_ in
      Acc.sub acc Comment line ~pos:i ~stop:from;
      (match comment_body acc line from ~open_ ~close ~nests ~level:0 with
       | `Closed j -> Next { i = j; depth; prev }
       | `Open level ->
         Done { mode = Comment { open_; close; nests; level }; depth })
    | None ->
      (match spec.hook line i ~depth with
       | Some (Token (kind, j)) -> next j kind ~prev:line.[j - 1]
       | Some (Open { from; close; escapes }) ->
         open_string ~start:i ~from ~close ~escapes ~multiline:true
       | None ->
         (match
            List.find spec.strings ~f:(fun (r : Rule.t) -> at line i r.open_)
          with
          | Some r ->
            let from = i + String.length r.open_ in
            let key =
              match spec.keys with
              | String_colon ->
                (* A key: the string, then [:]. *)
                (match
                   string_body
                     (Acc.create ())
                     String
                     line
                     from
                     ~close:r.close
                     ~escapes:r.escapes
                 with
                 | `Closed j when Char.equal (get line (skip_spaces line j)) ':'
                   -> Some j
                 | _ -> None)
              | No_keys | Ident_eq | Css -> None
            in
            (match key with
             | Some j -> next j Property ~prev:'"'
             | None ->
               open_string
                 ~start:i
                 ~from
                 ~close:r.close
                 ~escapes:r.escapes
                 ~multiline:r.multiline)
          | None ->
            if
              (Char.is_digit c
               || (Char.equal c '.' && Char.is_digit (get line (i + 1))))
              && not (i > 0 && ident_char spec line.[i - 1])
            then next (number_end line i) Number ~prev:'0'
            else if ident_start spec c
            then word spec acc line i ~depth ~prev
            else (
              let depth =
                match c with
                | '{' -> depth + 1
                | '}' -> Int.max 0 (depth - 1)
                | _ -> depth
              in
              Acc.sub acc Plain line ~pos:i ~stop:(i + 1);
              Next { i = i + 1; depth; prev = c }))))

and word spec acc line i ~depth ~prev : Step.t =
  let j = ident_end spec line i in
  let w = String.sub line ~pos:i ~len:(j - i) in
  let after = skip_spaces line j in
  match List.find spec.strings ~f:(fun (r : Rule.t) -> at line j r.open_) with
  | Some r when List.mem spec.prefixes (String.lowercase w) ~equal:String.equal
    ->
    (* A string with a prefix: [f"..."]. *)
    let from = j + String.length r.open_ in
    Acc.sub acc String line ~pos:i ~stop:from;
    (match
       string_body acc String line from ~close:r.close ~escapes:r.escapes
     with
     | `Closed k -> Next { i = k; depth; prev = '"' }
     | `Open when r.multiline ->
       Done { mode = Str { close = r.close; escapes = r.escapes }; depth }
     | `Open -> Done (State.code depth))
  | _ ->
    let kind : Kind.t =
      match spec.keys with
      | Css when String.is_prefix w ~prefix:"--" -> Variable
      | Css
        when Char.equal (get line after) ':'
             && (after + 1 >= String.length line || is_space line.[after + 1])
        -> Property
      | Css when depth = 0 || selector line i -> Tag
      | Ident_eq
        when Char.equal (get line after) '='
             && not (Char.equal (get line (after + 1)) '=') -> Property
      | _ when spec.heads && Char.equal prev '(' -> Keyword
      | _
        when spec.macros
             && Char.equal (get line j) '!'
             && not (Char.equal (get line (j + 1)) '=') -> Meta
      | _ -> Option.value (classify spec w) ~default:Plain
    in
    let j =
      match kind with
      | Meta when spec.macros -> j + 1
      | _ -> j
    in
    Acc.sub acc kind line ~pos:i ~stop:j;
    Next { i = j; depth; prev = 'a' }
;;

(* Tokens of [line] from [i] in code, then the state for the next line. *)
let rec code spec acc line i ~depth ~prev : State.t =
  if i >= String.length line
  then State.code depth
  else (
    match step spec acc line i ~depth ~prev with
    | Next { i; depth; prev } -> code spec acc line i ~depth ~prev
    | Done state -> state)
;;

(* A line in [spec], from [state]. *)
let generic spec acc line (state : State.t) : State.t =
  let depth = state.depth in
  match state.mode with
  | Comment { open_; close; nests; level } ->
    (match comment_body acc line 0 ~open_ ~close ~nests ~level with
     | `Closed j -> code spec acc line j ~depth ~prev:' '
     | `Open level -> { mode = Comment { open_; close; nests; level }; depth })
  | Str { close; escapes } ->
    (match string_body acc String line 0 ~close ~escapes with
     | `Closed j -> code spec acc line j ~depth ~prev:'"'
     | `Open -> state)
  | Code | Text _ | Tag _ | Embed _ | Fence _ ->
    code spec acc line 0 ~depth ~prev:' '
;;

(* ---- the languages *)

let ident_after line i =
  let n = String.length line in
  let rec go j =
    if j < n && (word_char line.[j] || Char.equal line.[j] '.')
    then go (j + 1)
    else j
  in
  go i
;;

(* ['x'], ['\n'], ['\u{1F600}']: a character; otherwise [None]. *)
let char_literal line i =
  if Char.equal (get line (i + 1)) '\\'
  then (
    match String.index_from line (i + 2) '\'' with
    | Some k when k - i <= 12 -> Some (k + 1)
    | _ -> None)
  else if
    Char.equal (get line (i + 2)) '\''
    && not (Char.equal (get line (i + 1)) '\'')
  then Some (i + 3)
  else (
    (* A multi-byte character. *)
    let c = Char.to_int (get line (i + 1)) in
    if c >= 0xc0
    then (
      let len = if c >= 0xf0 then 4 else if c >= 0xe0 then 3 else 2 in
      if Char.equal (get line (i + 1 + len)) '\''
      then Some (i + 2 + len)
      else None)
    else None)
;;

let ocaml =
  { base with
    blocks = [ "(*", "*)", true ]
  ; strings = [ Rule.v "\"" ~multiline:true ]
  ; keywords =
      words
        "and as assert begin class constraint do done downto else end \
         exception external for fun function functor if in include inherit \
         initializer lazy let match method module mutable new nonrec object of \
         open or private rec sig struct then to try type val virtual when \
         while with raise failwith"
  ; types =
      words
        "int float string bool char unit list option array ref bytes exn \
         result int32 int64 nativeint lazy_t format"
  ; constants = words "true false"
  ; capitals = true
  ; ident_char = [ '\'' ]
  ; hook =
      (fun line i ~depth:_ ->
        match line.[i] with
        | '\'' ->
          (match char_literal line i with
           | Some j -> Some (Token (String, j))
           | None ->
             let c = get line (i + 1) in
             if Char.is_lowercase c || Char.equal c '_'
             then Some (Token (Type, ident_after line (i + 1)))
             else None)
        | '{' ->
          let n = String.length line in
          let rec id j =
            if j < n && (Char.is_lowercase line.[j] || Char.equal line.[j] '_')
            then id (j + 1)
            else j
          in
          let j = id (i + 1) in
          if Char.equal (get line j) '|'
          then
            Some
              (Open
                 { from = j + 1
                 ; close =
                     "|" ^ String.sub line ~pos:(i + 1) ~len:(j - i - 1) ^ "}"
                 ; escapes = false
                 })
          else None
        | '['
          when Char.equal (get line (i + 1)) '@'
               || Char.equal (get line (i + 1)) '%' ->
          let n = String.length line in
          let rec marks j =
            if j < n && (Char.equal line.[j] '@' || Char.equal line.[j] '%')
            then marks (j + 1)
            else j
          in
          Some (Token (Meta, ident_after line (marks (i + 1))))
        | '%'
          when i > 0
               && word_char line.[i - 1]
               && Char.is_alpha (get line (i + 1)) ->
          Some (Token (Meta, ident_after line (i + 1)))
        | _ -> None)
  }
;;

let c_like_keywords =
  "break case continue default do else for if return switch while goto"
;;

let decorator line i ~depth:_ : Hook.t option =
  if Char.equal line.[i] '@' && ident_start base (get line (i + 1))
  then Some (Token (Meta, ident_after line (i + 1)))
  else None
;;

let javascript =
  { base with
    line_comments = [ "//" ]
  ; blocks = [ "/*", "*/", false ]
  ; strings = [ Rule.v "\""; Rule.v "'"; Rule.v "`" ~multiline:true ]
  ; keywords =
      words
        (c_like_keywords
         ^ " catch class const debugger delete export extends finally function \
            import in instanceof let new super this throw try typeof var void \
            with yield async await of static get set from as type interface \
            enum implements private protected public readonly declare \
            namespace abstract keyof satisfies")
  ; types = words "string number boolean any unknown never object symbol bigint"
  ; constants = words "true false null undefined NaN Infinity"
  ; capitals = true
  ; ident_start = [ '$' ]
  ; ident_char = [ '$' ]
  ; hook = decorator
  }
;;

let python =
  { base with
    line_comments = [ "#" ]
  ; strings =
      [ Rule.v "\"\"\"" ~multiline:true
      ; Rule.v "'''" ~multiline:true
      ; Rule.v "\""
      ; Rule.v "'"
      ]
  ; prefixes = [ "r"; "b"; "f"; "u"; "rb"; "br"; "fr"; "rf" ]
  ; keywords =
      words
        "and as assert async await break class continue def del elif else \
         except finally for from global if import in is lambda nonlocal not or \
         pass raise return try while with yield match case"
  ; types = words "int str float bool list dict set tuple bytes object type"
  ; constants = words "True False None"
  ; capitals = true
  ; hook =
      (fun line i ~depth ->
        if i = leading_spaces line then decorator line i ~depth else None)
  }
;;

let rust =
  { base with
    line_comments = [ "//" ]
  ; blocks = [ "/*", "*/", true ]
  ; strings = [ Rule.v "\"" ~multiline:true ]
  ; prefixes = [ "b" ]
  ; keywords =
      words
        "as async await break const continue crate dyn else enum extern fn for \
         if impl in let loop match mod move mut pub ref return self Self \
         static struct super trait type unsafe use where while"
  ; types =
      words
        "i8 i16 i32 i64 i128 isize u8 u16 u32 u64 u128 usize f32 f64 bool char \
         str"
  ; constants = words "true false"
  ; capitals = true
  ; macros = true
  ; hook =
      (fun line i ~depth:_ ->
        let n = String.length line in
        match line.[i] with
        | '\'' ->
          (match char_literal line i with
           | Some j -> Some (Token (String, j))
           | None when Char.is_alpha (get line (i + 1)) ->
             Some (Token (Type, ident_after line (i + 1)))
           | None -> None)
        | ('r' | 'b') when not (i > 0 && word_char line.[i - 1]) ->
          let j =
            if at line i "br" then i + 2 else if at line i "r" then i + 1 else i
          in
          let rec hashes k =
            if k < n && Char.equal line.[k] '#' then hashes (k + 1) else k
          in
          let k = hashes j in
          if j > i && Char.equal (get line k) '"'
          then
            Some
              (Open
                 { from = k + 1
                 ; close = "\"" ^ String.make (k - j) '#'
                 ; escapes = false
                 })
          else None
        | '#' when Char.equal (get line (i + 1)) '[' || at line (i + 1) "![" ->
          (match String.index_from line i ']' with
           | Some k -> Some (Token (Meta, k + 1))
           | None -> Some (Token (Meta, n)))
        | _ -> None)
  }
;;

let go =
  { base with
    line_comments = [ "//" ]
  ; blocks = [ "/*", "*/", false ]
  ; strings =
      [ Rule.v "\""; Rule.v "'"; Rule.v "`" ~escapes:false ~multiline:true ]
  ; keywords =
      words
        (c_like_keywords
         ^ " chan const defer fallthrough func go import interface map package \
            range select struct type var")
  ; types =
      words
        "bool byte complex64 complex128 error float32 float64 int int8 int16 \
         int32 int64 rune string uint uint8 uint16 uint32 uint64 uintptr any"
  ; constants = words "true false nil iota"
  }
;;

let c =
  { base with
    line_comments = [ "//" ]
  ; blocks = [ "/*", "*/", false ]
  ; strings = [ Rule.v "\""; Rule.v "'" ]
  ; keywords =
      words
        (c_like_keywords
         ^ " auto const extern inline register restrict signed sizeof static \
            struct typedef union unsigned volatile enum class namespace \
            template typename public private protected virtual override final \
            new delete this throw try catch using operator friend constexpr \
            noexcept static_cast dynamic_cast reinterpret_cast const_cast \
            explicit mutable decltype concept requires co_await co_return \
            co_yield import package extends implements interface")
  ; types =
      words
        "void char short int long float double bool size_t ssize_t int8_t \
         int16_t int32_t int64_t uint8_t uint16_t uint32_t uint64_t uintptr_t \
         wchar_t"
  ; constants = words "true false NULL nullptr null"
  ; caps_constants = true
  ; hook =
      (fun line i ~depth:_ ->
        let start = leading_spaces line in
        match line.[i] with
        | '#' when i = start ->
          Some (Token (Meta, ident_after line (skip_spaces line (i + 1))))
        | '<'
          when String.is_prefix
                 (String.drop_prefix line start)
                 ~prefix:"#include" ->
          (match String.index_from line i '>' with
           | Some k -> Some (Token (String, k + 1))
           | None -> None)
        | _ -> None)
  }
;;

let shell =
  { base with
    line_comments = [ "#" ]
  ; comment_after_space = true
  ; strings =
      [ Rule.v "'" ~escapes:false ~multiline:true
      ; Rule.v "\"" ~multiline:true
      ; Rule.v "`" ~multiline:true
      ]
  ; keywords =
      words
        "if then else elif fi for while until do done case esac in function \
         select time return exit break continue local export readonly declare \
         set unset shift source alias eval exec trap"
  ; constants = words "true false"
  ; ident_char = [ '-' ]
  ; hook =
      (fun line i ~depth:_ ->
        if not (Char.equal line.[i] '$')
        then None
        else (
          match get line (i + 1) with
          | '{' ->
            (match String.index_from line i '}' with
             | Some k -> Some (Token (Variable, k + 1))
             | None -> Some (Token (Variable, String.length line)))
          | '@' | '*' | '#' | '?' | '$' | '!' | '0' .. '9' ->
            Some (Token (Variable, i + 2))
          | c when Char.is_alpha c || Char.equal c '_' ->
            let n = String.length line in
            let rec go j =
              if j < n && word_char line.[j] then go (j + 1) else j
            in
            Some (Token (Variable, go (i + 1)))
          | _ -> None))
  }
;;

let json =
  { base with
    line_comments = [ "//" ]
  ; blocks = [ "/*", "*/", false ]
  ; strings = [ Rule.v "\"" ]
  ; constants = words "true false null"
  ; keys = String_colon
  }
;;

let nix =
  { base with
    line_comments = [ "#" ]
  ; blocks = [ "/*", "*/", false ]
  ; strings =
      [ Rule.v "''" ~escapes:false ~multiline:true
      ; Rule.v "\"" ~multiline:true
      ]
  ; keywords = words "assert else if in inherit let or rec then with"
  ; types = words "builtins import derivation throw abort"
  ; constants = words "true false null"
  ; keys = Ident_eq
  ; ident_char = [ '-'; '\'' ]
  ; hook =
      (fun line i ~depth:_ ->
        let path_end j =
          let n = String.length line in
          let rec go j =
            if j < n && (word_char line.[j] || String.mem "./-+~" line.[j])
            then go (j + 1)
            else j
          in
          go j
        in
        match line.[i] with
        | '<' when ident_start base (get line (i + 1)) ->
          let j = path_end (i + 1) in
          if Char.equal (get line j) '>'
          then Some (Token (String, j + 1))
          else None
        | '.' when at line i "./" || at line i "../" ->
          Some (Token (String, path_end i))
        | _ -> None)
  }
;;

let sql =
  { base with
    line_comments = [ "--" ]
  ; blocks = [ "/*", "*/", false ]
  ; strings = [ Rule.v "'" ~escapes:false ~multiline:true ]
  ; caseless = true
  ; keywords =
      words
        "select from where and or not insert into values update set delete \
         create table drop alter add column index view join inner left right \
         outer full cross on as group by order having limit offset union all \
         distinct case when then else end is in exists between like ilike \
         primary key foreign references default unique check constraint begin \
         commit rollback transaction with returning if replace cascade asc \
         desc trigger function returns language using"
  ; types =
      words
        "int integer bigint smallint text varchar char boolean bool real float \
         double numeric decimal date time timestamp timestamptz interval json \
         jsonb blob serial bigserial uuid bytea"
  ; constants = words "true false null"
  }
;;

let sexp =
  let not_atom = "()[]\";' \t" in
  { base with
    line_comments = [ ";" ]
  ; blocks = [ "#|", "|#", true ]
  ; strings = [ Rule.v "\"" ~multiline:true ]
  ; constants = words "true false nil #t #f"
  ; heads = true
  ; ident_start =
      List.filter (String.to_list "!$&*+-./<=>?^~#@") ~f:(fun c ->
        not (String.mem not_atom c))
  ; ident_char = String.to_list "!$&*+-./<=>?^~#@:,"
  ; hook =
      (fun line i ~depth:_ ->
        match line.[i] with
        | '%' when Char.equal (get line (i + 1)) '{' ->
          (match String.index_from line i '}' with
           | Some k -> Some (Token (Variable, k + 1))
           | None -> None)
        | ':' when ident_start base (get line (i + 1)) ->
          let n = String.length line in
          let rec go j =
            if j < n && not (String.mem not_atom line.[j])
            then go (j + 1)
            else j
          in
          Some (Token (Constant, go (i + 1)))
        | _ -> None)
  }
;;

let css =
  { base with
    blocks = [ "/*", "*/", false ]
  ; strings = [ Rule.v "\""; Rule.v "'" ]
  ; keys = Css
  ; ident_start = [ '-' ]
  ; ident_char = [ '-' ]
  ; hook =
      (fun line i ~depth ->
        match line.[i] with
        | '@' ->
          Some
            (Token
               ( Keyword
               , ident_end { base with ident_char = [ '-' ] } line (i + 1) ))
        | '#' when word_char (get line (i + 1)) ->
          let j = ident_end { base with ident_char = [ '-' ] } line (i + 1) in
          Some
            (Token ((if depth = 0 || selector line i then Tag else Number), j))
        | '.'
          when (depth = 0 || selector line i)
               && ident_start base (get line (i + 1)) ->
          Some
            (Token
               (Tag, ident_end { base with ident_char = [ '-' ] } line (i + 1)))
        | '!' when at line i "!important" -> Some (Token (Keyword, i + 10))
        | _ -> None)
  }
;;

(* TOML's and YAML's values. *)
let toml_value =
  { base with
    line_comments = [ "#" ]
  ; strings =
      [ Rule.v "\"\"\"" ~multiline:true
      ; Rule.v "'''" ~escapes:false ~multiline:true
      ; Rule.v "\""
      ; Rule.v "'" ~escapes:false
      ]
  ; constants = words "true false inf nan"
  ; ident_char = [ '-' ]
  }
;;

let yaml_value =
  { base with
    line_comments = [ "#" ]
  ; comment_after_space = true
  ; strings =
      [ Rule.v "\"" ~multiline:true; Rule.v "'" ~escapes:false ~multiline:true ]
  ; constants = words "true false null yes no on off True False Null ~"
  ; hook =
      (fun line i ~depth:_ ->
        match line.[i] with
        | ('&' | '*') when word_char (get line (i + 1)) ->
          Some
            (Token
               ( Variable
               , ident_end { base with ident_char = [ '-' ] } line (i + 1) ))
        | '!' ->
          let n = String.length line in
          let rec go j =
            if j < n && not (is_space line.[j]) then go (j + 1) else j
          in
          Some (Token (Meta, go (i + 1)))
        | _ -> None)
  }
;;

let toml acc line (state : State.t) : State.t =
  match state.mode with
  | Str _ | Comment _ -> generic toml_value acc line state
  | _ ->
    let start = leading_spaces line in
    Acc.sub acc Plain line ~pos:0 ~stop:start;
    if Char.equal (get line start) '['
    then (
      let close =
        match String.index_from line start ']' with
        | Some k -> if Char.equal (get line (k + 1)) ']' then k + 2 else k + 1
        | None -> String.length line
      in
      Acc.sub acc Tag line ~pos:start ~stop:close;
      code toml_value acc line close ~depth:0 ~prev:' ')
    else (
      (* key = value; the key may be dotted or quoted. *)
      match String.index_from line start '=' with
      | Some k
        when k > start
             && not
                  (String.exists
                     (String.sub line ~pos:start ~len:(k - start))
                     ~f:(fun c -> String.mem "#[{" c)) ->
        let key_end =
          let rec back j =
            if j > start && is_space line.[j - 1] then back (j - 1) else j
          in
          back k
        in
        Acc.sub acc Property line ~pos:start ~stop:key_end;
        Acc.sub acc Plain line ~pos:key_end ~stop:(k + 1);
        code toml_value acc line (k + 1) ~depth:0 ~prev:'='
      | _ -> code toml_value acc line start ~depth:0 ~prev:' ')
;;

(* The end of a YAML key at [i] ([key:] then a space or the line's end), if
   there is one. *)
let yaml_key line i =
  let n = String.length line in
  let rec go j =
    if j >= n
    then None
    else (
      match line.[j] with
      | ':' when j + 1 >= n || is_space line.[j + 1] -> Some j
      | '#' when j > i && is_space line.[j - 1] -> None
      | _ -> go (j + 1))
  in
  match get line i with
  | '"' | '\'' ->
    (match String.index_from line (i + 1) line.[i] with
     | Some k when Char.equal (get line (k + 1)) ':' -> Some (k + 1)
     | _ -> None)
  | '#' | '[' | '{' | '&' | '*' | '!' | '|' | '>' | '%' | '@' | '`' -> None
  | _ -> go i
;;

let yaml acc line (state : State.t) : State.t =
  let n = String.length line in
  let start = leading_spaces line in
  match state.mode with
  | Text { indent } when start > indent || start = n ->
    Acc.sub acc String line ~pos:0 ~stop:n;
    state
  | Str _ | Comment _ -> generic yaml_value acc line state
  | _ ->
    if
      String.equal (String.rstrip line) "---"
      || String.equal (String.rstrip line) "..."
    then (
      Acc.add acc Meta line;
      State.initial)
    else (
      Acc.sub acc Plain line ~pos:0 ~stop:start;
      (* Sequence items: "- - key: value". *)
      let rec items i =
        if Char.equal (get line i) '-' && (i + 1 >= n || is_space line.[i + 1])
        then (
          Acc.sub acc Keyword line ~pos:i ~stop:(i + 1);
          let j = skip_spaces line (i + 1) in
          Acc.sub acc Plain line ~pos:(i + 1) ~stop:j;
          items j)
        else i
      in
      let i = items start in
      let value j =
        let v = skip_spaces line j in
        Acc.sub acc Plain line ~pos:j ~stop:v;
        let rest = String.rstrip (String.drop_prefix line v) in
        let block =
          String.length rest >= 1
          && (Char.equal rest.[0] '|' || Char.equal rest.[0] '>')
          && String.for_all (String.drop_prefix rest 1) ~f:(fun c ->
            Char.is_digit c || Char.equal c '-' || Char.equal c '+')
        in
        if block
        then (
          Acc.sub acc Meta line ~pos:v ~stop:n;
          { State.mode = Text { indent = i }; depth = 0 })
        else if
          v < n
          && (not
                (String.mem "\"'#&*![{" line.[v]
                 || Char.is_digit line.[v]
                 || Char.equal line.[v] '-'))
          && Option.is_none
               (classify yaml_value (String.rstrip (String.drop_prefix line v)))
        then (
          (* A plain scalar: a string, up to a comment. *)
          let stop =
            match String.substr_index line ~pos:v ~pattern:" #" with
            | Some k -> k
            | None -> n
          in
          Acc.sub acc String line ~pos:v ~stop;
          code yaml_value acc line stop ~depth:0 ~prev:' ')
        else code yaml_value acc line v ~depth:0 ~prev:' '
      in
      match yaml_key line i with
      | Some k ->
        Acc.sub acc Property line ~pos:i ~stop:k;
        Acc.sub acc Plain line ~pos:k ~stop:(k + 1);
        value (k + 1)
      | None -> value i)
;;

let diff acc line : State.t =
  let kind : Kind.t =
    if
      at line 0 "+++"
      || at line 0 "---"
      || at line 0 "@@"
      || at line 0 "diff "
      || at line 0 "index "
    then Meta
    else if at line 0 "+"
    then Added
    else if at line 0 "-"
    then Removed
    else Plain
  in
  Acc.add acc kind line;
  State.initial
;;

(* ---- Markdown and HTML, which embed the others *)

let rec line_in lang acc text (state : State.t) : State.t =
  match (lang : Lang.t) with
  | Ocaml -> generic ocaml acc text state
  | Javascript -> generic javascript acc text state
  | Python -> generic python acc text state
  | Rust -> generic rust acc text state
  | Go -> generic go acc text state
  | C -> generic c acc text state
  | Shell -> generic shell acc text state
  | Json -> generic json acc text state
  | Nix -> generic nix acc text state
  | Sql -> generic sql acc text state
  | Sexp -> generic sexp acc text state
  | Css -> generic css acc text state
  | Toml -> toml acc text state
  | Yaml -> yaml acc text state
  | Diff -> diff acc text
  | Markdown -> markdown acc text state
  | Html -> html acc text state

and markdown acc line (state : State.t) : State.t =
  let n = String.length line in
  let start = leading_spaces line in
  let fence_at i =
    let run ch =
      let rec go j =
        if j < n && Char.equal line.[j] ch then go (j + 1) else j
      in
      go i - i
    in
    match get line i with
    | ('`' | '~') as ch when run ch >= 3 -> Some (String.make (run ch) ch)
    | _ -> None
  in
  match state.mode with
  | Fence { lang; fence; inner } ->
    (match fence_at start with
     | Some f
       when start < 4
            && Char.equal f.[0] fence.[0]
            && String.length f >= String.length fence
            && String.is_empty
                 (String.strip
                    (String.drop_prefix line (start + String.length f))) ->
       Acc.add acc Meta line;
       State.initial
     | _ ->
       (match lang with
        | Some lang ->
          let inner = line_in lang acc line inner in
          { state with mode = Fence { lang = Some lang; fence; inner } }
        | None ->
          Acc.add acc Plain line;
          state))
  | _ ->
    (match fence_at start with
     | Some fence when start < 4 ->
       Acc.add acc Meta line;
       let info = String.drop_prefix line (start + String.length fence) in
       { State.initial with
         mode = Fence { lang = Lang.of_name info; fence; inner = State.initial }
       }
     | _ ->
       let rest = String.drop_prefix line start in
       let hashes =
         String.length rest
         - String.length (String.lstrip rest ~drop:(Char.equal '#'))
       in
       if
         hashes >= 1
         && hashes <= 6
         && (hashes = String.length rest || is_space rest.[hashes])
       then (
         Acc.add acc Heading line;
         State.initial)
       else if
         (String.for_all (String.strip rest) ~f:(Char.equal '-')
          || String.for_all (String.strip rest) ~f:(Char.equal '*'))
         && String.length (String.strip rest) >= 3
       then (
         Acc.add acc Meta line;
         State.initial)
       else (
         Acc.sub acc Plain line ~pos:0 ~stop:start;
         let marker =
           match get line start with
           | '>' -> Some (Kind.Comment, start + 1)
           | ('-' | '*' | '+') when is_space (get line (start + 1)) ->
             Some (Keyword, start + 1)
           | '0' .. '9' ->
             let rec digits j =
               if Char.is_digit (get line j) then digits (j + 1) else j
             in
             let j = digits start in
             if
               (Char.equal (get line j) '.' || Char.equal (get line j) ')')
               && is_space (get line (j + 1))
             then Some (Keyword, j + 1)
             else None
           | _ -> None
         in
         let i =
           match marker with
           | Some (kind, j) ->
             Acc.sub acc kind line ~pos:start ~stop:j;
             j
           | None -> start
         in
         markdown_inline acc line i;
         State.initial))

and markdown_inline acc line i =
  if i < String.length line
  then markdown_inline acc line (markdown_token acc line i)

(* One inline token at [i]: where the next starts. *)
and markdown_token acc line i =
  let n = String.length line in
  let closing j s =
    match String.substr_index line ~pos:j ~pattern:s with
    | Some k when k > j -> Some k
    | _ -> None
  in
  let emit kind stop =
    Acc.sub acc kind line ~pos:i ~stop;
    stop
  in
  let plain () = emit Plain (i + 1) in
  match line.[i] with
  | '`' ->
    (match String.index_from line (i + 1) '`' with
     | Some k -> emit String (k + 1)
     | None -> plain ())
  | ('*' | '_') as ch ->
    let double = Char.equal (get line (i + 1)) ch in
    let mark = String.make (if double then 2 else 1) ch in
    let from = i + String.length mark in
    let flanked = from < n && not (is_space line.[from]) in
    let intraword = Char.equal ch '_' && i > 0 && word_char line.[i - 1] in
    (match closing from mark with
     | Some k when flanked && not intraword ->
       emit (if double then Strong else Emph) (k + String.length mark)
     | _ -> plain ())
  | '[' ->
    (match String.index_from line i ']' with
     | Some k when Char.equal (get line (k + 1)) '(' ->
       (match String.index_from line k ')' with
        | Some e ->
          Acc.sub acc Plain line ~pos:i ~stop:(k + 1);
          Acc.sub acc Link line ~pos:(k + 1) ~stop:(e + 1);
          e + 1
        | None -> plain ())
     | _ -> plain ())
  | '<' when at line (i + 1) "http" ->
    (match String.index_from line i '>' with
     | Some k -> emit Link (k + 1)
     | None -> plain ())
  | _ ->
    let rec run j =
      if j < n && not (String.mem "`*_[<" line.[j]) then run (j + 1) else j
    in
    emit Plain (Int.max (i + 1) (run i))

and html acc line (state : State.t) : State.t = html_from acc line 0 state

and html_from acc line i (state : State.t) : State.t =
  if i >= String.length line
  then state
  else (
    let i, state = html_token acc line i state in
    html_from acc line i state)

(* One token at [i]: where the next starts, in what state. *)
and html_token acc line i (state : State.t) : int * State.t =
  let n = String.length line in
  let name_end j =
    let rec go j =
      if j < n && (word_char line.[j] || String.mem "-:." line.[j])
      then go (j + 1)
      else j
    in
    go j
  in
  match state.mode with
  | Comment { open_; close; nests; level } ->
    (match comment_body acc line i ~open_ ~close ~nests ~level with
     | `Closed j -> j, State.initial
     | `Open level ->
       n, { state with mode = Comment { open_; close; nests; level } })
  | Str { close; escapes } ->
    (match string_body acc String line i ~close ~escapes with
     | `Closed j -> j, { state with mode = Tag "" }
     | `Open -> n, state)
  | Embed { lang; close; inner } ->
    let lower = String.lowercase line in
    (match String.substr_index lower ~pos:i ~pattern:close with
     | Some k ->
       ignore
         (line_in lang acc (String.sub line ~pos:i ~len:(k - i)) inner
          : State.t);
       k, State.initial
     | None ->
       let inner = line_in lang acc (String.drop_prefix line i) inner in
       n, { state with mode = Embed { lang; close; inner } })
  | Tag name ->
    let c = line.[i] in
    if is_space c
    then (
      let j = skip_spaces line i in
      Acc.sub acc Plain line ~pos:i ~stop:j;
      j, state)
    else if Char.equal c '>' || at line i "/>"
    then (
      let j = if Char.equal c '>' then i + 1 else i + 2 in
      Acc.sub acc Tag line ~pos:i ~stop:j;
      let embed lang close =
        { state with mode = Embed { lang; close; inner = State.initial } }
      in
      match String.lowercase name with
      | "script" when Char.equal c '>' -> j, embed Javascript "</script"
      | "style" when Char.equal c '>' -> j, embed Css "</style"
      | _ -> j, State.initial)
    else if Char.equal c '"' || Char.equal c '\''
    then (
      let close = String.of_char c in
      Acc.sub acc String line ~pos:i ~stop:(i + 1);
      match string_body acc String line (i + 1) ~close ~escapes:false with
      | `Closed j -> j, state
      | `Open -> n, { state with mode = Str { close; escapes = false } })
    else if word_char c
    then (
      let j = name_end i in
      Acc.sub acc Property line ~pos:i ~stop:j;
      j, state)
    else if Char.equal c '=' && not (String.mem " \t\"'>" (get line (i + 1)))
    then (
      (* An unquoted value. *)
      let rec go j =
        if j < n && not (String.mem " \t>" line.[j]) then go (j + 1) else j
      in
      Acc.sub acc Plain line ~pos:i ~stop:(i + 1);
      let j = go (i + 1) in
      Acc.sub acc String line ~pos:(i + 1) ~stop:j;
      j, state)
    else (
      Acc.sub acc Plain line ~pos:i ~stop:(i + 1);
      i + 1, state)
  | Code | Text _ | Fence _ ->
    if at line i "<!--"
    then (
      Acc.sub acc Comment line ~pos:i ~stop:(i + 4);
      ( i + 4
      , { state with
          mode =
            Comment { open_ = "<!--"; close = "-->"; nests = false; level = 0 }
        } ))
    else if at line i "<!" || at line i "<?"
    then (
      let j =
        match String.index_from line i '>' with
        | Some k -> k + 1
        | None -> n
      in
      Acc.sub acc Meta line ~pos:i ~stop:j;
      j, State.initial)
    else if
      Char.equal line.[i] '<'
      && (Char.is_alpha (get line (i + 1))
          || (Char.equal (get line (i + 1)) '/'
              && Char.is_alpha (get line (i + 2))))
    then (
      let from = if Char.equal line.[i + 1] '/' then i + 2 else i + 1 in
      let j = name_end from in
      Acc.sub acc Tag line ~pos:i ~stop:j;
      (* Closing tags are named "/name": they embed nothing. *)
      ( j
      , { state with
          mode = Tag (String.sub line ~pos:(i + 1) ~len:(j - i - 1))
        } ))
    else if Char.equal line.[i] '&'
    then (
      let entity k =
        k - i <= 10
        && String.for_all
             (String.sub line ~pos:(i + 1) ~len:(k - i - 1))
             ~f:(fun c -> Char.is_alphanum c || Char.equal c '#')
      in
      match String.index_from line i ';' with
      | Some k when entity k ->
        Acc.sub acc Constant line ~pos:i ~stop:(k + 1);
        k + 1, state
      | _ ->
        Acc.sub acc Plain line ~pos:i ~stop:(i + 1);
        i + 1, state)
    else (
      let rec run j =
        if j < n && not (String.mem "<&" line.[j]) then run (j + 1) else j
      in
      let j = Int.max (i + 1) (run i) in
      Acc.sub acc Plain line ~pos:i ~stop:j;
      j, state)
;;

let line lang state text =
  let acc = Acc.create () in
  let state = line_in lang acc text state in
  Acc.finish acc, state
;;

let tokens lang text =
  let acc = Acc.create () in
  let n = String.length text in
  let rec go pos state =
    if pos <= n
    then (
      let stop =
        match String.index_from text pos '\n' with
        | Some k -> k
        | None -> n
      in
      let state =
        line_in lang acc (String.sub text ~pos ~len:(stop - pos)) state
      in
      if stop < n
      then (
        Acc.add acc Plain "\n";
        go (stop + 1) state))
  in
  go 0 State.initial;
  Acc.finish acc
;;
