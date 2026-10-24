open! Core
open Prigh_web
module Kind = Highlight.Kind
module Lang = Highlight.Lang

(* Tokens as text: plain text as is, the others as [class text]. *)
let show lang text =
  let tokens = Highlight.tokens lang text in
  assert (
    String.equal
      text
      (String.concat
         (List.map tokens ~f:(fun (t : Highlight.Token.t) -> t.text))));
  List.iter tokens ~f:(fun { kind; text } ->
    match kind with
    | Plain -> print_string text
    | _ ->
      printf
        "[%s %s]"
        (String.chop_prefix_exn (Kind.css kind) ~prefix:"hl-")
        text);
  print_endline ""
;;

let%expect_test "OCaml" =
  show
    Ocaml
    {xx|(* a (* nested *) comment *)
let rec f (x : 'a list) = match x with
  | [] -> None
  | x :: _ -> Some (x, 'c', '\n', 0x1F, 3.14e-2, {id|raw "q"|id})
[@@deriving sexp_of]
let%bind y = Foo.bar "multi
line" in MAX_INT|xx};
  [%expect
    {|
    [com (* a (* nested *) comment *)]
    [kw let] [kw rec] f (x : [ty 'a] [ty list]) = [kw match] x [kw with]
      | [] -> [ty None]
      | x :: _ -> [ty Some] (x, [str 'c'], [str '\n'], [num 0x1F], [num 3.14e-2], [str {id|raw "q"|id}])
    [meta [@@deriving] sexp_of]
    [kw let][meta %bind] y = [ty Foo].bar [str "multi]
    [str line"] [kw in] [con MAX_INT]
    |}]
;;

let%expect_test "comments and strings carry over lines" =
  show Ocaml "(* open\nstill *) let";
  show Python "x = '''doc\nstring''' # done";
  show Javascript "const s = `a\n${b}` // c";
  show Rust "let s = r#\"raw\n\"# ; 'a: loop {}";
  [%expect
    {|
    [com (* open]
    [com still *)] [kw let]
    x = [str '''doc]
    [str string'''] [com # done]
    [kw const] s = [str `a]
    [str ${b}`] [com // c]
    [kw let] s = [str r#"raw]
    [str "#] ; [ty 'a]: [kw loop] {}
    |}]
;;

let%expect_test "JavaScript, Python, Rust, Go, C" =
  show
    Javascript
    {|import { x } from "./x.js";
@Component class A extends B { async m() { return this.x ?? null; } }|};
  show
    Python
    {|@dataclass
def f(self, n: int = 3) -> str:
    return f"{n}" if n else None|};
  show
    Rust
    {|#[derive(Debug)]
pub fn f<'a>(s: &'a str) -> Vec<u8> { println!("{}", b"x"); }|};
  show Go {|func main() { s := `raw`; var r rune = 'x'; fmt.Println(s, nil) }|};
  show
    C
    {|#include <stdio.h>
int main(void) { printf("%d\n", MAX); return 0; }|};
  [%expect
    {|
    [kw import] { x } [kw from] [str "./x.js"];
    [meta @Component] [kw class] [ty A] [kw extends] [ty B] { [kw async] m() { [kw return] [kw this].x ?? [con null]; } }
    [meta @dataclass]
    [kw def] f(self, n: [ty int] = [num 3]) -> [ty str]:
        [kw return] [str f"{n}"] [kw if] n [kw else] [con None]
    [meta #[derive(Debug)]]
    [kw pub] [kw fn] f<[ty 'a]>(s: &[ty 'a] [ty str]) -> [ty Vec]<[ty u8]> { [meta println!]([str "{}"], [str b"x"]); }
    [kw func] main() { s := [str `raw`]; [kw var] r [ty rune] = [str 'x']; fmt.Println(s, [con nil]) }
    [meta #include] [str <stdio.h>]
    [ty int] main([ty void]) { printf([str "%d\n"], [con MAX]); [kw return] [num 0]; }
    |}]
;;

let%expect_test "shell, Nix, SQL, sexp" =
  show Shell {|if [ -n "$HOME" ]; then echo ${X:-1} 'a#b' $1; fi # done|};
  show
    Nix
    {|{ pkgs ? import <nixpkgs> {} }: let x = ./src; in { name = "a-${x}"; s = ''
  multi
''; }|};
  show
    Sql
    {|SELECT id, name FROM users WHERE age > 30 -- adults
  AND note = 'it''s';|};
  show
    Sexp
    {|(library ; the lib
 (name prigh_web) (flags (:standard -w +a)) (deps %{bin:node}))|};
  [%expect
    {|
    [kw if] [ -n [str "$HOME"] ]; [kw then] echo [var ${X:-1}] [str 'a#b'] [var $1]; [kw fi] [com # done]
    { pkgs ? [ty import] [str <nixpkgs>] {} }: [kw let] [prop x] = [str ./src]; [kw in] { [prop name] = [str "a-${x}"]; [prop s] = [str '']
    [str   multi]
    [str '']; }
    [kw SELECT] id, name [kw FROM] users [kw WHERE] age > [num 30] [com -- adults]
      [kw AND] note = [str 'it''s'];
    ([kw library] [com ; the lib]
     ([kw name] prigh_web) ([kw flags] ([con :standard] -w +a)) ([kw deps] [var %{bin:node}]))
    |}]
;;

let%expect_test "JSON, YAML, TOML" =
  show Json {|{"name": "prigh", "n": 1.5, "ok": true, "list": [null, "x"]}|};
  show
    Yaml
    {|---
name: prigh  # a comment
on: [push]
jobs:
  - run: |
      echo "a: b"
    with: &anchor
      n: 3
      q: "quoted"|};
  show
    Toml
    {|[package]
name = "prigh" # comment
"quoted.key" = 'lit'
version = 1.0
[[bin]]
desc = """
multi
"""|};
  [%expect
    {|
    {[prop "name"]: [str "prigh"], [prop "n"]: [num 1.5], [prop "ok"]: [con true], [prop "list"]: [[con null], [str "x"]]}
    [meta ---]
    [prop name]: [str prigh ] [com # a comment]
    [prop on]: [push]
    [prop jobs]:
      [kw -] [prop run]: [meta |]
    [str       echo "a: b"]
        [prop with]: [var &anchor]
          [prop n]: [num 3]
          [prop q]: [str "quoted"]
    [tag [package]]
    [prop name] = [str "prigh"] [com # comment]
    [prop "quoted.key"] = [str 'lit']
    [prop version] = [num 1.0]
    [tag [[bin]]]
    [prop desc] = [str """]
    [str multi]
    [str """]
    |}]
;;

let%expect_test "Markdown, with fenced code highlighted in its language" =
  show
    Markdown
    {|# Title
Some *em*, **strong**, `code` and [a link](https://x.y).
> quoted
- item
1. first
```ocaml
let x = 1 (* c *)
```
after|};
  [%expect
    {|
    [head # Title]
    Some [em *em*], [strong **strong**], [str `code`] and [a link][link (https://x.y)].
    [com >] quoted
    [kw -] item
    [kw 1.] first
    [meta ```ocaml]
    [kw let] x = [num 1] [com (* c *)]
    [meta ```]
    after
    |}]
;;

let%expect_test "HTML and CSS, script and style embedded" =
  show
    Html
    {|<!DOCTYPE html>
<!-- a
comment -->
<a href="x" class='y'
   data-n=1>Tom &amp; Jerry</a>
<style>.x { color: #fff; }</style>
<script>let a = "<b>";</script>|};
  show
    Css
    {|@media (max-width: 760px) {
  .app:not(.narrow) > a:hover { --w: 4px; width: var(--w, 50%) !important; }
}
/* done */ #id { margin: .5em 0 }|};
  [%expect
    {|
    [meta <!DOCTYPE html>]
    [com <!-- a]
    [com comment -->]
    [tag <a] [prop href]=[str "x"] [prop class]=[str 'y']
       [prop data-n]=[str 1][tag >]Tom [con &amp;] Jerry[tag </a>]
    [tag <style>.x] { [prop color]: [num #fff]; }[tag </style>]
    [tag <script>][kw let] a = [str "<b>"];[tag </script>]
    [kw @media] ([prop max-width]: [num 760px]) {
      [tag .app]:[tag not]([tag .narrow]) > [tag a]:[tag hover] { [var --w]: [num 4px]; [prop width]: var([var --w], [num 50%]) [kw !important]; }
    }
    [com /* done */] [tag #id] { [prop margin]: [num .5em] [num 0] }
    |}]
;;

let%expect_test "diff" =
  show Diff "--- a/x\n+++ b/x\n@@ -1 +1 @@\n-old\n+new\n same";
  [%expect
    {|
    [meta --- a/x]
    [meta +++ b/x]
    [meta @@ -1 +1 @@]
    [del -old]
    [add +new]
     same
    |}]
;;

let%expect_test "languages by fence and by path" =
  List.iter
    [ "ocaml"
    ; "ML"
    ; "ts"
    ; "tsx"
    ; "py"
    ; "rust"
    ; "sh"
    ; "console"
    ; "jsonc"
    ; "yml"
    ; "{.python}"
    ; "c++"
    ; "dune"
    ; "patch"
    ; "text"
    ; ""
    ; "js title=\"x\""
    ]
    ~f:(fun name ->
      printf
        "%-14s %s\n"
        name
        ([%sexp_of: Lang.t option] (Lang.of_name name) |> Sexp.to_string));
  List.iter
    [ "src/main.ml"
    ; "a/b/dune"
    ; "flake.lock"
    ; "x.tsx"
    ; "Cargo.lock"
    ; ".bashrc"
    ; "README.md"
    ; "style.CSS"
    ; "noext"
    ; ".hidden"
    ; "page.html"
    ]
    ~f:(fun path ->
      printf
        "%-14s %s\n"
        path
        ([%sexp_of: Lang.t option] (Lang.of_path path) |> Sexp.to_string));
  [%expect
    {|
    ocaml          (Ocaml)
    ML             (Ocaml)
    ts             (Javascript)
    tsx            (Javascript)
    py             (Python)
    rust           (Rust)
    sh             (Shell)
    console        (Shell)
    jsonc          (Json)
    yml            (Yaml)
    {.python}      (Python)
    c++            (C)
    dune           (Sexp)
    patch          (Diff)
    text           ()
                   ()
    js title="x"   (Javascript)
    src/main.ml    (Ocaml)
    a/b/dune       (Sexp)
    flake.lock     (Json)
    x.tsx          (Javascript)
    Cargo.lock     (Toml)
    .bashrc        (Shell)
    README.md      (Markdown)
    style.CSS      (Css)
    noext          ()
    .hidden        ()
    page.html      (Html)
    |}]
;;

let%expect_test "any text tokenizes to itself, in every language" =
  let samples =
    [ ""
    ; "\n\n"
    ; "'"
    ; "\""
    ; "\\"
    ; "(*"
    ; "{|"
    ; "<"
    ; "<a"
    ; "```"
    ; "#"
    ; "$"
    ; "@"
    ; "r#"
    ; "'\\"
    ; "0x"
    ; "1e"
    ; "é'ü'"
    ; "a: |"
    ; "[x"
    ; "\"\"\""
    ; "<!--"
    ; "/*"
    ; "<script>"
    ; "%{"
    ; "-"
    ; "`*_["
    ]
  in
  List.iter Lang.all ~f:(fun lang ->
    List.iter samples ~f:(fun s ->
      List.iter samples ~f:(fun t ->
        let text = s ^ t ^ "\n" ^ t ^ s in
        let got =
          String.concat
            (List.map
               (Highlight.tokens lang text)
               ~f:(fun (t : Highlight.Token.t) -> t.text))
        in
        if not (String.equal got text)
        then
          printf "%s %S -> %S\n" (Sexp.to_string (Lang.sexp_of_t lang)) text got)));
  [%expect {| |}]
;;

let%expect_test "a 256 KiB line does not overflow the stack" =
  let line = String.concat (List.init 40_000 ~f:(fun i -> sprintf "%d," i)) in
  List.iter [ Lang.Json; Javascript; Html; Markdown; Css ] ~f:(fun lang ->
    printf "%d\n" (List.length (Highlight.tokens lang line)));
  [%expect
    {|
    80000
    80000
    1
    1
    80000
    |}]
;;

(* In the page: fenced code in replies (also while it streams), by the
   fence's language; none without one. *)
let reply ?(event = "message_end") text =
  sprintf
    {|{"event":%S,%s:{"role":"assistant","content":[{"type":"text","text":%S}],"stop_reason":{"type":"end_turn"},"usage":{"input":0,"output":0,"cache_read":0},"model":"m"}%s}|}
    event
    (if String.equal event "message_end" then {|"message"|} else {|"partial"|})
    text
    (if String.equal event "message_end"
     then ""
     else {|,"delta":{"type":"text_delta","text":""}|})
;;

let%expect_test "code blocks in the chat" =
  Chat_harness.show
    ~selector:".code pre"
    (Chat_harness.chat
       [ reply
           "Try:\n\n\
            ```python\n\
            def f(): return 'x'  # hi\n\
            ```\n\n\
            ```json\n\
            {\"a\": [1, null]}\n\
            ```\n\n\
            ```\n\
            no language\n\
            ```"
       ]);
  [%expect
    {|
    <pre class="copy-text">
      <code class="language-python">
        <span class="hl-kw"> def </span>
         f():
        <span class="hl-kw"> return </span>

        <span class="hl-str"> 'x' </span>

        <span class="hl-com"> # hi </span>
      </code>
    </pre>
    <pre class="copy-text">
      <code class="language-json">
        {
        <span class="hl-prop"> "a" </span>
        : [
        <span class="hl-num"> 1 </span>
        ,
        <span class="hl-con"> null </span>
        ]}
      </code>
    </pre>
    <pre class="copy-text">
      <code> no language </code>
    </pre>
    |}];
  (* Streaming: the open block is highlighted as it grows. *)
  Chat_harness.show
    ~selector:".code pre"
    (Chat_harness.chat
       ~running:true
       [ reply ~event:"message_update" "```rust\nfn main() { let s = \"" ]);
  [%expect
    {|
    <pre class="copy-text">
      <code class="language-rust">
        <span class="hl-kw"> fn </span>
         main() {
        <span class="hl-kw"> let </span>
         s =
        <span class="hl-str"> " </span>
      </code>
    </pre>
    |}]
;;

(* The file browser's text preview, by the file's name; past 64 KiB the
   colours stop, and say so. *)
let preview ~name text =
  let h =
    Harness.create
      ~state:
        (Harness.state_json
           ~fields:
             [ "active_host", `String "c7"
             ; "cwd", `String "/p"
             ; ( "hosts"
               , Jsonaf.of_string
                   {|[{"id":"c7","name":"laptop","cwd":"/p","session_id":"s1","session_name":null}]|}
               )
             ]
           ())
      ()
  in
  Harness.quiet h (fun () ->
    Harness.act h (Files Toggle);
    Harness.reply
      h
      "fs_list"
      (sprintf
         {|{"path":"/p","parent":"/","home":"/p","entries":[{"name":%S,"kind":"file","size":%d,"mtime":null}],"total":1,"hidden":0}|}
         name
         (String.length text));
    Harness.click h ~selector:(sprintf ".files-row[data-name='%s']" name);
    Harness.reply
      h
      "fs_preview"
      (Jsonaf.to_string
         (`Object
             [ "path", `String ("/p/" ^ name)
             ; "name", `String name
             ; "size", `Number (Int.to_string (String.length text))
             ; "mtime", `Null
             ; "kind", `String "text"
             ; "text", `String text
             ; "truncated", `False
             ]));
    ());
  h
;;

let%expect_test "the file preview" =
  let h = preview ~name:"flake.nix" "{ x = 1; # one\n  y = \"two\"; }\n" in
  Harness.show h ~selector:".files-code";
  [%expect
    {|
    (Files (Select flake.nix))
    <pre class="files-code">
      {
      <span class="hl-prop"> x </span>
       =
      <span class="hl-num"> 1 </span>
      ;
      <span class="hl-com"> # one </span>


      <span class="hl-prop"> y </span>
       =
      <span class="hl-str"> "two" </span>
      ; }

    </pre>
    |}];
  (* The line that passes 64 KiB is the last one highlighted. *)
  let big = "let a = 1\n" ^ String.make 70_000 'x' ^ "\nlet b = 2\n" in
  let h = preview ~name:"big.ml" big in
  Harness.elements h ~selector:".files-code > span";
  Harness.text h ~selector:".files-note";
  [%expect
    {|
    (Files (Select big.ml))
    <span class="hl-kw">
    <span class="hl-num">
    Colours stop after the first 64.0 KB.
    |}]
;;
