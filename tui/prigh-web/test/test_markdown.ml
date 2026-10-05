open! Core
open Prigh_web

let show text = Render.html (Markdown_view.render text)
let tree text = print_s [%sexp (Markdown.parse text : Markdown.Block.t list)]

let%expect_test "inline spans" =
  show
    "Some **bold**, *italic*, _also_, ***both***, ~~gone~~ and `code \
     **not bold**`.\n\
     snake_case_name stays, 2 * 3 * 4 too, and \\*escaped\\*.\n\
     **outer *inner* outer** and line end\\\n\
     next";
  [%expect {| |}]
;;

let%expect_test "links: safe ones open in a new tab, others are text" =
  show
    "[docs](https://example.com/a_(b)) <https://x.org> \
     https://bare.example/path. [mail](mailto:a@b.c) \
     [bad](javascript:alert(1)) [file](src/main.ml) ![shot](https://i/p.png)";
  [%expect {| |}]
;;

let%expect_test "headings, rules, quotes" =
  show "# Title\n## Sub ##\n###### Six\n#hashtag\n\n---\n> quoted *text*\nlazy line\n> > nested";
  [%expect {| |}]
;;

let%expect_test "lists: nested, ordered, tasks, loose" =
  show
    "- one\n\
    \  - nested\n\
    \    - deeper\n\
     - two\n\
     continued\n\n\
     3. three\n\
     4. four\n\
    \   ```sh\n\
    \   make\n\
    \   ```\n\n\
     - [ ] todo\n\
     - [x] done\n\n\
     Loose:\n\n\
     - loose\n\n\
     - items";
  [%expect {| |}]
;;

let%expect_test "list under a paragraph, numbered with bullets under it" =
  tree "Steps:\n1. build\n  - with dune\n2. test";
  [%expect {| |}]
;;

let%expect_test "tables" =
  show
    "| Name | Count | Note |\n\
     |:-----|------:|:----:|\n\
     | a | 1 | `x \\| y` |\n\
     | b | 22 |\n\
     after";
  [%expect {| |}]
;;

let%expect_test "fenced code, with and without a language" =
  show "```ocaml\nlet x = 1\n\n  indented\n```\n~~~\nplain <b>not html</b>\n~~~";
  [%expect {| |}]
;;

let%expect_test "streaming: every prefix renders, unfinished parts sensibly" =
  let full = "Here **is** a\n\n```py\nprint(1)\n```\n\n| a | b |\n|---|---|\n| 1 | 2 |\n" in
  (* No prefix may raise. *)
  for i = 0 to String.length full do
    ignore (Markdown.parse (String.prefix full i) : Markdown.Block.t list)
  done;
  tree "Here **is";
  tree "```py\nprint(1)";
  tree "| a | b |";
  tree "| a | b |\n|---";
  [%expect {| |}]
;;

let%expect_test "html is text" =
  show "<script>alert(1)</script> & <b>x</b>";
  [%expect {| |}]
;;
