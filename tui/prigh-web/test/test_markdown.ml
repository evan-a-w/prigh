open! Core
open Prigh_web

let show text = Render.html (Markdown_view.render text)
let tree text = print_s [%sexp (Markdown.parse text : Markdown.Block.t list)]

let%expect_test "inline spans" =
  show
    "Some **bold**, *italic*, _also_, ***both***, ~~gone~~ and `code **not \
     bold**`.\n\
     snake_case_name stays, 2 * 3 * 4 too, and \\*escaped\\*.\n\
     **outer *inner* outer** and line end\\\n\
     next";
  [%expect
    {|
    <div class="markdown">
      <p>
        Some
        <strong> bold </strong>
        ,
        <em> italic </em>
        ,
        <em> also </em>
        ,
        <strong>
          <em> both </em>
        </strong>
        ,
        <del> gone </del>
         and
        <code> code **not bold** </code>
        .
        <br/>
        snake_case_name stays, 2 * 3 * 4 too, and *escaped*.
        <br/>
        <strong>
          outer
          <em> inner </em>
           outer
        </strong>
         and line end
        <br/>
        next
      </p>
    </div>
    |}]
;;

let%expect_test "links: safe ones open in a new tab, others are text" =
  show
    "[docs](https://example.com/a_(b)) <https://x.org> \
     https://bare.example/path. [mail](mailto:a@b.c) \
     [bad](javascript:alert(1)) [file](src/main.ml) ![shot](https://i/p.png)";
  [%expect
    {|
    <div class="markdown">
      <p>
        <a href="https://example.com/a_(b)" target="_blank" rel="noopener noreferrer"> docs </a>

        <a href="https://x.org" target="_blank" rel="noopener noreferrer"> https://x.org </a>

        <a href="https://bare.example/path" target="_blank" rel="noopener noreferrer"> https://bare.example/path </a>
        .
        <a href="mailto:a@b.c" target="_blank" rel="noopener noreferrer"> mail </a>

        bad

        file

        <a href="https://i/p.png" target="_blank" rel="noopener noreferrer"> shot </a>
      </p>
    </div>
    |}]
;;

let%expect_test "headings, rules, quotes" =
  show
    "# Title\n\
     ## Sub ##\n\
     ###### Six\n\
     #hashtag\n\n\
     ---\n\
     > quoted *text*\n\
     lazy line\n\
     > > nested";
  [%expect
    {|
    <div class="markdown">
      <h1> Title </h1>
      <h2> Sub </h2>
      <h6> Six </h6>
      <p> #hashtag </p>
      <hr/>
      <blockquote>
        <p>
          quoted
          <em> text </em>
          <br/>
          lazy line
        </p>
        <blockquote>
          <p> nested </p>
        </blockquote>
      </blockquote>
    </div>
    |}]
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
  [%expect
    {|
    <div class="markdown">
      <ul>
        <li>
          one
          <ul>
            <li>
              nested
              <ul>
                <li> deeper </li>
              </ul>
            </li>
          </ul>
        </li>
        <li>
          two
          <br/>
          continued
        </li>
      </ul>
      <ol start="3">
        <li> three </li>
        <li>
          four
          <div class="code copyable">
            <div class="code-head">
              <span class="code-lang"> sh </span>
              <button type="button" title="Copy" class="copy"> Copy </button>
            </div>
            <pre class="copy-text">
              <code class="language-sh"> make </code>
            </pre>
          </div>
        </li>
      </ol>
      <ul>
        <li class="task">
          <input type="checkbox" disabled=""/>
          todo
        </li>
        <li class="task">
          <input type="checkbox" disabled="" checked=""/>
          done
        </li>
      </ul>
      <p> Loose: </p>
      <ul>
        <li>
          <p> loose </p>
        </li>
        <li>
          <p> items </p>
        </li>
      </ul>
    </div>
    |}]
;;

let%expect_test "list under a paragraph, numbered with bullets under it" =
  tree "Steps:\n1. build\n  - with dune\n2. test";
  [%expect
    {|
    ((Paragraph ((Text Steps:)))
     (List (start (1)) (tight true)
      (items
       (((checked ())
         (blocks
          ((Paragraph ((Text build)))
           (List (start ()) (tight true)
            (items (((checked ()) (blocks ((Paragraph ((Text "with dune"))))))))))))
        ((checked ()) (blocks ((Paragraph ((Text test))))))))))
    |}]
;;

let%expect_test "tables" =
  show
    "| Name | Count | Note |\n\
     |:-----|------:|:----:|\n\
     | a | 1 | `x \\| y` |\n\
     | b | 22 |\n\
     after";
  [%expect
    {|
    <div class="markdown">
      <div class="table-wrap">
        <table>
          <thead>
            <tr>
              <th class="left"> Name </th>
              <th class="right"> Count </th>
              <th class="center"> Note </th>
            </tr>
          </thead>
          <tbody>
            <tr>
              <td class="left"> a </td>
              <td class="right"> 1 </td>
              <td class="center">
                <code> x \| y </code>
              </td>
            </tr>
            <tr>
              <td class="left"> b </td>
              <td class="right"> 22 </td>
              <td class="center"> </td>
            </tr>
          </tbody>
        </table>
      </div>
      <p> after </p>
    </div>
    |}]
;;

let%expect_test "fenced code, with and without a language" =
  show "```ocaml\nlet x = 1\n\n  indented\n```\n~~~\nplain <b>not html</b>\n~~~";
  [%expect
    {|
    <div class="markdown">
      <div class="code copyable">
        <div class="code-head">
          <span class="code-lang"> ocaml </span>
          <button type="button" title="Copy" class="copy"> Copy </button>
        </div>
        <pre class="copy-text">
          <code class="language-ocaml"> let x = 1

      indented </code>
        </pre>
      </div>
      <div class="code copyable">
        <div class="code-head">
          <span class="code-lang"> text </span>
          <button type="button" title="Copy" class="copy"> Copy </button>
        </div>
        <pre class="copy-text">
          <code> plain <b>not html</b> </code>
        </pre>
      </div>
    </div>
    |}]
;;

let%expect_test "streaming: every prefix renders, unfinished parts sensibly" =
  let full =
    "Here **is** a\n\n```py\nprint(1)\n```\n\n| a | b |\n|---|---|\n| 1 | 2 |\n"
  in
  (* No prefix may raise. *)
  for i = 0 to String.length full do
    ignore (Markdown.parse (String.prefix full i) : Markdown.Block.t list)
  done;
  tree "Here **is";
  tree "```py\nprint(1)";
  tree "| a | b |";
  tree "| a | b |\n|---";
  [%expect
    {|
    ((Paragraph ((Text "Here **is"))))
    ((Code (lang py) (text "print(1)") (closed false)))
    ((Paragraph ((Text "| a | b |"))))
    ((Paragraph ((Text "| a | b |") Break (Text |---))))
    |}]
;;

let%expect_test "html is text" =
  show "<script>alert(1)</script> & <b>x</b>";
  [%expect
    {|
    <div class="markdown">
      <p> <script>alert(1)</script> & <b>x</b> </p>
    </div>
    |}]
;;

let%expect_test "streaming: spans open at the end show as if closed" =
  let partial text =
    print_s [%sexp (Markdown.parse ~partial:true text : Markdown.Block.t list)]
  in
  partial "Here **is";
  partial "Here **is *it";
  partial "Run `make te";
  partial "See [the docs](https://exa";
  partial "See [the do";
  partial "Ends with a marker **";
  partial "Only the end:\n\na *b c\n\nd *e";
  partial "- one\n- two **bo";
  partial "> quoted _it";
  partial "## Head `co";
  partial "2 * 3 *";
  [%expect
    {|
    ((Paragraph ((Text "Here ") (Strong ((Text is))))))
    ((Paragraph ((Text "Here ") (Strong ((Text "is ") (Emph ((Text it))))))))
    ((Paragraph ((Text "Run ") (Code "make te"))))
    ((Paragraph ((Text "See ") (Text "the docs"))))
    ((Paragraph ((Text "See ") (Text "the do"))))
    ((Paragraph ((Text "Ends with a marker "))))
    ((Paragraph ((Text "Only the end:"))) (Paragraph ((Text "a *b c")))
     (Paragraph ((Text "d ") (Emph ((Text e))))))
    ((List (start ()) (tight true)
      (items
       (((checked ()) (blocks ((Paragraph ((Text one))))))
        ((checked ())
         (blocks ((Paragraph ((Text "two ") (Strong ((Text bo))))))))))))
    ((Quote ((Paragraph ((Text "quoted ") (Emph ((Text it))))))))
    ((Heading 2 ((Text "Head ") (Code co))))
    ((Paragraph ((Text "2 * 3 "))))
    |}]
;;

let%expect_test "previews: the first line as plain text" =
  List.iter
    [ "## **Bold** heading\nmore"
    ; "\n\n- [x] a `task` with [a link](https://x.y)"
    ; "> quoted ~~text~~"
    ; "| a | b |\n|---|---|"
    ; "```ocaml\nlet x = 1\n```"
    ; "---\nafter the rule"
    ; ""
    ]
    ~f:(fun text -> print_endline (Markdown.preview text));
  [%expect
    {|
    Bold heading
    a task with a link
    quoted text
    a | b
    let x = 1
    after the rule
    |}]
;;

let%expect_test "line diffs" =
  let diff old new_ =
    print_s [%sexp (Line_diff.diff ~old ~new_ : Line_diff.Line.t list)]
  in
  diff "a\nb\nc" "a\nB\nc\nd";
  diff "" "new";
  diff "old" "";
  [%expect
    {|
    ((Same a) (Removed b) (Added B) (Same c) (Added d))
    ((Added new))
    ((Removed old))
    |}]
;;
