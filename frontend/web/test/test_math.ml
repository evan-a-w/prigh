open! Core
open Prigh_web

let tree ?partial text =
  print_s [%sexp (Markdown.parse ?partial text : Markdown.Block.t list)]
;;

let show text = Render.html (Markdown_view.render text)

let%expect_test "inline and display delimiters" =
  tree "Euler: $e^{i\\pi} + 1 = 0$ and \\(a^2\\), then";
  tree "$$\\int_0^1 x\\,dx$$ inline, and \\[x = 1\\] too";
  [%expect
    {|
    ((Paragraph
      ((Text "Euler: ") (Math (tex "e^{i\\pi} + 1 = 0") (display false))
       (Text " and ") (Math (tex a^2) (display false)) (Text ", then"))))
    ((Paragraph
      ((Math (tex "\\int_0^1 x\\,dx") (display true)) (Text " inline, and ")
       (Math (tex "x = 1") (display true)) (Text " too"))))
    |}]
;;

let%expect_test "dollar amounts and shell variables stay text" =
  List.iter
    [ "it costs $5 and $10"
    ; "$x$"
    ; "$ 5 $"
    ; "set $HOME and $PATH, or $HOME/$PATH"
    ; "echo \"$HOME\" is $HOME$"
    ; "US$3 or US$3.50$"
    ; "between $5 and $6$ items"
    ; "$x $"
    ; "a $5$10 tip"
    ; "escaped \\$x\\$ and \\$5"
    ; "$a\\$b$"
    ; "$$ $$ and $$"
    ]
    ~f:(fun text -> tree text);
  [%expect
    {|
    ((Paragraph ((Text "it costs $5 and $10"))))
    ((Paragraph ((Math (tex x) (display false)))))
    ((Paragraph ((Text "$ 5 $"))))
    ((Paragraph ((Text "set $HOME and $PATH, or $HOME/$PATH"))))
    ((Paragraph ((Text "echo \"$HOME\" is ") (Math (tex HOME) (display false)))))
    ((Paragraph ((Text "US$3 or US$3.50$"))))
    ((Paragraph
      ((Text "between $5 and ") (Math (tex 6) (display false)) (Text " items"))))
    ((Paragraph ((Text "$x $"))))
    ((Paragraph ((Text "a $5$10 tip"))))
    ((Paragraph ((Text "escaped $x$ and $5"))))
    ((Paragraph ((Math (tex "a\\$b") (display false)))))
    ((Paragraph ((Text "$$ ") (Math (tex " and ") (display true)))))
    |}]
;;

let%expect_test "never in code" =
  tree "`$x$` and `\\(y\\)` but $z$";
  tree "```\n$$\nx\n$$\n```";
  [%expect
    {|
    ((Paragraph
      ((Code $x$) (Text " and ") (Code "\\(y\\)") (Text " but ")
       (Math (tex z) (display false)))))
    ((Code (lang "") (text  "$$\
                           \nx\
                           \n$$") (closed true)))
    |}]
;;

let%expect_test "math hides markdown: emphasis, links and code inside are TeX" =
  tree "$a * b * c$ and *$x_1$ is _not_ $y_2$* and [$z$](https://z.org)";
  tree "$`x`$ and `$` then $y$";
  tree "**bold $a**b$**";
  [%expect
    {|
    ((Paragraph
      ((Math (tex "a * b * c") (display false)) (Text " and ")
       (Emph
        ((Math (tex x_1) (display false)) (Text " is ") (Emph ((Text not)))
         (Text " ") (Math (tex y_2) (display false))))
       (Text " and ")
       (Link (href https://z.org) (children ((Math (tex z) (display false))))))))
    ((Paragraph
      ((Math (tex `x`) (display false)) (Text " and ") (Code $) (Text " then ")
       (Math (tex y) (display false)))))
    ((Paragraph ((Strong ((Text "bold ") (Math (tex a**b) (display false)))))))
    |}]
;;

let%expect_test "adjacent and nested" =
  tree "$a$$b$ $a$,$b$ $$x$$$$y$$ \\(\\$\\)";
  tree "$\\text{$x$}$";
  [%expect
    {|
    ((Paragraph
      ((Math (tex a) (display false)) (Math (tex b) (display false)) (Text " ")
       (Math (tex a) (display false)) (Text ,) (Math (tex b) (display false))
       (Text " ") (Math (tex x) (display true)) (Math (tex y) (display true))
       (Text " ") (Math (tex "\\$") (display false)))))
    ((Paragraph ((Text "$\\text{") (Math (tex x) (display false)) (Text }$))))
    |}]
;;

let%expect_test "display math on lines of its own" =
  tree
    "Before\n\
     $$\n\
     \\begin{pmatrix}\n\
     a & b \\\\\n\
     c & d\n\
     \\end{pmatrix}\n\
     $$\n\
     after.";
  tree "\\[\n- x\n\n# y\n\\]";
  tree "$$ x = 1 $$";
  tree "```math\n\\frac{1}{2}\n```";
  [%expect
    {|
    ((Paragraph ((Text Before)))
     (Math  "\\begin{pmatrix}\
           \na & b \\\\\
           \nc & d\
           \n\\end{pmatrix}")
     (Paragraph ((Text after.))))
    ((Math  "- x\
           \n\
           \n# y"))
    ((Math "x = 1"))
    ((Code (lang math) (text "\\frac{1}{2}") (closed true)))
    |}]
;;

let%expect_test "streaming: math left open is text, nothing after it is lost" =
  List.iter
    [ "Price: $x^2 + "
    ; "Then \\(a + "
    ; "$$\n\\sum_i"
    ; "$$\n\\sum_i\n\n- a *list* after"
    ; "$$\n\\sum_i\n\n- a *list* after\n$$"
    ; "**bold $x"
    ]
    ~f:(fun text -> tree ~partial:true text);
  [%expect
    {|
    ((Paragraph ((Text "Price: $x^2 +"))))
    ((Paragraph ((Text "Then (a +"))))
    ((Paragraph ((Text $$) Break (Text "\\sum_i"))))
    ((Paragraph ((Text $$) Break (Text "\\sum_i")))
     (List (start ()) (tight true)
      (items
       (((checked ())
         (blocks
          ((Paragraph ((Text "a ") (Emph ((Text list))) (Text " after"))))))))))
    ((Math  "\\sum_i\
           \n\
           \n- a *list* after"))
    ((Paragraph ((Strong ((Text "bold $x"))))))
    |}]
;;

let%expect_test "streaming: blocks are not cut inside display math" =
  let text =
    "Intro.\n\n$$\na\n\nb\n$$\n\nAfter.\n\n```math\nx\n\ny\n```\n\nEnd"
  in
  List.iter (Markdown.block_starts text) ~f:(fun i ->
    print_endline
      (String.escaped (String.prefix (String.drop_prefix text i) 10)));
  [%expect
    {|
    $$\na\n\nb\n$$
    After.\n\n``
    ```math\nx\n
    End
    |}];
  for len = 0 to String.length text do
    let prefix = String.prefix text len in
    let rec parts pos = function
      | [] -> [ String.drop_prefix prefix pos ]
      | start :: rest ->
        String.sub prefix ~pos ~len:(start - pos) :: parts start rest
    in
    let pieces = parts 0 (Markdown.block_starts prefix) in
    let separately = List.concat_map pieces ~f:(fun p -> Markdown.parse p) in
    if not ([%equal: Markdown.Block.t list] separately (Markdown.parse prefix))
    then print_s [%message "differs" (prefix : string)]
  done;
  [%expect {| |}]
;;

let%expect_test "rendered: placeholders KaTeX draws into" =
  show "Inline $x^2$ and\n\n$$\n\\frac{a}{b}\n$$\n\n```math\ny\n```";
  [%expect
    {|
    <div class="markdown">
      <div class="md-part">
        <p dir="auto">
          Inline
          <span class="math math-inline"> x^2 </span>
           and
        </p>
      </div>
      <div class="md-part">
        <span class="math math-display"> \frac{a}{b} </span>
      </div>
      <div class="md-part">
        <span class="math math-display"> y </span>
      </div>
    </div>
    |}]
;;

let%expect_test "previews show the TeX" =
  print_endline (Markdown.preview "$$\n\\frac{a}{b}\n$$\n\nmore");
  print_endline (Markdown.preview "Area $\\pi r^2$ here");
  [%expect
    {|
    \frac{a}{b}
    Area \pi r^2 here
    |}]
;;
