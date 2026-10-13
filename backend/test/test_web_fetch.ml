open! Core
open! Prigh
open Eio.Std
module Server = Fake_http_server

let convert ?base html =
  let page = Html_text.convert ?base:(Option.map base ~f:Uri.of_string) html in
  Option.iter page.title ~f:(printf "title: %s\n");
  print_endline page.text
;;

let%expect_test "html: headings, paragraphs, lists, links, code, tables, quotes"
  =
  convert
    ~base:"https://example.com/docs/intro.html"
    {|<!DOCTYPE html>
<html><head><title>  Intro &amp; Setup
 </title><style>body { color: red }</style>
<script>var x = "<p>not text</p>";</script></head>
<body>
<nav><a href="/">Home</a> <a href="/docs">Docs</a></nav>
<h1>Getting   started</h1>
<p>Install it with <code>opam install eio</code>, then read
the <a href="guide.html#run">guide</a> or <a href="https://ocaml.org/">https://ocaml.org/</a>.
<!-- a comment <b>hidden</b> --></p>
<ul><li>First <strong>bold</strong> point</li><li>Second, <em>stressed</em>
<ol><li>nested one</li><li>nested two</li></ol></li></ul>
<pre><code>let () =
  print_endline "hi &lt;there&gt;"</code></pre>
<blockquote><p>Quoted text.</p><p>Second paragraph.</p></blockquote>
<table><tr><th>Name</th><th>Value</th></tr><tr><td>a</td><td>1</td></tr></table>
<p>Caf&eacute; &#8212; &#x41;&nbsp;B &unknown; 5 &lt; 6<br>new line<img src="x.png" alt="a chart"></p>
<hr><p>The end. <a href="javascript:void(0)">click</a> <a href="#top">top</a></p>
</body></html>|};
  [%expect
    {|
    title: Intro & Setup
    # Getting started

    Install it with `opam install eio`, then read the [guide](https://example.com/docs/guide.html#run) or https://ocaml.org/.

    - First **bold** point
    - Second, _stressed_
      1. nested one
      2. nested two

    ```
    let () =
      print_endline "hi <there>"
    ```

    > Quoted text.
    >
    > Second paragraph.

    Name | Value
    a | 1

    Café — A B &unknown; 5 < 6
    new line[image: a chart]

    ---

    The end. click top
    |}]
;;

let%expect_test "html: main or article only, unclosed tags, stray <" =
  convert
    {|<html><body><header>Site banner</header><div class="sidebar">Links</div>
<main><h2>Article</h2><p>Body text with a stray < sign and 1 <2 > 0.
<p>Unclosed paragraph<li>orphan item</main><footer>(c) 2026</footer>|};
  convert
    {|<div>menu</div><article><p>Only the article</p></article><div>footer</div>|};
  convert "plain text, no tags";
  convert "";
  [%expect
    {|
    ## Article

    Body text with a stray < sign and 1 <2 > 0.

    Unclosed paragraph
    - orphan item
    Only the article
    plain text, no tags
    |}]
;;

let%expect_test "html: a head never closed, self-closing svg and iframe" =
  convert
    {|<html><head><title>T</title><meta charset="utf-8"><body><p>Body kept</p></body>|};
  convert
    {|<p>Before <svg viewBox="0 0 1 1"/> after</p><iframe src="x"/><p>Still here</p>
<svg><path d="M0"/></svg><p>And here</p></svg></nav><p>Stray closes ignored</p>|};
  [%expect
    {|
    title: T
    Body kept
    Before after

    Still here

    And here

    Stray closes ignored
    |}]
;;

let%expect_test "html: many scripts convert in linear time" =
  let html =
    String.concat
      (List.init 20_000 ~f:(fun i ->
         sprintf "<p>para %d</p><script>var x = %d;</script>" i i))
  in
  let start = Time_ns.now () in
  let page = Html_text.convert html in
  let elapsed = Time_ns.diff (Time_ns.now ()) start in
  print_s
    [%sexp
      (List.length (String.split_lines page.text) : int)
    , (Time_ns.Span.( < ) elapsed (Time_ns.Span.of_sec 2.) : bool)];
  [%expect {| (39999 true) |}]
;;

let with_server f =
  Eio_main.run
  @@ fun env ->
  Switch.run
  @@ fun sw ->
  let dir = Filename_unix.realpath (Filename_unix.temp_dir "prigh-fetch" "") in
  let routes = String.Table.create () in
  let server =
    Server.start ~sw ~env ~handler:(fun request ->
      let path =
        match String.split request.request_line ~on:' ' with
        | _ :: path :: _ -> path
        | _ -> "/"
      in
      match Hashtbl.find routes path with
      | Some reply -> reply
      | None ->
        Server.Reply.simple
          404
          "<html><body><h1>Not Found</h1><p>No such page.</p></body></html>"
          ~headers:[ "Content-Type", "text/html" ])
  in
  let fetch ?timeout url =
    let args =
      `Object
        (("url", `String url)
         :: Option.to_list
              (Option.map timeout ~f:(fun t ->
                 "timeout", `Number (Int.to_string t))))
    in
    let result =
      Tool.execute
        Tool_web_fetch.tool
        (Tool.Context.create ~env ~cwd:dir ~call_id:"call-1" ())
        args
    in
    let mask s =
      String.substr_replace_all
        s
        ~pattern:(Server.url server "")
        ~with_:"http://SERVER"
      |> String.substr_replace_all ~pattern:Filename.temp_dir_name ~with_:"$TMP"
    in
    let text =
      match String.split_lines (mask result.text) with
      | lines when List.length lines > 12 ->
        String.concat
          ~sep:"\n"
          (List.take lines 3
           @ [ sprintf "... (%d lines)" (List.length lines - 6) ]
           @ List.drop lines (List.length lines - 3))
      | lines -> String.concat ~sep:"\n" lines
    in
    let text =
      Re.replace_string
        (Re.compile (Re.Perl.re {|prigh-output-[0-9]+/[^ ]*\.log|}))
        ~by:"prigh-output/<file>.log"
        text
      |> String.split_lines
      |> List.map ~f:(fun line ->
        if String.length line > 200
        then String.prefix line 100 ^ "..."
        else line)
      |> String.concat ~sep:"\n"
    in
    printf "%s%s\n" (if result.is_error then "ERROR " else "") text;
    List.iter result.images ~f:(fun image ->
      printf "[image %s]\n" image.mime_type)
  in
  f ~env ~routes ~fetch ~url:(Server.url server)
;;

let%expect_test "web_fetch: html, text, json, redirects, errors" =
  with_server (fun ~env:_ ~routes ~fetch ~url ->
    let html = [ "Content-Type", "text/html; charset=utf-8" ] in
    Hashtbl.set
      routes
      ~key:"/page"
      ~data:
        (Server.Reply.simple
           200
           ~headers:html
           {|<html><head><title>A page</title></head><body><p>Hello <a href="/other">other</a>.</p></body></html>|});
    Hashtbl.set
      routes
      ~key:"/moved"
      ~data:(Server.Reply.simple 301 "" ~headers:[ "Location", "/page" ]);
    Hashtbl.set
      routes
      ~key:"/abs"
      ~data:(Server.Reply.simple 302 "" ~headers:[ "Location", url "/moved" ]);
    Hashtbl.set
      routes
      ~key:"/loop"
      ~data:(Server.Reply.simple 302 "" ~headers:[ "Location", "/loop" ]);
    Hashtbl.set
      routes
      ~key:"/plain"
      ~data:
        (Server.Reply.simple
           200
           "just text\n"
           ~headers:[ "Content-Type", "text/plain" ]);
    Hashtbl.set
      routes
      ~key:"/json"
      ~data:
        (Server.Reply.simple
           200
           {|{"a":1}|}
           ~headers:[ "Content-Type", "application/json" ]);
    Hashtbl.set
      routes
      ~key:"/latin1"
      ~data:
        (Server.Reply.simple
           200
           "caf\xe9"
           ~headers:[ "Content-Type", "text/plain; charset=ISO-8859-1" ]);
    Hashtbl.set
      routes
      ~key:"/untyped"
      ~data:(Server.Reply.simple 200 "<!doctype html><p>sniffed</p>");
    Hashtbl.set
      routes
      ~key:"/pdf"
      ~data:
        (Server.Reply.simple
           200
           "%PDF-1.4\000\001"
           ~headers:[ "Content-Type", "application/pdf" ]);
    Hashtbl.set
      routes
      ~key:"/png"
      ~data:
        (Server.Reply.simple
           200
           (Image_fixtures.bytes Image_fixtures.png_3x2)
           ~headers:[ "Content-Type", "image/png" ]);
    Hashtbl.set
      routes
      ~key:"/api"
      ~data:
        (Server.Reply.simple
           500
           {|{"error":"boom"}|}
           ~headers:[ "Content-Type", "application/json" ]);
    fetch (url "/page");
    fetch (url "/moved");
    fetch (url "/abs");
    fetch (url "/loop");
    fetch (url "/plain");
    fetch (url "/json");
    fetch (url "/latin1");
    fetch (url "/untyped");
    fetch (url "/pdf");
    fetch (url "/png");
    fetch (url "/missing");
    fetch (url "/api");
    [%expect
      {|
      http://SERVER/page (HTTP 200, text/html, 100 B)
      Title: A page

      Hello [other](http://SERVER/other).
      http://SERVER/page (HTTP 200, text/html, 100 B)
      Title: A page

      Hello [other](http://SERVER/other).
      http://SERVER/page (HTTP 200, text/html, 100 B)
      Title: A page

      Hello [other](http://SERVER/other).
      ERROR http://SERVER/loop redirected more than 10 times (last to http://SERVER/loop); fetch the final URL directly, or check it in a browser
      http://SERVER/plain (HTTP 200, text/plain, 10 B)

      just text
      http://SERVER/json (HTTP 200, application/json, 7 B)

      {"a":1}
      http://SERVER/latin1 (HTTP 200, text/plain, 4 B)

      café
      http://SERVER/untyped (HTTP 200, no content type, 29 B)

      sniffed
      ERROR http://SERVER/pdf (HTTP 200, application/pdf, 10 B)
      web_fetch only shows text, HTML and images; download it with curl in bash (e.g. curl -sL -o FILE 'http://SERVER/pdf') and use a tool that reads application/pdf
      http://SERVER/png (HTTP 200, image/png, 84 B)
      [image image/png, 3x2]
      [image image/png]
      ERROR http://SERVER/missing (HTTP 404, text/html, 64 B)
      not found: check the URL
      # Not Found

      No such page.
      ERROR http://SERVER/api (HTTP 500, application/json, 16 B)
      the server failed: try again later
      {"error":"boom"}
      |}])
;;

let%expect_test
    "web_fetch: bad urls, timeouts, refused connections, size limits"
  =
  with_server (fun ~env:_ ~routes ~fetch ~url ->
    fetch "example.com";
    fetch "localhost:8080/x";
    fetch "ftp://example.com/file";
    fetch "http://";
    fetch ~timeout:0 (url "/");
    fetch ~timeout:500 (url "/");
    Hashtbl.set
      routes
      ~key:"/slow"
      ~data:
        { Server.Reply.status = 200
        ; headers = [ "Content-Type", "text/plain" ]
        ; chunks = [ 0., "start"; 3., "late" ]
        };
    fetch ~timeout:1 (url "/slow");
    fetch "http://127.0.0.1:1/";
    let big =
      String.concat (List.init 3000 ~f:(fun i -> sprintf "line %d\n" i))
    in
    Hashtbl.set
      routes
      ~key:"/long"
      ~data:
        (Server.Reply.simple 200 big ~headers:[ "Content-Type", "text/plain" ]);
    fetch (url "/long");
    let huge = String.make (Tool_web_fetch.max_bytes + 100) 'x' in
    Hashtbl.set
      routes
      ~key:"/huge"
      ~data:
        (Server.Reply.simple 200 huge ~headers:[ "Content-Type", "text/plain" ]);
    fetch (url "/huge");
    [%expect
      {|
      ERROR "example.com" is not a URL: it must start with http:// or https://, e.g. https://example.com
      ERROR "localhost:8080/x" is not a URL: it must start with http:// or https://, e.g. https://localhost:8080/x
      ERROR web_fetch only fetches http and https URLs, not ftp:
      ERROR "http://" has no host name
      ERROR invalid arguments: timeout must be between 1 and 120 seconds
      ERROR invalid arguments: timeout must be between 1 and 120 seconds
      ERROR fetching http://SERVER/slow timed out after 1s; try again with a larger timeout (at most 120)
      ERROR cannot fetch http://127.0.0.1:1/: connection refused; check the URL, and that its host is reachable from the machine the tools run on
      http://SERVER/long (HTTP 200, text/plain, 28.2 KB)

      line 0
      ... (1999 lines)


      [truncated: showing 2000 of 3000 lines (18.4 KB of 28.2 KB); the full text is in $TMP/prigh-output/<file>.log (read it with offset/limit, or grep it)]
      http://SERVER/huge (HTTP 200, text/plain, 5.0 MB, stopped at 5.0 MB)

      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...

      [truncated: showing 1 of 1 lines (50.0 KB of 5.0 MB); the full text is in $TMP/prigh-output/<file>.log (read it with offset/limit, or grep it)]
      |}])
;;
