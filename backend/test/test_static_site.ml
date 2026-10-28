open! Core
open! Prigh

let t0 = 1_700_000_000.

let site () =
  let root = Filename_unix.realpath (Filename_unix.temp_dir "prigh-site" "") in
  let write ?(mtime = t0) name data =
    let file = Filename.concat root name in
    Out_channel.write_all file ~data;
    Core_unix.utimes file ~access:mtime ~modif:mtime
  in
  write "index.html" "<html>hi</html>";
  write "app.js" "console.log('hello, hello, hello')";
  write "app.js.gz" "GZ";
  write "app.js.br" "BR!";
  write "old.css" "body {}";
  write ~mtime:(t0 -. 1.) "old.css.gz" "STALE";
  write "new.css" "body {}";
  write ~mtime:(t0 +. 1.) "new.css.gz" "FRESH";
  write ".secret" "no";
  write ".secret.gz" "no";
  write "gone.js.gz" "GZ";
  write ~mtime:1. "nix.js" "built by Nix";
  root
;;

(* The request (path and headers), then the response. *)
let show ?(meth = "GET") ?(headers = []) root path =
  printf
    "%s %s%s\n"
    meth
    path
    (String.concat
       (List.map headers ~f:(fun (k, v) -> sprintf " [%s: %s]" k v)));
  let ({ status; reason; headers; content_length; body }
        : Static_site.Response.t)
    =
    Static_site.respond ~root ~meth ~path ~headers
  in
  printf "  %d %s\n" status reason;
  List.iter headers ~f:(fun (k, v) -> printf "    %s: %s\n" k v);
  Option.iter content_length ~f:(printf "    Content-Length: %d\n");
  if not (String.is_empty body) then printf "    body: %S\n" body
;;

let%expect_test "static site: precompressed siblings by Accept-Encoding" =
  let root = site () in
  show root "/app.js";
  show root "/app.js" ~headers:[ "Accept-Encoding", "gzip, deflate" ];
  show root "/app.js" ~headers:[ "accept-encoding", "gzip, deflate, br, zstd" ];
  show root "/app.js" ~headers:[ "Accept-Encoding", "br;q=0, gzip;q=0.5" ];
  show root "/app.js" ~headers:[ "Accept-Encoding", "gzip;q=0" ];
  show root "/app.js" ~headers:[ "Accept-Encoding", "identity" ];
  show root "/app.js" ~headers:[ "Accept-Encoding", "*" ];
  show root "/app.js" ~headers:[ "Accept-Encoding", "*, br;q=0" ];
  [%expect
    {|
    GET /app.js
      200 OK
        Content-Type: text/javascript; charset=utf-8
        ETag: "046b754f6f8ab27aa6f33611"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 34
        body: "console.log('hello, hello, hello')"
    GET /app.js [Accept-Encoding: gzip, deflate]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        Content-Encoding: gzip
        ETag: "6145351cca86ee15e9b3edb8"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 2
        body: "GZ"
    GET /app.js [accept-encoding: gzip, deflate, br, zstd]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        Content-Encoding: br
        ETag: "3a1f758ad7420f37b1f5bfa7"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 3
        body: "BR!"
    GET /app.js [Accept-Encoding: br;q=0, gzip;q=0.5]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        Content-Encoding: gzip
        ETag: "6145351cca86ee15e9b3edb8"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 2
        body: "GZ"
    GET /app.js [Accept-Encoding: gzip;q=0]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        ETag: "046b754f6f8ab27aa6f33611"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 34
        body: "console.log('hello, hello, hello')"
    GET /app.js [Accept-Encoding: identity]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        ETag: "046b754f6f8ab27aa6f33611"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 34
        body: "console.log('hello, hello, hello')"
    GET /app.js [Accept-Encoding: *]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        Content-Encoding: br
        ETag: "3a1f758ad7420f37b1f5bfa7"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 3
        body: "BR!"
    GET /app.js [Accept-Encoding: *, br;q=0]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        Content-Encoding: gzip
        ETag: "6145351cca86ee15e9b3edb8"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 2
        body: "GZ"
    |}]
;;

let%expect_test "static site: a sibling older than the file is not served" =
  let root = site () in
  let gzip = [ "Accept-Encoding", "gzip" ] in
  show root "/old.css" ~headers:gzip;
  show root "/new.css" ~headers:gzip;
  (* No sibling at all: nothing varies. *)
  show root "/" ~headers:gzip;
  [%expect
    {|
    GET /old.css [Accept-Encoding: gzip]
      200 OK
        Content-Type: text/css; charset=utf-8
        ETag: "62368a1a29259b30bac235c0"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Content-Length: 7
        body: "body {}"
    GET /new.css [Accept-Encoding: gzip]
      200 OK
        Content-Type: text/css; charset=utf-8
        Content-Encoding: gzip
        ETag: "477b172177e8a23b2b2c30ce"
        Last-Modified: Tue, 14 Nov 2023 22:13:21 GMT
        Vary: Accept-Encoding
        Content-Length: 5
        body: "FRESH"
    GET / [Accept-Encoding: gzip]
      200 OK
        Content-Type: text/html; charset=utf-8
        ETag: "beb6d42e2e372ff35ff73af2"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Content-Length: 15
        body: "<html>hi</html>"
    |}]
;;

let%expect_test "static site: HEAD reports the length GET would send" =
  let root = site () in
  show root "/app.js" ~meth:"HEAD";
  show root "/app.js" ~meth:"HEAD" ~headers:[ "Accept-Encoding", "gzip" ];
  [%expect
    {|
    HEAD /app.js
      200 OK
        Content-Type: text/javascript; charset=utf-8
        ETag: "046b754f6f8ab27aa6f33611"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 34
    HEAD /app.js [Accept-Encoding: gzip]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        Content-Encoding: gzip
        ETag: "6145351cca86ee15e9b3edb8"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 2
    |}]
;;

let%expect_test "static site: revalidation answers 304" =
  let root = site () in
  let gzip = "Accept-Encoding", "gzip" in
  let etag headers =
    (Static_site.respond ~root ~meth:"GET" ~path:"/app.js" ~headers).headers
    |> fun h -> List.Assoc.find_exn h ~equal:String.equal "ETag"
  in
  let plain = etag [] in
  let gz = etag [ gzip ] in
  show root "/app.js" ~headers:[ "If-None-Match", plain ];
  show root "/app.js" ~headers:[ "If-None-Match", "W/" ^ plain ];
  show root "/app.js" ~headers:[ "If-None-Match", "\"other\", " ^ gz; gzip ];
  show root "/app.js" ~meth:"HEAD" ~headers:[ "If-None-Match", "*" ];
  (* The identity representation's tag does not validate the gzip one. *)
  show root "/app.js" ~headers:[ "If-None-Match", plain; gzip ];
  let last_modified = Static_site.For_testing.http_date t0 in
  show root "/index.html" ~headers:[ "If-Modified-Since", last_modified ];
  show
    root
    "/index.html"
    ~headers:
      [ "If-Modified-Since", Static_site.For_testing.http_date (t0 -. 1.) ];
  show root "/index.html" ~headers:[ "If-Modified-Since", "yesterday" ];
  (* Nix's mtime 1 is no date: no Last-Modified, and dates do not validate. *)
  show
    root
    "/nix.js"
    ~headers:[ "If-Modified-Since", Static_site.For_testing.http_date t0 ];
  (* If-None-Match wins over If-Modified-Since. *)
  show
    root
    "/index.html"
    ~headers:
      [ "If-None-Match", "\"other\""; "If-Modified-Since", last_modified ];
  [%expect
    {|
    GET /app.js [If-None-Match: "046b754f6f8ab27aa6f33611"]
      304 Not Modified
        ETag: "046b754f6f8ab27aa6f33611"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
    GET /app.js [If-None-Match: W/"046b754f6f8ab27aa6f33611"]
      304 Not Modified
        ETag: "046b754f6f8ab27aa6f33611"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
    GET /app.js [If-None-Match: "other", "6145351cca86ee15e9b3edb8"] [Accept-Encoding: gzip]
      304 Not Modified
        ETag: "6145351cca86ee15e9b3edb8"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
    HEAD /app.js [If-None-Match: *]
      304 Not Modified
        ETag: "046b754f6f8ab27aa6f33611"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
    GET /app.js [If-None-Match: "046b754f6f8ab27aa6f33611"] [Accept-Encoding: gzip]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        Content-Encoding: gzip
        ETag: "6145351cca86ee15e9b3edb8"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 2
        body: "GZ"
    GET /index.html [If-Modified-Since: Tue, 14 Nov 2023 22:13:20 GMT]
      304 Not Modified
        ETag: "beb6d42e2e372ff35ff73af2"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
    GET /index.html [If-Modified-Since: Tue, 14 Nov 2023 22:13:19 GMT]
      200 OK
        Content-Type: text/html; charset=utf-8
        ETag: "beb6d42e2e372ff35ff73af2"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Content-Length: 15
        body: "<html>hi</html>"
    GET /index.html [If-Modified-Since: yesterday]
      200 OK
        Content-Type: text/html; charset=utf-8
        ETag: "beb6d42e2e372ff35ff73af2"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Content-Length: 15
        body: "<html>hi</html>"
    GET /nix.js [If-Modified-Since: Tue, 14 Nov 2023 22:13:20 GMT]
      200 OK
        Content-Type: text/javascript; charset=utf-8
        ETag: "f2a81ba783fec921d1e9cca0"
        Content-Length: 12
        body: "built by Nix"
    GET /index.html [If-None-Match: "other"] [If-Modified-Since: Tue, 14 Nov 2023 22:13:20 GMT]
      200 OK
        Content-Type: text/html; charset=utf-8
        ETag: "beb6d42e2e372ff35ff73af2"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Content-Length: 15
        body: "<html>hi</html>"
    |}]
;;

let%expect_test "static site: 404, 405 and path safety cover the siblings" =
  let root = site () in
  let gzip = [ "Accept-Encoding", "gzip" ] in
  show root "/gone.js" ~headers:gzip;
  show root "/.secret" ~headers:gzip;
  show root "/../app.js" ~headers:gzip;
  show root "/app.js" ~meth:"POST" ~headers:gzip;
  (* A sibling asked for by name is just a file. *)
  show root "/app.js.gz";
  [%expect
    {|
    GET /gone.js [Accept-Encoding: gzip]
      404 Not Found
        Content-Length: 10
        body: "not found\n"
    GET /.secret [Accept-Encoding: gzip]
      404 Not Found
        Content-Length: 10
        body: "not found\n"
    GET /../app.js [Accept-Encoding: gzip]
      404 Not Found
        Content-Length: 10
        body: "not found\n"
    POST /app.js [Accept-Encoding: gzip]
      405 Method Not Allowed
        Content-Length: 19
        body: "method not allowed\n"
    GET /app.js.gz
      200 OK
        Content-Type: application/octet-stream
        ETag: "6145351cca86ee15e9b3edb8"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Content-Length: 2
        body: "GZ"
    |}]
;;

let%expect_test "static site: HTTP dates" =
  List.iter [ 0.; t0; 1_784_123_456.75 ] ~f:(fun time ->
    let date = Static_site.For_testing.http_date time in
    printf
      "%s -> %s\n"
      date
      (Sexp.to_string
         [%sexp (Static_site.For_testing.parse_http_date date : float option)]));
  List.iter
    [ "Sun, 06 Nov 1994 08:49:37 GMT"
    ; "Sunday, 06-Nov-94 08:49:37 GMT"
    ; "Sun, 06 Foo 1994 08:49:37 GMT"
    ; ""
    ]
    ~f:(fun date ->
      printf
        "%S -> %s\n"
        date
        (Sexp.to_string
           [%sexp (Static_site.For_testing.parse_http_date date : float option)]));
  [%expect
    {|
    Thu, 01 Jan 1970 00:00:00 GMT -> (0)
    Tue, 14 Nov 2023 22:13:20 GMT -> (1700000000)
    Wed, 15 Jul 2026 13:50:56 GMT -> (1784123456)
    "Sun, 06 Nov 1994 08:49:37 GMT" -> (784111777)
    "Sunday, 06-Nov-94 08:49:37 GMT" -> ()
    "Sun, 06 Foo 1994 08:49:37 GMT" -> ()
    "" -> ()
    |}]
;;

let%expect_test
    "static site: a same-sized rewrite with the same mtime (Nix) is new"
  =
  let root = site () in
  let file = Filename.concat root "index.html" in
  let etag () =
    let r = Static_site.respond ~root ~meth:"GET" ~path:"/" ~headers:[] in
    List.Assoc.find_exn r.headers ~equal:String.equal "ETag"
  in
  let before = etag () in
  Core_unix.unlink file;
  Out_channel.write_all file ~data:"<html>HI</html>";
  Core_unix.utimes file ~access:t0 ~modif:t0;
  show root "/" ~headers:[ "If-None-Match", before ];
  [%expect
    {|
    GET / [If-None-Match: "beb6d42e2e372ff35ff73af2"]
      200 OK
        Content-Type: text/html; charset=utf-8
        ETag: "14223a6c8f0c047e0f7abdf8"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Content-Length: 15
        body: "<html>HI</html>"
    |}]
;;

let%expect_test "static site: content types" =
  List.iter
    [ "index.html"
    ; "main.bc.js"
    ; "theme.css"
    ; "manifest.webmanifest"
    ; "icon.svg"
    ; "icon-192.png"
    ; "KaTeX_Main-Regular.woff2"
    ; "main.bc.map"
    ; "recursive-OFL.txt"
    ]
    ~f:(fun name ->
      printf "%-26s %s\n" name (Static_site.For_testing.content_type name));
  [%expect
    {|
    index.html                 text/html; charset=utf-8
    main.bc.js                 text/javascript; charset=utf-8
    theme.css                  text/css; charset=utf-8
    manifest.webmanifest       application/manifest+json
    icon.svg                   image/svg+xml
    icon-192.png               image/png
    KaTeX_Main-Regular.woff2   font/woff2
    main.bc.map                application/json
    recursive-OFL.txt          application/octet-stream
    |}]
;;

let%expect_test "static site: the web app manifest, precompressed" =
  let root = site () in
  let write name data =
    let file = Filename.concat root name in
    Out_channel.write_all file ~data;
    Core_unix.utimes file ~access:t0 ~modif:t0
  in
  write "manifest.webmanifest" {|{"name":"prigh"}|};
  write "manifest.webmanifest.br" "BR";
  show root "/manifest.webmanifest";
  show
    root
    "/manifest.webmanifest"
    ~headers:[ "Accept-Encoding", "gzip, deflate, br" ];
  [%expect
    {|
    GET /manifest.webmanifest
      200 OK
        Content-Type: application/manifest+json
        ETag: "5cc883bec9fd80842bfca9dc"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 16
        body: "{\"name\":\"prigh\"}"
    GET /manifest.webmanifest [Accept-Encoding: gzip, deflate, br]
      200 OK
        Content-Type: application/manifest+json
        Content-Encoding: br
        ETag: "bbaf8352442730e92c16c5ea"
        Last-Modified: Tue, 14 Nov 2023 22:13:20 GMT
        Vary: Accept-Encoding
        Content-Length: 2
        body: "BR"
    |}]
;;
