open! Core
open! Prigh
open Eio.Std
open! Test_web_fetch

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
    Closed_port.with_ (fun port -> fetch (sprintf "http://127.0.0.1:%d/" port));
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
      ERROR cannot fetch http://127.0.0.1:CLOSED/: connection refused; check the URL, and that its host is reachable from the machine the tools run on
      http://SERVER/long (HTTP 200, text/plain, 28.2 KB)

      line 0
      ... (1999 lines)


      [truncated: showing 2000 of 3000 lines (18.4 KB of 28.2 KB); the full text is in $TMP/prigh-output/<file>.log (read it with offset/limit, or grep it)]
      http://SERVER/huge (HTTP 200, text/plain, 5.0 MB, stopped at 5.0 MB)

      xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx...

      [truncated: showing 1 of 1 lines (50.0 KB of 5.0 MB); the full text is in $TMP/prigh-output/<file>.log (read it with offset/limit, or grep it)]
      |}])
;;
