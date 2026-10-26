open! Core
open! Import

let max_bytes = 5 * 1024 * 1024
let default_timeout_seconds = 30
let max_timeout_seconds = 120
let max_redirects = 10

let spec =
  { Tool_spec.name = "web_fetch"
  ; parallel_safe = true
  ; on_host = true
  ; freeform = None
  ; destructive = Never
  ; description =
      "Fetch a URL (http or https) and return its content as readable text: \
       HTML is converted to markdown with links kept, other text is returned \
       as is, images are attached. Follows redirects. Runs on the machine the \
       tools run on, so localhost is that machine. Long pages are truncated \
       and saved in full to a file you can read."
  ; parameters =
      Tool_args.schema
        ~required:[ "url" ]
        [ "url", `String, "The URL to fetch"
        ; ( "timeout"
          , `Integer
          , sprintf
              "Timeout in seconds (default %d, at most %d)"
              default_timeout_seconds
              max_timeout_seconds )
        ]
  }
;;

let user_agent = sprintf "Mozilla/5.0 (compatible; prigh/%s)" Version.to_string

let headers =
  [ "User-Agent", user_agent
  ; ( "Accept"
    , "text/html,application/xhtml+xml,text/markdown;q=0.9,text/plain;q=0.8,*/*;q=0.5"
    )
  ; "Accept-Encoding", "identity"
  ]
;;

exception Body_limit

module Body = struct
  type t =
    { response : Http_client.Response.t
    ; data : string
    ; cut : bool (** stopped at [max_bytes] *)
    }
end

let fetch_once ~env ~cancel ~timeout url =
  let buf = Buffer.create 65536 in
  let response = ref None in
  let on_chunk chunk =
    let room = max_bytes - Buffer.length buf in
    if String.length chunk > room
    then (
      Buffer.add_string buf (String.prefix chunk room);
      raise Body_limit)
    else Buffer.add_string buf chunk
  in
  let body response ~cut = { Body.response; data = Buffer.contents buf; cut } in
  match
    Http_client.get_stream
      ~env
      ~cancel
      ~timeout
      ~on_response:(fun r -> response := Some r)
      ~url
      ~headers
      ~on_chunk
      ()
  with
  | result -> Result.map result ~f:(body ~cut:false)
  | exception Body_limit -> Ok (body (Option.value_exn !response) ~cut:true)
;;

let is_redirect status =
  List.mem [ 301; 302; 303; 307; 308 ] status ~equal:Int.equal
;;

let resolve ~base location =
  Uri.to_string (Uri.resolve "" (Uri.of_string base) (Uri.of_string location))
;;

(* Follows redirects; returns the final URL and its body. *)
let fetch ~env ~cancel ~timeout url =
  let rec go url ~hops =
    match fetch_once ~env ~cancel ~timeout url with
    | Error e -> Error (`Http e)
    | Ok (body : Body.t) ->
      (match
         ( is_redirect body.response.status
         , Http_client.Response.header body.response "location" )
       with
       | true, Some location ->
         if hops >= max_redirects
         then Error (`Too_many_redirects (resolve ~base:url location))
         else go (resolve ~base:url location) ~hops:(hops + 1)
       | _ -> Ok (url, body))
  in
  go url ~hops:0
;;

module Media = struct
  type t =
    { mime : string
    ; charset : string option
    }

  let of_header header =
    match String.split (Option.value header ~default:"") ~on:';' with
    | [] -> { mime = ""; charset = None }
    | mime :: params ->
      { mime = String.lowercase (String.strip mime)
      ; charset =
          List.find_map params ~f:(fun param ->
            match String.lsplit2 (String.strip param) ~on:'=' with
            | Some (key, value)
              when String.Caseless.equal (String.strip key) "charset" ->
              Some
                (String.lowercase
                   (String.strip
                      ~drop:(fun c -> Char.equal c '"' || Char.is_whitespace c)
                      value))
            | _ -> None)
      }
  ;;

  let is_html t =
    List.mem [ "text/html"; "application/xhtml+xml" ] t.mime ~equal:String.equal
  ;;

  let is_text t =
    String.is_prefix t.mime ~prefix:"text/"
    || List.mem
         [ "application/json"
         ; "application/xml"
         ; "application/javascript"
         ; "application/ecmascript"
         ; "application/x-sh"
         ; "application/toml"
         ; "application/yaml"
         ; "application/x-yaml"
         ; "application/x-ndjson"
         ; "application/sql"
         ; "application/graphql"
         ; "image/svg+xml"
         ]
         t.mime
         ~equal:String.equal
    || String.is_suffix t.mime ~suffix:"+json"
    || String.is_suffix t.mime ~suffix:"+xml"
  ;;
end

let looks_like_html data =
  let head = String.lowercase (String.prefix data 1024) in
  String.is_substring head ~substring:"<html"
  || String.is_substring head ~substring:"<!doctype html"
;;

let looks_binary data = String.mem (String.prefix data 8192) '\000'

(* Latin-1 (and, near enough, Windows-1252) to UTF-8. *)
let latin1_to_utf8 s =
  let b = Buffer.create (String.length s) in
  String.iter s ~f:(fun c ->
    let n = Char.to_int c in
    if n < 0x80
    then Buffer.add_char b c
    else (
      Buffer.add_char b (Char.of_int_exn (0xC0 lor (n lsr 6)));
      Buffer.add_char b (Char.of_int_exn (0x80 lor (n land 0x3F)))));
  Buffer.contents b
;;

let decode (media : Media.t) data =
  match media.charset with
  | Some ("iso-8859-1" | "latin1" | "latin-1" | "windows-1252" | "us-ascii") ->
    latin1_to_utf8 data
  | Some _ | None -> data
;;

let size_text bytes =
  if bytes < 1024
  then sprintf "%d B" bytes
  else if bytes < 1024 * 1024
  then sprintf "%.1f KB" (Float.of_int bytes /. 1024.)
  else sprintf "%.1f MB" (Float.of_int bytes /. 1024. /. 1024.)
;;

let header ~url (body : Body.t) ~mime =
  sprintf
    "%s (HTTP %d, %s, %s%s)"
    url
    body.response.status
    (if String.is_empty mime then "no content type" else mime)
    (size_text (String.length body.data))
    (if body.cut then sprintf ", stopped at %s" (size_text max_bytes) else "")
;;

(* The page's text under the header, cut like other tools' output; the full
   text goes to a file. *)
let text_result (context : Tool.Context.t) ~header ?title text =
  let title =
    Option.value_map title ~default:"" ~f:(fun t -> "Title: " ^ t ^ "\n")
  in
  let shown = Truncate.head text in
  let note =
    if not shown.truncated
    then ""
    else (
      let saved =
        match Tool_bash.save_output context text with
        | path ->
          sprintf
            "the full text is in %s (read it with offset/limit, or grep it)"
            path
        | exception exn ->
          sprintf "saving the full text failed: %s" (Exn.to_string exn)
      in
      sprintf
        "\n\n[truncated: showing %d of %d lines (%s of %s); %s]"
        (Truncate.count_lines shown.text)
        shown.total_lines
        (size_text (String.length shown.text))
        (size_text shown.total_bytes)
        saved)
  in
  Tool.Result.ok (sprintf "%s\n%s\n%s%s" header title shown.text note)
;;

let content (context : Tool.Context.t) ~url (body : Body.t) =
  let media =
    Media.of_header (Http_client.Response.header body.response "content-type")
  in
  let header = header ~url body ~mime:media.mime in
  let data = body.data in
  if Media.is_html media || (String.is_empty media.mime && looks_like_html data)
  then (
    let page =
      Html_text.convert ~base:(Uri.of_string url) (decode media data)
    in
    text_result context ~header ?title:page.title page.text)
  else if Option.is_some (Image.sniff data)
  then
    if body.cut
    then
      Tool.Result.error
        (sprintf
           "%s\nthe image is larger than %s; download it with curl in bash"
           header
           (size_text max_bytes))
    else (
      match Image.load ~env:context.env ~cancel:context.cancel data with
      | Error e ->
        Tool.Result.error (sprintf "%s\n%s" header (Error.to_string_hum e))
      | Ok loaded ->
        Tool.Result.ok
          ~images:[ loaded.image ]
          (String.concat
             ~sep:"\n"
             ((header ^ "\n" ^ sprintf "[image %s]" (Image.describe loaded))
              :: Option.to_list loaded.note)))
  else if
    Media.is_text media
    || (String.is_empty media.mime && not (looks_binary data))
  then text_result context ~header (decode media data)
  else
    Tool.Result.error
      (sprintf
         "%s\n\
          web_fetch only shows text, HTML and images; download it with curl in \
          bash (e.g. curl -sL -o FILE '%s') and use a tool that reads %s"
         header
         url
         (if String.is_empty media.mime then "this format" else media.mime))
;;

let run (context : Tool.Context.t) args =
  let url = String.strip (Tool_args.string args "url") in
  let timeout =
    Option.value
      (Tool_args.int_opt args "timeout")
      ~default:default_timeout_seconds
  in
  if timeout < 1 || timeout > max_timeout_seconds
  then
    raise
      (Tool_args.Invalid
         (sprintf
            "timeout must be between 1 and %d seconds"
            max_timeout_seconds));
  let uri = Uri.of_string url in
  match Uri.scheme uri, Uri.host uri with
  | (None, _ | Some _, _) when not (String.is_substring url ~substring:"://") ->
    Tool.Result.error
      (sprintf
         "%S is not a URL: it must start with http:// or https://, e.g. \
          https://%s"
         url
         url)
  | Some ("http" | "https"), (None | Some "") ->
    Tool.Result.error (sprintf "%S has no host name" url)
  | Some ("http" | "https"), Some _ ->
    (match
       fetch
         ~env:context.env
         ~cancel:context.cancel
         ~timeout:(Time_ns.Span.of_int_sec timeout)
         url
     with
     | Error (`Http Cancelled) -> Tool.Result.error "[cancelled]"
     | Error (`Http (Timed_out | Stalled _)) ->
       Tool.Result.error
         (sprintf
            "fetching %s timed out after %ds; try again with a larger timeout \
             (at most %d)"
            url
            timeout
            max_timeout_seconds)
     | Error (`Http (Connection_failed message | Connection_lost message)) ->
       Tool.Result.error
         (sprintf
            "cannot fetch %s: %s; check the URL, and that its host is \
             reachable from the machine the tools run on"
            url
            message)
     | Error (`Too_many_redirects last) ->
       Tool.Result.error
         (sprintf
            "%s redirected more than %d times (last to %s); fetch the final \
             URL directly, or check it in a browser"
            url
            max_redirects
            last)
     | Ok (final, body) ->
       if body.response.status >= 200 && body.response.status < 300
       then content context ~url:final body
       else (
         let media =
           Media.of_header
             (Http_client.Response.header body.response "content-type")
         in
         let detail =
           if Media.is_html media
           then (Html_text.convert ~base:(Uri.of_string final) body.data).text
           else body.data
         in
         Tool.Result.error
           (sprintf
              "%s\n%s%s"
              (header ~url:final body ~mime:media.mime)
              (match body.response.status with
               | 401 | 403 ->
                 "the server refused access (it may need a login, or block \
                  automated clients)\n"
               | 404 | 410 -> "not found: check the URL\n"
               | 429 -> "rate limited: wait before trying again\n"
               | n when n >= 500 -> "the server failed: try again later\n"
               | _ -> "")
              (Truncate.head
                 ~max_lines:40
                 ~max_bytes:4000
                 (String.strip detail))
                .text)))
  | (None | Some _), _ ->
    Tool.Result.error
      (sprintf
         "web_fetch only fetches http and https URLs, not %s:"
         (Option.value (Uri.scheme uri) ~default:""))
;;

let tool = { Tool.spec; run }
