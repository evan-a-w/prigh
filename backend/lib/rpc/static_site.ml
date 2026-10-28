open! Core
open! Import

module Response = struct
  type t =
    { status : int
    ; reason : string
    ; headers : (string * string) list
    ; content_length : int option
    ; body : string
    }
  [@@deriving sexp_of]

  let text ~status ~reason body =
    { status
    ; reason
    ; headers = []
    ; content_length = Some (String.length body)
    ; body
    }
  ;;
end

let content_type path =
  match
    String.lowercase
      (Filename.split_extension path |> snd |> Option.value ~default:"")
  with
  | "html" -> "text/html; charset=utf-8"
  | "js" -> "text/javascript; charset=utf-8"
  | "css" -> "text/css; charset=utf-8"
  | "json" -> "application/json"
  | "webmanifest" -> "application/manifest+json"
  | "svg" -> "image/svg+xml"
  | "png" -> "image/png"
  | "ico" -> "image/x-icon"
  | "woff2" -> "font/woff2"
  | "map" -> "application/json"
  | _ -> "application/octet-stream"
;;

(* Only plain relative components: no [..], no absolute paths, no hidden
   files. *)
let safe_relative path =
  let parts =
    List.filter (String.split path ~on:'/') ~f:(Fn.non String.is_empty)
  in
  if
    List.exists parts ~f:(fun part ->
      String.equal part ".." || String.is_prefix part ~prefix:".")
  then None
  else Some (String.concat parts ~sep:"/")
;;

let days = [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let months =
  [| "Jan"
   ; "Feb"
   ; "Mar"
   ; "Apr"
   ; "May"
   ; "Jun"
   ; "Jul"
   ; "Aug"
   ; "Sep"
   ; "Oct"
   ; "Nov"
   ; "Dec"
  |]
;;

let http_date time =
  let tm = Core_unix.gmtime time in
  sprintf
    "%s, %02d %s %04d %02d:%02d:%02d GMT"
    days.(tm.tm_wday)
    tm.tm_mday
    months.(tm.tm_mon)
    (tm.tm_year + 1900)
    tm.tm_hour
    tm.tm_min
    tm.tm_sec
;;

(* The IMF-fixdate browsers send back, e.g. "Sun, 06 Nov 1994 08:49:37 GMT". *)
let parse_http_date date =
  match String.split (String.strip date) ~on:' ' with
  | [ _day; mday; month; year; time; "GMT" ] ->
    (match
       ( Int.of_string_opt mday
       , Array.findi months ~f:(fun _ m -> String.equal m month)
       , Int.of_string_opt year
       , List.map (String.split time ~on:':') ~f:Int.of_string_opt )
     with
     | Some mday, Some (mon, _), Some year, [ Some hour; Some min; Some sec ] ->
       (try
          Some
            (Core_unix.timegm
               { tm_sec = sec
               ; tm_min = min
               ; tm_hour = hour
               ; tm_mday = mday
               ; tm_mon = mon
               ; tm_year = year - 1900
               ; tm_wday = 0
               ; tm_yday = 0
               ; tm_isdst = false
               })
        with
        | _ -> None)
     | _ -> None)
  | _ -> None
;;

(* The codings [Accept-Encoding] allows (q > 0), e.g. "gzip, br;q=0". *)
let accepts ~headers coding =
  match
    List.Assoc.find headers ~equal:String.Caseless.equal "Accept-Encoding"
  with
  | None -> false
  | Some value ->
    let q_of name =
      List.find_map (String.split value ~on:',') ~f:(fun item ->
        match String.split item ~on:';' |> List.map ~f:String.strip with
        | [] -> None
        | token :: params when String.Caseless.equal token name ->
          let q =
            List.find_map params ~f:(fun p ->
              match String.lsplit2 p ~on:'=' with
              | Some (k, v) when String.Caseless.equal (String.strip k) "q" ->
                Float.of_string_opt (String.strip v)
              | _ -> None)
          in
          Some (Option.value q ~default:1.)
        | _ -> None)
    in
    (match q_of coding with
     | Some q -> Float.(q > 0.)
     | None ->
       Option.value_map (q_of "*") ~default:false ~f:(fun q -> Float.(q > 0.)))
;;

module Representation = struct
  type t =
    { file : string
    ; encoding : string option
    ; size : int
    ; mtime : float
    ; inode : int
    ; ctime : float
    }

  let stat ~file ~encoding =
    match Core_unix.stat file with
    | { st_kind = S_REG; st_size; st_mtime; st_ino; st_ctime; _ } ->
      Some
        { file
        ; encoding
        ; size = Int64.to_int_exn st_size
        ; mtime = st_mtime
        ; inode = st_ino
        ; ctime = st_ctime
        }
    | _ | (exception Core_unix.Unix_error _) -> None
  ;;

  (* By content, not size and mtime: Nix gives every file the mtime 1, so a
     new build's same-sized file would otherwise look unchanged. Hashed once
     per file version (ctime too: a rewrite may keep the size, mtime and
     inode). *)
  let etags : (int * float * int * float * string) String.Table.t =
    String.Table.create ()
  ;;

  let etag t =
    match Hashtbl.find etags t.file with
    | Some (size, mtime, inode, ctime, etag)
      when size = t.size
           && Float.equal mtime t.mtime
           && inode = t.inode
           && Float.equal ctime t.ctime -> etag
    | _ ->
      let etag =
        sprintf
          "\"%s\""
          (String.prefix
             (Digestif.SHA256.to_hex
                (Digestif.SHA256.digest_string (In_channel.read_all t.file)))
             24)
      in
      Hashtbl.set
        etags
        ~key:t.file
        ~data:(t.size, t.mtime, t.inode, t.ctime, etag);
      etag
  ;;
end

(* Precompressed siblings, best first. *)
let codings = [ "br", ".br"; "gzip", ".gz" ]

let fresh_siblings (original : Representation.t) =
  List.filter_map codings ~f:(fun (coding, suffix) ->
    Representation.stat ~file:(original.file ^ suffix) ~encoding:(Some coding)
    |> Option.filter ~f:(fun (r : Representation.t) ->
      Float.(r.mtime >= original.mtime)))
;;

(* Nix sets every mtime to 1, which says nothing. *)
let last_modified (r : Representation.t) =
  Option.some_if Float.(r.mtime > 1.) (http_date r.mtime)
;;

let not_modified ~headers (r : Representation.t) =
  let header name = List.Assoc.find headers ~equal:String.Caseless.equal name in
  match header "If-None-Match", header "If-Modified-Since" with
  | Some tags, _ ->
    let etag = Representation.etag r in
    List.exists (String.split tags ~on:',') ~f:(fun tag ->
      let tag = String.strip tag in
      String.equal tag "*"
      || String.equal (String.chop_prefix_if_exists tag ~prefix:"W/") etag)
  | None, Some date when Option.is_some (last_modified r) ->
    Option.value_map (parse_http_date date) ~default:false ~f:(fun since ->
      Float.(Float.round_down r.mtime <= since))
  | None, _ -> false
;;

let respond ~root ~meth ~path ~headers =
  let not_found () =
    Response.text ~status:404 ~reason:"Not Found" "not found\n"
  in
  if not (String.equal meth "GET" || String.equal meth "HEAD")
  then
    Response.text
      ~status:405
      ~reason:"Method Not Allowed"
      "method not allowed\n"
  else (
    let path = if String.equal path "/" then "/index.html" else path in
    match safe_relative path with
    | None -> not_found ()
    | Some rel ->
      let file = Filename.concat root rel in
      (match Representation.stat ~file ~encoding:None with
       | None -> not_found ()
       | Some original ->
         let siblings = fresh_siblings original in
         let (chosen : Representation.t) =
           List.find siblings ~f:(fun r ->
             accepts ~headers (Option.value_exn r.encoding))
           |> Option.value ~default:original
         in
         let headers' =
           (("ETag", Representation.etag chosen)
            :: Option.to_list
                 (Option.map (last_modified chosen) ~f:(fun d ->
                    "Last-Modified", d)))
           @
           if List.is_empty siblings then [] else [ "Vary", "Accept-Encoding" ]
         in
         if not_modified ~headers chosen
         then
           { status = 304
           ; reason = "Not Modified"
           ; headers = headers'
           ; content_length = None
           ; body = ""
           }
         else (
           let body =
             if String.equal meth "HEAD"
             then ""
             else In_channel.read_all chosen.file
           in
           { status = 200
           ; reason = "OK"
           ; headers =
               ("Content-Type", content_type file)
               :: Option.value_map
                    chosen.encoding
                    ~default:headers'
                    ~f:(fun e -> ("Content-Encoding", e) :: headers')
           ; content_length =
               Some
                 (if String.equal meth "HEAD"
                  then chosen.size
                  else String.length body)
           ; body
           })))
;;

module For_testing = struct
  let safe_relative = safe_relative
  let content_type = content_type
  let http_date = http_date
  let parse_http_date = parse_http_date
end
