open! Core
open! Import

let handles ~path =
  String.equal path "/files/download" || String.equal path "/files/upload"
;;

let content_type name =
  match
    String.lowercase
      (Filename.split_extension name |> snd |> Option.value ~default:"")
  with
  | "txt" | "md" | "ml" | "mli" | "c" | "h" | "py" | "sh" | "log" | "csv" ->
    "text/plain; charset=utf-8"
  | "json" -> "application/json"
  | "pdf" -> "application/pdf"
  | "png" -> "image/png"
  | "jpg" | "jpeg" -> "image/jpeg"
  | "gif" -> "image/gif"
  | "webp" -> "image/webp"
  | "zip" -> "application/zip"
  | "gz" | "tgz" -> "application/gzip"
  | "tar" -> "application/x-tar"
  | _ -> "application/octet-stream"
;;

(* RFC 6266: a plain fallback and the UTF-8 name. *)
let content_disposition ~inline name =
  let plain =
    String.map name ~f:(fun c ->
      if
        Char.to_int c < 0x20
        || Char.to_int c >= 0x7f
        || Char.equal c '"'
        || Char.equal c '\\'
      then '_'
      else c)
  in
  let encoded =
    String.concat_map name ~f:(fun c ->
      if Char.is_alphanum c || String.mem "!#$&+-.^_`|~" c
      then String.of_char c
      else sprintf "%%%02X" (Char.to_int c))
  in
  sprintf
    "%s; filename=\"%s\"; filename*=UTF-8''%s"
    (if inline then "inline" else "attachment")
    plain
    encoded
;;

let common_headers =
  [ "Connection", "close"
  ; "Cache-Control", "no-store"
  ; "Access-Control-Allow-Origin", "*"
  ; "X-Content-Type-Options", "nosniff"
  ]
;;

let head ~status ~reason headers =
  sprintf
    "HTTP/1.1 %d %s\r\n%s\r\n"
    status
    reason
    (String.concat
       (List.map (headers @ common_headers) ~f:(fun (k, v) ->
          sprintf "%s: %s\r\n" k v)))
;;

let respond
      flow
      ~status
      ~reason
      ?(content_type = "text/plain; charset=utf-8")
      body
  =
  Eio.Flow.copy_string
    (head
       ~status
       ~reason
       [ "Content-Type", content_type
       ; "Content-Length", Int.to_string (String.length body)
       ]
     ^ body)
    flow
;;

let respond_json flow ~status ~reason json =
  respond
    flow
    ~status
    ~reason
    ~content_type:"application/json"
    (Json.to_string json)
;;

let error_json flow ~status ~reason message =
  respond_json flow ~status ~reason (`Object [ "error", `String message ])
;;

let expired =
  "this link has expired or was used already: start the transfer again from \
   the file browser"
;;

let ticket query ~upload =
  match List.Assoc.find query ~equal:String.equal "ticket" with
  | None -> Error `Expired
  | Some id -> File_tickets.take ~upload id
;;

let int_member json key =
  match Json.member key json with
  | Some (`Number n) -> Int.of_string_opt n
  | _ -> None
;;

let download flow (ticket : File_tickets.Ticket.t) =
  let agent = ticket.agent in
  let path, archive, inline, mime =
    match ticket.kind with
    | Download { path } -> path, false, false, content_type path
    | Inline { path; mime } -> path, false, true, mime
    | Archive { path } -> path, true, false, "application/gzip"
    | Upload { path; _ } -> path, false, false, "application/octet-stream"
  in
  let length =
    if archive
    then Ok None
    else
      Or_error.map
        (File_ops.call agent Host_files.stat_op [ "path", `String path ])
        ~f:(fun stat -> int_member stat "size")
  in
  (* Files such as /proc's announce no size: send them as they come. *)
  let length = Or_error.map length ~f:(Option.filter ~f:(fun n -> n > 0)) in
  match length with
  | Error e ->
    respond flow ~status:404 ~reason:"Not Found" (Error.to_string_hum e ^ "\n")
  | Ok length ->
    (* Without a length the body is chunked, so that a transfer that fails
       midway (no last chunk) does not look complete. *)
    let chunked = Option.is_none length in
    let started = ref false in
    let left = ref (Option.value length ~default:Int.max_value) in
    let start () =
      if not !started
      then (
        started := true;
        Eio.Flow.copy_string
          (head
             ~status:200
             ~reason:"OK"
             ([ "Content-Type", mime
              ; ( "Content-Disposition"
                , content_disposition
                    ~inline
                    (Filename.basename path ^ if archive then ".tar.gz" else "")
                )
              ; "Content-Security-Policy", "sandbox; default-src 'none'"
              ]
              @
              match length with
              | Some n -> [ "Content-Length", Int.to_string n ]
              | None -> [ "Transfer-Encoding", "chunked" ]))
          flow)
    in
    let write raw =
      start ();
      (* A file that grew meanwhile is cut at the length announced, and the
         transfer stopped there. *)
      let cut = String.length raw > !left in
      let raw = String.prefix raw !left in
      left := !left - String.length raw;
      if not (String.is_empty raw)
      then
        Eio.Flow.copy_string
          (if chunked
           then sprintf "%x\r\n%s\r\n" (String.length raw) raw
           else raw)
          flow;
      if cut then raise Exit
    in
    (match File_ops.download agent ~path ~archive ~write with
     | Ok _ ->
       start ();
       if chunked then Eio.Flow.copy_string "0\r\n\r\n" flow
     | Error e when not !started ->
       respond
         flow
         ~status:502
         ~reason:"Bad Gateway"
         (Error.to_string_hum e ^ "\n")
     | Error _ -> ())
;;

let header headers name =
  List.Assoc.find headers ~equal:String.Caseless.equal name
;;

(* Reads what is left of the body so that the browser sees our answer rather
   than a reset connection; a large rest is not worth it. *)
let drain reader ~left =
  if left <= 16 * 1024 * 1024
  then (
    try Eio.Buf_read.skip left reader with
    | _ -> ())
;;

let upload flow ~headers ~reader (ticket : File_tickets.Ticket.t) =
  match ticket.kind with
  | Download _ | Inline _ | Archive _ ->
    (* [File_tickets.take ~upload:true] returns only upload tickets *)
    error_json flow ~status:400 ~reason:"Bad Request" "this is a download link"
  | Upload { path; conflict; size; make_dir } ->
    (match
       Option.bind (header headers "Content-Length") ~f:Int.of_string_opt
     with
     | None ->
       error_json
         flow
         ~status:411
         ~reason:"Length Required"
         "send the file with a Content-Length"
     | Some length when length > File_ops.max_upload ->
       error_json
         flow
         ~status:413
         ~reason:"Content Too Large"
         (sprintf
            "the file is larger than %d bytes: copy it to the host another way"
            File_ops.max_upload)
     | Some length when length <> size ->
       drain reader ~left:length;
       error_json
         flow
         ~status:400
         ~reason:"Bad Request"
         (sprintf
            "the upload was announced as %d bytes but has %d: try again"
            size
            length)
     | Some length ->
       Option.iter (header headers "Expect") ~f:(fun expect ->
         if String.Caseless.equal (String.strip expect) "100-continue"
         then Eio.Flow.copy_string "HTTP/1.1 100 Continue\r\n\r\n" flow);
       let left = ref length in
       let read n =
         let n = Int.min n !left in
         if n = 0
         then ""
         else (
           let data = Eio.Buf_read.take n reader in
           left := !left - n;
           data)
       in
       (match
          File_ops.upload ticket.agent ~path ~conflict ~size ~make_dir ~read ()
        with
        | Ok json -> respond_json flow ~status:200 ~reason:"OK" json
        | Error e ->
          let message = Error.to_string_hum e in
          drain reader ~left:!left;
          if String.is_substring message ~substring:"already exists"
          then error_json flow ~status:409 ~reason:"Conflict" message
          else error_json flow ~status:502 ~reason:"Bad Gateway" message))
;;

let serve ~meth ~path ~query ~headers ~reader flow =
  match meth with
  | "OPTIONS" ->
    Eio.Flow.copy_string
      (head
         ~status:204
         ~reason:"No Content"
         [ "Access-Control-Allow-Methods", "GET, PUT, POST, OPTIONS"
         ; "Access-Control-Allow-Headers", "Content-Type"
         ; "Access-Control-Max-Age", "600"
         ; "Content-Length", "0"
         ])
      flow
  | _ ->
    (match path, meth with
     | "/files/download", "GET" ->
       (match ticket query ~upload:false with
        | Error `Expired ->
          respond flow ~status:410 ~reason:"Gone" (expired ^ "\n")
        | Error `Wrong_endpoint ->
          respond
            flow
            ~status:400
            ~reason:"Bad Request"
            "this is an upload link\n"
        | Ok ticket -> download flow ticket)
     | "/files/upload", ("PUT" | "POST") ->
       (match ticket query ~upload:true with
        | Error `Expired -> error_json flow ~status:410 ~reason:"Gone" expired
        | Error `Wrong_endpoint ->
          error_json
            flow
            ~status:400
            ~reason:"Bad Request"
            "this is a download link"
        | Ok ticket -> upload flow ~headers ~reader ticket)
     | _ ->
       respond
         flow
         ~status:405
         ~reason:"Method Not Allowed"
         "GET /files/download or PUT /files/upload\n")
;;

module For_testing = struct
  let content_disposition = content_disposition
  let content_type = content_type
end
