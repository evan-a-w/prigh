open! Core
open! Import

let path = "/cli-signin/grant"

type authenticate =
  token:string option -> user:string option -> (unit, string) Result.t

let text ~status ~reason body =
  let response = Static_site.Response.text ~status ~reason (body ^ "\n") in
  { response with
    headers =
      [ "Content-Type", "text/plain; charset=utf-8"
      ; "Cache-Control", "no-store"
      ]
  }
;;

let header (request : Web_server.Request.t) name =
  Web_server.Request.header request name |> Option.map ~f:String.strip
;;

(* [https://host:port] to [host:port]. *)
let origin_authority origin =
  match String.lsplit2 origin ~on:':' with
  | Some (("http" | "https"), rest) -> String.chop_prefix rest ~prefix:"//"
  | Some _ | None -> None
;;

let same_origin request =
  let fetch_site = header request "Sec-Fetch-Site" in
  let hosts =
    List.filter_map
      [ header request "Host"
      ; Option.map (header request "X-Forwarded-Host") ~f:(fun hosts ->
          String.strip (List.hd_exn (String.split hosts ~on:',')))
      ]
      ~f:Fn.id
  in
  match header request "Origin" |> Option.bind ~f:origin_authority with
  | None -> false
  | Some origin ->
    (match fetch_site with
     | Some "same-origin" -> true
     | Some _ -> false
     | None -> List.mem hosts origin ~equal:String.Caseless.equal)
;;

let json_field json name =
  match json with
  | `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal name with
     | Some (`String s) when not (String.is_empty s) -> Some s
     | _ -> None)
  | _ -> None
;;

let respond
      ~(authenticate : authenticate)
      (request : Web_server.Request.t)
      ~body
  =
  let is_json =
    Option.exists (header request "Content-Type") ~f:(fun ct ->
      String.is_prefix (String.lowercase ct) ~prefix:"application/json")
  in
  if not (String.equal request.meth "POST")
  then text ~status:405 ~reason:"Method Not Allowed" "method not allowed"
  else if not (is_json && same_origin request)
  then
    text
      ~status:403
      ~reason:"Forbidden"
      "only prigh-web's own page may ask for this: open the sign-in link in \
       the browser (if a proxy in front of prigh rewrites Host, have it pass \
       the site's host as Host or X-Forwarded-Host)"
  else (
    match Json.parse body with
    | Error _ ->
      text ~status:400 ~reason:"Bad Request" "expected {\"token\", \"user\"}"
    | Ok json ->
      (match
         authenticate
           ~token:(json_field json "token")
           ~user:(json_field json "user")
       with
       | Error error -> text ~status:401 ~reason:"Unauthorized" error
       | Ok () ->
         (match header request "Cookie" with
          | None | Some "" ->
            text
              ~status:422
              ~reason:"Unprocessable Content"
              "no cookies reached prigh with this request: the program only \
               needs this when a sign-in proxy guards the site, and then the \
               proxy must pass the Cookie header on (nginx and Caddy do by \
               default)"
          | Some cookie ->
            let body = Json.to_string (`Object [ "cookie", `String cookie ]) in
            { status = 200
            ; reason = "OK"
            ; headers =
                [ "Content-Type", "application/json"
                ; "Cache-Control", "no-store"
                ]
            ; content_length = Some (String.length body)
            ; body
            })))
;;

let max_body = 64 * 1024

let handle ~authenticate (request : Web_server.Request.t) ~reader flow =
  let length =
    Option.bind (header request "Content-Length") ~f:Int.of_string_opt
  in
  let response =
    match length with
    | Some n when n >= 0 && n <= max_body ->
      respond ~authenticate request ~body:(Eio.Buf_read.take n reader)
    | (Some _ | None) when not (String.equal request.meth "POST") ->
      respond ~authenticate request ~body:""
    | Some _ | None ->
      text ~status:413 ~reason:"Content Too Large" "the body is too large"
  in
  Web_server.respond flow response
;;

let route ~authenticate (request : Web_server.Request.t) =
  Option.some_if (String.equal request.path path) (handle ~authenticate)
;;
