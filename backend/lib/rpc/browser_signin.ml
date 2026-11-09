open! Core
open! Import

module Program = struct
  type t =
    | Tui
    | Tool_host
    | Web
    | Desktop
  [@@deriving sexp_of, enumerate]

  let to_string = function
    | Tui -> "tui"
    | Tool_host -> "tool-host"
    | Web -> "web"
    | Desktop -> "desktop"
  ;;

  let of_string s = List.find all ~f:(fun t -> String.equal (to_string t) s)

  let name = function
    | Tui -> "prigh-tui"
    | Tool_host -> "prigh tool-host"
    | Web -> "prigh web"
    | Desktop -> "prigh desktop"
  ;;
end

module Grant = Browser_signin_grant

module Paste = struct
  type t =
    { secret : string option
    ; from : string
    ; read_line : unit -> string option
    }

  let create ?secret ~from ~read_line () = { secret; from; read_line }

  let stdin ~env =
    let reader =
      Eio.Buf_read.of_flow ~max_size:(64 * 1024) (Eio.Stdenv.stdin env)
    in
    { secret = None
    ; from = Core_unix.gethostname ()
    ; read_line =
        (fun () ->
          match Eio.Buf_read.line reader with
          | line -> Some line
          | exception (End_of_file | Eio.Buf_read.Buffer_limit_exceeded) -> None)
    }
  ;;

  let wanted ~flag ~getenv =
    let set name = Option.exists (getenv name) ~f:(Fn.non String.is_empty) in
    flag
    ||
    match Option.map (getenv "PRIGH_SIGNIN_PASTE") ~f:String.strip with
    | Some "1" -> true
    | Some "0" -> false
    | Some _ | None -> set "SSH_CONNECTION" || set "SSH_TTY"
  ;;

  let lifetime = Time_ns.Span.of_min 30.
end

let url ?paste remote ~program ~port ~state =
  sprintf
    "%s#cli-signin=%d.%s&program=%s%s"
    (Backend_address.web_url remote)
    port
    state
    (Program.to_string program)
    (match paste with
     | None -> ""
     | Some (key, from) ->
       sprintf
         "&paste=%s&from=%s"
         (Paste_code.public_key key)
         (Uri.pct_encode ~component:`Query_value from))
;;

let digest s = Digestif.SHA256.(to_raw_string (digest_string s))

(* Compared by digest, so that the time taken says nothing about the state. *)
let matches ~state given = String.equal (digest state) (digest given)

let page ~status ~reason ~title text =
  let body =
    sprintf
      "<!doctype html>\n\
       <html lang=\"en\"><head><meta charset=\"utf-8\">\n\
       <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">\n\
       <title>prigh: %s</title></head>\n\
       <body style=\"font: 16px/1.5 system-ui, sans-serif; max-width: 34em; \
       margin: 3em auto; padding: 0 1em\">\n\
       <h1 style=\"font-size: 1.4em\">%s</h1>\n\
       <p>%s</p>\n\
       </body></html>\n"
      title
      title
      text
  in
  { Static_site.Response.status
  ; reason
  ; headers =
      [ "Content-Type", "text/html; charset=utf-8"
      ; "Cache-Control", "no-store"
      ; ( "Content-Security-Policy"
        , "default-src 'none'; style-src 'unsafe-inline'" )
      ; "Referrer-Policy", "no-referrer"
      ]
  ; content_length = Some (String.length body)
  ; body
  }
;;

let max_body = 64 * 1024

let form_body (request : Web_server.Request.t) ~reader =
  match
    Option.bind
      (Web_server.Request.header request "Content-Length")
      ~f:(fun n -> Int.of_string_opt (String.strip n))
  with
  | Some n when n >= 0 && n <= max_body ->
    Some
      (Uri.query_of_encoded (Eio.Buf_read.take n reader)
       |> List.map ~f:(fun (name, values) ->
         name, String.concat ~sep:"," values))
  | Some _ | None -> None
;;

let answer ~state ~program ~host (request : Web_server.Request.t) ~reader =
  let name = Program.name program in
  let elsewhere =
    sprintf
      "This address only takes a sign-in from prigh-web's page: open the link \
       %s printed."
      name
  in
  if not (String.equal request.path "/callback")
  then
    `Continue (page ~status:404 ~reason:"Not Found" ~title:"Not here" elsewhere)
  else if not (String.equal request.meth "POST")
  then
    `Continue
      (page
         ~status:405
         ~reason:"Method Not Allowed"
         ~title:"Not here"
         elsewhere)
  else (
    let fields = Option.value (form_body request ~reader) ~default:[] in
    let field name =
      List.Assoc.find fields ~equal:String.equal name
      |> Option.filter ~f:(Fn.non String.is_empty)
    in
    match field "state" with
    | Some given when matches ~state given ->
      if Option.is_some (field "cancel")
      then
        `Done
          ( page
              ~status:200
              ~reason:"OK"
              ~title:"Not signed in"
              (sprintf
                 "You cancelled: %s was not signed in. You can close this tab."
                 name)
          , Error "the sign-in was cancelled in the browser" )
      else (
        match field "cookie" with
        | None ->
          `Done
            ( page
                ~status:400
                ~reason:"Bad Request"
                ~title:"Not signed in"
                (sprintf
                   "The page sent no sign-in cookie, so %s was not signed in: \
                    start the sign-in again from %s."
                   name
                   name)
            , Error "the page sent no sign-in cookie" )
        | Some proxy_cookie ->
          `Done
            ( page
                ~status:200
                ~reason:"OK"
                ~title:"Signed in"
                (sprintf
                   "%s on this computer is signed in to %s. You can close this \
                    tab."
                   name
                   host)
            , Ok
                { Grant.proxy_cookie
                ; token = field "token"
                ; user = field "user"
                } ))
    | Some _ | None ->
      `Continue
        (page
           ~status:403
           ~reason:"Forbidden"
           ~title:"Not this sign-in"
           (sprintf
              "%s is not waiting for this sign-in (a link from an earlier \
               one?): start the sign-in again from %s."
              name
              name)))
;;

let random_state () =
  Mirage_crypto_rng_unix.use_default ();
  Ohex.encode (Mirage_crypto_rng.generate 16)
;;

(* Pasted codes, one a line, until one opens or the sign-in expires; once
   the input ends, only the listener can finish. *)
let read_pastes (paste : Paste.t) key ~on_refused =
  let rec loop () =
    match paste.read_line () with
    | None -> Fiber.await_cancel ()
    | Some line when String.is_empty (String.strip line) -> loop ()
    | Some line ->
      (match Paste_code.open_ key ~now:(Time_ns.now ()) line with
       | Ok grant -> Ok grant
       | Error (Expired as e) -> Error (Paste_code.Error.to_string_hum e)
       | Error e ->
         on_refused (Paste_code.Error.to_string_hum e);
         loop ())
  in
  loop ()
;;

let run ~env ?state ?paste ?(on_refused = ignore) ~program ~on_url remote =
  let state = Option.value_or_thunk state ~default:random_state in
  let host = Backend_address.authority remote in
  Switch.run
  @@ fun sw ->
  let socket =
    Eio.Net.listen
      ~sw
      ~backlog:16
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  let port =
    match Eio.Net.listening_addr socket with
    | `Tcp (_, port) -> port
    | `Unix _ -> assert false
  in
  let finished, finish = Promise.create () in
  let is_finished = ref false in
  let route request ~reader flow =
    let response, result =
      if !is_finished
      then
        ( page
            ~status:410
            ~reason:"Gone"
            ~title:"Already done"
            "This sign-in is over: you can close this tab."
        , None )
      else (
        match answer ~state ~program ~host request ~reader with
        | `Continue response -> response, None
        | `Done (response, result) ->
          is_finished := true;
          response, Some result)
    in
    Web_server.respond flow response;
    Option.iter result ~f:(Promise.resolve finish)
  in
  let paste =
    Option.map paste ~f:(fun (paste : Paste.t) ->
      ( paste
      , Paste_code.create
          ?secret:paste.secret
          ~state
          ~expires:(Time_ns.add (Time_ns.now ()) Paste.lifetime)
          () ))
  in
  on_url
    ~url:
      (url
         ?paste:(Option.map paste ~f:(fun (paste, key) -> key, paste.from))
         remote
         ~program
         ~port
         ~state)
    ~port;
  (* Browsers open spare connections that may never send anything: each is
     served on its own, and whatever is left is dropped once it is done. *)
  Fiber.fork_daemon ~sw (fun () ->
    let rec accept () =
      let flow, _ = Eio.Net.accept ~sw socket in
      Fiber.fork_daemon ~sw (fun () ->
        (try
           Web_server.handle
             ~routes:(fun _ -> Some route)
             ~root:None
             ~websockets:[]
             ~on_lines:(fun ~read_line:_ ~write_line:_ -> ())
             flow
         with
         | Eio.Cancel.Cancelled _ as exn -> raise exn
         | _ -> ());
        Eio.Resource.close flow;
        `Stop_daemon);
      accept ()
    in
    accept ());
  match paste with
  | None -> Promise.await finished
  | Some (paste, key) ->
    Fiber.first
      (fun () -> Promise.await finished)
      (fun () -> read_pastes paste key ~on_refused)
;;

let run_on_terminal ~env ?state ~program ~say ?open_url ?paste ~why remote =
  run
    ~env
    ?state
    ~program
    ?paste
    ~on_refused:(fun error -> say (sprintf "%s. Paste the code:" error))
    remote
    ~on_url:(fun ~url ~port ->
      say
        (sprintf
           "%s: sign in through the browser: %s\n\
            %s\n\
            (Over SSH, forward the port to the browser's computer first: ssh \
            -L %d:127.0.0.1:%d ...)%s"
           why
           (match open_url with
            | Some _ -> "opening (if it does not open, open it yourself)"
            | None -> "open")
           url
           port
           port
           (match paste with
            | None -> " Waiting for the sign-in..."
            | Some _ -> "\nOr paste the code the page shows after Allow:"));
      Option.iter open_url ~f:(fun open_url -> open_url url))
;;
