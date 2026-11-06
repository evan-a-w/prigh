open! Core
open! Import

module Result = struct
  type t =
    | Open
    | Signin_required of
        { why : string
        ; login : string option
        }
    | Unreachable of string
  [@@deriving sexp_of]
end

module Response = struct
  type t =
    { code : int
    ; status : string
    ; location : string option
    }
end

let read_head reader : Response.t =
  let status = Eio.Buf_read.line reader in
  let code =
    match String.split status ~on:' ' with
    | _ :: code :: _ -> Int.of_string_opt code
    | _ -> None
  in
  let rec headers location =
    match Eio.Buf_read.line reader with
    | "" -> location
    | line ->
      (match String.lsplit2 line ~on:':' with
       | Some (name, value)
         when String.Caseless.equal (String.strip name) "Location" ->
         headers (Some (String.strip value))
       | Some _ | None -> headers location)
  in
  let location = headers None in
  match code with
  | Some code -> { code; status; location }
  | None ->
    failwithf "the server answered %S, not HTTP" (String.prefix status 80) ()
;;

let get ~env (remote : Backend_address.t) ~target =
  Switch.run
  @@ fun sw ->
  let flow = Backend_address.connect ~env ~sw remote in
  Eio.Flow.copy_string
    (sprintf
       "GET %s HTTP/1.1\r\n\
        Host: %s\r\n\
        %sAccept: text/html\r\n\
        Connection: close\r\n\
        \r\n"
       target
       (Backend_address.authority remote)
       (String.concat
          (List.map (Backend_address.upgrade_headers remote) ~f:(fun (k, v) ->
             sprintf "%s: %s\r\n" k v))))
    flow;
  read_head (Eio.Buf_read.of_flow flow ~max_size:(1024 * 1024))
;;

let max_hops = 5

let rec probe ~env ~hops (remote : Backend_address.t) (url : Uri.t) =
  let target =
    match Uri.path_and_query url with
    | "" -> "/"
    | target -> target
  in
  let response = get ~env remote ~target in
  let here = Backend_address.authority remote in
  match response.code, response.location with
  | code, Some location when code >= 300 && code < 400 ->
    let next = Uri.resolve "" url (Uri.of_string location) in
    let host = Option.map (Uri.host next) ~f:String.lowercase in
    let scheme = Option.map (Uri.scheme next) ~f:String.lowercase in
    let same_host =
      [%equal: string option] host (Some (String.lowercase remote.host))
    in
    (match scheme with
     | Some (("http" | "https") as scheme) when same_host ->
       let tls = String.equal scheme "https" in
       if hops >= max_hops || ((not tls) && remote.tls)
       then Result.Open
       else
         probe
           ~env
           ~hops:(hops + 1)
           { remote with
             tls
           ; port =
               Option.value (Uri.port next) ~default:(if tls then 443 else 80)
           }
           next
     | _ ->
       Signin_required
         { why =
             sprintf
               "%s answered with a login page (%s to %s)"
               here
               response.status
               (Uri.to_string (Uri.with_query next []))
         ; login =
             Option.map (Uri.host next) ~f:(fun host ->
               match Uri.port next with
               | Some port -> sprintf "%s:%d" host port
               | None -> host)
         })
  | (401 | 403 | 407), _ ->
    Signin_required
      { why = sprintf "%s refused the page (%s)" here response.status
      ; login = None
      }
  | _ -> Open
;;

let run ~env ?(timeout = Time_ns.Span.of_sec 15.) (remote : Backend_address.t) =
  if remote.lines
  then Result.Open
  else (
    match
      Eio.Time.with_timeout_exn
        (Eio.Stdenv.clock env)
        (Time_ns.Span.to_sec timeout)
        (fun () ->
           probe
             ~env
             ~hops:0
             remote
             (Uri.of_string (Backend_address.web_url remote)))
    with
    | result -> result
    | exception (Eio.Cancel.Cancelled _ as exn) -> raise exn
    | exception Eio.Time.Timeout ->
      Unreachable
        (sprintf
           "no answer within %s"
           (Time_ns.Span.to_string_hum ~decimals:0 timeout))
    | exception Failure message -> Unreachable message
    | exception exn -> Unreachable (Net_error.to_string_hum exn))
;;
