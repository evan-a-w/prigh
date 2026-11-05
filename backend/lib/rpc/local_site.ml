open! Core
open! Import

let header headers name =
  List.Assoc.find headers ~equal:String.Caseless.equal name
  |> Option.map ~f:String.strip
;;

(* [host], [host:port] or [[v6]] / [[v6]:port]. *)
let split authority =
  let port = function
    | "" -> Some None
    | p ->
      (match String.chop_prefix p ~prefix:":" with
       | Some p -> Option.map (Int.of_string_opt p) ~f:Option.some
       | None -> None)
  in
  match String.chop_prefix authority ~prefix:"[" with
  | Some rest ->
    (match String.lsplit2 rest ~on:']' with
     | Some (host, rest) ->
       Option.map (port rest) ~f:(fun port -> `V6 host, port)
     | None -> None)
  | None ->
    (match String.lsplit2 authority ~on:':' with
     | Some (host, p) ->
       Option.map (port (":" ^ p)) ~f:(fun port -> `Name host, port)
     | None -> Some (`Name authority, None))
;;

let is_address host =
  match Core_unix.Inet_addr.of_string host with
  | (_ : Core_unix.Inet_addr.t) -> true
  | exception _ -> false
;;

let local_host ~port authority =
  match split authority with
  | Some ((`V6 host | `Name host), given_port) ->
    (String.Caseless.equal host "localhost" || is_address host)
    && Option.for_all given_port ~f:(Int.equal port)
  | None -> false
;;

let refuse ~status ~reason body =
  let response = Static_site.Response.text ~status ~reason body in
  Error
    { response with headers = [ "Content-Type", "text/plain; charset=utf-8" ] }
;;

let check ~port ~headers =
  match header headers "Host" with
  | Some host when local_host ~port host ->
    (match header headers "Origin" with
     | None -> Ok ()
     | Some origin when String.Caseless.equal origin ("http://" ^ host) -> Ok ()
     | Some _ ->
       refuse
         ~status:403
         ~reason:"Forbidden"
         "only the page this site served may use it: open prigh's page itself\n")
  | Some _ | None ->
    refuse
      ~status:421
      ~reason:"Misdirected Request"
      (sprintf
         "this site answers only at http://127.0.0.1:%d/ (or localhost, or \
          this machine's IP address): open it there\n"
         port)
;;
