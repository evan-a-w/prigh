open! Core
open! Import

type t =
  | Single of Rpc_server.t
  | Namespaced of (Namespace.t * Rpc_server.t) list

let single server = Single server

let namespaced namespaces ~home ~legacy_auth_file ~create_server =
  Namespaced
    (List.map namespaces ~f:(fun namespace ->
       ( namespace
       , create_server
           namespace
           (Namespace.world namespace ~home ~legacy_auth_file) )))
;;

let lookup t ~token =
  match t with
  | Single server -> Option.some_if (Rpc_server.token_ok server token) server
  | Namespaced namespaces ->
    Option.bind token ~f:(fun token ->
      List.find_map namespaces ~f:(fun ((namespace : Namespace.t), server) ->
        Option.some_if (String.equal namespace.token token) server))
;;

let servers = function
  | Single server -> [ "", server ]
  | Namespaced namespaces ->
    List.map namespaces ~f:(fun ((namespace : Namespace.t), server) ->
      namespace.name, server)
;;

let member json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

(* The token of a [hello] request, and the request id to answer with. *)
let hello_token line =
  match Json.parse line with
  | Error _ -> `Null, None
  | Ok request ->
    let id = Option.value (member request "id") ~default:`Null in
    (match member request "method" with
     | Some (`String "hello") ->
       (match
          Option.bind (member request "params") ~f:(fun p -> member p "token")
        with
        | Some (`String token) -> id, Some token
        | _ -> id, None)
     | _ -> id, None)
;;

let rec first_line read_line =
  match read_line () with
  | None -> None
  | Some line when String.is_empty (String.strip line) -> first_line read_line
  | Some line -> Some line
;;

let serve_lines t ~read_line ~write_line =
  match t with
  | Single server -> Rpc_server.serve_lines server ~read_line ~write_line
  | Namespaced _ ->
    Option.iter (first_line read_line) ~f:(fun line ->
      let id, token = hello_token line in
      match
        Option.bind token ~f:(fun token -> lookup t ~token:(Some token))
      with
      | None ->
        write_line
          (Json.to_string
             (`Object
                 [ "type", `String "response"
                 ; "id", id
                 ; "ok", `False
                 ; "error", `String "unauthorised: bad or missing token"
                 ]))
      | Some server ->
        let pending = ref (Some line) in
        Rpc_server.serve_lines
          server
          ~read_line:(fun () ->
            match !pending with
            | Some line ->
              pending := None;
              Some line
            | None -> read_line ())
          ~write_line)
;;

let serve_connection t ~input ~output =
  let reader = Eio.Buf_read.of_flow input ~max_size:(64 * 1024 * 1024) in
  serve_lines
    t
    ~read_line:(fun () ->
      match Eio.Buf_read.line reader with
      | exception (End_of_file | Eio.Io _) -> None
      | line -> Some line)
    ~write_line:(fun line -> Eio.Flow.copy_string (line ^ "\n") output)
;;

let shutdown t =
  Fiber.List.iter (fun (_, server) -> Rpc_server.shutdown server) (servers t)
;;
