open! Core
open! Import

module Namespaced = struct
  type t =
    { access : User_access.t
    ; servers : (string * Rpc_server.t) list
    }
end

type t =
  | Single of Rpc_server.t
  | Namespaced of Namespaced.t

let single server = Single server

let namespaced access ~home ~legacy_auth_file ~create_server =
  Namespaced
    { access
    ; servers =
        List.map (User_access.namespaces access) ~f:(fun namespace ->
          ( namespace.name
          , create_server
              namespace
              (Namespace.world namespace ~home ~legacy_auth_file) ))
    }
;;

let authenticate t ?user ?as_user ~token () =
  match t with
  | Single server ->
    if Option.exists as_user ~f:(Fn.non String.is_empty)
    then Or_error.error_string User_access.no_users
    else if Rpc_server.credentials_ok server ?user token
    then Ok (server, None)
    else Or_error.error_string Rpc_server.unauthorised
  | Namespaced { access; servers } ->
    Or_error.map
      (User_access.authenticate access ?user ?as_user token)
      ~f:(fun (signed_in, user) ->
        List.Assoc.find_exn servers ~equal:String.equal user, Some signed_in)
;;

let lookup t ?user ?as_user ~token () =
  Result.ok (authenticate t ?user ?as_user ~token ()) |> Option.map ~f:fst
;;

let servers = function
  | Single server -> [ "", server ]
  | Namespaced { servers; _ } -> servers
;;

let member json name =
  match json with
  | `Object fields -> List.Assoc.find fields ~equal:String.equal name
  | _ -> None
;;

let rec first_line read_line =
  match read_line () with
  | None -> None
  | Some line when String.is_empty (String.strip line) -> first_line read_line
  | Some line -> Some line
;;

(* After [set_user] the connection continues with a [hello] to the new
   user's server that answers the [set_user] request: the first hello's
   client details, without its credentials or session (sessions are per
   user). *)
let switch_hello ~first_hello ~id =
  let params =
    match Option.bind first_hello ~f:(fun h -> member h "params") with
    | Some (`Object fields) ->
      List.filter fields ~f:(fun (name, _) ->
        not
          (List.mem
             [ "token"; "user"; "as_user"; "session" ]
             name
             ~equal:String.equal))
    | _ -> []
  in
  Json.to_string
    (`Object [ "id", id; "method", `String "hello"; "params", `Object params ])
;;

let rec serve_as
          servers
          ~signed_in
          ~first_hello
          ~read_line
          ~write_line
          ~first
          server
  =
  let pending = ref (Some first) in
  match
    Rpc_server.serve_lines
      server
      ~signed_in
      ~read_line:(fun () ->
        match !pending with
        | Some line ->
          pending := None;
          Some line
        | None -> read_line ())
      ~write_line
  with
  | None -> ()
  | Some (id, user) ->
    serve_as
      servers
      ~signed_in
      ~first_hello
      ~read_line
      ~write_line
      ~first:(switch_hello ~first_hello ~id)
      (List.Assoc.find_exn servers ~equal:String.equal user)
;;

let error_response id message =
  Json.to_string
    (`Object
        [ "type", `String "response"
        ; "id", id
        ; "ok", `False
        ; "error", `String message
        ])
;;

let serve_lines t ~read_line ~write_line =
  match t with
  | Single server ->
    (* No user to switch to: [set_user] fails before it can end this. *)
    ignore
      (Rpc_server.serve_lines server ~read_line ~write_line
       : (Json.t * string) option)
  | Namespaced { servers; _ } ->
    Option.iter (first_line read_line) ~f:(fun line ->
      let request = Result.ok (Json.parse line) in
      let id =
        Option.value
          (Option.bind request ~f:(fun r -> member r "id"))
          ~default:`Null
      in
      let hello =
        Option.filter request ~f:(fun r ->
          match member r "method" with
          | Some (`String "hello") -> true
          | _ -> false)
      in
      let param name =
        match
          Option.bind hello ~f:(fun h ->
            Option.bind (member h "params") ~f:(fun p -> member p name))
        with
        | Some (`String s) -> Some s
        | _ -> None
      in
      match hello with
      | None -> write_line (error_response id Rpc_server.unauthorised)
      | Some _ ->
        (match
           authenticate
             t
             ?user:(param "user")
             ?as_user:(param "as_user")
             ~token:(param "token")
             ()
         with
         | Error e -> write_line (error_response id (Error.to_string_hum e))
         | Ok (server, signed_in) ->
           serve_as
             servers
             ~signed_in:(Option.value_exn signed_in)
             ~first_hello:hello
             ~read_line
             ~write_line
             ~first:line
             server))
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
