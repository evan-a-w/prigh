open! Core
open! Import

module Meth = struct
  type t =
    | Get
    | Post
    | Put
    | Patch
    | Delete
  [@@deriving sexp_of, equal]

  let to_string = function
    | Get -> "GET"
    | Post -> "POST"
    | Put -> "PUT"
    | Patch -> "PATCH"
    | Delete -> "DELETE"
  ;;

  let to_http : t -> Http.Method.t = function
    | Get -> `GET
    | Post -> `POST
    | Put -> `PUT
    | Patch -> `PATCH
    | Delete -> `DELETE
  ;;
end

module Request = struct
  type t =
    { meth : Meth.t
    ; url : string
    ; headers : (string * string) list
    ; body : string option
    }
  [@@deriving sexp_of]
end

module Response = struct
  type t =
    { status : int
    ; headers : (string * string) list
    ; body : string
    }
  [@@deriving sexp_of]

  let header t name =
    List.find_map t.headers ~f:(fun (k, v) ->
      Option.some_if (String.Caseless.equal k name) v)
  ;;
end

type t = ?cancel:Cancellation.t -> Request.t -> Response.t Or_error.t

let timeout = Time_ns.Span.of_sec 30.

let real ~env ?cancel ({ meth; url; headers; body } : Request.t) =
  let body =
    match meth, body with
    | Get, _ -> None
    | (Post | Put | Patch), None -> Some ""
    | _, body -> body
  in
  match
    Http_client.request
      ~env
      ?cancel
      ~timeout
      ~meth:(Meth.to_http meth)
      ~url
      ~headers
      ?body
      ()
  with
  | Ok (response, body) ->
    Ok { Response.status = response.status; headers = response.headers; body }
  | Error Cancelled -> Auth_interaction.cancelled ()
  | Error e ->
    Or_error.errorf
      "%s: %s"
      (Uri.host_with_default ~default:url (Uri.of_string url))
      (Http_client.Error.to_string e)
;;
