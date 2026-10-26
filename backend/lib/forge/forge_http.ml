open! Core
open! Import

module Meth = struct
  type t =
    | Get
    | Post
  [@@deriving sexp_of, equal]
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
    ; body : string
    }
  [@@deriving sexp_of]
end

type t = ?cancel:Cancellation.t -> Request.t -> Response.t Or_error.t

let timeout = Time_ns.Span.of_sec 30.

let real ~env ?cancel ({ meth; url; headers; body } : Request.t) =
  let result =
    match meth with
    | Get -> Http_client.get ~env ?cancel ~timeout ~url ~headers ()
    | Post ->
      Http_client.post
        ~env
        ?cancel
        ~timeout
        ~url
        ~headers
        ~body:(Option.value body ~default:"")
        ()
  in
  match result with
  | Ok (response, body) -> Ok { Response.status = response.status; body }
  | Error Cancelled -> Auth_interaction.cancelled ()
  | Error e ->
    Or_error.errorf
      "%s: %s"
      (Uri.host_with_default ~default:url (Uri.of_string url))
      (Http_client.Error.to_string e)
;;
