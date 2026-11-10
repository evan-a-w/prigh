open! Core
open! Import

type t =
  { address : string option
  ; user : string option
  ; saved : string option
  }
[@@deriving sexp_of, equal]

let describe t =
  Option.value_map
    t.address
    ~default:"a local backend"
    ~f:Server_address.display_string
;;

(* The backend's [User_access.unauthorised]; other "unauthorised:" errors are
   about acting as another user. *)
let is_rejection error =
  String.is_substring error ~substring:"unauthorised: bad user name or password"
;;

let proxy_signin_marker = "a sign-in proxy guards it: "
let proxy_signin_error why = proxy_signin_marker ^ why

let proxy_signin_of_error error =
  String.chop_prefix error ~prefix:proxy_signin_marker
;;

module Change = struct
  type t =
    | Connect of string option
    | Browser_signin
    | Sign_in of
        { user : string option
        ; token : (string[@sexp.opaque])
        }
  [@@deriving sexp_of, equal]
end

module Outcome = struct
  type link = t [@@deriving sexp_of]

  type result =
    | Unreachable of string
    | Rejected of string
    | Signin_required of string
    | Connected of P.Json.t
  [@@deriving sexp_of]

  type t =
    { link : link
    ; result : result
    }
  [@@deriving sexp_of]
end

module Code_prompt = struct
  type t =
    | Wanted
    | Refused of string
  [@@deriving sexp_of, equal]
end
