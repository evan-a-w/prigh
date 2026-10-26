open! Core
open! Import

type t =
  { address : string option
  ; user : string option
  ; saved : string option
  }
[@@deriving sexp_of, equal]

let describe t = Option.value t.address ~default:"a local backend"

(* The backend's [User_access.unauthorised]; other "unauthorised:" errors are
   about acting as another user. *)
let is_rejection error =
  String.is_substring error ~substring:"unauthorised: bad user name or password"
;;

module Change = struct
  type t =
    | Connect of string option
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
    | Connected of P.Json.t
  [@@deriving sexp_of]

  type t =
    { link : link
    ; result : result
    }
  [@@deriving sexp_of]
end
