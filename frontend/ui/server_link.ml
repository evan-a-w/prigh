open! Core
open! Import

type t =
  { address : string option
  ; user : string option
  ; saved : string option
  }
[@@deriving sexp_of, equal]

let describe t = Option.value t.address ~default:"a local backend"

let parse_address text =
  let bad () =
    Error (sprintf "%S is not HOST:PORT (e.g. 127.0.0.1:7777)" text)
  in
  match String.rsplit2 text ~on:':' with
  | Some (host, port) when not (String.exists host ~f:Char.is_whitespace) ->
    (match Int.of_string_opt port with
     | Some port when port > 0 && port < 65536 ->
       Ok ((if String.is_empty host then "127.0.0.1" else host), port)
     | _ -> bad ())
  | _ -> bad ()
;;

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
