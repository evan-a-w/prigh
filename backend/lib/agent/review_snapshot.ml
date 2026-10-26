open! Core
open! Import

module Action = struct
  type t =
    | Approved
    | Unapproved
    | Commented
    | Replied
    | Accepted
    | Rejected
    | Edited
  [@@deriving sexp, jsonaf, equal, enumerate]

  let to_string = function
    | Approved -> "approved"
    | Unapproved -> "unapproved"
    | Commented -> "commented"
    | Replied -> "replied"
    | Accepted -> "accepted"
    | Rejected -> "rejected"
    | Edited -> "edited"
  ;;
end

module Content = struct
  type t =
    | Text of { md5 : string }
    | Absent
    | Binary of { md5 : string }
    | Too_large of { bytes : int }
  [@@deriving sexp, jsonaf, equal]
end

type t =
  { root : string
  ; path : string
  ; action : Action.t
  ; by : string option [@sexp.option] [@jsonaf.option]
  ; at : float
  ; content : Content.t
  }
[@@deriving sexp, jsonaf, equal]

let max_bytes = 1024 * 1024
let md5 data = Md5.to_hex (Md5.digest_string data)

let content_of data ~size : Content.t * string option =
  match data with
  | None -> Absent, None
  | Some _ when size > max_bytes -> Too_large { bytes = size }, None
  | Some data when Tool_read.looks_binary data ->
    Binary { md5 = md5 data }, None
  | Some data -> Text { md5 = md5 data }, Some data
;;
