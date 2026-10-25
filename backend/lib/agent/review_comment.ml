open! Core
open! Import

module Range = struct
  type t =
    { first : int
    ; last : int
    }
  [@@deriving sexp, jsonaf, equal]

  let to_string t =
    if t.first = t.last
    then Int.to_string t.first
    else sprintf "%d-%d" t.first t.last
  ;;
end

module Author = struct
  type t =
    | User
    | Agent
  [@@deriving sexp, jsonaf, equal]
end

module Edit = struct
  type t =
    { old_text : string
    ; new_text : string
    }
  [@@deriving sexp, jsonaf, equal]
end

module Proposal = struct
  module State = struct
    type t =
      | Pending
      | Accepted
      | Rejected
      | Failed of string
    [@@deriving sexp, jsonaf, equal]

    let to_string = function
      | Pending -> "pending"
      | Accepted -> "accepted"
      | Rejected -> "rejected"
      | Failed _ -> "failed"
    ;;
  end

  type t =
    { edits : Edit.t list
    ; state : State.t
    }
  [@@deriving sexp, jsonaf, equal]
end

module Post = struct
  type t =
    { author : Author.t
    ; text : string
    ; at : float
    ; proposal : Proposal.t option [@jsonaf.option]
    ; delivered : bool
    }
  [@@deriving sexp, jsonaf, equal]
end

module Status = struct
  type t =
    | Open
    | Resolved
  [@@deriving sexp, jsonaf, equal]

  let to_string = function
    | Open -> "open"
    | Resolved -> "resolved"
  ;;

  let of_string = function
    | "open" -> Ok Open
    | "resolved" -> Ok Resolved
    | s -> Or_error.errorf "unknown status %S: open or resolved" s
  ;;
end

type t =
  { id : string
  ; root : string
  ; path : string
  ; range : Range.t
  ; anchor : string
  ; status : Status.t
  ; thread : Post.t list
  }
[@@deriving sexp, jsonaf, equal]

let file t = Filename.concat t.root t.path

let unsent t =
  List.exists t.thread ~f:(fun (p : Post.t) ->
    Author.equal p.author User && not p.delivered)
;;

let numbered t =
  let lines = String.split_lines t.anchor in
  let width =
    String.length (Int.to_string (t.range.first + List.length lines))
  in
  List.mapi lines ~f:(fun i line ->
    sprintf "%*d  %s" width (t.range.first + i) line)
  |> String.concat ~sep:"\n"
;;

let post_text (p : Post.t) =
  let who =
    match p.author with
    | User -> "user"
    | Agent -> "you"
  in
  let attrs =
    (if p.delivered then "" else " new=\"true\"")
    ^
    match p.proposal with
    | Some { state; _ } ->
      sprintf " proposal=\"%s\"" (Proposal.State.to_string state)
    | None -> ""
  in
  sprintf "<post from=\"%s\"%s>\n%s\n</post>" who attrs (String.strip p.text)
;;

let to_prompt t =
  String.concat
    ~sep:"\n"
    ([ sprintf
         "<comment id=\"%s\" file=\"%s\" lines=\"%s\" status=\"%s\">"
         t.id
         (file t)
         (Range.to_string t.range)
         (Status.to_string t.status)
     ; "<code>"
     ; numbered t
     ; "</code>"
     ]
     @ List.map t.thread ~f:post_text
     @ [ "</comment>" ])
;;
