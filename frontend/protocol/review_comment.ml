open! Core

module Author = struct
  type t =
    | User
    | Agent
  [@@deriving sexp_of, equal]
end

module Edit = struct
  type t =
    { old_text : string
    ; new_text : string
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind old_text = Json.string_field j "old_text" in
    let%map new_text = Json.string_field j "new_text" in
    { old_text; new_text }
  ;;
end

module Proposal = struct
  module State = struct
    type t =
      | Pending
      | Accepted
      | Rejected
      | Failed of string
    [@@deriving sexp_of, equal]
  end

  type t =
    { edits : Edit.t list
    ; state : State.t
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind edits = Json.list_field j "edits" ~f:Edit.of_json in
    let%bind error = Json.string_opt_field j "error" in
    let%map state =
      match%bind Json.string_field j "state" with
      | "pending" -> Ok State.Pending
      | "accepted" -> Ok State.Accepted
      | "rejected" -> Ok State.Rejected
      | "failed" -> Ok (State.Failed (Option.value error ~default:""))
      | other -> Or_error.errorf "unknown proposal state %S" other
    in
    { edits; state }
  ;;
end

module Post = struct
  type t =
    { author : Author.t
    ; text : string
    ; at : Time_ns.t option
    ; proposal : Proposal.t option
    ; delivered : bool
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind author =
      match%bind Json.string_field j "author" with
      | "agent" -> Ok Author.Agent
      | _ -> Ok Author.User
    in
    let%bind text = Json.string_field j "text" in
    let%bind at = Json.time_ms_opt_field j "at_ms" in
    let%bind proposal =
      match Json.field j "proposal" with
      | None -> Ok None
      | Some p -> Proposal.of_json p >>| Option.some
    in
    let%map delivered = Json.bool_default_field j "delivered" ~default:true in
    { author; text; at; proposal; delivered }
  ;;
end

module Status = struct
  type t =
    | Open
    | Resolved
  [@@deriving sexp_of, equal]

  let to_string = function
    | Open -> "open"
    | Resolved -> "resolved"
  ;;
end

type t =
  { id : string
  ; root : string
  ; path : string
  ; first : int
  ; last : int
  ; anchor : string
  ; status : Status.t
  ; thread : Post.t list
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind root = Json.string_field j "root" in
  let%bind path = Json.string_field j "path" in
  let%bind first = Json.int_field j "first" in
  let%bind last = Json.int_field j "last" in
  let%bind anchor = Json.string_field j "anchor" in
  let%bind status =
    Json.string_field j "status"
    >>| function
    | "resolved" -> Status.Resolved
    | _ -> Status.Open
  in
  let%map thread = Json.list_field j "thread" ~f:Post.of_json in
  { id; root; path; first; last; anchor; status; thread }
;;

let unsent t =
  List.exists t.thread ~f:(fun p ->
    Author.equal p.author User && not p.delivered)
;;

let pending t =
  List.exists t.thread ~f:(fun p ->
    match p.proposal with
    | Some { state = Pending; _ } -> true
    | Some _ | None -> false)
;;
