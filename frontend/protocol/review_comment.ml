open! Core

module Side = struct
  type t =
    | Old
    | New
  [@@deriving sexp_of, equal]

  let to_string = function
    | Old -> "old"
    | New -> "new"
  ;;
end

module Columns = struct
  type t =
    { start_col : int
    ; end_col : int
    }
  [@@deriving sexp_of, equal]
end

module Range = struct
  type t =
    { first : int
    ; last : int
    ; side : Side.t
    ; columns : Columns.t option [@sexp.option]
    }
  [@@deriving sexp_of, equal]

  let lines ~first ~last = { first; last; side = New; columns = None }

  let label t =
    let lines =
      if t.first = t.last
      then sprintf "line %d" t.first
      else sprintf "lines %d–%d" t.first t.last
    in
    match t.side with
    | New -> lines
    | Old -> "old " ^ lines
  ;;

  let selected t ~anchor =
    match t.columns with
    | None -> anchor
    | Some { start_col; end_col } ->
      let lines = Array.of_list (String.split_lines anchor) in
      let n = Array.length lines in
      let clamp line i = Int.max 0 (Int.min i (String.length line)) in
      if n = 0
      then ""
      else if n = 1
      then (
        let line = lines.(0) in
        let start = clamp line start_col in
        String.sub line ~pos:start ~len:(Int.max 0 (clamp line end_col - start)))
      else
        String.concat
          ~sep:"\n"
          ((String.drop_prefix lines.(0) (clamp lines.(0) start_col)
            :: List.init (n - 2) ~f:(fun i -> lines.(i + 1)))
           @ [ String.prefix lines.(n - 1) (clamp lines.(n - 1) end_col) ])
  ;;

  let params t =
    [ "side", Json.str (Side.to_string t.side) ]
    @
    match t.columns with
    | None -> []
    | Some { start_col; end_col } ->
      [ "start_col", Json.int start_col; "end_col", Json.int end_col ]
  ;;

  let of_json j =
    let open Or_error.Let_syntax in
    let%bind first = Json.int_field j "first" in
    let%bind last = Json.int_field j "last" in
    let%bind side =
      Json.string_opt_field j "side"
      >>| function
      | Some "old" -> Side.Old
      | Some _ | None -> Side.New
    in
    let int_opt name =
      match Json.field j name with
      | None | Some `Null -> Ok None
      | Some _ -> Json.int_field j name >>| Option.some
    in
    let%bind start_col = int_opt "start_col" in
    let%map end_col = int_opt "end_col" in
    let columns =
      match start_col, end_col with
      | Some start_col, Some end_col -> Some { Columns.start_col; end_col }
      | _ -> None
    in
    { first; last; side; columns }
  ;;
end

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
  ; range : Range.t
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
  let%bind range = Range.of_json j in
  let%bind anchor = Json.string_field j "anchor" in
  let%bind status =
    Json.string_field j "status"
    >>| function
    | "resolved" -> Status.Resolved
    | _ -> Status.Open
  in
  let%map thread = Json.list_field j "thread" ~f:Post.of_json in
  { id; root; path; range; anchor; status; thread }
;;

let selected t = Range.selected t.range ~anchor:t.anchor

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
