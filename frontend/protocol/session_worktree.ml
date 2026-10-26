open! Core

module Source = struct
  type t =
    | Created
    | Entered
    | Pull_request of int
    | Agent of
        { agent : string
        ; task : string
        }
  [@@deriving sexp_of, equal]
end

type t =
  { path : string
  ; branch : string option
  ; root : string
  ; source : Source.t
  }
[@@deriving sexp_of, equal]

let of_json j =
  let open Or_error.Let_syntax in
  let%bind path = Json.string_field j "path" in
  let%bind branch = Json.string_opt_field j "branch" in
  let%bind root = Json.string_field j "root" in
  let%bind source = Json.string_field j "source" in
  let%map source : Source.t =
    match source with
    | "created" -> Ok Created
    | "entered" -> Ok Entered
    | "pull_request" ->
      let%map number = Json.int_field j "pr" in
      Source.Pull_request number
    | "agent" ->
      let%bind agent = Json.string_field j "agent" in
      let%map task = Json.string_field j "task" in
      Source.Agent { agent; task }
    | other -> Or_error.errorf "unknown worktree source %S" other
  in
  { path; branch; root; source }
;;

let label t =
  match t.branch with
  | Some branch -> branch
  | None -> Filename.basename t.path
;;

let contains t ~dir =
  String.equal t.path dir || String.is_prefix dir ~prefix:(t.path ^ "/")
;;
