open! Core

module Permissions = struct
  type t =
    { allow : string list
    ; ask : string list
    ; deny : string list
    }
  [@@deriving sexp_of, equal]

  let empty = { allow = []; ask = []; deny = [] }

  let of_json j =
    let open Or_error.Let_syntax in
    let list name =
      Json.optional_list_field j name ~f:Json.to_string_or_error
    in
    let%bind allow = list "allow" in
    let%bind ask = list "ask" in
    let%map deny = list "deny" in
    { allow; ask; deny }
  ;;

  let to_json t =
    let strings l = `Array (List.map l ~f:(fun s -> `String s)) in
    `Object
      [ "allow", strings t.allow; "ask", strings t.ask; "deny", strings t.deny ]
  ;;
end

type t =
  { scoped_models : string list
  ; approval_mode : Approval_mode.t
  ; permissions : Permissions.t
  ; default_model : string option
  ; default_thinking : string option
  ; fallback_models : string list
  ; default_cwd : string option
  ; worktrees_dir : string option
  ; archive_after_days : float
  ; notify : string
  ; snippets : Snippet.t list
  ; compact_at_tokens : int
  ; compaction_model : string option
  ; mask_observations : bool
  }
[@@deriving sexp_of, equal]

let default =
  { scoped_models = []
  ; approval_mode = Auto
  ; permissions = Permissions.empty
  ; default_model = None
  ; default_thinking = None
  ; fallback_models = []
  ; default_cwd = None
  ; worktrees_dir = None
  ; archive_after_days = 7.
  ; notify = "bell"
  ; snippets = []
  ; compact_at_tokens = 200_000
  ; compaction_model = None
  ; mask_observations = true
  }
;;

let of_json j =
  let open Or_error.Let_syntax in
  let strings name =
    match Json.field j name with
    | None -> Ok []
    | Some (`Array items) ->
      Or_error.all (List.map items ~f:Json.to_string_or_error)
    | Some _ -> Or_error.errorf "%s must be an array of strings" name
  in
  let%bind scoped_models = strings "scoped_models" in
  (* Older backends send [confirm_tools]. *)
  let%bind approval_mode =
    match Json.field j "approval_mode", Json.field j "confirm_tools" with
    | Some mode, _ -> Approval_mode.of_json mode
    | None, Some `True -> Ok Approval_mode.Ask
    | None, (None | Some `False) -> Ok Approval_mode.Auto
    | None, Some _ -> Or_error.error_string "confirm_tools must be a boolean"
  in
  let%bind permissions =
    match Json.field j "permissions" with
    | None -> Ok Permissions.empty
    | Some p -> Permissions.of_json p
  in
  let optional_string name =
    match Json.field j name with
    | None | Some `Null -> Ok None
    | Some (`String s) -> Ok (Some s)
    | Some _ -> Or_error.errorf "%s must be a string" name
  in
  let%bind default_model = optional_string "default_model" in
  let%bind default_thinking = optional_string "default_thinking" in
  let%bind fallback_models = strings "fallback_models" in
  let%bind default_cwd = optional_string "default_cwd" in
  let%bind worktrees_dir = optional_string "worktrees_dir" in
  let%bind archive_after_days =
    match Json.field j "archive_after_days" with
    | None | Some `Null -> Ok default.archive_after_days
    | Some (`Number n) ->
      Or_error.try_with (fun () -> Float.of_string n)
      |> Or_error.tag ~tag:"archive_after_days must be a number"
    | Some _ -> Or_error.error_string "archive_after_days must be a number"
  in
  let%bind notify = optional_string "notify" in
  let%bind compact_at_tokens =
    match Json.field j "compact_at_tokens" with
    | None | Some `Null -> Ok default.compact_at_tokens
    | Some (`Number n) ->
      Or_error.try_with (fun () -> Int.of_string n)
      |> Or_error.tag ~tag:"compact_at_tokens must be a whole number"
    | Some _ -> Or_error.error_string "compact_at_tokens must be a whole number"
  in
  let%bind compaction_model = optional_string "compaction_model" in
  let%bind mask_observations =
    match Json.field j "mask_observations" with
    | None | Some `Null -> Ok default.mask_observations
    | Some `True -> Ok true
    | Some `False -> Ok false
    | Some _ -> Or_error.error_string "mask_observations must be a boolean"
  in
  let%map snippets = Json.optional_list_field j "snippets" ~f:Snippet.of_json in
  { scoped_models
  ; approval_mode
  ; permissions
  ; default_model
  ; default_thinking
  ; fallback_models
  ; default_cwd
  ; worktrees_dir
  ; archive_after_days
  ; notify = Option.value notify ~default:"bell"
  ; snippets
  ; compact_at_tokens
  ; compaction_model
  ; mask_observations
  }
;;

let to_json t =
  let optional = Option.value_map ~default:`Null ~f:(fun s -> `String s) in
  let strings l = `Array (List.map l ~f:(fun s -> `String s)) in
  `Object
    ([ "scoped_models", strings t.scoped_models
     ; "approval_mode", `String (Approval_mode.to_string t.approval_mode)
     ; "permissions", Permissions.to_json t.permissions
     ; "default_model", optional t.default_model
     ; "default_thinking", optional t.default_thinking
     ; "fallback_models", strings t.fallback_models
     ; "default_cwd", optional t.default_cwd
     ]
     @ Option.value_map t.worktrees_dir ~default:[] ~f:(fun dir ->
       [ "worktrees_dir", `String dir ])
     @ [ "archive_after_days", `Number (sprintf "%g" t.archive_after_days)
       ; "notify", `String t.notify
       ; "snippets", Snippet.list_to_json t.snippets
       ; "compact_at_tokens", `Number (Int.to_string t.compact_at_tokens)
       ; "compaction_model", optional t.compaction_model
       ; ("mask_observations", if t.mask_observations then `True else `False)
       ])
;;
