open! Core

type t =
  { id : string
  ; path : string
  ; name : string option
  ; description : string option
  ; cwd : string
  ; created_at : string
  ; updated_at : string option
  ; first_prompt : string option
  ; message_count : int
  ; parent : string option
  ; live : bool (** loaded in the backend *)
  ; running : bool (** its turn, or background subagents or jobs *)
  ; background : int (** running background subagents and jobs *)
  ; clients : int (** frontends attached to it *)
  ; waiting : int (** confirmations and questions waiting for the user *)
  ; failed : bool (** its last reply ended in an error *)
  ; archived : bool
  ; pinned : bool option
    (** [None]: the backend cannot pin sessions (it predates [set_pinned]) *)
  ; bytes : int (** on disk *)
  ; host : string option (** the tool host its cwd is on, when recorded *)
  ; repo : Repo.t option
    (** where its cwd is in git; [None] outside a repository, or unknown *)
  ; worktrees : Session_worktree.t list
  ; home : Session_home.t option
    (** those that matter to it, not removed yet, by when they first did *)
  }
[@@deriving sexp_of, equal]

type summary = t

let of_json j =
  let open Or_error.Let_syntax in
  let%bind id = Json.string_field j "id" in
  let%bind path = Json.string_field j "path" in
  let%bind name = Json.string_opt_field j "name" in
  let%bind description = Json.string_opt_field j "description" in
  let%bind cwd = Json.string_field j "cwd" in
  let%bind created_at = Json.string_field j "created_at" in
  let%bind updated_at = Json.string_opt_field j "updated_at" in
  let%bind first_prompt = Json.string_opt_field j "first_prompt" in
  let%bind message_count = Json.int_field j "message_count" in
  let%bind parent = Json.string_opt_field j "parent" in
  let%bind host = Json.string_opt_field j "host" in
  let%bind repo =
    match Json.field j "repo" with
    | None | Some `Null -> Ok None
    | Some r -> Repo.of_json r >>| Option.some
  in
  let%bind worktrees =
    Json.optional_list_field j "worktrees" ~f:Session_worktree.of_json
  in
  let%map home = Session_home.field j in
  let opt_bool name =
    Result.ok (Json.bool_field j name) |> Option.value ~default:false
  in
  let opt_int name =
    Result.ok (Json.int_field j name) |> Option.value ~default:0
  in
  { id
  ; path
  ; name
  ; description
  ; cwd
  ; created_at
  ; updated_at
  ; first_prompt
  ; message_count
  ; parent
  ; live = opt_bool "live"
  ; running = opt_bool "running"
  ; background = opt_int "background"
  ; clients = opt_int "clients"
  ; waiting = opt_int "waiting"
  ; failed = opt_bool "failed"
  ; archived = opt_bool "archived"
  ; pinned = Result.ok (Json.bool_field j "pinned")
  ; bytes = opt_int "bytes"
  ; host
  ; repo
  ; worktrees
  ; home
  }
;;

let blurb t =
  match t.description, t.first_prompt with
  | Some text, _ | None, Some text ->
    String.concat ~sep:" " (String.split_lines text)
  | None, None -> "(empty)"
;;

let size_string bytes =
  let b = Float.of_int bytes in
  if bytes < 1024
  then sprintf "%d B" bytes
  else if Float.(b < 1024. * 1024.)
  then sprintf "%.1f KB" (b /. 1024.)
  else if Float.(b < 1024. * 1024. * 1024.)
  then sprintf "%.1f MB" (b /. (1024. *. 1024.))
  else sprintf "%.1f GB" (b /. (1024. *. 1024. *. 1024.))
;;

let total_bytes ts = List.sum (module Int) ts ~f:(fun t -> t.bytes)

module Change = struct
  type nonrec t =
    { changed : t list
    ; removed : string list
    }
  [@@deriving sexp_of, equal]

  let of_json j =
    let open Or_error.Let_syntax in
    match Json.field j "changed", Json.field j "removed" with
    | None, None -> Ok None
    | _ ->
      let%bind changed = Json.optional_list_field j "changed" ~f:of_json in
      let%map removed =
        Json.optional_list_field j "removed" ~f:(function
          | `String id -> Ok id
          | _ -> Or_error.error_string "removed: expected session ids")
      in
      Some { changed; removed }
  ;;

  (* Newest first, as [list_sessions] orders them: [updated_at] is the
     file's mtime in a fixed-width UTC format, so its order is time's. *)
  let insert sessions (s : summary) =
    let newer (o : summary) =
      match o.updated_at, s.updated_at with
      | Some o, Some s -> String.( >= ) o s
      | Some _, None -> true
      | None, Some _ -> false
      | None, None -> true
    in
    let before, after = List.split_while sessions ~f:newer in
    before @ (s :: after)
  ;;

  let apply { changed; removed } sessions =
    let gone =
      String.Set.of_list
        (removed @ List.map changed ~f:(fun (s : summary) -> s.id))
    in
    List.fold
      changed
      ~init:
        (List.filter sessions ~f:(fun (s : summary) -> not (Set.mem gone s.id)))
      ~f:insert
  ;;
end
